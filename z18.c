/*@z18.c:Galley Transfer:Declarations@****************************************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.02)                       */
/*  COPYRIGHT (C) 1994 Jeffrey H. Kingston                                   */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@cs.su.oz.au)                                   */
/*  Basser Department of Computer Science                                    */
/*  The University of Sydney 2006                                            */
/*  AUSTRALIA                                                                */
/*                                                                           */
/*  This program is free software; you can redistribute it and/or modify     */
/*  it under the terms of the GNU General Public License as published by     */
/*  the Free Software Foundation; either version 1, or (at your option)      */
/*  any later version.                                                       */
/*                                                                           */
/*  This program is distributed in the hope that it will be useful,          */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/*  GNU General Public License for more details.                             */
/*                                                                           */
/*  You should have received a copy of the GNU General Public License        */
/*  along with this program; if not, write to the Free Software              */
/*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.                */
/*                                                                           */
/*  FILE:         z18.c                                                      */
/*  MODULE:       Galley Transfer                                            */
/*  EXTERNS:      TransferInit(), TransferBegin(), TransferComponent(),      */
/*                TransferEnd(), TransferClose()                             */
/*                                                                           */
/*****************************************************************************/
#include "externs"

#define	MAX_DEPTH  	30			/* max depth of galleys      */
static OBJECT		root_galley = nil;	/* the root galley           */
static OBJECT		targets[MAX_DEPTH];	/* currently open \Inputs    */
static CONSTRAINT	constraints[MAX_DEPTH];	/* their COL constraints     */
static int		itop;			/* stack top	             */
static CONSTRAINT	initial_constraint;	/* initial COL constraint    */
static STYLE		InitialStyle;		/* initial style             */

#if DEBUG_ON
static debug_targets()
{ int i;  OBJECT tmp;
  for( i = 0;  i <= itop;  i++ )
  { if( targets[i] == nil || Down(targets[i]) == targets[i] )  tmp = nil;
    else Child(tmp, Down(targets[i]));
    debug3(DGT, D, "  target[%d] %s = %s", i,
      EchoConstraint(&constraints[i]), EchoObject(tmp));
  }
} /* end debug_targets */
#endif


/*@::TransferInit()@**********************************************************/
/*                                                                           */
/*  TransferInit(InitEnv)                                                    */
/*                                                                           */
/*  Initialise this module.  The initial environment is InitEnv.             */
/*                                                                           */
/*****************************************************************************/

TransferInit(InitEnv)
OBJECT InitEnv;
{ OBJECT dest, x, y, recs, inners, nothing, dest_index, up_hd;
  debug1(DGT, D, "TransferInit( %s )", EchoObject(InitEnv));
  SetConstraint(initial_constraint, MAX_LEN-1, MAX_LEN-1, MAX_LEN-1);

  /* set initial environment and style */
  SetGap(line_gap(InitialStyle),  FALSE, FALSE, FIXED_UNIT, MARK_MODE, 18*PT);
  SetGap(space_gap(InitialStyle), FALSE, TRUE,  FIXED_UNIT, EDGE_MODE,  1*EM);
  font(InitialStyle)            = 0;			/* i.e. undefined    */
  colour(InitialStyle)          = 0;			/* i.e. undefined    */
  language(InitialStyle)        = 0;			/* i.e. undefined    */
  hyph_style(InitialStyle)      = HYPH_UNDEF;
  fill_style(InitialStyle)      = FILL_UNDEF;
  display_style(InitialStyle)   = DISPLAY_UNDEF;

  /* construct destination for root galley */
  up_hd = New(HEAD);
  dest_index = New(RECEIVING);
  dest = New(CLOSURE);  actual(dest) = PrintSym;
  actual(dest_index) = dest;
  external(dest) = TRUE;
  threaded(dest) = FALSE;
  blocked(dest_index) = FALSE;
  Link(up_hd, dest_index);

  /* construct root galley */
  root_galley = New(HEAD);
  FposCopy(fpos(root_galley), *no_fpos);
  actual(root_galley) = whereto(root_galley) = ready_galls(root_galley) = nil;
  backward(root_galley) = must_expand(root_galley) = sized(root_galley) =FALSE;
  x = New(CLOSURE);  actual(x) = InputSym;
  Link(root_galley, x);
  SizeGalley(root_galley, InitEnv, TRUE, FALSE, FALSE, FALSE, &InitialStyle,
		&initial_constraint, nil, &nothing, &recs, &inners);
  assert( recs   == nil , "TransferInit: recs   != nil!" );
  assert( inners == nil , "TransferInit: inners != nil!" );
  Link(dest_index, root_galley);

  /* initialise target and constraint stacks */
  Child(y, Down(root_galley));
  assert( type(y) == RECEPTIVE && type(actual(y)) == CLOSURE &&
	actual(actual(y)) == InputSym, "TransferInit: initial galley!" );
  assert( external(actual(y)), "TransferInit: input sym not external!" );
  blocked(y) = TRUE;
  targets[itop = 0] = New(ACAT);
  Link(targets[itop], y);
  Constrained(actual(y), &constraints[itop], COL);
  debug2(DSC, D, "Constrained( %s, COL ) = %s",
	EchoObject(y), EchoConstraint(&constraints[itop]));

  debug0(DGT, D, "TransferInit returning.");
  ifdebug(DGT, DD, debug_targets());
} /* end TransferInit */


/*@::TransferBegin()@*********************************************************/
/*                                                                           */
/*  OBJECT TransferBegin(x)                                                  */
/*                                                                           */
/*  Commence the transfer of a new galley whose header is invocation x.      */
/*                                                                           */
/*****************************************************************************/

OBJECT TransferBegin(x)
OBJECT x;
{ OBJECT xsym, index, y, link, env, new_env, hold_env, res, hd, target;
  CONSTRAINT c;
  debug1(DGT, D, "TransferBegin( %s )", EchoObject(x));
  ifdebug(DGT, DD, debug_targets());
  assert( type(x) == CLOSURE, "TransferBegin: non-CLOSURE!" );

  /* add an automatically generated @Tag parameter to x if required */
  if( has_tag(actual(x)) )  CrossAddTag(x);

  /* construct new (inner) env chain */
  if( Down(targets[itop]) == targets[itop] )
    Error(18, 1, "cannot attach galley %s", FATAL,&fpos(x),SymName(actual(x)));
  Child(target, Down(targets[itop]));
  xsym = actual(x);
  env = GetEnv(actual(target));
  debug1(DGT, DD, "  current env chain: %s", EchoObject(env));
  if( has_body(xsym) )
  {
    /* prepare a copy of x for inclusion in environment */
    y = CopyObject(x, no_fpos);

    /* attach its environment */
    AttachEnv(env, y);

    /* now the new environment is y catenated with the old one */
    debug0(DCR, DD, "calling SetEnv from TransferBegin (a)");
    new_env = SetEnv(y, nil);
  }
  else new_env = env;
  hold_env = New(ACAT);  Link(hold_env, new_env);
  debug1(DGT, DD, "  new env chain: %s", EchoObject(new_env));

  /* convert x into an unsized galley called hd */
  index = New(UNATTACHED);
  hd = New(HEAD);
  FposCopy(fpos(hd), fpos(x));
  actual(hd) = xsym;
  backward(hd) = TargetSymbol(x, &whereto(hd));
  ready_galls(hd) = nil;
  must_expand(hd) = TRUE;
  sized(hd) = FALSE;
  Link(index, hd);
  Link(hd, x);
  AttachEnv(env, x);

  /* search for destination for hd and release it */
  Link(Up(target), index);
  debug0(DGF,D, "");
  debug1(DGF,D, "  calling FlushGalley(%s) from TransferBegin, root_galley =",
    SymName(actual(hd)));
  ifdebug(DGF, D, DebugGalley(root_galley, 4));
  if( whereto(hd) == nil || !uses_extern_target(whereto(hd)) ) /* &&& */
    FlushGalley(hd);

  /* if failed to flush, undo everything and exit */
  Parent(index, Up(hd));
  if( type(index) == UNATTACHED && !sized(hd) )
  { DeleteNode(index);
    DisposeObject(hold_env);
    if( LastDown(x) != x )
    { Child(env, LastDown(x));
      if( type(env) == ENV )  DisposeChild(LastDown(x));
    }
    debug1(DGT,D, "TransferBegin returning failed, x: %s", EchoObject(x));
    return x;
  }

  if( has_rpar(actual(hd)) )
  {
    /* set up new target to be inner \InputSym, or nil if none */
    if( ++itop >= MAX_DEPTH )
      Error(18, 2, "galley nested too deeply (max is %d)",
	FATAL, &fpos(x), MAX_DEPTH);
    targets[itop] = New(ACAT);  target = nil;
    for( link = Down(hd);  link != hd;  link = NextDown(link) )
    { Child(y, link);
      if( type(y) == RECEPTIVE && actual(actual(y)) == InputSym )
      {
	Constrained(actual(y), &constraints[itop], COL);
	if( FitsConstraint(0, 0, constraints[itop]) )
	{ Link(targets[itop], y);  target = y;
	  debug2(DSC, D, "Constrained( %s, COL ) = %s",
	    EchoObject(y), EchoConstraint(&constraints[itop]));
	  env = DetachEnv(actual(y));
	  AttachEnv(new_env, actual(y));
	}
	else
	{ Error(18, 3, "galley %s deleted (insufficient width at target)",
	    WARN, &fpos(hd), SymName(actual(hd)));
	}
	break;
      }
    }

    /* return a token appropriate to the new target */
    if( target == nil || external(actual(target)) )
      res = NewToken(GSTUB_EXT, no_fpos, 0, 0, precedence(xsym), nil);
    else
    { Constrained(actual(target), &c, ROW);
      if( constrained(c) )
	Error(18, 4, "right parameter of %s is vertically constrained",
	  FATAL, &fpos(target), SymName(xsym));
      else res = NewToken(GSTUB_INT, no_fpos, 0, 0, precedence(xsym), nil);
    }
  }
  else res = NewToken(GSTUB_NONE, no_fpos, 0, 0, precedence(xsym), nil);

  DisposeObject(hold_env);
  debug1(DGT, D, "TransferBegin returning %s", Image(type(res)));
  ifdebug(DGT, DD, debug_targets());
  return res;
} /* end TransferBegin */

/*@::TransferComponent()@*****************************************************/
/*                                                                           */
/*  TransferComponent(x)                                                     */
/*                                                                           */
/*  Transfer component x of a galley.                                        */
/*                                                                           */
/*****************************************************************************/

TransferComponent(x)
OBJECT x;
{ OBJECT y, env, start_search, recs, inners, nothing, hd, dest, dest_index;
  debug1(DGT, D, "TransferComponent( %s )", EchoObject(x));
  ifdebug(DGT, DD, debug_targets());

  /* if no dest_index, discard x and exit */
  if( Down(targets[itop]) == targets[itop] )
  { DisposeObject(x);
    debug0(DGT, D, "TransferComponent returning (no target).");
    return;
  }
  Child(dest_index, Down(targets[itop]));
  assert( external(actual(dest_index)), "TransferComponent: internal!" );

  /* make the component into a galley */
  hd = New(HEAD);
  FposCopy(fpos(hd), fpos(x));
  actual(hd) = whereto(hd) = ready_galls(hd) = nil;
  backward(hd) = must_expand(hd) = sized(hd) = FALSE;
  Link(hd, x);
  dest = actual(dest_index);
  env = GetEnv(dest);
  debug1(DGT, DD, "  current env chain: %s", EchoObject(env));
  SizeGalley(hd, env, TRUE, threaded(dest), FALSE, TRUE, &save_style(dest),
	&constraints[itop], nil, &nothing, &recs, &inners);
  if( recs != nil )  ExpandRecursives(recs);

  /* promote the components, remembering where old spot was */
  start_search = PrevDown(Up(dest_index));
  debug0(DSA, D, "  calling AdjustSize from TransferComponent");
  AdjustSize(dest, back(hd, COL), fwd(hd, COL), COL);
  Promote(hd, hd, dest_index);
  DeleteNode(hd);

  /* flush any widowed galleys attached to \Input */
  if( Down(dest_index) != dest_index )
  { OBJECT tinners, index;
    tinners = New(ACAT);
    while( Down(dest_index) != dest_index )
    { Child(y, Down(dest_index));
      assert( type(y) == HEAD, "TransferComponent: input child!" );
      DetachGalley(y);
      Parent(index, Up(y));
      MoveLink(Up(index), NextDown(start_search), PARENT);
      Link(tinners, index);
    }
    FlushInners(tinners, nil);
  }

  /* flush any galleys inside hd */
  if( inners != nil )  FlushInners(inners, nil);

  /* flush parent galley, if needed */
  if( blocked(dest_index) )
  { blocked(dest_index) = FALSE;
    Parent(y, Up(dest_index));
    debug0(DGF, D, "  calling FlushGalley from TransferComponent");
    FlushGalley(y);
  }
  
  debug0(DGT, D, "TransferComponent returning.");
  ifdebug(DGT, DD, debug_targets());
} /* end TransferComponent */


/*@::TransferEnd()@***********************************************************/
/*                                                                           */
/*  TransferEnd(x)                                                           */
/*                                                                           */
/*  End the transfer of a galley.                                            */
/*                                                                           */
/*****************************************************************************/

TransferEnd(x)
OBJECT x;
{ OBJECT recs, inners, nothing, z, env, dest, hd, dest_index, y, start_search;
  debug1(DGT, D, "TransferEnd( %s )", EchoObject(x));
  ifdebug(DGT, DD, debug_targets());

  /* if no dest_index, discard x and exit */
  if( Down(targets[itop]) == targets[itop] )
  { DisposeObject(x);  DisposeObject(targets[itop--]);
    debug0(DGT, D, "TransferEnd returning: no dest_index");
    return;
  }
  Child(dest_index, Down(targets[itop]));

  /* make the component into a galley */
  hd = New(HEAD);  FposCopy(fpos(hd), fpos(x));
  actual(hd) = whereto(hd) = ready_galls(hd) = nil;
  backward(hd) = must_expand(hd) = sized(hd) = FALSE;
  Link(hd, x);  dest = actual(dest_index);  env = GetEnv(dest);
  debug1(DGT, DD, "  current env chain: %s", EchoObject(env));
  SizeGalley(hd, env, external(dest), threaded(dest), FALSE, TRUE,
	&save_style(dest), &constraints[itop], nil, &nothing, &recs, &inners);
  if( recs != nil )  ExpandRecursives(recs);

  /* promote the components, remembering where old spot was */
  start_search = PrevDown(Up(dest_index));
  debug0(DSA, D, "calling AdjustSize from TransferEnd (a)");
  AdjustSize(dest, back(hd, COL), fwd(hd, COL), COL);
  if( !external(dest) )
  { Child(z, LastDown(hd));
    debug0(DSA, D, "calling AdjustSize from TransferEnd (b)");
    AdjustSize(dest, back(z, ROW), fwd(z, ROW), ROW);
    Interpose(dest, VCAT, hd, z);
  }
  Promote(hd, hd, dest_index);  DeleteNode(hd);

  /* flush any widowed galleys attached to \Input */
  if( Down(dest_index) != dest_index )
  { OBJECT tinners, index;
    tinners = New(ACAT);
    while( Down(dest_index) != dest_index )
    { Child(y, Down(dest_index));
      assert( type(y) == HEAD, "TransferComponent: input child!" );
      DetachGalley(y);
      Parent(index, Up(y));
      MoveLink(Up(index), NextDown(start_search), PARENT);
      Link(tinners, index);
    }
    FlushInners(tinners, nil);
  }

  /* flush any galleys inside hd */
  if( inners != nil )  FlushInners(inners, nil);

  /* close dest_index, and flush parent galley if needed */
  if( blocked(dest_index) )
  { Parent(y, Up(dest_index));
    DeleteNode(dest_index);
    debug0(DGF, D, "  calling FlushGalley from TransferEnd");
    FlushGalley(y);
  }
  else DeleteNode(dest_index);
  
  /* pop target stack and exit */
  DisposeObject(targets[itop--]);
  debug0(DGT, D, "TransferEnd returning.");
  ifdebug(DGT, DD, debug_targets());
} /* end TransferEnd */

/*@::TransferClose()@*********************************************************/
/*                                                                           */
/*  TransferClose()                                                          */
/*                                                                           */
/*  Close this module.                                                       */
/*                                                                           */
/*****************************************************************************/

TransferClose()
{ OBJECT inners;
  debug0(DGT, D, "TransferClose()");
  debug0(DGA, D, "  calling FreeGalley from TransferClose");
  if( LastDown(root_galley) != root_galley )
  { inners = nil;
    FreeGalley(root_galley, root_galley, &inners, nil, nil);
    if( inners != nil )  FlushInners(inners, nil);
    debug0(DGF, D, "  calling FlushGalley from TransferClose");
    FlushGalley(root_galley);
  }
  debug0(DGT, D, "TransferClose returning.");
}
