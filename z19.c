/*@z19.c:Galley Attaching:DetachGalley()@*************************************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.06)                       */
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
/*  FILE:         z19.c                                                      */
/*  MODULE:       Galley Attaching                                           */
/*  EXTERNS:      SearchGalley(), AttachGalley(), DetachGalley()             */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  OBJECT InterposeVScale(y, scale_factor)                                  */
/*                                                                           */
/*  Interpose a @VScale symbol above y with the given scale factor.          */
/*                                                                           */
/*****************************************************************************/

static OBJECT InterposeVScale(OBJECT y, int scale_factor)
{ OBJECT res;
  res = New(VSCALE);
  FposCopy(fpos(res), fpos(y));
  back(res, COL) = back(y, COL);
  fwd(res, COL)  = fwd(y, COL);
  back(res, ROW) = (back(y, ROW) * scale_factor) / SF;
  fwd(res, ROW)  = (fwd(y, ROW) * scale_factor) / SF;
  ReplaceNode(res, y);
  Link(res, y);
  return res;
} /* end InterposeVScale */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT InterposeHigh(y)                                                  */
/*                                                                           */
/*  Interpose a @High symbol above y with the same size as y, with a value   */
/*  which prevents any further increase in the size of y.                    */
/*                                                                           */
/*****************************************************************************/

static OBJECT InterposeHigh(OBJECT y)
{ OBJECT res;
  res = New(HIGH);
  FposCopy(fpos(res), fpos(y));
  back(res, COL) = back(y, COL);
  fwd(res, COL)  = fwd(y, COL);
  back(res, ROW) = back(y, ROW);
  fwd(res, ROW)  = fwd(y, ROW);
  SetConstraint(constraint(res), MAX_LEN, size(res, ROW), MAX_LEN);
  ReplaceNode(res, y);
  Link(res, y);
  return res;
} /* end InterposeHigh */


/*****************************************************************************/
/*                                                                           */
/*  DetachGalley(hd)                                                         */
/*                                                                           */
/*  Detach galley hd from its target.                                        */
/*                                                                           */
/*****************************************************************************/

void DetachGalley(OBJECT hd)
{ OBJECT prnt, index;
  assert( type(hd) == HEAD && Up(hd) != hd, "DetachGalley: precondition!" );
  debug1(DGA, D, "DetachGalley( %s )", SymName(actual(hd)));
  Parent(prnt, Up(hd));
  assert( Up(prnt) != prnt, "DetachGalley: parent!" );
  index = New(UNATTACHED);
  pinpoint(index) = nilobj;
  MoveLink(Up(hd), index, PARENT);
  Link(NextDown(Up(prnt)), index);
  debug0(DGA, D, "DetachGalley returning.");
} /* end DetachGalley */


/*@::SearchGalley()@**********************************************************/
/*                                                                           */
/*  OBJECT SearchGalley(start, sym, forwards, subgalleys, closures, input)   */
/*                                                                           */
/*  Search a galley and its sub-galleys for a target which uses sym.  The    */
/*  meanings of the flags are as follows:                                    */
/*                                                                           */
/*    forwards     If TRUE, search forwards from just after start, else      */
/*                 search backwards from just before start                   */
/*    subgalleys   If TRUE, search down into sub-galleys of this galley      */
/*    closures     If TRUE, closures in this galley are acceptable results   */
/*    input        If TRUE, InputSym is an acceptable result                 */
/*                                                                           */
/*****************************************************************************/

OBJECT SearchGalley(OBJECT start, OBJECT sym, BOOLEAN forwards,
BOOLEAN subgalleys, BOOLEAN closures, BOOLEAN input)
{ OBJECT y, res, z, zlink, link;
  debug5(DGA, DDD, "[ SearchGalley(start, %s, %s, %s, %s, %s)", SymName(sym),
	forwards ? "fwd" : "back", subgalleys ? "subgalleys" : "nosubgalleys",
	closures ? "closures" : "noclosures", input ? "input" : "noinput");
  assert( type(start) == LINK || type(start) == HEAD, "SearchGalley: start!" );

  link = forwards ? NextDown(start) : PrevDown(start);
  res = nilobj;
  while( res == nilobj && type(link) != HEAD )
  { Child(y, link);
    switch( type(y) )
    {
      case UNATTACHED:
      case RECEIVING:
	
        debug1(DGA, DDD, "  examining %s", EchoIndex(y));
	if( subgalleys )
	for( zlink = Down(y); zlink!=y && res==nilobj; zlink=NextDown(zlink) )
	{ Child(z, zlink);
	  res = SearchGalley(z, sym, TRUE, TRUE, TRUE, input);
	}
	if( !res && input && type(y)==RECEIVING && actual(actual(y))==InputSym )
	  res = y;
	break;


      case RECEPTIVE:
	
        debug1(DGA, DDD, "  examining %s", EchoIndex(y));
	if( closures && type(actual(y)) == CLOSURE
		     && SearchUses(actual(actual(y)), sym) )  res = y;
	else if( input && actual(actual(y)) == InputSym )  res = y;
	break;


      default:
	
	break;

    }
    link = forwards ? NextDown(link) : PrevDown(link);
  }
  debug1(DGA, DDD, "] SearchGalley returning %s", EchoIndex(res));
  return res;
} /* end SearchGalley */


/*@@**************************************************************************/
/*                                                                           */
/*  int AttachGalley(hd, inners, suspend_pt)                                 */
/*                                                                           */
/*  Attach galley hd, which may be unsized, to a destination.  This involves */
/*  searching for a destination forward or back from the attachment point of */
/*  hd and promoting up to and including the first definite component of hd. */
/*                                                                           */
/*  Although AttachGalley never flushes any galleys, it may identify some    */
/*  galleys which should be flushed, even if the attach is itself not        */
/*  successful.  These are returned in *inners, or nilobj if none.           */
/*                                                                           */
/*  The integer returned by AttachGalley indicates what happened to hd:      */
/*                                                                           */
/*    ATTACH_KILLED     The galley was sized to begin with but no target     */
/*                      for it could be found.  The galley has been killed   */
/*                      and that's the end of it.                            */
/*                                                                           */
/*    ATTACH_INPUT      When searching for a target for the galley we came   */
/*                      upon InputSym, suggesting that the target might be   */
/*                      still to be read.  So the galley has been linked to  */
/*                      that InputSym and must now wait.                     */
/*                                                                           */
/*    ATTACH_NOTARGET   The galley is unsized and no target could be found   */
/*                      for it.  This is fine, it just means that we can't   */
/*                      flush the galley now and we must try again later.    */
/*                                                                           */
/*    ATTACH_SUSPEND    The galley is sized and a target was found for it,   */
/*                      but the first component of the galley proved to be   */
/*                      indefinite so could not be promoted.  The galley     */
/*                      remains unattached but is moved to just before its   */
/*                      target so that it can find it easily later when its  */
/*                      first component becomes definite and it is flushed.  */
/*                                                                           */
/*    ATTACH_NULL       The galley is sized and a target was found for it,   */
/*                      but the body of the galley proved to be null (i.e.   */
/*                      there were no definite components to be flushed).    */
/*                      This is to be treated just like the normal case      */
/*                      following, except that the target is replaced by     */
/*                      @Null rather than by its body.                       */
/*                                                                           */
/*    ATTACH_ACCEPT     The galley is sized and a target was found for it,   */
/*                      and one component of the galley has been promoted.   */
/*                                                                           */
/*****************************************************************************/

int AttachGalley(OBJECT hd, OBJECT *inners, OBJECT *suspend_pt)
{ OBJECT hd_index;		/* the index of hd in the enclosing galley   */
  OBJECT hd_inners;		/* inner galleys of hd, if unsized           */
  OBJECT dest;			/* the target @Galley hd empties into        */
  OBJECT dest_index;		/* the index of dest                         */
  OBJECT target;		/* the target indefinite containing dest     */
  OBJECT target_index;		/* the index of target                       */
  OBJECT target_galley;		/* the body of target, made into a galley    */
  OBJECT tg_inners;		/* inner galleys of target_galley            */
  BOOLEAN need_precedes;	/* true if destination lies before galley    */
  OBJECT recs;			/* list of recursive definite objects        */
  OBJECT link, y;		/* for scanning through the components of hd */
  CONSTRAINT c;			/* temporary variable holding a constraint   */
  OBJECT env, n1, tmp, zlink, z, sym;	/* placeholders and temporaries	     */
  BOOLEAN was_sized;		/* true if sized(hd) initially               */

  debug2(DGA, D, "[ AttachGalley(Galley %s into %s)",
	SymName(actual(hd)), SymName(whereto(hd)));
  ifdebug(DGA, DD, DebugGalley(hd, nilobj, 4));
  assert( Up(hd) != hd, "AttachGalley: no index!" );
  Parent(hd_index, Up(hd));
  assert( type(hd_index) == UNATTACHED, "AttachGalley: not UNATTACHED!" );
  hd_inners = tg_inners = nilobj;
  was_sized = sized(hd);

  for(;;)
  {
    /*************************************************************************/
    /*                                                                       */
    /*  Search for a destination for hd.  If hd is unsized, search for       */
    /*  inner galleys preceding it first of all, then for receptive objects  */
    /*  following it, possibly in inner galleys.  If no luck, exit.          */
    /*  If hd is sized, search only for receptive objects in the current     */
    /*  galley below the current spot, and fail if cannot find any.          */
    /*                                                                       */
    /*************************************************************************/

    sym = whereto(hd);
    if( sized(hd) )
    {
      /* sized galley case: search on from current spot */
      target_index = SearchGalley(Up(hd_index), sym, TRUE, FALSE, TRUE, TRUE);
      if( target_index == nilobj )
      {	
	/* search failed to find any new target, so kill the galley */
	for( link = Down(hd); link != hd; link = NextDown(link) )
	{ Child(y, link);
	  if( type(y) == SPLIT )  Child(y, DownDim(y, ROW));
	  if( is_definite(type(y)) )  break;
	}
	if( link != hd )
	  Error(19, 1, "galley %s deleted from here (no target)",
	    WARN, &fpos(y), SymName(actual(hd)));
	if( hd_inners != nilobj )  DisposeObject(hd_inners), hd_inners=nilobj;
	if( tg_inners != nilobj )  DisposeObject(tg_inners), tg_inners=nilobj;
	KillGalley(hd);
	*inners = nilobj;
	debug0(DGA, D, "] AttachGalley returning ATTACH_KILLED");
	return ATTACH_KILLED;
      }
      else if( actual(actual(target_index)) == InputSym )
      {
	/* search found input object, so suspend on that */
	DeleteNode(hd_index);
	Link(target_index, hd);
	*inners = nilobj;
	debug0(DGA, D, "] AttachGalley returning ATTACH_INPUT");
	return ATTACH_INPUT;
      }

    }
    else /* unsized galley, either backwards or normal */
    {
      if( backward(hd) )
      {	target_index= SearchGalley(Up(hd_index), sym, FALSE, TRUE,TRUE,FALSE);
	need_precedes = FALSE;
      }
      else
      {	target_index = SearchGalley(Up(hd_index), sym, FALSE,TRUE,FALSE,FALSE);
	need_precedes = (target_index != nilobj);
	if( target_index == nilobj )
	  target_index = SearchGalley(Up(hd_index), sym, TRUE,TRUE,TRUE,FALSE);
      }

      /* if no luck, exit without error */
      if( target_index == nilobj )
      {	*inners = nilobj;
	debug0(DGA, D, "] AttachGalley returning ATTACH_NOTARGET");
	return ATTACH_NOTARGET;
      }
    }
    assert( type(target_index) == RECEPTIVE, "AttachGalley: target_index!" );
    target = actual(target_index);
    assert( type(target) == CLOSURE, "AttachGalley: target!" );

    /* set target_galley to the expanded value of target */
    debug1(DYY, D, "[ EnterErrorBlock(FALSE) (expanding target %s)",
      SymName(actual(target)));
    EnterErrorBlock(FALSE);
    target_galley = New(HEAD);
    FposCopy(fpos(target_galley), fpos(target));
    actual(target_galley) = actual(target);
    whereto(target_galley) = ready_galls(target_galley) = nilobj;
    backward(target_galley) = must_expand(target_galley) = FALSE;
    sized(target_galley) = FALSE;
    Constrained(target, &c, COL);
    if( !constrained(c) )
      Error(19, 2, "receptive symbol %s has unconstrained width",
	FATAL, &fpos(target), SymName(actual(target)));
    debug2(DSC, DD, "Constrained( %s, COL ) = %s",
	EchoObject(target), EchoConstraint(&c));
    debug1(DGA, DDD, "  expanding %s", EchoObject(target));
    tmp = CopyObject(target, no_fpos);
    Link(target_galley, tmp);
    if( !FitsConstraint(0, 0, c) )
    { debug0(DGA, D, "  reject: target_galley horizontal constraint is -1");
      goto REJECT;
    }
    env = DetachEnv(tmp);
    SizeGalley(target_galley, env, external(target), threaded(target),
	non_blocking(target_index), trigger_externs(target_index),
	&save_style(target), &c, whereto(hd), &dest_index, &recs, &tg_inners);
    debug1(DGA, DD, "  SizeGalley tg_inners: %s", DebugInnersNames(tg_inners));
    if( recs != nilobj )  ExpandRecursives(recs);
    dest = actual(dest_index);

    /* verify that hd satisfies any horizontal constraint on dest */
    debug1(DGA, DDD, "  checking COL fit of hd in %s", SymName(actual(dest)));
    Constrained(dest, &c, COL);
    debug2(DSC, DD, "Constrained( %s, COL ) = %s",
	EchoObject(dest), EchoConstraint(&c));
    assert( constrained(c), "AttachGalley: dest unconstrained!" );
    if( !FitsConstraint(0, 0, c) )
    { debug0(DGA, D, "  reject: hd horizontal constraint is -1");
      goto REJECT;
    }
    if( !sized(hd) )
    {
      debug2(DYY, D, "[ EnterErrorBlock(TRUE) (sizing galley %s into %s)",
	SymName(actual(hd)), SymName(whereto(hd)));
      EnterErrorBlock(TRUE);
      n1 = nilobj;
      Child(y, Down(hd));
      env = DetachEnv(y);
      /*** to set non_blocking() to FALSE seems doubtful!
      SizeGalley(hd, env, TRUE, threaded(dest), FALSE, TRUE,
		&save_style(dest), &c, nilobj, &n1, &recs, &hd_inners);
      *** */
      SizeGalley(hd, env, TRUE, threaded(dest), non_blocking(target_index),
	TRUE, &save_style(dest), &c, nilobj, &n1, &recs, &hd_inners);
      debug1(DGA,DD,"  SizeGalley hd_inners: %s", DebugInnersNames(hd_inners));
      if( recs != nilobj )  ExpandRecursives(recs);
      if( need_precedes )		/* need an ordering constraint */
      {	OBJECT index1 = New(PRECEDES);
	OBJECT index2 = New(FOLLOWS);
	blocked(index2) = FALSE;
	tmp = MakeWord(WORD, STR_EMPTY, no_fpos);
	Link(index1, tmp);  Link(index2, tmp);
	Link(Up(hd_index), index1);
	Link(Down(hd), index2);
	debug0(DGA, D, "  inserting PRECEDES and FOLLOWS");
      }
      LeaveErrorBlock(TRUE);
      debug0(DYY, D, "] LeaveErrorBlock(TRUE) (finished sizing galley)");
    }
    if( !FitsConstraint(back(hd, COL), fwd(hd, COL), c) )
    { debug3(DGA, D, "  reject: hd %s,%s does not fit target_galley %s",
	EchoLength(back(hd, COL)), EchoLength(fwd(hd, COL)),
	EchoConstraint(&c));
      Error(19, 3, "too little horizontal space for galley %s at %s",
	WARN, &fpos(hd), SymName(actual(hd)), SymName(actual(dest)));
      goto REJECT;
    }

    /* check status of first component of hd */
    debug0(DGA, DDD, "  now ready to attach; hd =");
    ifdebug(DGA, DDD, DebugObject(hd));
    for( link = Down(hd);  link != hd;  link = NextDown(link) )
    {
      Child(y, link);
      debug1(DGA, DDD, "  examining %s", EchoIndex(y));
      if( type(y) == SPLIT )  Child(y, DownDim(y, ROW));
      switch( type(y) )
      {

	case EXPAND_IND:
	case SCALE_IND:
	case GALL_PREC:
	case GALL_FOLL:
	case GALL_TARG:
	case CROSS_PREC:
	case CROSS_FOLL:
	case CROSS_TARG:
	case PAGE_LABEL_IND:
	    
	  break;


	case PRECEDES:
	case UNATTACHED:
	    
	  if( was_sized )
	  { /* SizeGalley was not called, so hd_inners was not set by it */
	    if( hd_inners == nilobj )  hd_inners = New(ACAT);
	    Link(hd_inners, y);
	  }
	  break;


	case RECEPTIVE:

	  /* ***
	  if( non_blocking(y) )
	  { link = PrevDown(link);
	    DeleteNode(y);
	  }
	  else
	  *** */
	  goto SUSPEND;


	case RECEIVING:
	    
	  /* ***
	  if( non_blocking(y) )
	  { while( Down(y) != y )
	    { Child(z, Down(y));
	      DetachGalley(z);
	      KillGalley(z);
	    }
	    link = PrevDown(link);
	    DeleteNode(y);
	  }
	  else
	  *** */
	  goto SUSPEND;


	case FOLLOWS:
	    
	  Child(tmp, Down(y));
	  if( Up(tmp) == LastUp(tmp) )
	  { link = pred(link, CHILD);
	    debug0(DGA, DD, "  disposing FOLLOWS");
	    DisposeChild(NextDown(link));
	    break;
	  }
	  Parent(tmp, Up(tmp));
	  assert(type(tmp) == PRECEDES, "Attach: PRECEDES!");
	  switch( CheckComponentOrder(tmp, target_index) )
	  {
	    case CLEAR:		DeleteNode(tmp);
				link = pred(link, CHILD);
				DisposeChild(NextDown(link));
				break;

	    case PROMOTE:	break;

	    case BLOCK:		debug0(DGA, DD, "CheckContraint: BLOCK");
				goto SUSPEND;

	    case CLOSE:		debug0(DGA, D, "  reject: CheckContraint");
				goto REJECT;
	  }
	  break;


	case GAP_OBJ:

	  if( !join(gap(y)) )  seen_nojoin(hd) = TRUE;
	  break;


	case CLOSURE:
	case CROSS:
	case NULL_CLOS:
	case PAGE_LABEL:

	  break;


	case WORD:
	case QWORD:
	case ONE_COL:
	case ONE_ROW:
	case WIDE:
	case HIGH:
	case HSHIFT:
	case VSHIFT:
	case HSCALE:
	case VSCALE:
	case HCONTRACT:
	case VCONTRACT:
	case HEXPAND:
	case VEXPAND:
	case PADJUST:
	case HADJUST:
	case VADJUST:
	case ROTATE:
	case SCALE:
	case INCGRAPHIC:
	case SINCGRAPHIC:
	case GRAPHIC:
	case ACAT:
	case HCAT:
	case ROW_THR:
	    
	  /* make sure y is not joined to a target below */
	  for( zlink = NextDown(link);  zlink != hd;  zlink = NextDown(zlink) )
	  { Child(z, zlink);
	    switch( type(z) )
	    {
	      case RECEPTIVE:	if( non_blocking(z) )
				{ zlink = PrevDown(zlink);
				  DeleteNode(z);
				}
				else
				{ y = z;
				  goto SUSPEND;
				}
				break;

	      case RECEIVING:	if( non_blocking(z) )
				{ zlink = PrevDown(zlink);
				  while( Down(z) != z )
				  { Child(tmp, Down(y));
				    DetachGalley(tmp);
				    KillGalley(tmp);
				  }
				  DeleteNode(z);
				}
				else
				{ y = z;
				  goto SUSPEND;
				}
				break;

	      case GAP_OBJ:	if( !join(gap(z)) )  zlink = PrevDown(hd);
				break;

	      default:		break;
	    }
	  }

	  /* if HCAT, try vertical hyphenation before doing anything else */
	  if( type(y) == HCAT )  VerticalHyphenate(y);

	  /* check availability of vertical space for the first component */
	  if( !external(dest) )
	  { Constrained(dest, &c, ROW);
	    debug2(DSC, DD, "Constrained( %s, ROW ) = %s",
	      EchoObject(dest), EchoConstraint(&c));
	    if( !FitsConstraint(back(y, ROW), fwd(y, ROW), c) )
	    { BOOLEAN scaled;

	      /* if forcing galley doesn't fit, try scaling first component */
	      scaled = FALSE;
	      if( actual(hd) != nilobj && force_target(actual(hd))
		 && size(y, ROW) > 0)
	      { int scale_factor;
		scale_factor = ScaleToConstraint(back(y,ROW), fwd(y,ROW), &c);
		/* scale_factor = (bfc(c) * SF) / size(y, ROW); */
		if( scale_factor > 0.5 * SF )
		{ char num1[20], num2[20];
		  sprintf(num1, "%.1fc", (float) size(y, ROW) / CM);
		  sprintf(num2, "%.1fc", (float) bfc(c) / CM);
		  Error(19, 7, "%s object too high for %s space; %s inserted",
		    WARN, &fpos(y), num1, num2, KW_VSCALE);
		  y = InterposeVScale(y, scale_factor);
		  scaled = TRUE;
		}
	      }

	      /* otherwise we must reject, and warn the user */
	      if( !scaled )
	      { Error(19, 4, "this component of %s did not fit into its nearest target",
		  WARN, &fpos(y), SymName(actual(hd)));
	        debug3(DGA, D, "  reject: vsize %s,%s in %s; y=",
		  EchoLength(back(y, ROW)), EchoLength(fwd(y, ROW)),
		  EchoConstraint(&c));
	        ifdebug(DGA, D, DebugObject(y));
	        goto REJECT;
	      }
	    }
	    debug0(DSA, D, "calling AdjustSize from AttachGalley (a)");
	    AdjustSize(dest, back(y, ROW), fwd(y, ROW), ROW);
	  }
	  if( !external(target) )
	  { Constrained(target, &c, ROW);
	    debug2(DSC, DD, "Constrained( %s, ROW ) = %s",
			EchoObject(target), EchoConstraint(&c));
	    Child(z, LastDown(target_galley));
	    assert( !is_index(type(z)), "AttachGalley: is_index(z)!" );
	    assert( back(z, ROW) >= 0 && fwd(z, ROW) >= 0,
			"AttachGalley: negative z sizes!" );
	    if( !FitsConstraint(back(z, ROW), fwd(z, ROW), c) )
	    { BOOLEAN scaled;

	      /* if forcing galley doesn't fit, try scaling z */
	      scaled = FALSE;
	      if( actual(hd) != nilobj && force_target(actual(hd))
		 && size(z, ROW) > 0 )
	      { int scale_factor;
		/* scale_factor = (bfc(c) * SF) / size(z, ROW); */
		scale_factor = ScaleToConstraint(back(z,ROW), fwd(z,ROW), &c);
		if( scale_factor > 0.5 * SF )
		{ char num1[20], num2[20];
		  sprintf(num1, "%.1fc", (float) size(z, ROW) / CM);
		  sprintf(num2, "%.1fc", (float) bfc(c) / CM);
		  Error(19, 7, "%s object too high for %s space; %s inserted",
		    WARN, &fpos(y), num1, num2, KW_VSCALE);
		  z = InterposeHigh(z);
		  z = InterposeVScale(z, scale_factor);
		  scaled = TRUE;
		}
	      }

	      if( !scaled )
	      { Error(19, 5, "this component of %s did not fit into its nearest target",
	          WARN, &fpos(y), SymName(actual(hd)));
	        debug3(DGA, D, "  reject: size was %s,%s in %s; y =",
		  EchoLength(back(z, ROW)), EchoLength(fwd(z, ROW)),
		  EchoConstraint(&c));
	        ifdebug(DGA, D, DebugObject(y));
	        goto REJECT;
	      }
	    }
	    debug0(DSA, D, "calling AdjustSize from AttachGalley (b)");
	    AdjustSize(target, back(z, ROW), fwd(z, ROW), ROW);
	  }
	  goto ACCEPT;


	default:
	    
	  Error(19, 6, "AttachGalley: %s", INTERN, &fpos(y), Image(type(y)));
	  break;

      } /* end switch */
    } /* end for */

    /* null galley: promote whole galley without expanding the target */
    debug0(DGA, D, "  null galley");
    if( tg_inners != nilobj )  DisposeObject(tg_inners), tg_inners = nilobj;
    DisposeObject(target_galley);
    LeaveErrorBlock(FALSE);
    debug0(DYY, D, "] LeaveErrorBlock(FALSE) (null galley)");

    /* kill off any null objects within the galley, then transfer it */
    /* don't use Promote() since it does extra unwanted things here  */
    for( link = Down(hd);  link != hd;  link = NextDown(link) )
    { Child(y, link);
      switch( type(y) )
      {

	case GAP_OBJ:
	case CLOSURE:
	case CROSS:
	case NULL_CLOS:
	case PAGE_LABEL:
	
	  link = PrevDown(link);
	  debug1(DGA, D, "  null galley, disposing %s", Image(type(y)));
	  DisposeChild(NextDown(link));
	  break;

	
	default:
	
	  break;
      }
    }
    TransferLinks(NextDown(hd), hd, Up(target_index));

    /* attach hd temporarily to target_index */
    MoveLink(Up(hd), target_index, PARENT);
    assert( type(hd_index) == UNATTACHED, "AttachGalley: type(hd_index)!" );
    DeleteNode(hd_index);

    /* return; only hd_inners needs to be flushed now */
    *inners = hd_inners;
    debug0(DGA, D, "] AttachGalley returning ATTACH_NULL");
    return ATTACH_NULL;


    REJECT:
	
      /* reject first component */
      debug1(DGA, D, "  reject %s", EchoObject(y));
      LeaveErrorBlock(TRUE);
      debug0(DYY, D, "] LeaveErrorBlock(TRUE) (REJECT)");
      if( tg_inners != nilobj )  DisposeObject(tg_inners), tg_inners = nilobj;
      DisposeObject(target_galley);
      if( backward(hd) && !sized(hd) )
      {
	/* move to just before the failed target */
	MoveLink(Up(hd_index), Up(target_index), PARENT);
      }
      else
      {
	/* move to just after the failed target */
	MoveLink(Up(hd_index), NextDown(Up(target_index)), PARENT);
      }
      continue;


    SUSPEND:
	
      /* suspend at first component */
      debug1(DGA, D, "  suspend %s", EchoIndex(y));
      blocked(y) = TRUE;
      LeaveErrorBlock(FALSE);
      debug0(DYY, D, "] LeaveErrorBlock(FALSE) (SUSPEND)");
      if( tg_inners != nilobj )  DisposeObject(tg_inners), tg_inners = nilobj;
      DisposeObject(target_galley);
      MoveLink(Up(hd_index), Up(target_index), PARENT);
      if( was_sized )
      { /* nothing new to flush if suspending and already sized */
	if( hd_inners != nilobj )  DisposeObject(hd_inners), hd_inners=nilobj;
	*inners = nilobj;
      }
      else
      { /* flush newly discovered inners if not sized before */
	*inners = hd_inners;
      }
      debug0(DGA, D, "] AttachGalley returning ATTACH_SUSPEND");
      *suspend_pt = y;
      return ATTACH_SUSPEND;


    ACCEPT:
	
      /* accept first component; now committed to the attach */
      debug1(DGA, D, "  accept %s", EchoObject(y));
      LeaveErrorBlock(TRUE);
      debug0(DYY, D, "] LeaveErrorBlock(TRUE) (ACCEPT)");

      /* adjust horizontal sizes */
      debug0(DSA, D, "calling AdjustSize from AttachGalley (c)");
      AdjustSize(dest, back(hd, COL), fwd(hd, COL), COL);
      debug0(DSA, D, "calling AdjustSize from AttachGalley (d)");
      AdjustSize(target, back(target_galley, COL),
				fwd(target_galley, COL), COL);
		
      /* attach hd to dest */
      MoveLink(Up(hd), dest_index, PARENT);
      assert( type(hd_index) == UNATTACHED, "AttachGalley: type(hd_index)!" );
      DeleteNode(hd_index);

      /* move first component of hd into dest */
      /* nb Interpose must be done after all AdjustSize calls */
      if( !external(dest) )   Interpose(dest, VCAT, hd, y);
      Promote(hd, link == hd ? hd : NextDown(link), dest_index);

      /* move target_galley into target */
      /* nb Interpose must be done after all AdjustSize calls */
      if( !external(target) )
      {	Child(z, LastDown(target_galley));
	Interpose(target, VCAT, z, z);
      }
      Promote(target_galley, target_galley, target_index);
      DeleteNode(target_galley);
      assert(Down(target_index)==target_index, "AttachGalley: target_ind");
      if( blocked(target_index) )  blocked(dest_index) = TRUE;
      DeleteNode(target_index);

      /* return; both tg_inners and hd_inners need to be flushed now;        */
      /* if was_sized, hd_inners contains the inners of the first component; */
      /* otherwise it contains the inners of all components, from SizeGalley */
      if( tg_inners == nilobj ) *inners = hd_inners;
      else if( hd_inners == nilobj ) *inners = tg_inners;
      else
      {	TransferLinks(Down(hd_inners), hd_inners, tg_inners);
	DeleteNode(hd_inners);
	*inners = tg_inners;
      }
      debug0(DGA, D, "] AttachGalley returning ATTACH_ACCEPT");
      return ATTACH_ACCEPT;

  } /* end for */
} /* end AttachGalley */
