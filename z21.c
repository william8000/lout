/*@z21.c:Galley Maker:SizeGalley()@*******************************************/
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
/*  FILE:         z21.c                                                      */
/*  MODULE:       Galley Maker                                               */
/*  EXTERNS:      SizeGalley()                                               */
/*                                                                           */
/*****************************************************************************/
#include "externs"

/*****************************************************************************/
/*                                                                           */
/*  SizeGalley(hd, env, rows, joined, nonblock, trig, style, c, target,      */
/*                                                dest_index, recs, inners)  */
/*                                                                           */
/*  Convert unsized galley hd into sized format.  The input parameters are:  */
/*                                                                           */
/*    hd          the galley to be converted                                 */
/*    env         its environment (needs to be "held" while manifesting)     */
/*    rows        TRUE if the resulting galley may have more than one row    */
/*    joined      TRUE if the resulting galley must be simply joined         */
/*    nonblock    Set the non_blocking() field of RECEPTIVEs to this value   */
/*    trig        TRUE if indefinites of hd may trigger external galleys     */
/*    *style      The initial style                                          */
/*    *c          the width constraint hd should conform to                  */
/*    target      if non-nilobj, expand indefinite objects to reveal a       */
/*                @Galley within this symbol                                 */
/*                                                                           */
/*  The output parameters, in addition to the converted hd, are:             */
/*                                                                           */
/*    dest_index  the index of the @Galley found within target, if any       */
/*    recs        list of all RECURSIVE indexes found (or nilobj if none)    */
/*    inners      list of all UNATTACHED indexes found (or nilobj if none),  */
/*                not including any that come after the target or InputSym.  */
/*                                                                           */
/*****************************************************************************/

void SizeGalley(OBJECT hd, OBJECT env, BOOLEAN rows, BOOLEAN joined,
BOOLEAN nonblock, BOOLEAN trig, STYLE *style, CONSTRAINT *c, OBJECT target,
OBJECT *dest_index, OBJECT *recs, OBJECT *inners)
{ OBJECT y, link, z, crs, t, tlink, zlink, tmp;
  OBJECT extras, tmp1, tmp2, bt[2], ft[2], hold_env;
  BOOLEAN after_target;
  
  assert( type(hd) == HEAD && Down(hd) != hd, "SizeGalley: precondition!" );
  assert( !sized(hd), "SizeGalley: already sized!" );
  debug6(DGM, D, "SizeGalley(hd, -, %s, %s, %s, %s, %s, %s, -, -, -), hd =",
	bool(joined), bool(nonblock), bool(trig), EchoStyle(style),
	EchoConstraint(c), SymName(target));
  debug1(DGM, D, "  env = %s", EchoObject(env));
  ifdebug(DGM, DD, DebugObject(hd));

  /* manifest the child of hd, making sure it is simply joined if required */
  tmp1 = target;
  Child(y, Down(hd));
  crs = nilobj;
  bt[COL] = ft[COL] = bt[ROW] = ft[ROW] = nilobj;
  hold_env = New(ACAT);  Link(hold_env, env);
  debug0(DOM, D, "  [ calling Manifest from SizeGalley");
  if( joined )
  { bt[COL] = New(THREAD);  ft[COL] = New(THREAD);
    debug0(DGM, D, "  SizeGalley calling Manifest (joined)");
    y = Manifest(y, env, style, bt, ft, &tmp1, &crs, TRUE, must_expand(hd));
    assert( Down(bt[COL]) != bt[COL] && Down(ft[COL]) != ft[COL],
	"SizeGalley: threads!" );
    Child(tmp1, Down(bt[COL]));  Child(tmp2, Down(ft[COL]));
    if( Down(bt[COL]) != LastDown(bt[COL]) ||
	  Down(ft[COL]) != LastDown(ft[COL]) || tmp1 != tmp2 )
      Error(21, 1, "galley %s must have just one column mark",
	FATAL, &fpos(y), SymName(actual(hd)) );
    DisposeObject(bt[COL]);  DisposeObject(ft[COL]);
  }
  else
  { debug0(DGM, D, "  SizeGalley calling Manifest (not joined)");
    y = Manifest(y, env, style, bt, ft, &tmp1, &crs, TRUE, must_expand(hd));
  }
  debug0(DOM, D, "  ] returning from Manifest in SizeGalley");
  DisposeObject(hold_env);

  /* horizontally size and break hd */
  debug0(DGM, DD, "SizeGalley: after manifesting, hd =");
  ifdebug(DGM, DD, DebugObject(hd));
  debug0(DGM, DD, "SizeGalley horizontally sizing and breaking hd:");
  CopyConstraint(constraint(hd), *c);
  extras = New(ACAT);
  debug0(DGM, D, "  SizeGalley calling MinSize(COL):");
  y = MinSize(y, COL, &extras);
  debug0(DOB, DD, "  calling BreakObject from SizeGalley");
  debug0(DGM, D, "  SizeGalley calling BreakObject:");
  y = BreakObject(y, c);
  back(hd, COL) = back(y, COL);
  fwd(hd, COL)  = fwd(y, COL);

  assert( FitsConstraint(back(hd, COL), fwd(hd, COL), *c),
	"SizeGalley: BreakObject failed to fit!" );
  debug2(DSF, D, "MinSize(hd, COL) = %s,%s",
	  EchoLength(back(hd, COL)), EchoLength(fwd(hd, COL)) );

  /* get the rows of hd to the top level, if required */
  seen_nojoin(hd) = FALSE;
  if( rows )
  { /* OBJECT prev_gap = nilobj; */
    debug0(DGM, DD, "SizeGalley cleaning up rows of hd:");
    for( link = hd;  NextDown(link) != hd;  link = NextDown(link) )
    { Child(y, NextDown(link));
      debug2(DGM, DD, "  cleaning %s: %s", Image(type(y)), EchoObject(y));
      switch( type(y) )
      {
	case GAP_OBJ:

	  /* prev_gap = y; */
	  if( !join(gap(y)) )  seen_nojoin(hd) = TRUE;
	  break;


	case VCAT:
	  
	  TransferLinks(Down(y), y, Up(y));
	  DisposeChild(Up(y));
	  link = PrevDown(link);
	  break;


	case SPLIT:
	  
	  assert(Up(y)==LastUp(y), "SizeGalley COL_THR: Up(y)!=LastUp(y)!");
	  Child(z, DownDim(y, ROW));
	  if( is_indefinite(type(z)) )  external(z) = TRUE;
	  else if( type(z) == VCAT )
	  { OBJECT hor, thor, clink, dlink;
	    Child(hor, DownDim(y, COL));
	    assert( type(hor) == COL_THR, "SizeGalley: missing COL_THR!" );
	    Parent(thor, UpDim(z, COL));
	    assert( hor == thor, "SizeGalley/SPLIT: hor != thor!" );
	    clink = DownDim(y, COL);
	    dlink = UpDim(z, COL);
	    for( tlink = LastDown(z);  tlink != z;  tlink = PrevDown(tlink) )
	    { Child(t, tlink);
	      if( type(t) == GAP_OBJ )  Link(NextDown(link), t);
	      else
	      {	tmp = New(SPLIT);
		back(tmp, COL) = back(hor, COL);
		fwd(tmp, COL) = fwd(hor, COL);
		Link(NextDown(link), tmp);
		Link(tmp, NextUp(clink));
		Link(NextDown(dlink), t);
		Link(tmp, t);
	      }
	    }
	    DeleteLink(dlink);
	    assert(Up(y)==LastUp(y), "SizeGalley COL_THR: Up(y) != LastUp(y)!");
	    DisposeChild(Up(y));
	    link = PrevDown(link);
	  }
	  break;


	case CLOSURE:
	case HEAD:
	  
	  external(y) = TRUE;
	  break;


	default:
	  
	  break;
      }
    }
  }

  /* determine a scale factor for {} @Scale objects */
  /* NB AdjustSize cannot be done correctly until after seen_nojoin is set */
  for( link = Down(extras);  link != extras;  link = NextDown(link) )
  { Child(y, link);
    if( type(y) == SCALE_IND )
    {
      /* check that all is in order */
      CONSTRAINT zc;  OBJECT t;  LENGTH b, f;
      z = actual(y);
      assert( type(z) == SCALE, "SizeObject: type(z) != SCALE!" );
      assert( bc(constraint(z)) == 0, "SizeObject: bc(constraint(z)) != 0" );
      assert( Down(z) != z, "SizeObject SCALE: Down(z) == z!" );
      Child(t, Down(z));

      /* use @Scale COL size constraint to determine a suitable scale factor */
      Constrained(z, &zc, COL);
      debug2(DGM, D, "Constrained(%s, -, COL) = %s", EchoObject(z),
	EchoConstraint(&zc));
      if( !constrained(zc) )
      { Error(21, 2, "replacing infinite scale factor (unconstrained width) by 1.0",
	  WARN, &fpos(z));
	bc(constraint(z)) = fc(constraint(z)) = 1 * SF;
      }
      else if( size(t, COL) == 0 )
      { Error(21, 3, "replacing infinite scale factor (zero width object) by 1.0",
	  WARN, &fpos(z));
	bc(constraint(z)) = fc(constraint(z)) = 1 * SF;
      }
      else if( (float) bfc(zc) / size(t, COL) > 100.0 )
      { Error(21, 4, "replacing very large scale factor (over 100) by 1.0",
	  WARN, &fpos(z));
	bc(constraint(z)) = fc(constraint(z)) = 1 * SF;
      }
      else if( (float) bfc(zc) / size(t, COL) < 0.01 )
      { if( bfc(zc) == 0 )
	  Error(21, 5, "object deleted (scale factor is zero)",
	    WARN, &fpos(z));
	else
	  Error(21, 6, "object deleted (scale factor is smaller than 0.01)",
	    WARN, &fpos(z));
	bc(constraint(z)) = fc(constraint(z)) = 1 * SF;
	tmp = MakeWord(WORD, STR_EMPTY, &fpos(t));
	back(tmp, COL) = fwd(tmp, COL) = 0;
	back(tmp, ROW) = fwd(tmp, ROW) = 0;
	word_font(tmp) = word_colour(tmp) = word_language(tmp) = 0;
	word_hyph(tmp) = FALSE;
	ReplaceNode(tmp, t);
	DisposeObject(t);
	t = tmp;
      }
      else bc(constraint(z)) = fc(constraint(z)) = (bfc(zc) * SF)/size(t, COL);

      /* calculate scaled size and adjust */
      b = (back(t, COL) * fc(constraint(z))) / SF;
      f = (fwd(t, COL) * fc(constraint(z))) / SF;
      debug3(DGM, D, "AdjustSize(%s, %s, %s, COL)", EchoObject(z),
	EchoLength(b), EchoLength(f));
      AdjustSize(z, b, f, COL);

      /* if already vertically sized (because inside @Rotate) adjust that */
      if( vert_sized(z) )
      { b = (back(t, ROW) * fc(constraint(z))) / SF;
	f = (fwd(t, ROW) * fc(constraint(z))) / SF;
	debug3(DGM, D, "AdjustSize(%s, %s, %s, ROW)", EchoObject(z),
	  EchoLength(b), EchoLength(f));
	AdjustSize(z, b, f, ROW);
      }
    }
  }
  DisposeObject(extras);

  /* size the rows of hd and attach indices where needed */
  debug0(DGM, D, "  SizeGalley calling MinSize(ROW):");
  debug0(DGM, DD, "SizeGalley sizing rows of hd =");
  ifdebug(DGM, DD, DebugObject(hd));
  *recs = *inners = *dest_index = nilobj;
  after_target = FALSE;
  for( link = Down(hd);  link != hd;  link = NextDown(link) )
  { Child(y, link);

    if( type(y) == GAP_OBJ || is_index(type(y)) )  continue;
    debug0(DGM, DDD, "  ROW sizing:");
    ifdebug(DGM, DDD, DebugObject(y));
    extras = New(ACAT);
    y = MinSize(y, ROW, &extras);
    debug3(DSF, D, "MinSize( %s , ROW ) = %s,%s", EchoObject(y),
	  EchoLength(back(y, ROW)), EchoLength(fwd(y, ROW)) );
    debug0(DGM, DDD, "  ROW result:");
    ifdebug(DGM, DDD, DebugObject(y));

    /* now attach indexes in front of y */
    for( zlink = Down(extras);  zlink != extras;  zlink = NextDown(zlink) )
    { Child(z, zlink);
      blocked(z) = FALSE;
      /* debug1(DCR, D, "  extra: %s", EchoObject(z)); */
      debug2(DGM, D, "  extra%s: %s",
	after_target ? " after_target" : "", EchoObject(z));
      switch( type(z) )
      {
	case RECEPTIVE:

	  /* debug2(DCR, D, "  ... uses_ext  = %s, trig = %s",
	    bool(uses_extern_target(actual(actual(z)))), bool(trig)); */
	  trigger_externs(z) = uses_extern_target(actual(actual(z))) && trig;
	  non_blocking(z)    = nonblock;
	  if( actual(actual(z)) == GalleySym )  *dest_index = z;
	  if( actual(actual(z)) == GalleySym || actual(actual(z)) == InputSym )
	    after_target = TRUE;
	  break;


	case RECURSIVE:

	  if( *recs == nilobj )  *recs = New(ACAT);
	  Link(*recs, z);
	  break;


	case UNATTACHED:

	  if( !after_target )	/* *** new semantics *** */
	  { if( *inners == nilobj )  *inners = New(ACAT);
	    Link(*inners, z);
	  }
	  break;

		
	case SCALE_IND:
	case EXPAND_IND:
	case GALL_PREC:
	case GALL_FOLL:
	case GALL_TARG:
	case CROSS_PREC:
	case CROSS_FOLL:
	case CROSS_TARG:
	case PAGE_LABEL_IND:

	  debug1(DCR, DD, "  SizeGalley: %s", EchoObject(z));
	  break;


	default:
	  
	  Error(21, 7, "SizeGalley: %s", INTERN, no_fpos, Image(type(z)));
	  break;

      }
    }
    TransferLinks(Down(extras), extras, link);
    assert( Down(extras) == extras && Up(extras) == extras, "SizeG: extras!");
    Dispose(extras);
  }
  
  /* insinuate cross references */
  if( crs != nilobj )
  { 
    debug1(DCR, D, "SizeGalley insinuating %s", EchoObject(crs));
    TransferLinks(Down(crs), crs, Down(hd));
    DisposeObject(crs);
  }

  /* check that *dest_index was found if it was required, and exit */
  if( target != nilobj && *dest_index == nilobj )
    Error(21, 8, "unexpected absence of %s from the body of %s",
      FATAL, &fpos(hd), SymName(target), SymName(actual(hd)));
  debug3(DGM, D, "SizeGalley returning %s,%s  %s;  hd =",
    EchoLength(back(hd, COL)), EchoLength(fwd(hd, COL)),
    EchoConstraint(&constraint(hd)));
  ifdebug(DGM, DD, DebugObject(hd));
  sized(hd) = TRUE;

} /* end SizeGalley */
