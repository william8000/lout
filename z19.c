/*@z19.c:Galley Attaching:AttachGalley(), DetachGalley()@*********************/
/*                                                                           */
/*  LOUT: A HIGH-LEVEL LANGUAGE FOR DOCUMENT FORMATTING (VERSION 2.03)       */
/*  COPYRIGHT (C) 1993 Jeffrey H. Kingston                                   */
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
/*  EXTERNS:      AttachGalley(), DetachGalley()                             */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
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

OBJECT SearchGalley(start, sym, forwards, subgalleys, closures, input)
OBJECT start, sym;  BOOLEAN forwards, subgalleys, closures, input;
{ OBJECT y, res, z, zlink, link;
  debug5(DGA, D, "[SearchGalley( start, %s, %s, %s, %s, %s )", SymName(sym),
	forwards ? "fwd" : "back", subgalleys ? "subgalleys" : "nosubgalleys",
	closures ? "closures" : "noclosures", input ? "input" : "noinput");
  assert( type(start) == LINK || type(start) == HEAD, "SearchGalley: start!" );

  link = forwards ? NextDown(start) : PrevDown(start);
  res = nil;
  while( res == nil && type(link) != HEAD )
  { Child(y, link);
    debug1(DGA, DD, "  examining %s", EchoObject(null, y));
    switch( type(y) )
    {
      case UNATTACHED:
      case RECEIVING:
	
	if( subgalleys )
	for( zlink = Down(y); zlink!=y && res==nil;  zlink = NextDown(zlink) )
	{ Child(z, zlink);
	  res = SearchGalley(z, sym, TRUE, TRUE, TRUE, input);
	}
	if( !res && input && type(y)==RECEIVING && actual(actual(y))==InputSym )
	  res = y;
	break;


      case RECEPTIVE:
	
	if( closures && type(actual(y)) == CLOSURE
		     && SearchUses(actual(actual(y)), sym) )  res = y;
	else if( input && actual(actual(y)) == InputSym )  res = y;
	break;


      default:
	
	break;

    }
    link = forwards ? NextDown(link) : PrevDown(link);
  }
  debug1(DGA, D, "]SearchGalley returning %s", EchoObject(null, res));
  return res;
} /* end SearchGalley */


/*@@**************************************************************************/
/*                                                                           */
/*  AttachGalley(hd, inners)                                                 */
/*                                                                           */
/*  Attach galley hd, which may be unsized, to a destination.  This involves */
/*  searching for a destination forward or back from hd's attachment point,  */
/*  and promoting up to and including the first definite component of hd.    */
/*                                                                           */
/*  Although AttachGalley never flushes any galleys, it may identify some    */
/*  galleys which should be flushed, even if the attach is itself not        */
/*  successful.  These are returned in *inners, or nil if none.              */
/*                                                                           */
/*****************************************************************************/

AttachGalley(hd, inners)
OBJECT hd, *inners;
{ OBJECT index;			/* the index of hd in the enclosing galley   */
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

  debug2(DGA, D, "[AttachGalley(Galley %s into %s)",
	SymName(actual(hd)), SymName(whereto(hd)));
  ifdebug(DGA, DD, EchoObject(stderr, hd));
  assert( Up(hd) != hd, "AttachGalley: no index!" );
  Parent(index, Up(hd));
  assert( type(index) == UNATTACHED, "AttachGalley: not UNATTACHED!" );
  *inners = hd_inners = tg_inners = nil;
  was_sized = sized(hd);

  for(;;)
  {
    /*************************************************************************/
    /*                                                                       */
    /*  Search for a destination for hd.  If hd is unsized, search for       */
    /*  inner galleys preceding it first of all, then for receptive objects  */
    /*  following it, possibly in inner galleys.  If no luck, exit.          */
    /*  If hd is sized, search only for receptive objects in the current     */
    /*  galley below the current spot, and fail if can't find any.           */
    /*                                                                       */
    /*************************************************************************/

    sym = whereto(hd);
    if( sized(hd) )
    {
      /* sized galley case: search on from current spot */
      target_index = SearchGalley(Up(index), sym, TRUE, FALSE, TRUE, TRUE);
      if( target_index == nil )
      {	
	/* search failed to find any new target, so kill the galley */
	for( link = Down(hd); link != hd; link = NextDown(link) )
	{ Child(y, link);
	  if( type(y) == SPLIT )  Child(y, DownDim(y, ROW));
	  if( is_definite(type(y)) )  break;
	}
	if( link != hd )
	    Error(WARN, &fpos(y), "galley %s deleted from here: no target",
		SymName(actual(hd)));
	debug0(DGA, D, "calling KillGalley from AttachGalley (a)");
	KillGalley(hd);
	debug0(DGA, D, "]AttachGalley returning: no target for sized galley");
	return;
      }
      else if( actual(actual(target_index)) == InputSym )
      {
	/* search found input object, so suspend on that */
	DeleteNode(index);
	Link(target_index, hd);
	debug0(DGA, D, "]AttachGalley returning: InputSym");
	return;
      }

    }
    else /* unsized galley, either backwards or normal */
    {
      if( backward(hd) )
      {	target_index= SearchGalley(Up(index), sym, FALSE, TRUE, TRUE, FALSE);
	need_precedes = FALSE;
      }
      else
      {	target_index = SearchGalley(Up(index), sym, FALSE, TRUE, FALSE, FALSE);
	need_precedes = (target_index != nil);
	if( target_index == nil )
	  target_index = SearchGalley(Up(index), sym, TRUE, TRUE, TRUE, FALSE);
      }

      /* if no luck, exit without error */
      if( target_index == nil )
      {	debug0(DGA, D, "]AttachGalley returning: no target for unsized galley");
	return;
      }
    }
    assert( type(target_index) == RECEPTIVE, "AttachGalley: target_index!" );
    target = actual(target_index);
    assert( type(target) == CLOSURE, "AttachGalley: target!" );

    /* set target_galley to the expanded value of target */
    EnterErrorBlock(FALSE);
    target_galley = New(HEAD);
    FposCopy(fpos(target_galley), fpos(target));
    actual(target_galley) = actual(target);
    whereto(target_galley) = ready_galls(target_galley) = nil;
    backward(target_galley) = must_expand(target_galley) = FALSE;
    sized(target_galley) = FALSE;
    Constrained(target, &c, COL);
    if( !constrained(c) )  Error(FATAL, &fpos(target),
       "receptive symbol %s has unconstrained width", SymName(actual(target)));
    debug2(DSC, D, "Constrained( %s, COL ) = %s",
	EchoObject(null, target), EchoConstraint(&c));
    debug1(DGA, DD, "  expanding %s", EchoObject(null, target));
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
    if( recs != nil )  ExpandRecursives(recs);
    dest = actual(dest_index);

    /* verify that hd satisfies any horizontal constraint on dest */
    debug1(DGA, DD, "  checking COL fit of hd in %s", SymName(actual(dest)));
    Constrained(dest, &c, COL);
    debug2(DSC, D, "Constrained( %s, COL ) = %s",
	EchoObject(null, dest), EchoConstraint(&c));
    assert( constrained(c), "AttachGalley: dest unconstrained!" );
    if( !sized(hd) )
    { EnterErrorBlock(TRUE);
      if( !FitsConstraint(0, 0, c) )
      {	debug0(DGA, D, "  reject: hd horizontal constraint is -1");
	goto REJECT;
      }
      n1 = nil;
      Child(y, Down(hd));
      env = DetachEnv(y);
      /*** to set non_blocking() to FALSE seems doubtful!
      SizeGalley(hd, env, TRUE, threaded(dest), FALSE, TRUE,
		&save_style(dest), &c, nil, &n1, &recs, &hd_inners);
      *** */
      SizeGalley(hd, env, TRUE, threaded(dest), non_blocking(target_index),
	TRUE, &save_style(dest), &c, nil, &n1, &recs, &hd_inners);
      if( recs != nil )  ExpandRecursives(recs);
      if( need_precedes )		/* need an ordering constraint */
      {	OBJECT index1 = New(PRECEDES);
	OBJECT index2 = New(FOLLOWS);
	blocked(index2) = FALSE;
	tmp = MakeWord("", no_fpos);
	Link(index1, tmp);  Link(index2, tmp);
	Link(Up(index), index1);
	Link(Down(hd), index2);
	debug0(DGA, D, "  inserting PRECEDES and FOLLOWS");
      }
      LeaveErrorBlock(TRUE);
    }
    if( !FitsConstraint(back(hd, COL), fwd(hd, COL), c) )
    { debug3(DGA, D, "  reject: hd %s,%s does not fit target_galley %s",
	EchoLength(back(hd, COL)), EchoLength(fwd(hd, COL)),
	EchoConstraint(&c));
      Error(WARN, &fpos(hd),"too little horizontal space for galley %s at %s",
	SymName(actual(hd)), SymName(actual(dest)));
      goto REJECT;
    }

    /* check status of first component of hd */
    debug0(DGA, DD, "  now ready to attach; hd =");
    ifdebug(DGA, DD, EchoObject(stderr, hd));
    for( link = Down(hd);  link != hd;  link = NextDown(link) )
    {
      Child(y, link);
      debug1(DGA, DD, "  examining %s", EchoObject(null, y));
      if( type(y) == SPLIT )  Child(y, DownDim(y, ROW));
      switch( type(y) )
      {

	case EXPAND_IND:
	case GALL_PREC:
	case GALL_FOLL:
	case GALL_TARG:
	case CROSS_PREC:
	case CROSS_FOLL:
	case CROSS_TARG:
	    
	  break;


	case PRECEDES:
	case UNATTACHED:
	    
	  if( was_sized )
	  { /* SizeGalley was not called, so hd_inners was not set by it */
	    if( hd_inners == nil )  hd_inners = New(ACAT);
	    Link(hd_inners, y);
	  }
	  break;


	case RECEPTIVE:

	  if( non_blocking(y) )
	  { link = PrevDown(link);
	    DeleteNode(y);
	  }
	  else goto SUSPEND;
	  break;


	case RECEIVING:
	    
	  if( non_blocking(y) )
	  { while( Down(y) != y )
	    { Child(z, Down(y));
	      DetachGalley(z);
	      KillGalley(z);
	    }
	    link = PrevDown(link);
	    DeleteNode(y);
	  }
	  else goto SUSPEND;
	  break;


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
	  switch( CheckConstraint(tmp, target_index) )
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
	case NULL_CLOS:
	case CROSS:

	  break;


	case WORD:
	case ONE_COL:
	case ONE_ROW:
	case WIDE:
	case HIGH:
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

	  /* check availability of vertical space for the first component */
	  if( !external(dest) )
	  { Constrained(dest, &c, ROW);
	    debug2(DSC, D, "Constrained( %s, ROW ) = %s",
			EchoObject(null, dest), EchoConstraint(&c));
	    if( !FitsConstraint(back(y, ROW), fwd(y, ROW), c) )
	    { Error(WARN, &fpos(y),
		"insufficient vertical space for this component of %s in %s",
		SymName(actual(hd)), SymName(actual(dest)));
	      debug3(DGA, D, "  reject: vsize %s,%s in %s; y=",
		EchoLength(back(y, ROW)), EchoLength(fwd(y, ROW)),
		EchoConstraint(&c));
	      ifdebug(DGA, D, EchoObject(stderr, y));
	      goto REJECT;
	    }
	    debug0(DSA, D, "calling AdjustSize from AttachGalley (a)");
	    AdjustSize(dest, back(y, ROW), fwd(y, ROW), ROW);
	  }
	  if( !external(target) )
	  { Constrained(target, &c, ROW);
	    debug2(DSC, D, "Constrained( %s, ROW ) = %s",
			EchoObject(null, target), EchoConstraint(&c));
	    Child(z, LastDown(target_galley));
	    assert( !is_index(type(z)), "AttachGalley: is_index(z)!" );
	    assert( back(z, ROW) >= 0 && fwd(z, ROW) >= 0,
			"AttachGalley: negative z sizes!" );
	    if( !FitsConstraint(back(z, ROW), fwd(z, ROW), c) )
	    { Error(WARN, &fpos(y),
		"insufficient vertical space for this component of %s in %s",
		SymName(actual(hd)), SymName(actual(target)));
	      debug3(DGA, D, "  reject: size was %s,%s in %s; y =",
		EchoLength(back(z, ROW)), EchoLength(fwd(z, ROW)),
		EchoConstraint(&c));
	      ifdebug(DGA, D, EchoObject(stderr, y));
	      goto REJECT;
	    }
	    debug0(DSA, D, "calling AdjustSize from AttachGalley (b)");
	    AdjustSize(target, back(z, ROW), fwd(z, ROW), ROW);
	  }
	  goto ACCEPT;


	default:
	    
	  Error(INTERN, &fpos(y), "AttachGalley: %s", Image(type(y)));
	  break;

      } /* end switch */
    } /* end for */

    /* empty galley; promote any indexes, kill the galley, and exit */
    /* this bypasses target_galley, which is not expanded in the empty case */
    debug0(DGA, D, "  empty galley");
    if( tg_inners != nil )  DisposeObject(tg_inners), tg_inners = nil;
    DisposeObject(target_galley);
    LeaveErrorBlock(FALSE);
    if( LastDown(hd) != hd )  Promote(hd, hd, target_index);
    debug0(DGA, D, "calling KillGalley from AttachGalley (b)");
    KillGalley(hd);

    /* return; only hd_inners needs to be flushed now */
    *inners = hd_inners;
    debug0(DGA, D, "]AttachGalley returning killed: empty galley");
    return;


    REJECT:
	
      /* reject first component */
      LeaveErrorBlock(TRUE);
      if( tg_inners != nil )  DisposeObject(tg_inners), tg_inners = nil;
      DisposeObject(target_galley);
      if( backward(hd) && !sized(hd) )
      {
	/* move to just before the failed target */
	MoveLink(Up(index), Up(target_index), PARENT);
      }
      else
      {
	/* move to just after the failed target */
	MoveLink(Up(index), NextDown(Up(target_index)), PARENT);
      }
      continue;


    SUSPEND:
	
      /* suspend at first component */
      debug1(DGA, D, "  suspend %s", EchoObject(null, y));
      blocked(y) = TRUE;
      LeaveErrorBlock(FALSE);
      if( tg_inners != nil )  DisposeObject(tg_inners), tg_inners = nil;
      DisposeObject(target_galley);
      MoveLink(Up(index), Up(target_index), PARENT);
      if( was_sized )
      { /* nothing new to flush if suspending and already sized */
	if( hd_inners != nil )  DisposeObject(hd_inners), hd_inners = nil;
      }
      else
      { /* flush newly discovered inners if not sized before */
	*inners = hd_inners;
      }
      debug0(DGA, D, "]AttachGalley returning: suspending.");
      return;


    ACCEPT:
	
      /* accept first component; now committed to the attach */
      debug1(DGA, D, "  accept %s", EchoObject(null, y));
      LeaveErrorBlock(TRUE);

      /* adjust horizontal sizes */
      debug0(DSA, D, "calling AdjustSize from AttachGalley (c)");
      AdjustSize(dest, back(hd, COL), fwd(hd, COL), COL);
      debug0(DSA, D, "calling AdjustSize from AttachGalley (d)");
      AdjustSize(target, back(target_galley, COL),
				fwd(target_galley, COL), COL);
		
      /* attach hd to dest */
      MoveLink(Up(hd), dest_index, PARENT);
      assert( type(index) == UNATTACHED, "AttachGalley: type(index)!" );
      DeleteNode(index);

      /* move first component of hd into dest */
      /* nb Interpose must be done after all AdjustSize calls */
      if( !external(dest) )   Interpose(dest, VCAT, hd, y);
      Promote(hd, NextDown(link), dest_index);

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
      if( tg_inners == nil ) *inners = hd_inners;
      else if( hd_inners == nil ) *inners = tg_inners;
      else
      {	TransferLinks(Down(hd_inners), hd_inners, tg_inners);
	DeleteNode(hd_inners);
	*inners = tg_inners;
      }
      debug0(DGA, D, "]AttachGalley returning (accept)");
      return;

  } /* end for */
} /* end AttachGalley */


/*****************************************************************************/
/*                                                                           */
/*  DetachGalley(hd)                                                         */
/*                                                                           */
/*  Detach galley hd from its target.                                        */
/*                                                                           */
/*****************************************************************************/

DetachGalley(hd)
OBJECT hd;
{ OBJECT prnt, index;
  debug1(DGA, D, "DetachGalley( %s )", EchoObject(null, hd));
  assert( type(hd) == HEAD && Up(hd) != hd, "DetachGalley: precondition!" );
  Parent(prnt, Up(hd));
  assert( Up(prnt) != prnt, "DetachGalley: parent!" );
  index = New(UNATTACHED);
  MoveLink(Up(hd), index, PARENT);
  Link(NextDown(Up(prnt)), index);
  debug0(DGA, D, "DetachGalley returning.");
} /* end DetachGalley */
