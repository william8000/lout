/*@z22.c:Galley Service:FlushInners(), Promote(), KillGalley()@***************/
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
/*  FILE:         z22.c                                                      */
/*  MODULE:       Galley Service                                             */
/*  EXTERNS:      FlushInners(), ExpandRecursives(), Promote(),              */
/*                KillGalley(), FreeGalley(), Interpose(),                   */
/*                TargetSymbol(), CheckConstraint()                          */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define	LAST_ADJUST	1
#define	ALL_ADJUST	2


/*****************************************************************************/
/*                                                                           */
/*  static MakeDead(y)                                                       */
/*                                                                           */
/*  Convert object y into a DEAD object and remove it to the dead store.     */
/*                                                                           */
/*****************************************************************************/

static MakeDead(y)
OBJECT y;
{ static int	dead_count = 0;		/* number of DEAD objects seen       */
  static OBJECT	dead_store = nil;	/* where DEAD objects are kept       */

  debug1(DGS, DDD, "MakeDead( %s )", Image(type(y)));
  if( dead_store == nil )  dead_store = New(ACAT);
  type(y) = DEAD;
  MoveLink(Up(y), dead_store, PARENT);
  if( dead_count >= 100 )  DisposeChild(Down(dead_store));
  else dead_count++;
  debug1(DGS, DDD, "MakeDead returning (dead_count = %d).", dead_count);
} /* end MakeDead */

/*****************************************************************************/
/*                                                                           */
/*  FlushInners(inners, hd)                                                  */
/*                                                                           */
/*  Flush each galley on the list inners.  These have become flushable       */
/*  by being promoted off the top of galley hd; if hd is the root galley,    */
/*  identifiable by having PrintSym as target, don't flush inners at all.    */
/*                                                                           */
/*****************************************************************************/

FlushInners(inners, hd)
OBJECT inners, hd;
{ OBJECT y, z, tmp, dest_index;

  /* check for root galley case */
  if( hd != nil )
  { assert( Up(hd) != hd, "FlushInners: Up(hd)!" );
    Parent(dest_index, Up(hd));
    if( actual(actual(dest_index)) == PrintSym )
    { DisposeObject(inners);
      return;
    }
  }

  while( Down(inners) != inners )
  { Child(y, Down(inners));
    DeleteLink(Down(inners));
    switch( type(y) )
    {

      case DEAD:
      
	break;


      case RECEIVING:
      case UNATTACHED:
      
	assert( Down(y) != y, "FlushInners: UNATTACHED!");
	Child(z, Down(y));
	debug0(DGF, D, "  calling FlushGalley from FlushInners (a)");
	FlushGalley(z);
	break;


      case PRECEDES:
      
	Child(tmp, Down(y));
	if( Up(tmp) != LastUp(tmp) )
	{ Parent(tmp, LastUp(tmp));
	  assert(type(tmp)==FOLLOWS, "FlushInners: FOLLOWS!");
	  if( blocked(tmp) )
	  { blocked(tmp) = FALSE;
	    Parent(z, Up(tmp));
	    debug0(DGF, D, "  calling FlushGalley from FlushInners (b)");
	    FlushGalley(z);
	  }
	}
	break;


      default:
      
	Error(INTERN,&fpos(y),"FlushInners %s", Image(type(y)));
	break;
    }
  }
  Dispose(inners);
} /* end FlushInners */


/*@@**************************************************************************/
/*                                                                           */
/*  ExpandRecursives(recs)                                                   */
/*                                                                           */
/*  Expand each of the recursive definite objects in the list recs.          */
/*                                                                           */
/*****************************************************************************/

ExpandRecursives(recs)
OBJECT recs;
{ CONSTRAINT non_c, hc, vc;
  OBJECT target_index, target, z, n1, inners, newrecs, hd, tmp, env;
  debug0(DCR, D, "ExpandRecursives(recs)");
  SetConstraint(non_c, MAX_LEN, MAX_LEN, MAX_LEN);  n1 = nil;
  assert(recs != nil, "ExpandRecursives: recs == nil!");
  while( Down(recs) != recs )
  { Child(target_index, Down(recs));
    DeleteLink( Down(recs) );
    assert( type(target_index) == RECURSIVE, "ExpandRecursives: index!" );
    target = actual(target_index);
    debug2(DCR, DD, "  expanding %s %s", Image(type(target_index)),
      EchoObject(null, target));

    /* expand body of target, convert to galley, and check size */
    hd = New(HEAD);  actual(hd) = actual(target);
    whereto(hd) = ready_galls(hd) = nil;  must_expand(hd) = TRUE;
    backward(hd) = sized(hd) = FALSE;
    tmp =  CopyObject(target, &fpos(target));
    env = DetachEnv(tmp);
    Link(hd, tmp);  Link(target_index, hd);
    SizeGalley(hd, env, external(target), threaded(target), FALSE, FALSE,
      &save_style(target), &non_c, nil, &n1, &newrecs, &inners);
    debug0(DCR, DDD, "    as galley:");
    ifdebug(DCR, DDD, EchoObject(stderr, hd));
    Constrained(target, &hc, COL);
    debug2(DSC, D, "Constrained( %s, COL ) = %s",
      EchoObject(null, target), EchoConstraint(&hc));
    debug3(DCR, DD, "    horizontal size: (%s, %s); constraint: %s",
      EchoLength(back(hd, COL)), EchoLength(fwd(hd, COL)), EchoConstraint(&hc));
    if( !FitsConstraint(back(hd, COL), fwd(hd, COL), hc) )
    { DisposeChild(Up(hd));
      if( inners != nil ) DisposeObject(inners);
      if( newrecs != nil ) DisposeObject(newrecs);
      DeleteNode(target_index);
      debug0(DCR, DD, "    rejecting (too wide)");
      continue;
    }
    if( !external(target) )
    { Constrained(target, &vc, ROW);
      debug2(DSC, D, "Constrained( %s, ROW ) = %s",
			EchoObject(null, target), EchoConstraint(&vc));
      Child(z, LastDown(hd));
      debug3(DCR, DD, "    vsize: (%s, %s); constraint: %s",
	EchoLength(back(z, ROW)), EchoLength(fwd(z, ROW)), EchoConstraint(&vc));
      if( !FitsConstraint(back(z, ROW), fwd(z, ROW), vc) )
      {	DisposeChild(Up(hd));
	if( inners != nil ) DisposeObject(inners);
	if( newrecs != nil ) DisposeObject(newrecs);
	DeleteNode(target_index);
	debug0(DCR, DD, "    rejecting (too high)");
	continue;
      }
    }

    /* object fits; adjust sizes and promote */
    debug0(DSA, D, "calling AdjustSize from ExpandRecursives (a)");
    AdjustSize(target, back(hd, COL), fwd(hd, COL), COL);
    if( !external(target) )
    { debug0(DSA, D, "calling AdjustSize from ExpandRecursives (b)");
      AdjustSize(target, back(z, ROW), fwd(z, ROW), ROW);
      Interpose(target, VCAT, z, z);
    }
    Promote(hd, hd, target_index);  DeleteNode(hd);
    DeleteNode(target_index);
    if( inners != nil )  FlushInners(inners, nil);
    if( newrecs != nil )  MergeNode(recs, newrecs);
  } /* end while */
  Dispose(recs);
  debug0(DCR, D, "ExpandRecursives returning.");
} /* end ExpandRecursives */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT FindSplitInGalley(hd)                                             */
/*                                                                           */
/*  Search simply joined galley hd for a SPLIT object, which must be there.  */
/*                                                                           */
/*****************************************************************************/

static OBJECT FindSplitInGalley(hd)
OBJECT hd;
{ OBJECT link, y;
  debug0(DGF, D, "FindSplitInGalley(hd)");
  for( link = Down(hd);  link != hd;  link = NextDown(link) )
  { Child(y, link);
    if( is_definite(type(y)) )  break;
  }
  if( link == hd )
  { debug0(DGF, D, "FindSplitInGalley failing, no definite component; hd =");
    ifdebug(DGF, D, EchoObject(stderr, hd));
    Error(INTERN, &fpos(hd), "missing galley component");
  }
  while( type(y) != SPLIT )  switch( type(y) )
  {
    case VCAT:
    case ONE_ROW:
    case WIDE:
    case HIGH:
    case VCONTRACT:
    case VEXPAND:
    case PADJUST:
    case VADJUST:

      Child(y, Down(y));
      break;


    case CLOSURE:
    case NULL_CLOS:
    case HCAT:
    case WORD:
    case ACAT:
    case ROW_THR:
    case COL_THR:
    case ONE_COL:
    case SCALE:
    case HSCALE:
    case VSCALE:
    case HCONTRACT:
    case HEXPAND:
    case HADJUST:
    case ROTATE:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case GRAPHIC:

      debug0(DGF, D, "FindSplitInGalley(hd) failing, hd =");
      ifdebug(DGF, D, EchoObject(stderr, hd));
      Error(INTERN, &fpos(y), "FindSplitInGalley failed", Image(type(y)));
      break;


    default:
    
      Error(INTERN, &fpos(y), "FindSplitInGalley found %s", Image(type(y)));
      break;

  }
  debug0(DGF, D, "FindSplitInGalley returning.");
  return y;
} /* end FindSplitInGalley */

/*@@**************************************************************************/
/*                                                                           */
/*  Promote(x, stop_link, dest_index)                                        */
/*                                                                           */
/*  Promote components of galley x into its destination (dest), up to but    */
/*  not including the one linked to x by link stop_link, which always        */
/*  follows a component.  No size adjustments are made, except that when     */
/*  two col_thr nodes are merged, a COL adjustment is made to the result.    */
/*                                                                           */
/*****************************************************************************/

Promote(x, stop_link, dest_index)
OBJECT x, stop_link, dest_index;
{
  /* these four variables refer to the root galley only */
  static BOOLEAN first = TRUE;	/* TRUE when the first component not written */
  static int	prec_back;	/* back value of preceding component         */
  static int	prec_fwd;	/* fwd value of preceding component          */
  static GAP	prec_gap;	/* preceding gap                             */

  OBJECT dest, link, y, z, tmp1, tmp2;
  int dim;  CONSTRAINT c;
  debug1(DGS, D, "Promote(%s, stop_link)", SymName(actual(x)));

  assert( type(x) == HEAD, "Promote: x!" );
  assert( type(stop_link) == LINK || stop_link == x, "Promote: stop_link!" );
  assert( stop_link != Down(x), "Promote: stop_link == Down(x)!" );
  type(dest_index) = RECEIVING;
  dest = actual(dest_index);

  /* insert final gap if galley is ending */
  if( stop_link != x )
  { Child(y, stop_link);
    assert( type(y) == GAP_OBJ, "Promote: missing GAP_OBJ!" );
    stop_link = NextDown(stop_link);
  }
  else
  { y = New(GAP_OBJ);
    FposCopy(fpos(y), fpos(x));
    hspace(y) = 0;  vspace(y) = 1;
    ClearGap(gap(y));
    Link(stop_link, y);
  }

  /* error if promoting a seen_nojoin galley into a threaded destination */
  if( seen_nojoin(x) && threaded(dest) )
    Error(FATAL, &fpos(x), "galley %s must have a single column mark",
	SymName(actual(x)));
  if( seen_nojoin(x) )  join(gap(y)) = FALSE; /* to make nojoin status clear */

  /* if promoting out of root galley, do special things */
  if( actual(dest) == PrintSym )
  { CONSTRAINT c;
    link = x;
    while( NextDown(link) != stop_link )
    { Child(y, NextDown(link));
      debug1(DGS, D, "root promote %s", EchoObject(null, y));
      if( type(y) == SPLIT )  Child(y, DownDim(y, ROW));
      switch( type(y) )
      {

	case PRECEDES:
      
	  DisposeChild(NextDown(link));
	  break;
	

	case UNATTACHED:
      
	  assert( Down(y) != y, "FlushRootGalley: UNATTACHED!" );
	  Child(z, Down(y));
	  assert( type(z) == HEAD, "FlushRootGalley: unattached HEAD!" );
	  if( sized(z) )
	  {
	    /* galley is part flushed, leave it here */
	    link = NextDown(link);
	  }
	  else if( backward(z) )
	  {
	    /* galley is preceding, send to CrossSequence */
	    OBJECT t;
	    type(y) = GALL_PREC;
	    Child(t, Down(z));
	    actual(y) = CrossMake(whereto(z), t, GALL_PREC);
	    DisposeChild(Down(y));
	    CrossSequence(actual(y));
	    DisposeChild(NextDown(link));
	  }
	  else
	  {
	    /* galley was never attached, print message and kill it */
	    Error(WARN, &fpos(z), "Galley %s deleted - never attached",
			SymName(actual(z)));
	    KillGalley(z);
	  }
	  break;


	case EXPAND_IND:
      
	  /* expand @HExpand or @VExpand to occupy everything possible */
	  dim = type(actual(y)) == HEXPAND ? COL : ROW;
          debug1(DGP, D, " flushing %s", EchoObject(null, y));
	  Constrained(actual(y), &c, dim);
	  if( constrained(c) )
	  { LENGTH b = back(actual(y), dim);
	    LENGTH f = fwd(actual(y), dim);
	    EnlargeToConstraint(&b, &f, &c);
	    debug2(DGP, D, "FlushRoot call AdjustSize(x, %s,%s, dim)",
			EchoLength(b), EchoLength(f));
	    debug1(DSA, D, "Promote %s AdjustSize", Image(type(actual(y))));
	    AdjustSize(actual(y), b, f, dim);
	  }
	  DisposeChild(NextDown(link));
	  break;


	case GALL_PREC:
	case GALL_FOLL:
	case GALL_TARG:
	case CROSS_PREC:
	case CROSS_FOLL:
	case CROSS_TARG:
	      
	  CrossSequence(actual(y));
	  DisposeChild(NextDown(link));
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

	case CLOSURE:
	case NULL_CLOS:
	case CROSS:

	  /* print this component */
	  debug0(DCR, D, "Promote --");
	  if( !is_indefinite(type(y)) && size(y, ROW) != 0 )
	  {
	    /* move down as specified by the gap */
	    if( first )
	    { PrintPrologue(size(x, COL), size(y, ROW));
	      first = FALSE;
	    }
	    else PrintOriginIncrement(prec_back - back(y, ROW)
	          + MinGap(prec_fwd, back(y, ROW), fwd(y, ROW), &prec_gap));
	    debug1(DGF,D, "  Promote calling FixAndPrint %s", Image(type(y)));
	    /* old interface ***
	    SetConstraint(c, back(x, COL), size(x, COL), fwd(x, COL));
	    FixAndPrintObject(y, back(x, COL), &c, COL, FALSE, 0, 0);
	    SetConstraint(c, back(y, ROW), size(y, ROW), fwd(y, ROW));
	    FixAndPrintObject(y, back(y,ROW), &c, ROW, FALSE, size(y,ROW), 0);
	    *** */
	    FixAndPrintObject(y, back(x, COL), back(x, COL), fwd(x, COL),
	      COL, LAST_ADJUST, FALSE, LAST_ADJUST, 0, 0);
	    FixAndPrintObject(y, back(y,ROW), back(y, ROW), fwd(y, ROW),
	      ROW, LAST_ADJUST, FALSE, LAST_ADJUST, size(y,ROW), 0);
	    prec_back = back(y, ROW);  prec_fwd = fwd(y, ROW);
	  }
	  DisposeChild(NextDown(link));
	  break;


	case GAP_OBJ:

	  GapCopy(prec_gap, gap(y));
	  DisposeChild(NextDown(link));
	  break;


	default:
      
	  Error(INTERN, &fpos(y), "Promote (root): %s", Image(type(y)));
	  break;
	
      }
    }
    debug0(DGS, D, "Promote returning (root galley).");
    return;
  }

  /* prepare the promotion */
  if( external(dest) )
  { if( threaded(dest) )
    { Parent(tmp1, UpDim(dest, COL));
      assert( type(tmp1) == COL_THR, "Promote: tmp1 not COL_THR!" );
      y = FindSplitInGalley(x);
      assert( type(y) == SPLIT, "Promote: FindSplitInGalley!" );
      Child(tmp2, DownDim(y, COL));
      assert( type(tmp2) == COL_THR, "Promote: tmp2 not COL_THR!" );
      if( tmp1 != tmp2 )
      { LENGTH b = max(back(tmp1, COL), back(tmp2, COL));
	LENGTH f = max(fwd(tmp1, COL),  fwd(tmp2, COL));
	debug0(DSA, D, "calling AdjustSize(tmp1) from Promote (node merging)");
	AdjustSize(tmp1, b, f, COL);
	debug0(DSA, D, "calling AdjustSize(tmp2) from Promote (node merging)");
	AdjustSize(tmp2, b, f, COL);
	MergeNode(tmp1, tmp2);
      }
    }
    link = Up(dest_index);
  }
  else
  { for( link = x;  NextDown(link) != stop_link;  )
    { Child(y, NextDown(link));
      if( is_index(type(y)) )  MoveLink(NextDown(link), Up(dest_index), PARENT);
      else link = NextDown(link);
    }
    assert( Down(x) != stop_link, "Promote: Down(x) == stop_link!" );
    assert( UpDim(dest, ROW) == UpDim(dest, COL), "Promote: dims!" );
    link = Up(dest);
  }
  
  /* promote components */
  TransferLinks(Down(x), stop_link, link);

  debug0(DGS, D, "Promote returning.");
} /* end Promote */


/*@@**************************************************************************/
/*                                                                           */
/*  KillGalley(hd)                                                           */
/*                                                                           */
/*  Kill galley hd, which may be sized or unsized.  The galley's index must  */
/*  be UNATTACHED; it is moved out of its present location to a secret spot. */
/*                                                                           */
/*****************************************************************************/

KillGalley(hd)
OBJECT hd;
{ OBJECT prnt, link, y, z;
  debug2(DGA, D, "[ KillGalley(Galley %s into %s)",
	SymName(actual(hd)), SymName(whereto(hd)));
  assert( type(hd) == HEAD && Up(hd) != hd, "KillGalley: precondition!" );
  Parent(prnt, Up(hd));
  assert( type(prnt) == UNATTACHED, "KillGalley: UNATTACHED precondition!" );
  assert( Up(prnt) != prnt, "KillGalley: prnt!" );

  if( ready_galls(hd) != nil )
  { DisposeObject(ready_galls(hd));
    ready_galls(hd) = nil;
  }
  link = hd;
  while( NextDown(link) != hd )
  { Child(y, NextDown(link));
    switch( type(y) )
    {
      case RECEIVING:	while( Down(y) != y )
			{ Child(z, Down(y));
			  DetachGalley(z);
			}
			DeleteNode(y);
			break;
		
      case RECEPTIVE:	assert( Down(y) == y, "KillGalley: RECEPTIVE!" );
			DeleteNode(y);
			break;

      case UNATTACHED:	assert( Down(y) != y, "KillGalley: UNATTACHED!" );
			Child(z, Down(y));
			KillGalley(z);
			break;

      case HEAD:	Error(INTERN, &fpos(y), "KillGalley: HEAD!");
			break;

      default:		DisposeChild(NextDown(link));
			break;
    }
  }

  /* move index into dead_store */
  MakeDead(prnt);
  debug0(DGA, D, "] KillGalley returning.");
} /* end KillGalley */


/*@@**************************************************************************/
/*                                                                           */
/*  FreeGalley(hd, stop_link, inners, relocate_link, sym)                    */
/*                                                                           */
/*  Free galley hd up to but not including stop_link.  *Inners is well-      */
/*  defined, either nil or an ACAT of galleys to be flushed.                 */
/*                                                                           */
/*  Relocate_link defines what to do any galley attached to one of the       */
/*  freed targets.  If it is non-nil, galley hd is searched onwards from     */
/*  it to see if a target can be found there.  If so, the galley is          */
/*  relocated to just before that point.  If not, or if relocate_link is     */
/*  nil, the galley is freed and added to *inners for flushing.  If such     */
/*  galley's whereto() is sym, it is freed, not relocated, because the       */
/*  cause of this call to FreeGalley is also targeted to sym, and it will    */
/*  consume all possible targets of sym.                                     */
/*                                                                           */
/*****************************************************************************/

FreeGalley(hd, stop_link, inners, relocate_link, sym)
OBJECT hd, stop_link, *inners, relocate_link, sym;
{ OBJECT link, y, z, zlink, srch, index;
  assert( type(hd) == HEAD && sized(hd), "FreeGalley: pre!");
  assert( Up(hd) != hd, "FreeGalley: Up(hd)!" );
  assert( *inners == nil || type(*inners) == ACAT, "FreeGalley: ACAT!" );
  debug3(DGA, D, "[ FreeGalley(Galley %s into %s); rl %s nil",
    SymName(actual(hd)), SymName(whereto(hd)), relocate_link==nil ? "==":"!=");

  /* close targets and move or flush any inner galleys */
  for( link = Down(hd);  link != stop_link;  link = NextDown(link) )
  { Child(y, link);
    if( type(y) == RECEIVING && actual(actual(y)) == InputSym )
      Error(WARN, &fpos(actual(y)), "forcing galley past input point");
    else if( type(y) == RECEIVING )
    {
      /* either relocate or free each galley */
      for( zlink = Down(y);  zlink != y; )
      {	Child(z, zlink);
	zlink = NextDown(zlink);
	assert( type(z) == HEAD, "FreeGalley/RECEIVING: type(z) != HEAD!" );
	debug1(DGA, D, "FreeGalley examining galley %s", SymName(actual(z)));
	if( relocate_link != nil && whereto(z) != sym &&
	    (srch = SearchGalley(relocate_link, whereto(z), TRUE,
	    FALSE, TRUE, FALSE)) != nil )
	{ DetachGalley(z);
	  Parent(index, Up(z));
	  MoveLink(Up(index), Up(srch), PARENT);  /* just before new dest */
	}
	else
	{ debug0(DGA, D, "  calling FreeGalley from FreeGalley");
	  FreeGalley(z, z, inners, nil, sym);
	  if( *inners == nil )  *inners = New(ACAT);
	  Link(*inners, y);
	}
      }
      non_blocking(y) = TRUE;
    }
    else if( type(y) == RECEPTIVE )  non_blocking(y) = TRUE;
  }
  debug0(DGA, D, "] FreeGalley returning.");
} /* end FreeGalley */


/*****************************************************************************/
/*                                                                           */
/*  Interpose(z, typ, x, y)                                                  */
/*                                                                           */
/*  Insert a new typ object above z.  Its sizes are to be taken from x       */
/*  (column) and y (row).                                                    */
/*                                                                           */
/*****************************************************************************/

Interpose(z, typ, x, y)
OBJECT z;  int typ;  OBJECT x, y;
{ OBJECT encl = New(typ);
  FposCopy(fpos(encl), fpos(y));
  ReplaceNode(encl, z);  Link(encl, z);
  back(encl, COL) = back(x, COL);
  fwd(encl, COL) = fwd(x, COL);
  back(encl, ROW) = back(y, ROW);
  fwd(encl, ROW) = fwd(y, ROW);
} /* end Interpose */

/*@@**************************************************************************/
/*                                                                           */
/*  BOOLEAN TargetSymbol(x, sym)                                             */
/*                                                                           */
/*  Examine the parameters of closure x, which is known to have a @Target.   */
/*  Return TRUE if the target is preceding, and set sym to the symbol value. */
/*                                                                           */
/*****************************************************************************/

BOOLEAN TargetSymbol(x, sym)
OBJECT x, *sym;
{ OBJECT y, link, cr, lpar, rpar;
  debug1(DGS, D, "TargetSymbol( %s )", EchoObject(null, x));
  assert( type(x) == CLOSURE, "TargetSymbol: type(x) != CLOSURE!" );
  assert( has_target(actual(x)), "TargetSymbol: x has no target!" );

  /* search the free variable list of x for @Target */
  cr = nil;
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link);
    if( type(y) == PAR && is_target(actual(y)) )
    { assert( Down(y) != y, "TargetSymbol: Down(PAR)!" );
      Child(cr, Down(y));
      break;
    }
  }

  /* search the children list of actual(x) for a default value of @Target */
  if( cr == nil )
  for( link = Down(actual(x));  link != actual(x);  link = NextDown(link) )
  { Child(y, link);
    if( is_target(y) )
    { cr = sym_body(y);
      break;
    }
  }
  
  if( cr != nil )
  {
    /* check that cr is indeed a cross-reference object */
    debug1(DGS, DD, "TargetSymbol examining %s", EchoObject(null, cr));
    debug1(DGS, DD, "  type(cr) = %s", Image( (int) type(cr)) );
    if( type(cr) != CROSS )
      Error(FATAL, &fpos(cr), "target of %s is not a cross-reference",
	SymName(actual(x)));

    /* extract *sym from the left parameter */
    Child(lpar, Down(cr));
    if( type(lpar) != CLOSURE )
      Error(FATAL,&fpos(lpar),"left parameter of %s is not a symbol",KW_CROSS);
    *sym = actual(lpar);

    /* extract direction from the right parameter */
    Child(rpar, NextDown(Down(cr)));
    if( type(rpar) != WORD || (strcmp(string(rpar), KW_PRECEDING) != 0 &&
	strcmp(string(rpar), KW_FOLLOWING) != 0) )
      Error(WARN, &fpos(rpar), "replacing %s%s? by %s%s%s",
	SymName(actual(lpar)), KW_CROSS, SymName(actual(lpar)),
	KW_CROSS, KW_FOLLOWING);
    return type(rpar) == WORD && strcmp(string(rpar), KW_PRECEDING) == 0;
  }
  else
  { Error(INTERN, &fpos(x), "TargetSymbol: couldn't find x's @Target");
    return FALSE;
  }
} /* end TargetSymbol */


/*@@**************************************************************************/
/*                                                                           */
/*  int CheckConstraint(preceder, follower)                                  */
/*                                                                           */
/*  Check the ordering relation between components preceder and follower,    */
/*  and return its current status:                                           */
/*                                                                           */
/*      CLEAR     follower definitely follows preceder, and always will;     */
/*      PROMOTE   follower is not prevented from following preceder;         */
/*      CLOSE     follower must move down its galley to follow preceder;     */
/*      BLOCK     follower cannot be guaranteed to follow preceder.          */
/*                                                                           */
/*****************************************************************************/

int CheckConstraint(preceder, follower)
OBJECT preceder, follower;
{ OBJECT prec_galley, foll_galley, z;  int res;
  debug2(DGS, D, "CheckConstraint( %s, %s )",
    EchoObject(null, preceder), EchoObject(null, follower));
  Parent(prec_galley, Up(preceder));
  Parent(foll_galley, Up(follower));
  if( prec_galley == foll_galley )
  { res = CLOSE;
    for( z = Up(follower);  z != foll_galley;  z = pred(z, CHILD) )
    if( z == Up(preceder) )
    { res = CLEAR;
      break;
    }
  }
  else
  { res = PROMOTE;
    while( Up(prec_galley) != prec_galley )
    { Parent(z, Up(prec_galley));	/* index of galley */
      Parent(prec_galley, Up(z));	/* enclosing galley */
      if( prec_galley == foll_galley )
      {	res = BLOCK;
	break;
      }
    }
  }
  debug1(DGS, D, "CheckConstraint returning %s", Image(res));
  return res;
} /* end CheckConstraint */
