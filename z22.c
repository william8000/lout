/*@z22.c:Galley Service:Interpose()@******************************************/
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
/*  FILE:         z22.c                                                      */
/*  MODULE:       Galley Service                                             */
/*  EXTERNS:      Interpose(), FlushInners(), ExpandRecursives(),            */
/*                Promote(), KillGalley(), FreeGalley(),                     */
/*                TargetSymbol(), CheckComponentOrder()                      */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define	LAST_ADJUST	1
#define	ALL_ADJUST	2


/*****************************************************************************/
/*                                                                           */
/*  Interpose(z, typ, x, y)                                                  */
/*                                                                           */
/*  Insert a new typ object above z.  Its sizes are to be taken from x       */
/*  (column) and y (row).                                                    */
/*                                                                           */
/*****************************************************************************/

void Interpose(OBJECT z, int typ, OBJECT x, OBJECT y)
{ OBJECT encl = New(typ);
  FposCopy(fpos(encl), fpos(y));
  ReplaceNode(encl, z);  Link(encl, z);
  back(encl, COL) = back(x, COL);
  fwd(encl, COL) = fwd(x, COL);
  back(encl, ROW) = back(y, ROW);
  fwd(encl, ROW) = fwd(y, ROW);
} /* end Interpose */


/*@::FlushInners()@***********************************************************/
/*                                                                           */
/*  FlushInners(inners, hd)                                                  */
/*                                                                           */
/*  Flush each galley on the list inners.  These have become flushable       */
/*  by being promoted off the top of galley hd; if hd is the root galley,    */
/*  identifiable by having PrintSym as target, do not flush inners at all.   */
/*                                                                           */
/*****************************************************************************/

void FlushInners(OBJECT inners, OBJECT hd)
{ OBJECT y, z, tmp, dest_index;

  /* check for root galley case */
  if( hd != nilobj )
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
      
	if( Down(y) != y )	/* bug fix (was assert before) */
	{ assert( Down(y) != y, "FlushInners: UNATTACHED!");
	  Child(z, Down(y));
	  debug0(DGF, D, "  calling FlushGalley from FlushInners (a)");
	  if( whereto(z)==nilobj || !uses_extern_target(whereto(z)) ) /* &&& */
	    FlushGalley(z);
	}
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
	    if( whereto(z)==nilobj || !uses_extern_target(whereto(z)) )/* &&& */
	      FlushGalley(z);
	  }
	}
	break;


      case GALL_PREC:

	/* someone else is looking after this now */
	break;


      default:
      
	Error(22, 1, "FlushInners: %s", INTERN, &fpos(y), Image(type(y)));
	break;
    }
  }
  Dispose(inners);
} /* end FlushInners */


/*@::ExpandRecursives()@******************************************************/
/*                                                                           */
/*  ExpandRecursives(recs)                                                   */
/*                                                                           */
/*  Expand each of the recursive definite objects in the list recs.          */
/*                                                                           */
/*****************************************************************************/

void ExpandRecursives(OBJECT recs)
{ CONSTRAINT non_c, hc, vc;
  OBJECT target_index, target, z, n1, inners, newrecs, hd, tmp, env;
  debug0(DCR, D, "ExpandRecursives(recs)");
  SetConstraint(non_c, MAX_LEN, MAX_LEN, MAX_LEN);  n1 = nilobj;
  assert(recs != nilobj, "ExpandRecursives: recs == nilobj!");
  while( Down(recs) != recs )
  { Child(target_index, Down(recs));  DeleteLink( Down(recs) );
    assert( type(target_index) == RECURSIVE, "ExpandRecursives: index!" );
    target = actual(target_index);
    debug2(DCR, DD, "  expanding %s %s", Image(type(target_index)),
      EchoObject(target));

    /* expand body of target, convert to galley, and check size */
    hd = New(HEAD);  actual(hd) = actual(target);  must_expand(hd) = TRUE;
    whereto(hd) = ready_galls(hd) = nilobj;  backward(hd) = sized(hd) = FALSE;
    tmp =  CopyObject(target, &fpos(target));  env = DetachEnv(tmp);
    Link(hd, tmp);  Link(target_index, hd);
    SizeGalley(hd, env, external(target), threaded(target), FALSE, FALSE,
      &save_style(target), &non_c, nilobj, &n1, &newrecs, &inners);
    debug0(DCR, DDD, "    as galley:");
    ifdebug(DCR, DDD, DebugObject(hd));
    Constrained(target, &hc, COL);
    debug2(DSC, DD, "Constrained( %s, COL ) = %s",
      EchoObject(target), EchoConstraint(&hc));
    debug3(DCR, DD, "    horizontal size: (%s, %s); constraint: %s",
      EchoLength(back(hd, COL)), EchoLength(fwd(hd, COL)), EchoConstraint(&hc));
    if( !FitsConstraint(back(hd, COL), fwd(hd, COL), hc) )
    { DisposeChild(Up(hd));
      if( inners != nilobj ) DisposeObject(inners);
      if( newrecs != nilobj ) DisposeObject(newrecs);
      DeleteNode(target_index);
      debug0(DCR, DD, "    rejecting (too wide)");
      continue;
    }
    if( !external(target) )
    { Constrained(target, &vc, ROW);
      debug2(DSC, DD, "Constrained( %s, ROW ) = %s",
	EchoObject(target), EchoConstraint(&vc));
      Child(z, LastDown(hd));
      debug3(DCR, DD, "    vsize: (%s, %s); constraint: %s",
	EchoLength(back(z, ROW)), EchoLength(fwd(z, ROW)), EchoConstraint(&vc));
      if( !FitsConstraint(back(z, ROW), fwd(z, ROW), vc) )
      {	DisposeChild(Up(hd));
	if( inners != nilobj ) DisposeObject(inners);
	if( newrecs != nilobj ) DisposeObject(newrecs);
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
    if( inners != nilobj )  FlushInners(inners, nilobj);
    if( newrecs != nilobj )  MergeNode(recs, newrecs);
  } /* end while */
  Dispose(recs);
  debug0(DCR, D, "ExpandRecursives returning.");
} /* end ExpandRecursives */

/*@::FindSplitInGalley()@*****************************************************/
/*                                                                           */
/*  static OBJECT FindSplitInGalley(hd)                                      */
/*                                                                           */
/*  Search simply joined galley hd for a SPLIT object, which must be there.  */
/*                                                                           */
/*****************************************************************************/

static OBJECT FindSplitInGalley(OBJECT hd)
{ OBJECT link, y;
  debug0(DGF, D, "FindSplitInGalley(hd)");
  for( link = Down(hd);  link != hd;  link = NextDown(link) )
  { Child(y, link);
    if( is_definite(type(y)) )  break;
  }
  if( link == hd )
  { debug0(DGF, D, "FindSplitInGalley failing, no definite component; hd =");
    ifdebug(DGF, D, DebugObject(hd));
    Error(22, 2, "FindSplit: missing galley component", INTERN, &fpos(hd));
  }
  while( type(y) != SPLIT )  switch( type(y) )
  {
    case VCAT:
    case ONE_ROW:
    case WIDE:
    case HIGH:
    case HSHIFT:
    case VSHIFT:
    case VCONTRACT:
    case VEXPAND:
    case PADJUST:
    case VADJUST:

      Child(y, Down(y));
      break;


    case CLOSURE:
    case NULL_CLOS:
    case PAGE_LABEL:
    case HCAT:
    case WORD:
    case QWORD:
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
      ifdebug(DGF, D, DebugObject(hd));
      Error(22, 3, "FindSplitInGalley failed", INTERN, &fpos(y),Image(type(y)));
      break;


    default:
    
      Error(22, 4, "FindSplitInGalley: %s", INTERN, &fpos(y), Image(type(y)));
      break;

  }
  debug0(DGF, D, "FindSplitInGalley returning.");
  return y;
} /* end FindSplitInGalley */

/*@::Promote()@***************************************************************/
/*                                                                           */
/*  Promote(x, stop_link, dest_index)                                        */
/*                                                                           */
/*  Promote components of galley x into its destination (dest), up to but    */
/*  not including the one linked to x by link stop_link, which always        */
/*  follows a component.  No size adjustments are made, except that when     */
/*  two col_thr nodes are merged, a COL adjustment is made to the result.    */
/*                                                                           */
/*****************************************************************************/

void Promote(OBJECT x, OBJECT stop_link, OBJECT dest_index)
{
  /* these two variables refer to the root galley only */
  static BOOLEAN first = TRUE;	/* TRUE when the first component not written */
  static OBJECT page_label=nilobj;	/* current page label object         */

  OBJECT dest, link, y, z, tmp1, tmp2;  FULL_CHAR *label_string;
  int dim;
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
    /* SetGap(gap(y), FALSE, seen_nojoin(x), FIXED_UNIT, NO_MODE, 0); */
    SetGap(gap(y), FALSE, TRUE, FIXED_UNIT, NO_MODE, 0);
    /* SetGap(gap(y), FALSE, threaded(dest), FIXED_UNIT, NO_MODE, 0); */
    /* ClearGap(gap(y)); */
    Link(stop_link, y);
  }

  /* error if promoting a seen_nojoin galley into a threaded destination */
  if( seen_nojoin(x) && threaded(dest) )
    Error(22, 5, "galley %s must have a single column mark",
      FATAL, &fpos(x), SymName(actual(x)));

  /* make nojoin status clear by adding an extra gap at start if needed */
  if( !external(dest) && seen_nojoin(x) && join(gap(y)) )
  { OBJECT prnt, extra_null, extra_gap;

    /* add nojoin gap at start */
    Parent(prnt, Up(dest));  /* can't be threaded */
    assert( type(prnt) == VCAT, "Promote: nojoin case, can't find VCAT" );
    extra_null = New(NULL_CLOS);
    back(extra_null, COL) = fwd(extra_null, COL) = 0;
    back(extra_null, ROW) = fwd(extra_null, ROW) = 0;
    extra_gap = New(GAP_OBJ);
    hspace(extra_gap) = vspace(extra_gap) = 0;
    SetGap(gap(extra_gap), FALSE, FALSE, FIXED_UNIT, EDGE_MODE, 0);
    Link(Down(prnt), extra_gap);
    Link(Down(prnt), extra_null);
    debug0(DGS, D, "  Promote adding extra nojoin gap");
    /* join(gap(y)) = FALSE; */
  }


  /* if promoting out of root galley, do special things */
  if( actual(dest) == PrintSym )
  { CONSTRAINT c;
    link = x;
    while( NextDown(link) != stop_link )
    { Child(y, NextDown(link));
      debug1(DGS, D, "root promote %s", EchoObject(y));
      if( type(y) == SPLIT )  Child(y, DownDim(y, ROW));
      switch( type(y) )
      {

	case SCALE_IND:
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
	    pinpoint(y) = nilobj;
	    Child(t, Down(z));
	    actual(y) = CrossMake(whereto(z), t, GALL_PREC);
	    DisposeChild(Down(y));
	    CrossSequence(actual(y));
	    DisposeChild(NextDown(link));
	  }
	  else
	  {
	    /* galley was never attached, print message and kill it */
	    Error(22, 6, "galley %s deleted (never attached)",
	      WARN, &fpos(z), SymName(actual(z)));
	    KillGalley(z);
	  }
	  break;


	case EXPAND_IND:
      
	  /* expand @HExpand or @VExpand to occupy everything possible */
	  dim = type(actual(y)) == HEXPAND ? COL : ROW;
          debug1(DGP, D, " flushing %s", EchoObject(y));
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


	case PAGE_LABEL_IND:

	  if( page_label != nilobj )
	  { DisposeObject(page_label);
	    page_label = nilobj;
	  }
	  Child(z, Down(y));
	  assert( type(z) == PAGE_LABEL, "Promote: type(z) != PAGE_LABEL!" );
	  assert( Down(z) != z, "Promote: PAGE_LABEL Down(z) == z!" );
	  Child(page_label, Down(z));
	  DeleteLink(Up(page_label));
	  debug2(DGP, D, " new page label %s %s", Image(type(page_label)),
	    EchoObject(page_label));
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

	case CLOSURE:
	case NULL_CLOS:
	case PAGE_LABEL:
	case CROSS:

	  /* print this component */
	  debug0(DCR, D, "Promote --");
	  if( !is_indefinite(type(y)) && size(y, ROW) != 0 )
	  {
	    /* fix horizontally; work out which fonts needed */
	    /* *** nice in theory (preserves joins), but ... 
	    FixAndPrintObject(y, back(x, COL), back(x, COL), fwd(x, COL),
	      COL, LAST_ADJUST, FALSE, LAST_ADJUST, 0, 0);
	    *** */
	    FixAndPrintObject(y, back(y, COL), back(y, COL), fwd(y, COL),
	      COL, LAST_ADJUST, FALSE, LAST_ADJUST, 0, 0);

	    /* print prefatory or page separating material, including fonts */
	    label_string = page_label != nilobj && is_word(type(page_label)) ?
	      string(page_label) : AsciiToFull("?");
	    if( first )
	    { PrintBeforeFirst(size(x, COL), size(y, ROW), label_string);
	      first = FALSE;
	    }
	    else PrintBetween(size(x, COL), size(y, ROW), label_string);
	    if( page_label != nilobj )
	    { DisposeObject(page_label);
	      page_label = nilobj;
	    }

	    /* fix and print vertically */
	    debug1(DGF,D, "  Promote calling FixAndPrint %s", Image(type(y)));
	    FixAndPrintObject(y, back(y,ROW), back(y, ROW), fwd(y, ROW),
	      ROW, LAST_ADJUST, FALSE, LAST_ADJUST, size(y,ROW), 0);

	  }
	  DisposeChild(NextDown(link));

	  /* scavenge any filter files now not needed */
	  FilterScavenge(FALSE);
	  break;


	case GAP_OBJ:

	  DisposeChild(NextDown(link));
	  break;


	default:
      
	  Error(22, 7, "Promote: %s", INTERN, &fpos(y), Image(type(y)));
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


/*@::MakeDead(), KillGalley()@************************************************/
/*                                                                           */
/*  static MakeDead(y)                                                       */
/*                                                                           */
/*  Convert object y into a DEAD object and remove it to the dead store.     */
/*                                                                           */
/*****************************************************************************/

static void MakeDead(OBJECT y)
{ static int	dead_count = 0;		/* number of DEAD objects seen       */
  static OBJECT	dead_store = nilobj;	/* where DEAD objects are kept       */

  debug1(DGS, DDD, "MakeDead( %s )", Image(type(y)));
  if( dead_store == nilobj )  dead_store = New(ACAT);
  type(y) = DEAD;
  MoveLink(Up(y), dead_store, PARENT);
  if( dead_count >= 150 )  DisposeChild(Down(dead_store));
  else dead_count++;
  debug1(DGS, DDD, "MakeDead returning (dead_count = %d).", dead_count);
} /* end MakeDead */


/*****************************************************************************/
/*                                                                           */
/*  KillGalley(hd)                                                           */
/*                                                                           */
/*  Kill galley hd, which may be sized or unsized.  The index of hd must     */
/*  be UNATTACHED; it is moved out of its present location to a secret spot. */
/*                                                                           */
/*****************************************************************************/

void KillGalley(OBJECT hd)
{ OBJECT prnt, link, y, z;
  debug2(DGF, D, "[ KillGalley(Galley %s into %s)",
	SymName(actual(hd)), SymName(whereto(hd)));
  assert( type(hd) == HEAD && Up(hd) != hd, "KillGalley: precondition!" );
  Parent(prnt, Up(hd));
  assert( type(prnt) == UNATTACHED, "KillGalley: UNATTACHED precondition!" );
  assert( Up(prnt) != prnt, "KillGalley: prnt!" );

  if( ready_galls(hd) != nilobj )
  { DisposeObject(ready_galls(hd));
    ready_galls(hd) = nilobj;
  }
  for( link = hd; NextDown(link) != hd; )
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
			Child(z, Down(y));  KillGalley(z);
			break;

      case HEAD:	Error(22, 8, "KillGalley: head", INTERN, &fpos(y));
			break;

      default:		DisposeChild(NextDown(link));
			break;
    }
  }

  /* move index into dead_store */
  MakeDead(prnt);
  debug0(DGF, D, "] KillGalley returning.");
} /* end KillGalley */


/*@::FreeGalley()@************************************************************/
/*                                                                           */
/*  FreeGalley(hd, stop_link, inners, relocate_link, sym)                    */
/*                                                                           */
/*  Free galley hd up to but not including stop_link.  *Inners is well-      */
/*  defined, either nilobj or an ACAT of galleys to be flushed.              */
/*                                                                           */
/*  Relocate_link defines what to do any galley attached to one of the       */
/*  freed targets.  If it is non-nilobj, galley hd is searched onwards from  */
/*  it to see if a target can be found there.  If so, the galley is          */
/*  relocated to just before that point.  If not, or if relocate_link is     */
/*  nilobj, the galley is freed and added to *inners for flushing.  If the   */
/*  whereto() of such galley is sym, it is freed, not relocated, because the */
/*  cause of this call to FreeGalley is also targeted to sym, and it will    */
/*  consume all possible targets of sym.                                     */
/*                                                                           */
/*****************************************************************************/

void FreeGalley(OBJECT hd, OBJECT stop_link, OBJECT *inners,
OBJECT relocate_link, OBJECT sym)
{ OBJECT link, y, z, zlink, srch, index;
  assert( type(hd) == HEAD && sized(hd), "FreeGalley: pre!");
  assert( Up(hd) != hd, "FreeGalley: Up(hd)!" );
  assert( *inners == nilobj || type(*inners) == ACAT, "FreeGalley: ACAT!" );
  debug3(DGA, D, "[ FreeGalley(Galley %s into %s); rl %s nilobj",
    SymName(actual(hd)), SymName(whereto(hd)),
    relocate_link == nilobj ? "==" : "!=");

  /* close targets and move or flush any inner galleys */
  for( link = Down(hd);  link != stop_link;  link = NextDown(link) )
  { Child(y, link);
    if( type(y) == RECEIVING && actual(actual(y)) == InputSym )
      Error(22, 9, "forcing galley after input point", WARN, &fpos(actual(y)));
    else if( type(y) == RECEIVING )
    {
      /* either relocate or free each galley */
      for( zlink = Down(y);  zlink != y; )
      {	Child(z, zlink);
	zlink = NextDown(zlink);
	assert( type(z) == HEAD, "FreeGalley/RECEIVING: type(z) != HEAD!" );
	debug1(DGA, D, "FreeGalley examining galley %s", SymName(actual(z)));
	if( relocate_link != nilobj && whereto(z) != sym &&
	    (srch = SearchGalley(relocate_link, whereto(z), TRUE,
	    FALSE, TRUE, FALSE)) != nilobj )
	{ DetachGalley(z);
	  Parent(index, Up(z));
	  MoveLink(Up(index), Up(srch), PARENT);  /* just before new dest */
	}
	else
	{ debug0(DGA, D, "  calling FreeGalley from FreeGalley");
	  FreeGalley(z, z, inners, nilobj, sym);
	  if( *inners == nilobj )  *inners = New(ACAT);
	  Link(*inners, y);
	}
      }
      non_blocking(y) = TRUE;
    }
    else if( type(y) == RECEPTIVE )  non_blocking(y) = TRUE;
  }
  debug0(DGA, D, "] FreeGalley returning.");
} /* end FreeGalley */


/*@::TargetSymbol()@**********************************************************/
/*                                                                           */
/*  BOOLEAN TargetSymbol(x, sym)                                             */
/*                                                                           */
/*  Examine the parameters of closure x, which is known to have a @Target.   */
/*  Return TRUE if the target is preceding, and set sym to the symbol value. */
/*                                                                           */
/*****************************************************************************/

BOOLEAN TargetSymbol(OBJECT x, OBJECT *sym)
{ OBJECT y, link, cr, lpar, rpar;
  debug1(DGS, D, "TargetSymbol( %s )", EchoObject(x));
  assert( type(x) == CLOSURE, "TargetSymbol: type(x) != CLOSURE!" );
  assert( has_target(actual(x)), "TargetSymbol: x has no target!" );

  /* search the free variable list of x for @Target */
  cr = nilobj;
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link);
    if( type(y) == PAR && is_target(actual(y)) )
    { assert( Down(y) != y, "TargetSymbol: Down(PAR)!" );
      Child(cr, Down(y));
      break;
    }
  }

  /* search the children list of actual(x) for a default value of @Target */
  if( cr == nilobj )
  for( link = Down(actual(x));  link != actual(x);  link = NextDown(link) )
  { Child(y, link);
    if( is_target(y) )
    { cr = sym_body(y);
      break;
    }
  }
  
  if( cr != nilobj )
  {
    /* check that cr is indeed a cross-reference object */
    debug1(DGS, DD, "TargetSymbol examining %s", EchoObject(cr));
    debug1(DGS, DD, "  type(cr) = %s", Image( (int) type(cr)) );
    if( type(cr) != CROSS )
      Error(22, 10, "target of %s is not a cross reference",
	FATAL, &fpos(cr), SymName(actual(x)));

    /* extract *sym from the left parameter */
    Child(lpar, Down(cr));
    if( type(lpar) != CLOSURE )
      Error(22, 11, "left parameter of %s is not a symbol",
	FATAL, &fpos(lpar), KW_CROSS);
    *sym = actual(lpar);

    /* extract direction from the right parameter */
    Child(rpar, NextDown(Down(cr)));
    if( !is_word(type(rpar)) ||
	( !StringEqual(string(rpar), KW_PRECEDING) &&
	  !StringEqual(string(rpar), KW_FOLLOWING) ) )
      Error(22, 12, "replacing %s%s? by %s%s%s",
	WARN, &fpos(rpar), SymName(actual(lpar)), KW_CROSS,
	SymName(actual(lpar)), KW_CROSS, KW_FOLLOWING);
    return is_word(type(rpar)) && StringEqual(string(rpar), KW_PRECEDING);
  }
  else
  { Error(22, 13, "TargetSymbol: missing target of x", INTERN, &fpos(x));
    return FALSE;
  }
} /* end TargetSymbol */


/*@::CheckComponentOrder()@***************************************************/
/*                                                                           */
/*  int CheckComponentOrder(preceder, follower)                              */
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

int CheckComponentOrder(OBJECT preceder, OBJECT follower)
{ OBJECT prec_galley, foll_galley, z;  int res;
  debug2(DGS, D, "CheckComponentOrder( %s, %s )",
    EchoObject(preceder), EchoObject(follower));
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
  debug1(DGS, D, "CheckComponentOrder returning %s", Image(res));
  return res;
} /* end CheckComponentOrder */
