/*@z20.c:Galley Flushing:ParentFlush()@***************************************/
/*                                                                           */
/*  LOUT: A HIGH-LEVEL LANGUAGE FOR DOCUMENT FORMATTING (VERSION 2.05)       */
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
/*  FILE:         z20.c                                                      */
/*  MODULE:       Galley Flushing                                            */
/*  EXTERNS:      FlushGalley()                                              */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  ParentFlush(dest_index, kill)                                            */
/*                                                                           */
/*  Flush the galley which is the parent of dest_index, if likely to flush.  */
/*  If kill is TRUE, delete dest_index.                                      */
/*                                                                           */
/*****************************************************************************/

#define ParentFlush(dest_index, kill)					\
if( prnt_flush )							\
{ debug0(DGF,D, "  ParentFlush calling FlushGalley (prnt)");		\
  Parent(prnt, Up(dest_index));						\
  if( kill )  DeleteNode(dest_index);					\
  debug0(DGF, D, "  calling FlushGalley from ParentFlush");		\
  FlushGalley(prnt);							\
  prnt_flush = FALSE;							\
}									\
else if( kill )  DeleteNode(dest_index)


/*@::FlushGalley()@***********************************************************/
/*                                                                           */
/*  FlushGalley(hd)                                                          */
/*                                                                           */
/*  Flush galley hd as far as possible.  It could be the root galley.        */
/*                                                                           */
/*****************************************************************************/

FlushGalley(hd)
OBJECT hd;
{ OBJECT dest;			/* the target galley hd empties into         */
  OBJECT dest_index;		/* the index of dest                         */
  OBJECT inners;		/* list of galleys and PRECEDES to flush     */
  OBJECT link, y;		/* for scanning through the components of hd */

  CONSTRAINT dest_constraint;	/* the vertical size constraint on dest      */
  int f;			/* candidate replacement value for dest_fwd  */

  OBJECT dest_encl;		/* the VCAT enclosing dest, if any           */
  int    dest_side;		/* if dest_encl != nil, the side dest is on  */
  BOOLEAN need_adjust;		/* TRUE as soon as dest_encl needs adjusting */
  LENGTH dest_back, dest_fwd;	/* the current size of dest_encl or dest     */
  LENGTH frame_size;		/* the total constraint of dest_encl         */
  OBJECT prec_gap;		/* the gap preceding dest, if any, else nil  */
  OBJECT prec_def;		/* the component preceding dest, if any      */
  OBJECT succ_gap;		/* the gap following dest, if any, else nil  */
  OBJECT succ_def;		/* the component following dest, if any      */
  OBJECT stop_link;		/* most recently seen gap link of hd         */
  BOOLEAN prnt_flush;		/* TRUE when the parent of hd needs a flush  */
  OBJECT zlink, z, tmp, prnt;

  debug1(DGF, D, "[ FlushGalley %s (hd)", SymName(actual(hd)));
  prnt_flush = FALSE;

  RESUME:
  assert( type(hd) == HEAD, "FlushGalley: type(hd) != HEAD!" );
  debug1(DGF, D, "  resuming FlushGalley %s, hd =", SymName(actual(hd)));
  ifdebug(DGF, DD, DebugObject(hd));
  assert( Up(hd) != hd, "FlushGalley: resume found no parent to hd!" );


  /*@@************************************************************************/
  /*                                                                         */
  /*  The first step is to examine the parent of galley hd to determine the  */
  /*  status of the galley.  If this is not suitable for flushing, we do     */
  /*  what we can to change the status.  If still no good, return; so if     */
  /*  this code does not return, then the galley is ready to flush into a    */
  /*  destination in the normal way, and the following variables are set:    */
  /*                                                                         */
  /*     dest_index   the parent of the galley and index of its destination  */
  /*     dest         the destination of the galley, a @Galley object        */
  /*                                                                         */
  /***************************************************************************/

  Parent(dest_index, Up(hd));
  switch( type(dest_index) )
  {

    case DEAD:
    
      /* the galley has been killed off while this process was sleeping */
      debug1(DGF, D, "] FlushGalley %s returning (DEAD)", SymName(actual(hd)));
      debug1(DGF, D, "    prnt_flush = %s", bool(prnt_flush));
      return;


    case UNATTACHED:
    
      /* the galley is currently not attached to a destination */
      AttachGalley(hd, &inners);
      Parent(dest_index, Up(hd));
      if( type(dest_index)!=RECEIVING || actual(actual(dest_index))==InputSym )
      {	if( type(dest_index) != DEAD )
	{ ParentFlush(dest_index, FALSE);
	  if( inners != nil ) FlushInners(inners, nil);
	}
	debug1(DGF,D,"] FlushGalley %s retn, no attach", SymName(actual(hd)));
	debug1(DGF, D, "    prnt_flush = %s", bool(prnt_flush));
	return;
      }

      /* if hd is a forcing galley, close all predecessors */
      if( actual(hd) != nil && force_target(actual(hd)) )
      {	Parent(prnt, Up(dest_index));
	debug0(DGA, DD, "  force: prnt =");
	ifdebug(DGA, DD, DebugObject(prnt));
	debug1(DGA, D,"  calling FreeGalley from FlushGalley(%s)",
	  SymName(actual(hd)));
	FreeGalley(prnt, Up(dest_index), &inners, Up(dest_index), whereto(hd));
	prnt_flush = TRUE;
	debug0(DGA, DD, "  force: after FreeGalley, prnt =");
	ifdebug(DGA, DD, DebugObject(prnt));
      }
      else prnt_flush = prnt_flush || blocked(dest_index);
      debug1(DGF, D, "    prnt_flush = %s", bool(prnt_flush));

      if( inners != nil ) FlushInners(inners, nil);
      goto RESUME;
      break;


    case RECEIVING:
    
      if( actual(actual(dest_index)) == InputSym )
      { ParentFlush(dest_index, FALSE);
	debug1(DGF, D, "] FlushGalley %s retn, input", SymName(actual(hd)));
	debug1(DGF, D, "    prnt_flush = %s", bool(prnt_flush));
	return;
      }
      break;


    default:
    
      Error(INTERN, &fpos(hd), "FlushGalley: %s ind!", Image(type(dest_index)));
      break;
  }
  dest = actual(dest_index);
  debug1(DGF, DD, "  dest_index: %s", EchoObject(dest_index));


  /*@@************************************************************************/
  /*                                                                         */
  /*  The second step is to examine the components of the galley one by one  */
  /*  to determine if they can be promoted.  Each component has the format   */
  /*                                                                         */
  /*    { <index> } <object>                                                 */
  /*                                                                         */
  /*  and is always followed by a gap object (except the last component).    */
  /*  An index indicates that the following object has some interesting      */
  /*  feature, and it points to that feature inside the object.  There are   */
  /*  two possible actions for each component, in addition to accepting it:  */
  /*                                                                         */
  /*    REJECT:   The component does not fit, so detach the galley           */
  /*    SUSPEND:  The component is incomplete; go to sleep and wait          */
  /*                                                                         */
  /***************************************************************************/

  stop_link = dest_encl = inners = nil;
  need_adjust = FALSE;

  /***************************************************************************/
  /*                                                                         */
  /*  Loop invariant                                                         */
  /*                                                                         */
  /*  The children of hd up to but not including Child(link) have been       */
  /*  examined and pronounced to be promotable.                              */
  /*                                                                         */
  /*  stop_link is the link of the most recently encountered gap object of   */
  /*  hd, or nil if no gap object has been encountered yet.                  */
  /*                                                                         */
  /*  if dest_encl is non-nil, then the destination is not external,         */
  /*  dest_encl is its parent, and the following variables are defined:      */
  /*                                                                         */
  /*    prec_gap         gap object preceding dest (which must exist)        */
  /*    prec_def         first definite object preceding dest (must exist)   */
  /*    dest_back        back(dest_encl) including effect of accepted compts */
  /*    dest_fwd         fwd(dest_encl) including effect of accepted compts  */
  /*    dest_side        BACK or FWD, i.e. which side of the mark dest is on */
  /*    dest_constraint  the size constraint on dest                         */
  /*    frame_size       size of frame enclosing dest_encl                   */
  /*                                                                         */
  /*  if dest_encl is nil, these variables are not defined.                  */
  /*                                                                         */
  /*  need_adjust is true if at least one definite component has been        */
  /*  accepted for promotion and the destination is internal; hence,         */
  /*  dest_encl is defined and its size needs to be adjusted.                */
  /*                                                                         */
  /*  inners is the set of all PRECEDES and UNATTACHED indexes found.        */
  /*                                                                         */
  /***************************************************************************/

  for( link = Down(hd);  link != hd;  link = NextDown(link) )
  {
    Child(y, link);
    if( type(y) == SPLIT )  Child(y, DownDim(y, ROW));
    debug1(DGF, DD, "  try to flush %s", EchoObject(y));
    switch( type(y) )
    {

      case GAP_OBJ:

	prec_gap = y;
	stop_link = link;
	if( !join(gap(y)) )  seen_nojoin(hd) = TRUE;
	break;


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
	  
	if( inners == nil )  inners = New(ACAT);
	Link(inners, y);
	break;


      case RECEIVING:
      case RECEPTIVE:
	  
	goto SUSPEND;


      case FOLLOWS:
	  
	Child(tmp, Down(y));
	if( Up(tmp) == LastUp(tmp) )
	{ link = PrevDown(link);
	  DisposeChild(NextDown(link));
	  break;
	}
	Parent(tmp, Up(tmp));
	assert(type(tmp) == PRECEDES, "Flush: PRECEDES!");
	switch( CheckConstraint(tmp, dest_index) )
	{
	  case CLEAR:	DeleteNode(tmp);
			link = PrevDown(link);
			DisposeChild(NextDown(link));
			break;

	  case PROMOTE:	break;

	  case BLOCK:	goto SUSPEND;

	  case CLOSE:	goto REJECT;
	}
	break;


      case WORD:
      case QWORD:
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

	/* make sure y is not joined to a target below */
	for( zlink = NextDown(link); zlink != hd; zlink = NextDown(zlink) )
	{ Child(z, zlink);
	  switch( type(z) )
	  {
	    case RECEPTIVE:
	    case RECEIVING:	y = z;
				goto SUSPEND;
				break;

	    case GAP_OBJ:	if( !join(gap(z)) )  zlink = PrevDown(hd);
				break;

	    default:		break;
	  }
	}

	/* check size constraint */
	if( !external(dest) )
	{
	  /* initialise dest_encl etc if not done yet */
	  if( dest_encl == nil )
	  { assert( UpDim(dest,COL) == UpDim(dest,ROW), "FlushG: UpDims!" );
	    Parent(dest_encl, NextDown(Up(dest)));
	    assert( type(dest_encl) == VCAT, "FlushGalley: dest != VCAT!" );
	    SetNeighbours(Up(dest), FALSE, &prec_gap, &prec_def,
	      &succ_gap, &succ_def, &dest_side);
	    assert(prec_gap != nil || is_indefinite(type(y)),
	      "FlushGalley: prec_gap == nil && !is_indefinite(type(y))!" );
	    assert(succ_gap == nil, "FlushGalley: succ_gap != nil!" );
	    assert(dest_side == FWD || is_indefinite(type(y)),
	      "FlushGalley: dest_side != FWD || !is_indefinite(type(y))!");
	    dest_back = back(dest_encl, ROW);
	    dest_fwd  = fwd(dest_encl, ROW);
	    Constrained(dest_encl, &dest_constraint, ROW);
	    frame_size = constrained(dest_constraint) ? bfc(dest_constraint) :0;
	  }

	  if( !is_indefinite(type(y)) )
	  { /* calculate effect of adding y to dest */
	    f = dest_fwd  + fwd(y, ROW) - fwd(prec_def, ROW) +
		  ActualGap(fwd(prec_def, ROW), back(y, ROW),
			fwd(y, ROW), &gap(prec_gap), frame_size,
			dest_back + dest_fwd - fwd(prec_def, ROW));
	    debug3(DGF, DD, "  b,f: %s,%s;   dest_encl: %s",
			EchoLength(dest_back), EchoLength(f),
			EchoConstraint(&dest_constraint));

	    /* check new size against constraint */
	    if( !FitsConstraint(dest_back,f,dest_constraint) )
	      goto REJECT;
	    if( units(gap(prec_gap))==FRAME_UNIT && width(gap(prec_gap)) > FR )
	      goto REJECT;

	    /* accept component */
	    dest_fwd = f;  prec_def = y;
	    need_adjust = TRUE;
	  }

	} /* end if( !external(dest) ) */

	/* accept this component into dest */
	debug1(DGF, D, "  accept %s", EchoObject(y));
	prnt_flush = prnt_flush || blocked(dest_index);
	debug1(DGF, D, "    prnt_flush = %s", bool(prnt_flush));
	if( inners != nil )
	{ Promote(hd, NextDown(link), dest_index);
	  if( need_adjust )
	  { debug0(DSA, D, "  calling AdjustSize from FlushGalley (ACCEPT)");
	    AdjustSize(dest_encl, dest_back, dest_fwd, ROW);
	  }
	  FlushInners(inners, hd);
	  goto RESUME;
	}
	break;


      default:
	  
	Error(INTERN, &fpos(y), "FlushGalley: %s", Image(type(y)));
	break;

    } /* end switch */

  } /* end for */


  /* EMPTY: */

    /* galley is now completely accepted; clean up and exit */
    debug0(DGF, DD, "  galley empty now");
    if( inners != nil )  DisposeObject(inners);
    if( Down(hd) != hd )
    { Promote(hd, hd, dest_index);
      if( need_adjust )
      { debug0(DSA, D, "  calling AdjustSize from FlushGalley (EMPTY)");
	AdjustSize(dest_encl, dest_back, dest_fwd, ROW);
      }
    }
    DetachGalley(hd);
    debug0(DGF, D, "  calling KillGalley from FlushGalley");
    KillGalley(hd);
    ParentFlush(dest_index, TRUE);
    debug1(DGF,D,"] FlushGalley %s returning (emptied).", SymName(actual(hd)));
      debug1(DGF, D, "    prnt_flush = %s", bool(prnt_flush));
    return;


  REJECT:
  
    /* reject this component and move to a new dest */
    debug1(DGF, D, "  reject %s", EchoObject(y));
    assert(actual(dest) != PrintSym, "FlushGalley: reject print!");
    if( inners != nil )  DisposeObject(inners);
    if( stop_link != nil )
    { Promote(hd, stop_link, dest_index);
      if( need_adjust )
      { debug0(DSA, D, "  calling AdjustSize from FlushGalley (REJECT)");
	AdjustSize(dest_encl, dest_back, dest_fwd, ROW);
      }
    }
    DetachGalley(hd);
    assert( type(dest_index) == RECEIVING, "FlushGalley/REJECT: dest_index!" );
    prnt_flush = prnt_flush || blocked(dest_index); /* **** bug fix **** */
    DeleteNode(dest_index);
    goto RESUME;


  SUSPEND:
  
    /* suspend this component */
    debug1(DGF, D, "  suspend %s", EchoObject(y));
    if( inners != nil )  DisposeObject(inners);
    if( stop_link != nil )
    { Promote(hd, stop_link, dest_index);
      if( need_adjust )
      { debug0(DSA, D, "  calling AdjustSize from FlushGalley (SUSPEND)");
	AdjustSize(dest_encl, dest_back, dest_fwd, ROW);
      }
    }

    /* check whether external galleys can remove the blockage */
    if( type(y) == RECEPTIVE && ready_galls(hd) != nil && AllowCrossDb )
    { OBJECT eg, val, index2, hd2, tag, seq, newsym;
      BOOLEAN found, gall;  FULL_CHAR newtag[MAX_LINE], newseq[MAX_LINE];

      /* get first ready galley in from cross reference database */
      Child(eg, Down(ready_galls(hd)));
      val = ReadFromFile(eg_fnum(eg), eg_fpos(eg), nil);
      if( val == nil ) Error(FATAL, &fpos(y),
	"Error in database file %s", FileName(eg_fnum(eg)));
      assert( type(val) == CLOSURE, "AttachG: db CLOSURE!" );
      index2 = New(UNATTACHED);
      hd2 = New(HEAD);
      FposCopy(fpos(hd2), fpos(val));
      actual(hd2) = actual(val);
      backward(hd2) = TargetSymbol(val, &whereto(hd2));
      backward(hd2) = sized(hd2) = FALSE;
      ready_galls(hd2) = nil;
      must_expand(hd2) = TRUE;
      Link(index2, hd2);
      Link(hd2, val);
      Link(Up(y), index2);

      /* set up the next ready galley for reading next time */
      Child(tag, Down(eg));  Child(seq, LastDown(eg));
      do /* skip duplicate seq values */
      {	found = DbRetrieveNext(OldCrossDb, &gall, &newsym,
		 newtag, newseq, &eg_fnum(eg), &eg_fpos(eg), &eg_cont(eg));
	debug2(DGF, D, "  ext gall  found:   %15s  gall:    %15s",
			bool(gall), bool(found));
	debug2(DGF, D, "  ext gall  new sym: %15s  old sym: %15s",
			SymName(newsym), SymName(eg_symbol(eg)));
	debug2(DGF, D, "  ext gall  new tag: %15s  old tag: %15s",
			newtag, string(tag));
	debug2(DGF, D, "  ext gall  new seq: %15s  old seq: %15s",
			newseq, string(seq));
	if( found )  found = gall && newsym == eg_symbol(eg) &&
			StringEqual(newtag, string(tag));
      } while( found && StringEqual(newseq, string(seq)) );
      if( found )
      {	DisposeChild(Up(tag));
	DisposeChild(Up(seq));
	tag = MakeWord(WORD, newtag, no_fpos);
	seq = MakeWord(WORD, newseq, no_fpos);
	Link(eg, tag);  Link(eg, seq);
	debug1(DGF,D, "  another ext gall: into %s", SymName(newsym));
      }
      else
      {	DisposeChild(Up(eg));
	debug1(DGF,D, "  last ext gall into ", SymName(eg_symbol(eg)));
	if( Down(ready_galls(hd)) == ready_galls(hd) )
	{ Dispose(ready_galls(hd));
	  ready_galls(hd) = nil;
	  debug0(DGF,D, "  all ext galls exhausted");
	}
      }

      /* flush the ready galley found above, and resume */
      debug2(DGF, D, "  ext gall FlushGalley (%s into %s)",
			SymName(actual(hd2)), SymName(whereto(hd2)));
      debug0(DGF, D, "  calling FlushGalley from FlushGalley/SUSPEND");
      FlushGalley(hd2);
      goto RESUME;
    }
    else if( type(y) == RECEPTIVE && trigger_externs(y) && AllowCrossDb )
    { OBJECT sym, cr, ins, tag, seq, eg, cnt;  BOOLEAN found;
      FULL_CHAR newseq[MAX_LINE];  FILE_NUM tfnum;  long tfpos, tcont;
      debug1(DGF, D, "  ext gall target %s", SymName(actual(actual(y))));
      for( sym = FirstExternTarget(actual(actual(y)), &cnt);
	     sym != nil;  sym = NextExternTarget(actual(actual(y)), &cnt) )
      {
	debug1(DGF, D, "  ext gall gall_targ %s", SymName(sym));
	cr = GallTargEval(sym, &fpos(actual(y)));
	ins = New(GALL_TARG);
	actual(ins) = cr;
	Link(Up(y), ins);
	Child(tag, LastDown(cr));
	assert( is_word(type(tag)), "FlushGalley: cr is_word(type(tag))!" );
	found = DbRetrieve(OldCrossDb, TRUE, sym, string(tag),
		newseq, &tfnum, &tfpos, &tcont);
	if( found )
	{ if( ready_galls(hd) == nil )  ready_galls(hd) = New(ACAT);
	  eg = New(EXT_GALL);
	  debug1(DGF, D, "  ext gall retrieved: into %s", SymName(sym));
	  eg_fnum(eg) = tfnum;
	  eg_fpos(eg) = tfpos;
	  eg_symbol(eg) = sym;
	  eg_cont(eg) = tcont;
	  tag = MakeWord(WORD, string(tag), no_fpos);
	  Link(eg, tag);
	  seq = MakeWord(WORD, newseq, no_fpos);
	  Link(eg, seq);
	  Link(ready_galls(hd), eg);
	}
      }
      trigger_externs(y) = FALSE;
      if( ready_galls(hd) != nil )  goto RESUME;
    } /* end if external galleys */

    /* if non-blocking, delete the index and resume */
    if( type(y) == RECEPTIVE && non_blocking(y) )
    { DeleteNode(y);
      goto RESUME;
    }
    else if( type(y) == RECEIVING && non_blocking(y) )
    {	
      if( Down(y) == y )
      {	DeleteNode(y);
      }
      else
      {	Child(z, Down(y));
	DetachGalley(z);
      }
      goto RESUME;
    }

    /* if all the above fail to remove the blockage, suspend */
    blocked(y) = TRUE;
    ParentFlush(dest_index, FALSE);
      debug1(DGF, D, "    prnt_flush = %s", bool(prnt_flush));
    debug1(DGF, D, "] FlushGalley %s returning (suspend)", SymName(actual(hd)));
    return;

} /* end FlushGalley */
