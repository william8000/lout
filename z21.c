/*@z21.c:Galley Maker:SizeGalley()@*******************************************/
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
/*    env         its environment                                            */
/*    rows        TRUE if the resulting galley may have more than one row    */
/*    joined      TRUE if the resulting galley must be simply joined         */
/*    nonblock    Set the non_blocking() field of RECEPTIVEs to this value   */
/*    trig        TRUE if galley's indefinites may trigger external galleys  */
/*    *style      The initial style                                          */
/*    *c          the width constraint hd should conform to                  */
/*    target      if non-nil, expand indefinite objects to reveal a          */
/*                @Galley within this symbol                                 */
/*                                                                           */
/*  The output parameters, in addition to the converted hd, are:             */
/*                                                                           */
/*    dest_index  the index of the @Galley found within target, if any       */
/*    recs        list of all RECURSIVE indexes found (or nil if none)       */
/*    inners      list of all UNATTACHED indexes found (or nil if none)      */
/*                                                                           */
/*****************************************************************************/

SizeGalley(hd, env, rows, joined, nonblock, trig, style, c, target,
						dest_index, recs, inners)
OBJECT hd, env;  BOOLEAN rows, joined, nonblock, trig;  STYLE *style;
CONSTRAINT *c;  OBJECT target, *dest_index, *recs, *inners;
{ OBJECT y, link, z, crs, t, tlink, zlink, tmp;
  OBJECT extras, tmp1, tmp2, bt[2], ft[2];
  
  assert( type(hd) == HEAD && Down(hd) != hd, "SizeGalley: precondition!" );
  assert( !sized(hd), "SizeGalley: already sized!" );
  debug6(DGM, D, "SizeGalley(hd, -, %s, %s, %s, %s, %s, %s, -, -, -), hd =",
	bool(joined), bool(nonblock), bool(trig), EchoStyle(style),
	EchoConstraint(c), SymName(target));
  ifdebug(DGM, DD, EchoObject(stderr, hd));

  /* manifest hd's child, making sure it is simply joined if required */
  tmp1 = target;
  Child(y, Down(hd));
  crs = nil;
  bt[COL] = ft[COL] = bt[ROW] = ft[ROW] = nil;
  if( joined )
  { bt[COL] = New(THREAD);  ft[COL] = New(THREAD);
    y = Manifest(y, env, style, bt, ft, &tmp1, &crs, TRUE, must_expand(hd));
    assert( Down(bt[COL]) != bt[COL] && Down(ft[COL]) != ft[COL],
	"SizeGalley: threads!" );
    Child(tmp1, Down(bt[COL]));  Child(tmp2, Down(ft[COL]));
    if( Down(bt[COL]) != LastDown(bt[COL]) ||
	  Down(ft[COL]) != LastDown(ft[COL]) || tmp1 != tmp2 )
      Error(FATAL, &fpos(y), "galley %s must have just one column mark",
						SymName(actual(hd)) );
    DisposeObject(bt[COL]);  DisposeObject(ft[COL]);
  }
  else y = Manifest(y, env, style, bt, ft, &tmp1, &crs, TRUE, must_expand(hd));

  /* horizontally size and break hd */
  debug0(DGM, DD, "SizeGalley: after manifesting, hd =");
  ifdebug(DGM, DD, EchoObject(stderr, hd));
  debug0(DGM, DD, "SizeGalley horizontally sizing and breaking hd:");
  CopyConstraint(constraint(hd), *c);
  y = MinSize(y, COL, &extras);
  debug0(DOB, DD, "  calling BreakObject from SizeGalley");
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
  { /* OBJECT prev_gap = nil; */
    debug0(DGM, DD, "SizeGalley cleaning up rows of hd:");
    for( link = hd;  NextDown(link) != hd;  link = NextDown(link) )
    { Child(y, NextDown(link));
      debug2(DGM,DD,"  cleaning %s: %s", Image(type(y)), EchoObject(null,y));
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

  /* size the rows of hd and attach indices where needed */
  debug0(DGM, DD, "SizeGalley sizing rows of hd =");
  ifdebug(DGM, DD, EchoObject(stderr, hd));
  *recs = *inners = *dest_index = nil;
  for( link = Down(hd);  link != hd;  link = NextDown(link) )
  { Child(y, link);
    if( type(y) == GAP_OBJ || is_index(type(y)) )  continue;
    debug0(DGM, DDD, "  ROW sizing:");
    ifdebug(DGM, DDD, EchoObject(stderr, y));
    extras = New(ACAT);
    y = MinSize(y, ROW, &extras);
    debug3(DSF, D, "MinSize( %s , ROW ) = %s,%s", EchoObject(null, y),
	  EchoLength(back(y, ROW)), EchoLength(fwd(y, ROW)) );
    debug0(DGM, DDD, "  ROW result:");
    ifdebug(DGM, DDD, EchoObject(stderr, y));

    /* now attach indexes in front of y */
    for( zlink = Down(extras);  zlink != extras;  zlink = NextDown(zlink) )
    { Child(z, zlink);
      blocked(z) = FALSE;
      /* debug1(DCR, D, "  extra: %s", EchoObject(null, z)); */
      debug1(DGM, DD, "  extra: %s", EchoObject(null, z));
      switch( type(z) )
      {
	case RECEPTIVE:

	  /* debug2(DCR, D, "  ... uses_ext  = %s, trig = %s",
	    bool(uses_extern_target(actual(actual(z)))), bool(trig)); */
	  trigger_externs(z) = uses_extern_target(actual(actual(z))) && trig;
	  non_blocking(z)    = nonblock;
	  if( actual(actual(z)) == GalleySym )  *dest_index = z;
	  break;


	case RECURSIVE:

	  if( *recs == nil )  *recs = New(ACAT);
	  Link(*recs, z);
	  break;


	case UNATTACHED:

	  if( *inners == nil )  *inners = New(ACAT);
	  Link(*inners, z);
	  break;

		
	case EXPAND_IND:
	case GALL_PREC:
	case GALL_FOLL:
	case GALL_TARG:
	case CROSS_PREC:
	case CROSS_FOLL:
	case CROSS_TARG:

	  debug1(DCR, DD, "  SizeGalley: %s", EchoObject(null, z));
	  break;


	default:
	  
	  Error(INTERN, no_fpos, "SizeGalley: %s", Image(type(z)) );
	  break;

      }
    }
    TransferLinks(Down(extras), extras, link);
    assert( Down(extras) == extras && Up(extras) == extras, "SizeG: extras!");
    Dispose(extras);
  }
  
  /* insinuate cross references */
  if( crs != nil )
  { 
    debug1(DCR, D, "SizeGalley insinuating %s", crs);
    TransferLinks(Down(crs), crs, Down(hd));
    DisposeObject(crs);
  }

  /* check that *dest_index was found if it was required, and exit */
  if( target != nil && *dest_index == nil )
    Error(FATAL, &fpos(hd), "Unexpected absence of %s from the body of %s",
      SymName(target), SymName(actual(hd)));
  debug3(DGM, D, "SizeGalley returning %s,%s  %s;  hd =",
    EchoLength(back(hd, COL)), EchoLength(fwd(hd, COL)),
    EchoConstraint(&constraint(hd)));
  ifdebug(DGM, DD, EchoObject(stderr, hd));
  sized(hd) = TRUE;

} /* end SizeGalley */
