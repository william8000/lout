/*@z13.c:Object Breaking:BreakJoinedGroup()@**********************************/
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
/*  FILE:         z13.c                                                      */
/*  MODULE:       Object Breaking                                            */
/*  EXTERNS:      BreakObject()                                              */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define	broken(x)	back(x, ROW)	/* OK since no vertical sizes yet    */


/*****************************************************************************/
/*                                                                           */
/*  static BreakJoinedGroup(start, stop, m, c, res_back, res_fwd)            */
/*                                                                           */
/*  Break joined group of components of a VCAT, beginning from Child(start)  */
/*  inclusive and ending at Child(stop) inclusive.  Break component m first  */
/*  because it is the widest.                                                */
/*                                                                           */
/*****************************************************************************/

static BreakJoinedGroup(start, stop, m, c, res_back, res_fwd)
OBJECT start, stop, m;  CONSTRAINT *c;  LENGTH *res_back, *res_fwd;
{ OBJECT y, link, z;  LENGTH b, f;  CONSTRAINT yc;
  ifdebug(DOB, DD, Child(y, start));
  ifdebug(DOB, DD, Child(z, stop));
  debug1(DOB, DD, "BreakJoinedGroup(start, stop, m, %s, -, -)",
	EchoConstraint(c));
  CopyConstraint(yc, *c);
  if( m != nil )
  { m = BreakObject(m, &yc);
    b = back(m, COL);
    f = fwd(m, COL);
    SetConstraint(yc, min(bc(yc), bfc(yc)-f), bfc(yc), min(fc(yc), bfc(yc)-b));
  }
  else b = f = 0;
  for( link = start;  link != NextDown(stop);  link = NextDown(link) )
  { Child(y, link);
    if( !is_definite(type(y)) || y == m )  continue;
    y = BreakObject(y, &yc);
    b = max(b, back(y, COL));
    f = max(f, fwd(y, COL));
    SetConstraint(yc, min(bc(yc), bfc(yc)-f), bfc(yc), min(fc(yc), bfc(yc)-b));
  }
  assert( FitsConstraint(b, f, *c), "BreakJoinedGroup: result does not fit!" );
  *res_back = b;  *res_fwd = f;
  debug2(DOB, DD,"BreakJoinedGroup returning (%s, %s)",
	EchoLength(b), EchoLength(f));
} /* end BreakJoinedGroup */


/*@::BreakVcat()@*************************************************************/
/*                                                                           */
/*  static OBJECT BreakVcat(x, c)                                            */
/*                                                                           */
/*  Break a VCAT to satisfy constraint c.  This is tedious because every     */
/*  group of components between //  ...  // must be broken separately.       */
/*                                                                           */
/*****************************************************************************/

OBJECT BreakVcat(x, c)
OBJECT x;  CONSTRAINT *c;
{ OBJECT y, link, start_group, m;  LENGTH b, f; int dble_fwd;  CONSTRAINT tc;
  BOOLEAN dble_found;
  debug1(DOB, DD, "[ BreakVcat(x, %s)", EchoConstraint(c));
  assert(Down(x) != x, "BreakVcat: Down(x) == x!" );
  SetConstraint(tc, MAX_LEN, min(bfc(*c), fc(*c)), MAX_LEN);
  
  dble_found = FALSE;  dble_fwd = 0;  start_group = nil;
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link);
    if( is_index(type(y)) )  continue;
    if( type(y) == GAP_OBJ )
    { assert( start_group != nil, "BreakVcat: start_group == nil!" );
      if( !join(gap(y)) )
      {
	/* finish off and break this group */
	if( !FitsConstraint(b, f, tc) )
	  BreakJoinedGroup(start_group, link, m, &tc, &b, &f);
	dble_found = TRUE;
	dble_fwd = max(dble_fwd, b + f);
	start_group = nil;
	debug1(DOB, DD, "  end group, dble_fwd: %s", EchoLength(dble_fwd));
      }
    }
    else if( start_group == nil )
    {	
      /* start new group */
      b = back(y, COL);  f = fwd(y, COL);
      start_group = link;  m = y;
      debug2(DOB, DD, "  starting group (b = %s, f = %s):",
	EchoLength(b), EchoLength(f));
      ifdebug(DOB, DD, DebugObject(y));
    }
    else
    {
      /* continue with current group */
      b = max(b, back(y, COL));  f = max(f, fwd(y, COL));
      if( fwd(y, COL) > fwd(m, COL) )  m = y;
      debug3(DOB, DD, "  in group%s (b = %s, f = %s):",
	m == y ? " (new max)" : "",
	EchoLength(b), EchoLength(f));
      ifdebug(DOB, DD, DebugObject(y));
    }
  }
  assert( start_group != nil, "BreakVcat: start_group == nil (2)!" );

  if( dble_found )
  {	
    /* finish off and break this last group, and set sizes of x */
    if( !FitsConstraint(b, f, tc) )
      BreakJoinedGroup(start_group, LastDown(x), m, &tc, &b, &f);
    dble_fwd = max(dble_fwd, b + f);
    debug1(DOB, DD, "  ending last group, dble_fwd: %s",EchoLength(dble_fwd));
    back(x, COL) = 0;  fwd(x, COL) = min(MAX_LEN, dble_fwd);
  }
  else
  {
    /* finish off and break this last and only group, and set sizes of x */
    debug2(DOB, DD, "  BreakVcat ending last and only group (%s, %s)",
	EchoLength(b), EchoLength(f));
    BreakJoinedGroup(start_group, LastDown(x), m, c, &b, &f);
    back(x, COL) = b;  fwd(x, COL) = f;
  }

  debug0(DOB, DD, "] BreakVcat returning x:");
  ifdebug(DOB, DD, DebugObject(x));
  debug2(DOB, DD, "  (size is %s, %s)",
	EchoLength(back(x, COL)), EchoLength(fwd(x, COL)));
  return x;
} /* end BreakVcat */


/*@::BreakTable()@************************************************************/
/*                                                                           */
/*  static OBJECT BreakTable(x, c)                                           */
/*                                                                           */
/*  Break table (HCAT) x to satisfy constraint c.                            */
/*                                                                           */
/*  Outline of algorithm:                                                    */
/*                                                                           */
/*     bcount = number of components to left of mark;                        */
/*     fcount = no. of components on and right of mark;                      */
/*     bwidth = what back(x) would be if all components had size (0, 0);     */
/*     fwidth = what fwd(x) would be if all components had size (0, 0);      */
/*     Set all components of x to Unbroken (broken(y) holds this flag);      */
/*     while( an Unbroken component of x exists )                            */
/*     {   my = the Unbroken component of x of minimum width;                */
/*         mc = desirable constraint for my (see below);                     */
/*         BreakObject(my, &mc);                                             */
/*         Set my to Broken and update bcount, fcount, bwidth, fwidth        */
/*            to reflect the actual size of my, now broken;                  */
/*     }                                                                     */
/*                                                                           */
/*  The constraint mc is chosen in an attempt to ensure that:                */
/*                                                                           */
/*     a)  Any sufficiently narrow components will not break;                */
/*     b)  All broken components will have the same bfc(mc), if possible;    */
/*     c)  All available space is used.                                      */
/*                                                                           */
/*****************************************************************************/

static OBJECT BreakTable(x, c)
OBJECT x;  CONSTRAINT *c;
{ LENGTH bwidth, fwidth;	/* running back(x) and fwd(x)		     */
  int    bcount, fcount;	/* running no. of components		     */
  OBJECT mlink, my;		/* minimum-width unbroken component	     */
  BOOLEAN ratm;			/* TRUE when my has a mark to its right      */
  int    mside;			/* side of the mark my is on: BACK, ON, FWD  */
  LENGTH msize;			/* size of my (minimal among unbroken)	     */
  CONSTRAINT mc;		/* desirable constraint for my		     */
  OBJECT pg, prec_def;		/* preceding definite object of my           */
  OBJECT sg, succ_def;		/* succeeding definite object of my          */
  LENGTH pd_extra, sd_extra;	/* space availiable for free each side of my */
  LENGTH av_colsize;		/* the size of each unbroken component       */
				/* if they are all assigned equal width      */
  LENGTH fwd_max, back_max;	/* maximum space available forward of or     */
				/* back of the mark, when columns are even   */
  LENGTH col_size;		/* the column size actually used in breaking */
  LENGTH prev_col_size;		/* previous column size (try to keep equal)  */
  LENGTH beffect, feffect;	/* the amount bwidth, fwidth must increase   */
				/* when my is broken			     */
  OBJECT link, y, prev, g;  LENGTH tmp, tmp2;

  debug1(DOB, D, "[ BreakTable( x, %s )", EchoConstraint(c));

  /* Initialise csize, bcount, fcount, bwidth, fwidth and broken(y) */
  bcount = fcount = 0;  bwidth = fwidth = 0;  prev = nil;
  prev_col_size = 0;
  Child(y, Down(x));
  assert( type(y) != GAP_OBJ, "BreakTable: GAP_OBJ!" );
  assert( !is_index(type(y)), "BreakTable: index!" );
  broken(y) = is_indefinite(type(y));
  if( !broken(y) )  prev = y, fcount = 1;

  for( link = NextDown(Down(x));  link != x;  link = NextDown(NextDown(link)) )
  {
    /* find the next gap g and following child y */
    Child(g, link);
    assert( type(g) == GAP_OBJ, "BreakTable: GAP_OBJ!" );
    assert( NextDown(link) != x, "BreakTable: GAP_OBJ is last!" );
    Child(y, NextDown(link));

    assert( type(y) != GAP_OBJ, "BreakTable: GAP_OBJ!" );
    assert( !is_index(type(y)), "BreakTable: index!" );
    broken(y) = is_indefinite(type(y));
    if( !broken(y) )
    { if( prev == nil )  fcount = 1;
      else if( mark(gap(g)) )
      {	bcount += fcount;
	bwidth += fwidth + MinGap(0, 0, 0, &gap(g));
	fcount  = 1;  fwidth = 0;
      }
      else
      {	fwidth += MinGap(0, 0, 0, &gap(g));
	fcount += 1;
      }
      prev = y;
    }
  }

  /* if column gaps alone are too wide, kill them all */
  if( !FitsConstraint(bwidth, fwidth, *c) )
  {
    debug2(DOB, DD, "column gaps alone too wide: bwidth: %s; fwidth: %s",
       EchoLength(bwidth), EchoLength(fwidth));
    Error(13, 1, "reducing column gaps to 0i (object is too wide)",
      WARN, &fpos(x));
    for( link = Down(x);  link != x;  link = NextDown(link) )
    { Child(g, link);
      if( type(g) == GAP_OBJ )
      {	SetGap(gap(g), mark(gap(g)), join(gap(g)), FIXED_UNIT, EDGE_MODE, 0);
      }
    }
    bwidth = fwidth = 0;
  }

  /* break each column, from smallest to largest */
  while( bcount + fcount > 0 && FitsConstraint(bwidth, fwidth, *c) )
  {
    debug2(DOB, DD, "bcount: %d;  bwidth: %s", bcount, EchoLength(bwidth));
    debug2(DOB, DD, "fcount: %d;  fwidth: %s", fcount, EchoLength(fwidth));

    /* find a minimal-width unbroken component my */
    my = nil;  msize = size(x, COL);       /* an upper bound for size(y) */
    for( link = Down(x);  ;  link = NextDown(link) )
    { Child(y, link);
      assert( type(y) != GAP_OBJ, "BreakTable: type(y) == GAP_OBJ!" );
      if( !broken(y) && (size(y, COL) < msize || my == nil) )
      {	msize = size(y, COL);
	my = y;  mlink = link;
	ratm = FALSE;
      }

      /* next gap */
      link = NextDown(link);
      if( link == x )  break;
      Child(g, link);
      assert( type(g) == GAP_OBJ, "BreakTable: type(g) != GAP_OBJ!" );
      if( mark(gap(g)) )  ratm = TRUE;
    }

    /* find neighbouring definite objects and resulting pd_extra and sd_extra */
    SetNeighbours(mlink, ratm, &pg, &prec_def, &sg, &succ_def, &mside);
    debug2(DOB, DD, "my (%s): %s", Image(mside), EchoObject(my));
    pd_extra = pg == nil ? 0 :
      ExtraGap(broken(prec_def) ? fwd(prec_def,COL) : 0, 0, &gap(pg), BACK);
    sd_extra = sg == nil ? 0 :
      ExtraGap(0, broken(succ_def) ? back(succ_def,COL) : 0, &gap(sg), FWD);
    debug2(DOB, DD, "pd_extra:   %s;  sd_extra:      %s",
		EchoLength(pd_extra), EchoLength(sd_extra) );

    /* calculate desirable constraints for my */
    av_colsize = (bfc(*c) - bwidth - fwidth) / (bcount + fcount);
    debug1(DOB, DD, "av_colsize = %s", EchoLength(av_colsize));
    debug1(DOB, DD, "prev_col_size = %s", EchoLength(prev_col_size));
    switch( mside )
    {

      case BACK:
      
	back_max = min(bc(*c), bwidth + av_colsize * bcount);
	col_size = (back_max - bwidth) / bcount;
	if( col_size > prev_col_size && col_size - prev_col_size < PT )
	  col_size = prev_col_size;
	SetConstraint(mc, col_size + pd_extra, col_size + pd_extra + sd_extra,
			col_size + sd_extra );
	break;


      case ON:
      
	fwd_max = min(fc(*c), fwidth + av_colsize * fcount);
	col_size = (fwd_max - fwidth) / fcount;
	if( col_size > prev_col_size && col_size - prev_col_size < PT )
	  col_size = prev_col_size;
	tmp = min(MAX_LEN, pd_extra + back(my, COL) + col_size + sd_extra);
	SetConstraint(mc, pd_extra + back(my, COL), tmp, col_size + sd_extra);
	break;


      case FWD:
      
	fwd_max = min(fc(*c), fwidth + av_colsize * fcount);
	col_size = (fwd_max - fwidth) / fcount;
	if( col_size > prev_col_size && col_size - prev_col_size < PT )
	  col_size = prev_col_size;
	SetConstraint(mc, col_size + pd_extra, col_size + pd_extra + sd_extra,
			col_size + sd_extra );
	break;


      default:
      
	Error(13, 2, "BreakTable: %d", INTERN, no_fpos, mside);
	break;
    }
    debug1(DOB, DD, "col_size = %s", EchoLength(col_size));
    prev_col_size = col_size;

    /* now break my according to these constraints, and accept it */
    my = BreakObject(my, &mc);  broken(my) = TRUE;

    /* calculate the effect of accepting my on bwidth and fwidth */
    if( pg != nil )
    { tmp = broken(prec_def) ? fwd(prec_def, COL) : 0;
      beffect = MinGap(tmp, back(my, COL), fwd(my, COL), &gap(pg)) -
	        MinGap(tmp, 0,             0,            &gap(pg));
    }
    else beffect = back(my, COL);

    if( sg != nil )
    { tmp = broken(succ_def) ? back(succ_def, COL) : 0;
      tmp2 = broken(succ_def) ? fwd(succ_def, COL) : 0;
      feffect = MinGap(fwd(my, COL), tmp, tmp2, &gap(sg)) -
	        MinGap(0,            tmp, tmp2, &gap(sg));
    }
    else feffect = fwd(my, COL);

    switch( mside )
    {
	case BACK:	bwidth += beffect + feffect;
			bcount--;
			break;
	
	case ON:	bwidth += beffect;  fwidth += feffect;
			fcount--;
			break;

	case FWD:	fwidth += beffect + feffect;
			fcount--;
			break;
	
	default:	Error(13, 3, "BreakTable: %d", INTERN, no_fpos, mside);
			break;
    }

  } /* end while */

  back(x, COL) = bwidth;
  fwd(x, COL) = fwidth;

  debug2(DOB, D,  "] BreakTable returning %s,%s; x =",
    EchoLength(bwidth), EchoLength(fwidth));
  ifdebug(DOB, D, DebugObject(x));
  return x;
} /* end BreakTable */


/*@::BreakObject()@***********************************************************/
/*                                                                           */
/*  OBJECT BreakObject(x, c)                                                 */
/*                                                                           */
/*  Break lines of object x so that it satisfies constraint c.               */
/*                                                                           */
/*****************************************************************************/

OBJECT BreakObject(x, c)
OBJECT x;  CONSTRAINT *c;
{ OBJECT link, y;  CONSTRAINT yc;  LENGTH f;
  debug3(DOB, DD,  "[ BreakObject(x (%s,%s),  %s), x =",
	EchoLength(back(x, COL)), EchoLength(fwd(x, COL)), EchoConstraint(c));
  ifdebug(DOB, DD, DebugObject(x));

  if( FitsConstraint(back(x, COL), fwd(x, COL), *c) )
  { debug0(DOB, DD, "] BreakObject returning (fits).");
    return x;
  }
  switch( type(x) )
  {

    case ROTATE:
    
      Error(13, 4, "%s deleted (too wide; cannot break %s)",
	WARN, &fpos(x), KW_ROTATE, KW_ROTATE);
      y = MakeWord(WORD, STR_EMPTY, &fpos(x));
      back(y, COL) = fwd(y, COL) = 0;
      ReplaceNode(y, x);
      DisposeObject(x);
      x = y;
      break;


    case SCALE:

      InvScaleConstraint(&yc, bc(constraint(x)), c);
      Child(y, Down(x));
      y = BreakObject(y, &yc);
      back(x, COL) = (back(y, COL) * bc(constraint(x))) / SF;
      fwd(x, COL) =  (fwd(y, COL)  * bc(constraint(x))) / SF;
      break;


    case WORD:
    case QWORD:
    
      Error(13, 5, "word %s deleted (too wide)", WARN, &fpos(x), string(x));
      y = MakeWord(WORD, STR_EMPTY, &fpos(x));
      back(y, COL) = fwd(y, COL) = 0;
      ReplaceNode(y, x);
      DisposeObject(x);
      x = y;
      break;


    case WIDE:
    
      /* ***
      Error(13, 6, "%s %s reduced (too wide)", WARN, &fpos(x),
	EchoLength(bfc(constraint(x))), KW_WIDE);
      *** */
      debug3(DOB, D, "[ BreakObject(WIDE) (a): %s,%s: %s",
	EchoLength(back(x, COL)), EchoLength(fwd(x, COL)), 
	EchoConstraint(&constraint(x)));
      MinConstraint(&constraint(x), c);
      debug3(DOB, D, "  BreakObject(WIDE) (b): %s,%s: %s",
	EchoLength(back(x, COL)), EchoLength(fwd(x, COL)), 
	EchoConstraint(&constraint(x)));
      Child(y, Down(x));
      y = BreakObject(y, &constraint(x));
      back(x, COL) = back(y, COL);
      fwd(x, COL) = fwd(y, COL);
      debug3(DOB, D, "  BreakObject(WIDE) (c): %s,%s: %s",
	EchoLength(back(x, COL)), EchoLength(fwd(x, COL)), 
	EchoConstraint(&constraint(x)));
      EnlargeToConstraint(&back(x, COL), &fwd(x, COL), &constraint(x));
      debug3(DOB, D, "] BreakObject(WIDE) (d): %s,%s: %s",
	EchoLength(back(x, COL)), EchoLength(fwd(x, COL)), 
	EchoConstraint(&constraint(x)));
      break;


    case INCGRAPHIC:
    case SINCGRAPHIC:

      Error(13, 7, "%s or %s deleted (too wide)", WARN, &fpos(x), KW_INCGRAPHIC,
	KW_SINCGRAPHIC);
      y = MakeWord(WORD, STR_EMPTY, &fpos(x));
      back(y, COL) = fwd(y, COL) = 0;
      ReplaceNode(y, x);
      DisposeObject(x);
      x = y;
      break;


    case HIGH:
    case VSCALE:
    case VSHIFT:
    case HCONTRACT: 
    case VCONTRACT:
    case HEXPAND: 
    case VEXPAND:
    case PADJUST: 
    case HADJUST: 
    case VADJUST:
    case ONE_ROW:
    case ONE_COL:
    
      assert( Down(x) == LastDown(x), "BreakObject: downs!" );
      Child(y, Down(x));
      y = BreakObject(y, c);
      back(x, COL) = back(y, COL);
      fwd(x, COL) = fwd(y, COL);
      break;


    case HSHIFT:

      Child(y, Down(x));
      f = FindShift(x, y, COL);
      SetConstraint(yc,
	min(bc(*c), bfc(*c)) - f, bfc(*c), min(fc(*c), bfc(*c)) + f);
      BreakObject(y, &yc);
      f = FindShift(x, y, COL);
      back(x, COL) = min(MAX_LEN, max(0, back(y, COL) + f));
      fwd(x, COL)  = min(MAX_LEN, max(0, fwd(y, COL)  - f));
      break;


    case GRAPHIC:
    
      Child(y, LastDown(x));
      y = BreakObject(y, c);
      back(x, COL) = back(y, COL);
      fwd(x, COL) = fwd(y, COL);
      break;


    case SPLIT:
    
      Child(y, DownDim(x, COL));
      y = BreakObject(y, c);
      back(x, COL) = back(y, COL);
      fwd(x, COL) = fwd(y, COL);
      break;


    case ACAT:
    
      if( back(x, COL) > 0 )
      { int sz;
	/* shift the column mark of x to the left edge */
	sz = size(x, COL);
	fwd(x, COL) = min(sz, MAX_LEN);
	back(x, COL) = 0;
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{  Child(y, link);
	   if( type(y) == GAP_OBJ )  mark(gap(y)) = FALSE;
	}
	if( FitsConstraint(back(x, COL), fwd(x, COL), *c) )
	{ Error(13, 8, "column mark of unbroken paragraph moved left",
	    WARN, &fpos(x));
	  break;
	}
	Error(13, 9, "column mark of paragraph moved left before breaking",
	  WARN, &fpos(x));
      }
      x = FillObject(x, c);
      break;


    case HCAT:
    
      x = BreakTable(x, c);
      break;


    case COL_THR:
    
      BreakJoinedGroup(Down(x), LastDown(x), nil, c, &back(x,COL), &fwd(x,COL));
      break;


    case VCAT:
    
      x = BreakVcat(x, c);
      break;
			

    default:
    
      Error(13, 10, "BreakObject: %s", INTERN, &fpos(x), Image(type(x)) );
      break;

  }
  assert( back(x, COL) >= 0, "BreakObject: back(x, COL) < 0!" );
  assert( fwd(x, COL) >= 0, "BreakObject: fwd(x, COL) < 0!" );
  debug0(DOB, DD,  "] BreakObject returning, x =");
  ifdebug(DOB, DD,  DebugObject(x));
  debug2(DOB, DD, "  (size is %s, %s)",
	EchoLength(back(x, COL)), EchoLength(fwd(x, COL)));
  return x;
} /* end BreakObject */
