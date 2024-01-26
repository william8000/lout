/*@z12.c:Size Finder:MinSize()@***********************************************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.43)                       */
/*  COPYRIGHT (C) 1991, 2008 Jeffrey H. Kingston                             */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@it.usyd.edu.au)                                */
/*  School of Information Technologies                                       */
/*  The University of Sydney 2006                                            */
/*  AUSTRALIA                                                                */
/*                                                                           */
/*  This program is free software; you can redistribute it and/or modify     */
/*  it under the terms of the GNU General Public License as published by     */
/*  the Free Software Foundation; either Version 3, or (at your option)      */
/*  any later version.                                                       */
/*                                                                           */
/*  This program is distributed in the hope that it will be useful,          */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/*  GNU General Public License for more details.                             */
/*                                                                           */
/*  You should have received a copy of the GNU General Public License        */
/*  along with this program; if not, write to the Free Software              */
/*  Foundation, Inc., 59 Temple Place, Suite 330, Boston MA 02111-1307 USA   */
/*                                                                           */
/*  FILE:         z12.c                                                      */
/*  MODULE:       Size Finder                                                */
/*  EXTERNS:      MinSize()                                                  */
/*                                                                           */
/*****************************************************************************/
#include "externs.h"
#define line_breaker(g)							\
  (vspace(g) > 0 || (units(gap(g)) == FRAME_UNIT && width(gap(g)) > FR))
#define IG_LOOKING	0
#define IG_NOFILE	1
#define IG_BADFILE	2
#define IG_BADSIZE	3
#define	IG_OK		4

#if DEBUG_ON
static int debug_depth = 1;
static int debug_depth_max = 7;
#endif


/*****************************************************************************/
/*                                                                           */
/*  BuildSpanner(x)                                                          */
/*                                                                           */
/*  Build a spanning structure starting at x.                                */
/*                                                                           */
/*****************************************************************************/

static BOOLEAN BuildSpanner(OBJECT x)
{ OBJECT link, prnt, y, hspanner = nilobj, vspanner = nilobj, end_link, t;
  OBJECT hprnt, vprnt, spanobj;
  BOOLEAN need_hspanner, need_vspanner;
  debug1(DSF, DD, "BuildSpanner(%s)", EchoObject(x));
  assert( type(x) == START_HVSPAN || type(x) == START_HSPAN ||
	  type(x) == START_VSPAN , "BuildSpanner: type(x) != SPAN!" );
  Child(spanobj, Down(x))
    ;
  assert(Up(spanobj) == LastUp(spanobj), "BuildSpanner: spanobj!" );
  DeleteLink(Up(spanobj));

  need_hspanner = (type(x) == START_HVSPAN || type(x) == START_HSPAN);
  if( need_hspanner )
  {
    /* check that column context is legal, if not exit with FALSE */
    Parent(hprnt, UpDim(x, COLM))
      ;
    if( type(hprnt) != COL_THR )
    {
      Error(12, 10, "%s deleted (not in column)", WARN,&fpos(x),Image(type(x)));
      return FALSE;
    }

    /* build hspanner object and interpose it between x and spanobj */
    New(hspanner, HSPANNER);
    FposCopy(fpos(hspanner), fpos(x));
    spanner_broken(hspanner) = FALSE;
    Link(x, hspanner);
    Link(hspanner, spanobj);

    /* link later members of the spanner on the same row mark to hspanner    */
    /* by definition this is every member across to the last @HSpan before a */
    /* @StartHVSpan or @StartHSpan or @StartVSpan or @VSpan or end of row    */
    Parent(prnt, UpDim(x, ROWM))
      ;
    if( type(prnt) != ROW_THR )
    {
      Error(12, 11, "%s symbol out of place", FATAL, &fpos(x), Image(type(x)));
    }
    assert(type(prnt) == ROW_THR, "BuildSpanner: type(prnt)!");
    spanner_sized(hspanner) = spanner_fixed(hspanner) = 0;
    spanner_count(hspanner) = 1;
    end_link = NextDown(UpDim(x, ROWM));
    for( link = NextDown(UpDim(x, ROWM)); link != prnt; link = NextDown(link) )
    { Child(y, link)
        ;
      debug2(DSF, DD, "  examining ver %s %s", Image(type(y)), EchoObject(y));
      if( type(y) == HSPAN )
        end_link = NextDown(link);
      else if( type(y) == START_HVSPAN || type(y) == START_HSPAN ||
	       type(y) == START_VSPAN  || type(y) == VSPAN )
        break;
    }
    for( link = NextDown(UpDim(x,ROWM)); link!=end_link; link = NextDown(link) )
    {
      /* each of these components becomes @HSpan and is added to hspanner */
      Child(y, link)
        ;
      New(t, HSPAN);
      FposCopy(fpos(t), fpos(y));
      ReplaceNode(t, y);
      DisposeObject(y);
      Link(t, hspanner);
      spanner_count(hspanner)++;
    }
  }
  else Link(x, spanobj);

  need_vspanner = (type(x) == START_HVSPAN || type(x) == START_VSPAN);
  if( need_vspanner )
  {
    /* check that row context is legal, if not exit with FALSE */
    Parent(vprnt, UpDim(x, ROWM))
      ;
    if( type(vprnt) != ROW_THR )
    {
      Error(12, 12, "%s deleted (not in row)", WARN, &fpos(x), Image(type(x)));
      return FALSE;
    }

    /* build vspanner object and interpose it between x and spanobj */
    New(vspanner, VSPANNER);
    FposCopy(fpos(vspanner), fpos(x));
    spanner_broken(vspanner) = FALSE;
    Link(x, vspanner);
    Link(vspanner, spanobj);

    /* link later members of the spanner on the same column mark to vspanner */
    /* by definition this is every member down to the last @VSpan before a   */
    /* @StartHVSpan or @StartHSpan or @StartVSpan or @HSpan or end of column */
    Parent(prnt, UpDim(x, COLM))
      ;
    assert(type(prnt) == COL_THR, "BuildSpanner: type(prnt)!");
    spanner_sized(vspanner) = spanner_fixed(vspanner) = 0;
    spanner_count(vspanner) = 1;
    end_link = NextDown(UpDim(x, COLM));
    for( link = NextDown(UpDim(x, COLM)); link != prnt; link = NextDown(link) )
    { Child(y, link)
        ;
      debug2(DSF, DD, "  examining hor %s %s", Image(type(y)), y);
      if( type(y) == VSPAN )
        end_link = NextDown(link);
      else if( type(y) == START_HVSPAN || type(y) == START_HSPAN ||
	       type(y) == START_VSPAN  || type(y) == HSPAN )
        break;
    }
    for( link = NextDown(UpDim(x,COLM)); link!=end_link; link = NextDown(link) )
    {
      /* each of these components becomes @VSpan and is added to vspanner */
      Child(y, link)
        ;
      New(t, VSPAN);
      FposCopy(fpos(t), fpos(y));
      ReplaceNode(t, y);
      DisposeObject(y);
      Link(t, vspanner);
      spanner_count(vspanner)++;
    }
  }
  else Link(x, spanobj);

  debug2(DSF, DD, "BuildSpanner returning TRUE (rows = %d, cols = %d)",
    need_vspanner ? spanner_count(vspanner) : 0,
    need_hspanner ? spanner_count(hspanner) : 0);
  ifdebug(DSF, DD, DebugObject(x));
  return TRUE;
}


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN FindSpannerGap(thr, cat_op, gp)                                  */
/*                                                                           */
/*  For the purposes of calculating spanning spacing, find the gap between   */
/*  this object and the preceding one under the nearest cat_op.              */
/*                                                                           */
/*  If found, set &gp to the gap object and return TRUE; else return FALSE.  */
/*                                                                           */
/*****************************************************************************/

static BOOLEAN FindSpannerGap(OBJECT thr, unsigned dim, unsigned cat_op,
  OBJECT *res)
{ OBJECT link, x;

  /* find nearest enclosing cat_op that we aren't the first element of */
  link = UpDim(thr, dim);
  Parent(x, link)
    ;
  while( (type(x) != cat_op || type(PrevDown(link)) != LINK) && Up(x) != x )
  { link = UpDim(x, dim);
    Parent(x, link)
      ;
  }

  /* if found and a gap precedes thr's component of it, return that gap */
  if( type(x) == cat_op && type(PrevDown(link)) == LINK )
  { Child(*res, PrevDown(link))
      ;
    assert(type(*res) == GAP_OBJ, "FindSpannerGap: type(*res)!" );
  }
  else if( type(x) == HEAD && gall_dir(x)==dim && type(PrevDown(link))==LINK )
  { Child(*res, PrevDown(link))
      ;
    assert(type(*res) == GAP_OBJ, "FindSpannerGap (HEAD): type(*res)!" );
    nobreak(gap(*res)) = TRUE;
  }
  else *res = nilobj;

  debug1(DSF, DD, "  FindSpannerGap returning %s", EchoObject(*res));
  return (*res != nilobj);
}


/*****************************************************************************/
/*                                                                           */
/*  void SpannerAvailableSpace(x, dim, rb, rf)                               */
/*                                                                           */
/*  Work out the total space available to hold this spanner, and set         */
/*  (*rb, *rf) to record this value.  This space equals the total width      */
/*  of all columns (and their intervening gaps) spanned, with the mark       */
/*  of the last column being the one separating rb from rf.                  */
/*                                                                           */
/*****************************************************************************/

void SpannerAvailableSpace(OBJECT y, int dim, FULL_LENGTH *resb,
					      FULL_LENGTH *resf)
{ OBJECT slink, s, thr, gp, prevthr;
  FULL_LENGTH b = 0, f = 0;  /* initial values not used */
  unsigned thr_type, cat_type;

  assert( type(y) == HSPANNER || type(y) == VSPANNER, "SpannerAvail!");
  debug4(DSF, DD, "SpannerAvailableSpace(%d %s %s, %s)",
    spanner_count(y), Image(type(y)), EchoObject(y), dimen(dim));
  if( dim == COLM )
  { thr_type = COL_THR;
    cat_type = HCAT;
  }
  else
  { thr_type = ROW_THR;
    cat_type = VCAT;
  }

  /* first calculate the total space consumed in earlier spans */
  /* Invariant: (b, f) is the size up to and including prev    */
  /*                                                           */
  prevthr = nilobj;
  for( slink = Up(y);  slink != y;  slink = NextUp(slink) )
  { Parent(s, slink)
      ;
    Parent(thr, UpDim(s, dim))
      ;
    if( type(thr) == thr_type )
    {
      assert( thr_state(thr) == SIZED, "SpannerAvailableSpace: thr_state!" );
      if( prevthr == nilobj )
      {
        /* this is the first column spanned over */
        b = back(thr, dim);
        f = fwd(thr, dim);
        debug4(DSF, DD, "  first component %s,%s: b = %s, f = %s",
          EchoLength(back(thr, dim)), EchoLength(fwd(thr, dim)),
	  EchoLength(b), EchoLength(f));
      }
      else if( FindSpannerGap(thr, dim, cat_type, &gp) )
      {
        /* this is a subquent column spanned over */
        b += MinGap(fwd(prevthr, dim), back(thr, dim), fwd(thr, dim), &gap(gp));
	f = fwd(thr, dim);
        debug5(DSF, DD, "  later component %s,%s: gp = %s, b = %s, f = %s",
          EchoLength(back(thr, dim)), EchoLength(fwd(thr, dim)), EchoObject(gp),
	  EchoLength(b), EchoLength(f));
      }
      else
      {
        Error(12, 13, "search for gap preceding %s failed, using zero",
  	  WARN, &fpos(s), Image(type(s)));
        b += fwd(prevthr, dim) + back(thr, dim);
	f = fwd(thr, dim);
        debug4(DSF, DD, "  later component %s,%s: (no gap), b = %s, f = %s",
          EchoLength(back(thr, dim)), EchoLength(fwd(thr, dim)),
	  EchoLength(b), EchoLength(f));
      }
    }
    else
      Error(12, 14, "%s deleted (out of place)", WARN,&fpos(s),Image(type(s)));
    prevthr = thr;
  }

  *resb = b;
  *resf = f;
  SetConstraint(constraint(y), MAX_FULL_LENGTH, b+f, MAX_FULL_LENGTH);
  debug2(DSF, DD, "SpannerAvailableSpace returning %s,%s",
    EchoLength(*resb), EchoLength(*resf));
} /* end SpannerAvailableSpace */


/*@@**************************************************************************/
/*                                                                           */
/*  FULL_CHAR *IndexType(index)                                              */
/*                                                                           */
/*  Return some string that will give a meaningful idea to end users of      */
/*  what this index is pointing at.                                          */
/*                                                                           */
/*****************************************************************************/

static FULL_CHAR *IndexType(OBJECT index)
{ FULL_CHAR *res;
  debug1(DVH, DD, "IndexType(%s)", Image(type(index)));
  switch( type(index) )
  {
    case DEAD:
    case UNATTACHED:
    case GALL_PREC:
    case GALL_FOLL:
    case GALL_FOLL_OR_PREC:
    case RECEPTIVE:
    case RECEIVING:
    case PRECEDES:
    case FOLLOWS:
    case GALL_TARG:

      res = AsciiToFull("galley or galley target");
      break;


    case RECURSIVE:

      res = AsciiToFull("recursive symbol");
      break;


    case SCALE_IND:
    case COVER_IND:
    case EXPAND_IND:
    case PAGE_LABEL_IND:

      res = Image(type(actual(index)));
      break;


    case CROSS_TARG:
    case CROSS_PREC:
    case CROSS_FOLL:
    case CROSS_FOLL_OR_PREC:

      res = AsciiToFull("cross reference or cross reference target");
      break;


    default:

      assert1(FALSE, "IndexType: unknown index", Image(type(index)));
      res = STR_EMPTY;
      break;
  }
  debug1(DVH, DD, "IndexType returning %s", res);
  return res;
} /* end IndexType */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT MinSize(x, dim, extras)                                           */
/*                                                                           */
/*  Set fwd(x, dim) and back(x, dim) to their minimum possible values.       */
/*  If dim == ROWM, construct an extras list and return it in *extras.       */
/*                                                                           */
/*****************************************************************************/

OBJECT MinSize(OBJECT x, int dim, OBJECT *extras)
{ OBJECT y, z, link, prev, t, g, full_name, catch_extras;
  FULL_LENGTH b, f, dble_fwd;
  BOOLEAN dble_found, found, will_expand, cp;
  FILE *fp;

  debug2(DSF, DD, "[ MinSize( %s, %s, extras )", EchoObject(x), dimen(dim));
  debugcond4(DSF, D, dim == COLM && debug_depth++ < debug_depth_max,
    "%*s[ MinSize(COLM, %s %d)", (debug_depth-1)*2, " ",
    Image(type(x)), (int) x);
  ifdebug(DSF, DDD, DebugObject(x));

  switch( type(x) )
  {

    case WORD:
    case QWORD:
    
      if( dim == COLM )  FontWordSize(x);
      break;


    case CROSS:
    case FORCE_CROSS:

      /* add index to the cross-ref */
      if( dim == ROWM )
      {	New(z, cross_type(x)); /* CROSS_PREC, CROSS_FOLL or CROSS_FOLL_OR_PREC */
	debug2(DCR, DD, "  MinSize CROSS: %ld %s", (long) x, EchoObject(x));
	actual(z) = x;
	Link(z, x);		/* new code to avoid disposal */
	Link(*extras, z);
	debug2(DCR, DD, "  MinSize index: %ld %s", (long) z, EchoObject(z));
      }
      back(x, dim) = fwd(x, dim) = 0;
      break;


    case PAGE_LABEL:
    
      if( dim == ROWM )
      { New(z, PAGE_LABEL_IND);
	actual(z) = x;
	Link(z, x);
	Link(*extras, z);
      }
      back(x, dim) = fwd(x, dim) = 0;
      break;


    case NULL_CLOS:
    
      back(x, dim) = fwd(x, dim) = 0;
      break;


    case HEAD:

      if( dim == ROWM )
      {	
	/* replace the galley x by a dummy closure y */
	New(y, NULL_CLOS);
	FposCopy(fpos(y), fpos(x));
	ReplaceNode(y, x);

	if( has_key(actual(x)) )
	{
	  /* galley is sorted, make insinuated cross-reference */
	  New(z, foll_or_prec(x));
	  pinpoint(z) = y;
	  Child(t, Down(x))
	    ;
	  actual(z) = CrossMake(whereto(x), t, (int) type(z));
	  Link(*extras, z);
	  DisposeObject(x);
	  debug1(DCR, DDD, "  MinSize: %s", EchoObject(z));
	}
	else
	{
	  /* galley is following, make UNATTACHED */
	  New(z, UNATTACHED);  Link(z, x);
	  actual(z) = nilobj;
	  non_blocking(z) = TRUE;
	  blocked(z) = FALSE;
	  pinpoint(z) = y;
	  Link(*extras, z);
	  debug1(DCR, DDD, "  MinSize: %s", EchoObject(z));
	  debug2(DGA, D, "  MinSize UNATTACHED %s at %s",
	    SymName(actual(x)), EchoFilePos(&fpos(x)));
	}
	x = y;	/* now sizing y, not x */
	back(x, COLM) = fwd(x, COLM) = 0;  /* fix non-zero size @Null bug!! */
      }
      else
      {
	debug2(DGT, DD, "MinSize setting external_ver(%s %s) = FALSE",
	  Image(type(x)), SymName(actual(x)));
	external_ver(x) = external_hor(x) = FALSE;
      }
      back(x, dim) = fwd(x, dim) = 0;
      break;


    case CLOSURE:

      assert( !has_target(actual(x)), "MinSize: CLOSURE has target!" );
      if( dim == ROWM )
      { if( indefinite(actual(x)) )
	{ New(z, RECEPTIVE);
	  actual(z) = x;
	  Link(*extras, z);
	  debug1(DCR, DDD, "  MinSize: %s", EchoObject(z));
	}
	else if( recursive(actual(x)) )
	{ New(z, RECURSIVE);
	  actual(z) = x;
	  Link(*extras, z);
	  debug1(DCR, DDD, "  MinSize: %s", EchoObject(z));
	}
	else
	{ assert(FALSE, "MinSize: definite non-recursive closure");
	}
      }
      else
      {
	debug2(DGT, DD, "MinSize setting external_ver(%s %s) = FALSE",
	  Image(type(x)), SymName(actual(x)));
	external_ver(x) = external_hor(x) = FALSE;/* nb must be done here!*/
      }
      back(x, dim) = fwd(x, dim) = 0;
      break;


    case ONE_COL:
    case ONE_ROW:
    case HCONTRACT:
    case VCONTRACT:
    case HLIMITED:
    case VLIMITED:
    
      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      back(x, dim) = back(y, dim);
      fwd(x, dim)  = fwd(y, dim);
      break;


    case BACKGROUND:

      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      Child(y, LastDown(x))
        ;
      y = MinSize(y, dim, extras);
      back(x, dim) = back(y, dim);
      fwd(x, dim)  = fwd(y, dim);
      break;


    case START_HVSPAN:
    case START_HSPAN:
    case START_VSPAN:
    case HSPAN:
    case VSPAN:

      /* if first touch, build the spanner */
      if( (type(x) == START_HVSPAN || type(x) == START_HSPAN ||
	   type(x) == START_VSPAN) && dim == COLM )
      {
        if( !BuildSpanner(x) )
	{
	  t = MakeWord(WORD, STR_EMPTY, &fpos(x));
	  ReplaceNode(t, x);
	  x = t;
	  back(x, COLM) = fwd(x, COLM) = 0;
	  break;
	}
      }

      /* if first vertical touch, break if necessary */
      if( (type(x) == START_HVSPAN || type(x) == START_HSPAN) && dim == ROWM )
      { CONSTRAINT c;
 
        /* find the HSPANNER */
	Child(t, DownDim(x, COLM))
	  ;
        assert( type(t) == HSPANNER, "MinSize/SPAN: type(t) != HSPANNER!" );
 
        /* find the available space for this HSPANNER and break it */
        SpannerAvailableSpace(t, COLM, &b, &f);
        SetConstraint(c, MAX_FULL_LENGTH, b+f, MAX_FULL_LENGTH);
        debug2(DSF,DD, "  BreakObject(%s,%s)",EchoObject(t),EchoConstraint(&c));
        t = BreakObject(t, &c);
        spanner_broken(t) = TRUE;
      }
 
      /* make sure that HSPAN links to HSPANNER, VSPAN to VSPANNER      */
      /* NB must follow breaking since that could affect the value of y */
      Child(y, DownDim(x, dim))
        ;
      if( (type(x) == HSPAN && type(y) != HSPANNER) ||
	  (type(x) == VSPAN && type(y) != VSPANNER) )
      {
	if( dim == COLM )
	  Error(12, 15, "%s replaced by empty object (out of place)",
	    WARN, &fpos(x), Image(type(x)));
        back(x, dim) = fwd(x, dim) = 0;
	break;
      }

      /* now size the object */
      if( (type(x)==HSPAN && dim==ROWM) || (type(x)==VSPAN && dim==COLM) )
      {
	/* perp dimension, covered by preceding @Span, so may be zero. */
	back(x, dim) = fwd(x, dim) = 0;
      }
      else if( type(y) != HSPANNER && type(y) != VSPANNER )
      {
	/* no spanning in this dimension */
	MinSize(y, dim, extras);
	back(x, dim) = back(y, dim);
	fwd(x, dim) = fwd(y, dim);
      }
      else if( ++spanner_sized(y) != spanner_count(y) )
      {
	/* not the last column or row, so say zero */
	back(x, dim) = fwd(x, dim) = 0;
      }
      else
      {
	/* this is the last column or row of a spanner.  Its width is its */
	/* natural width minus anything that will fit over the top of the */
	/* things it spans.                                               */

	MinSize(y, dim, extras);
	SpannerAvailableSpace(y, dim, &b, &f);
	back(x, dim) = 0;
	fwd(x, dim) = find_max(size(y, dim) - b, 0);
	debug3(DSF, DD, "  size(y, %s) = %s,%s", dimen(dim),
	  EchoLength(back(y, dim)), EchoLength(fwd(y, dim)));
      }
      debug4(DSF, DD, "finishing MinSize(%s) of %s span, reporting %s,%s",
	dimen(dim), spanner_count(y) != 1 ? "multi-column" : "one-column",
	EchoLength(back(x, dim)), EchoLength(fwd(x, dim)));
      break;


    case HSPANNER:
    case VSPANNER:

      assert( (type(x) == HSPANNER) == (dim == COLM), "MinSize: SPANNER!" );
      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      back(x, dim) = back(y, dim);
      fwd(x, dim)  = fwd(y, dim);
      break;


    case HEXPAND:
    case VEXPAND:

      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      back(x, dim) = back(y, dim);
      fwd(x, dim)  = fwd(y, dim);

      /* insert index into *extras for expanding later */
      if( dim == ROWM )
      {	New(z, EXPAND_IND);
	actual(z) = x;
	Link(*extras, z);
	/* Can't do Link(z, x) because Constrained goes up and finds z */
	debug2(DCR, DD, "  MinSize index: %ld %s", (long) z, EchoObject(z));
      }	
      break;


    case END_HEADER:
    case CLEAR_HEADER:

      /* remember, these have a dummy child for threads to attach to */
      back(x, dim) = fwd(x, dim) = 0;
      Child(y, Down(x))
        ;
      back(y, dim) = fwd(y, dim) = 0;
      break;


    case BEGIN_HEADER:
    case SET_HEADER:
    
      /* remember, there are multiple copies of each header */
      for( link = NextDown(Down(x));  link != x;  link = NextDown(link) )
      {
        Child(y, link)
          ;
        New(catch_extras, ACAT);
        y = MinSize(y, dim, &catch_extras);
        if( Down(catch_extras) != catch_extras )
        {
	  /* we can't handle things that generate extras inside headers */
	  Child(z, Down(catch_extras))
	    ;
	  Error(12, 16, "%s contains object of illegal type: %s",
	    FATAL, &fpos(x), Image(type(x)), IndexType(z));
        }
	DisposeObject(catch_extras);
        back(x, dim) = back(y, dim);
        fwd(x, dim)  = fwd(y, dim);
      }
      debug4(DSF, D, "MinSize(%s, %s) := (%s, %s)", Image(type(x)),
	dimen(dim), EchoLength(back(x, dim)), EchoLength(fwd(x, dim)));
      break;


    case PLAIN_GRAPHIC:
    case GRAPHIC:
    case LINK_SOURCE:
    case LINK_DEST:
    case LINK_DEST_NULL:
    case LINK_URL:
    
      Child(y, LastDown(x))
        ;
      y = MinSize(y, dim, extras);
      back(x, dim) = back(y, dim);
      fwd(x, dim)  = fwd(y, dim);
      break;


    case HCOVER:
    case VCOVER:

      /* work out size and set to 0 if parallel */
      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      if( (dim == COLM) == (type(x) == HCOVER) )
	back(x, dim) = fwd(x, dim) = 0;
      else
      {	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
      }

      /* insert index into *extras for revising size later */
      if( dim == ROWM )
      {	New(z, COVER_IND);
	actual(z) = x;
	Link(*extras, z);
	/* Can't do Link(z, x) because Constrained goes up and finds z */
	debug2(DCR, DD, "  MinSize index: %ld %s", (long) z, EchoObject(z));
      }	
      break;


    case HMIRROR:
    case VMIRROR:

      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      if( (dim == COLM) == (type(x) == HMIRROR) )
      {	back(x, dim) = fwd(y, dim);
	fwd(x, dim)  = back(y, dim);
      }
      else
      {	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
      }
      break;


    case HSCALE:
    case VSCALE:

      /* work out size and set to 0 if parallel */
      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      if( (dim == COLM) == (type(x) == HSCALE) )
	back(x, dim) = fwd(x, dim) = 0;
      else
      {	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
      }
      break;


    case ROTATE:
    
      Child(y, Down(x))
        ;
      if( dim == COLM )
      {	y = MinSize(y, COLM, extras);
	New(whereto(x), ACAT);
	back(whereto(x), COLM) = back(whereto(x), ROWM) = fwd(whereto(x), COLM) = fwd(whereto(x), ROWM) = 0;
	y = MinSize(y, ROWM, &whereto(x));
	RotateSize(&back(x, COLM), &fwd(x, COLM), &back(x, ROWM), &fwd(x, ROWM),
	  y, sparec(constraint(x)));
      }
      else
      {	TransferLinks(Down(whereto(x)), whereto(x), *extras);
	Dispose(whereto(x));
      }
      break;
	

    case SCALE:

      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      if( dim == COLM )
      { back(x, dim) = (back(y, dim) * bc(constraint(x))) / SF;
        fwd(x, dim)  = (fwd(y, dim)  * bc(constraint(x))) / SF;
	if( bc(constraint(x)) == 0 )  /* Lout-supplied factor required */
        { New(z, SCALE_IND);
	  actual(z) = x;
	  Link(*extras, z);
	  debug1(DSF, DDD, "  MinSize: %s", EchoObject(z));
	  vert_sized(x) = FALSE;
        }	
      }
      else
      { back(x, dim) = (back(y, dim) * fc(constraint(x))) / SF;
        fwd(x, dim)  = (fwd(y, dim)  * fc(constraint(x))) / SF;
	vert_sized(x) = TRUE;
      }
      break;


    case KERN_SHRINK:


      Child(y, LastDown(x))
        ;
      y = MinSize(y, dim, extras);
      if( dim == COLM )
      { FULL_CHAR ch_left, ch_right;  FULL_LENGTH ksize;
	debug3(DSF, DD, "MinSize(%s,%s %s, COLM)",
	  EchoLength(back(y, COLM)), EchoLength(fwd(y, COLM)),
	  EchoObject(x));

	/* default value if don't find anything */
       	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);

	/* find first character of left parameter */
	ch_right = (FULL_CHAR) '\0';
	Child(y, Down(x))
	  ;
	while( type(y) == ACAT )
	{ Child(y, Down(y))
	    ;
	}
	if( is_word(type(y)) )  ch_right = string(y)[0];

	/* find last character of right parameter */
	ch_left = (FULL_CHAR) '\0';
	Child(y, LastDown(x))
	  ;
	while( type(y) == ACAT )
	{ Child(y, LastDown(y))
	    ;
	}
	if( is_word(type(y)) )  ch_left = string(y)[StringLength(string(y))-1];

	/* adjust if successful */
	if( ch_left != (FULL_CHAR) '\0' && ch_right != (FULL_CHAR) '\0' )
	{
	  MAPPING m = font_mapping(finfo[word_font(y)].font_table);
  	  FULL_CHAR *unacc = MapTable[m]->map[MAP_UNACCENTED];
	  ksize = FontKernLength(word_font(y), unacc, ch_left, ch_right);
	  debug4(DSF, DD, "  FontKernLength(%s, %c, %c) = %s",
	    FontName(word_font(y)), (char) ch_left, (char) ch_right,
	    EchoLength(ksize));
	  fwd(x, dim) += ksize;
	}

      }
      else
      {	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
      }
      break;


    case WIDE:

      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      if( dim == COLM )
      { y = BreakObject(y, &constraint(x));
        assert( FitsConstraint(back(y, dim), fwd(y, dim), constraint(x)),
		"MinSize: BreakObject failed to fit!" );
        back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
	EnlargeToConstraint(&back(x, dim), &fwd(x, dim), &constraint(x));
      }
      else
      {	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
      }
      break;


    case HIGH:
    
      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      if( dim == ROWM )
      { if( !FitsConstraint(back(y, dim), fwd(y, dim), constraint(x)) )
        { Error(12, 1, "forced to enlarge %s from %s to %s", WARN, &fpos(x),
	    KW_HIGH, EchoLength(bfc(constraint(x))), EchoLength(size(y, dim)));
	  debug0(DSF, DD, "offending object was:");
	  ifdebug(DSF, DD, DebugObject(y));
	  SetConstraint(constraint(x), MAX_FULL_LENGTH, size(y, dim), MAX_FULL_LENGTH);
        }
        back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
	EnlargeToConstraint(&back(x, dim), &fwd(x, dim), &constraint(x));
      }
      else
      {	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
      }
      break;


    case HSHIFT:
    case VSHIFT:

      Child(y, Down(x))
        ;
      y = MinSize(y, dim, extras);
      if( (dim == COLM) == (type(x) == HSHIFT) )
      { f = FindShift(x, y, dim);
	back(x, dim) = find_min(MAX_FULL_LENGTH, find_max(0, back(y, dim) + f));
	fwd(x, dim)  = find_min(MAX_FULL_LENGTH, find_max(0, fwd(y, dim)  - f));
      }
      else
      { back(x, dim) = back(y, dim);
	fwd(x, dim) = fwd(y, dim);
      }
      break;


    case SPLIT:
    
      link = DownDim(x, dim);
      Child(y, link)
        ;
      y = MinSize(y, dim, extras);
      back(x, dim) = back(y, dim);
      fwd(x, dim)  = fwd(y, dim);
      break;


    case ACAT:

      if( fill_style(save_style(x)) == FILL_OFF )
      { OBJECT new_line, g, z, res;  BOOLEAN jn;

	/* convert ACAT to VCAT of lines if more than one line */
	/* first, compress all ACAT children                   */
	for( link = x;  NextDown(link) != x;  link = NextDown(link) )
	{ Child(y, NextDown(link))
	    ;
	  if( type(y) == ACAT )
	  {
	    TransferLinks(Down(y), y, NextDown(link));
	    DisposeChild(Up(y));
	    link = PrevDown(link);
	  }
	}

	/* check each definite subobject in turn for a linebreak preceding */
	FirstDefinite(x, link, y, jn);
	if( link != x )
	{
	  res = nilobj;
	  NextDefiniteWithGap(x, link, y, g, jn);
	  while( link != x )
	  {
	    /* check whether we need to break the paragraph here at g */
	    if( mode(gap(g)) != NO_MODE && line_breaker(g) )
	    {
	      /* if this is our first break, build res */
	      if( res == nilobj )
	      {
		New(res, VCAT);
		adjust_cat(res) = FALSE;
		ReplaceNode(res, x);
	      }

	      /* make new line of stuff up to g and append it to res */
	      New(new_line, ACAT);
	      TransferLinks(NextDown(x), Up(g), new_line);
	      StyleCopy(save_style(new_line), save_style(x));
	      adjust_cat(new_line) = padjust(save_style(x));
	      Link(res, new_line);
	      debug2(DSF, D, "  new_line(adjust_cat %s) = %s",
		bool_str(adjust_cat(new_line)), EchoObject(new_line));

	      /* may need to insert space at start of remainder */
	      if( hspace(g)>0 || display_style(save_style(x))==DISPLAY_ORAGGED )
	      {
		/* make an empty word to occupy the first spot */
		z = MakeWord(WORD, STR_EMPTY, &fpos(g));
		word_font(z) = font(save_style(x));
		word_colour(z) = colour(save_style(x));
		word_underline_colour(z) = underline_colour(save_style(x));
		word_texture(z) = texture(save_style(x));
		word_outline(z) = outline(save_style(x));
		word_language(z) = language(save_style(x));
		word_baselinemark(z) = baselinemark(save_style(x));
		word_strut(z) = strut(save_style(x));
		word_ligatures(z) = ligatures(save_style(x));
		word_hyph(z) = hyph_style(save_style(x)) == HYPH_ON;
		underline(z) = UNDER_OFF;
		back(z, COLM) = fwd(z, COLM) = 0;
		Link(Down(x), z);

		/* follow the empty word with a gap of the right width */
		New(z, GAP_OBJ);
		hspace(z) = hspace(g);
		vspace(z) = 0;
		underline(z) = UNDER_OFF;
		GapCopy(gap(z), space_gap(save_style(x)));
		if( display_style(save_style(x)) == DISPLAY_ORAGGED )
		  width(gap(z)) = outdent_len(save_style(x));
		else
		  width(gap(z)) *= hspace(z);
		Link(NextDown(Down(x)), z);

		debug2(DSF, D, "  hspace(g) = %d, width(gap(z)) = %s",
		  hspace(g), EchoLength(width(gap(z))));
	      }

	      /* append a gap to res (recycle g) */
	      MoveLink(Up(g), res, PARENT);
	      GapCopy(gap(g), line_gap(save_style(x)));
	      /* *** old formula before blanklinescale 
	      width(gap(g)) *= find_max(1, vspace(g));
	      *** */
	      if( vspace(g) > 1 )
	        width(gap(g)) +=
		 (width(gap(g))*blanklinescale(save_style(x))*(vspace(g)-1))/SF;
	    }
	    NextDefiniteWithGap(x, link, y, g, jn);
	  }

	  /* at end of loop, if we have a res, leftover last line is linked */
	  if( res != nilobj )
	  {
	    Link(res, x);
	    x = res;
	  }
	}
      }
      /* *** NB NO BREAK *** */


    case HCAT:
    case VCAT:
    
      if( (dim == ROWM) == (type(x) == VCAT) )
      {
	/********************************************************************/
	/*                                                                  */
	/*  Calculate sizes parallel to join direction; loop invariant is:  */
	/*                                                                  */
	/*     If prev == nilobj, there are no definite children equal to   */
	/*        or to the left of Child(link).                            */
	/*     If prev != nilobj, prev is the rightmost definite child to   */
	/*        the left of Child(link), and (b, f) is the total size up  */
	/*        to the mark of prev i.e. not including fwd(prev).         */
	/*     g is the most recent gap, or nilobj if none found yet.       */
	/*     will_expand == TRUE when a gap is found that is likely to    */
	/*        enlarge when ActualGap is called later on.                */
	/*                                                                  */
	/********************************************************************/

	prev = g = nilobj;  will_expand = FALSE;  must_expand(x) = FALSE;
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link)
	    ;
	  if( is_index(type(y)) )
	  { if( dim == ROWM )
	    { link = PrevDown(link);
	      MoveLink(NextDown(link), *extras, PARENT);
	    }
	    continue;
	  }
	  else if( type(y) == type(x) )
	  { link = PrevDown(link);
	    TransferLinks(Down(y), y, NextDown(link));
	    DisposeChild(Up(y));
	    continue;
	  }
	  else if( type(y) == GAP_OBJ )  g = y;
	  else /* calculate size of y and accumulate it */
	  { if( is_word(type(y)) )
	    { if( dim == COLM )
	      {
		/* compress adjacent words if compatible */
		if( prev != nilobj && width(gap(g)) == 0 && nobreak(gap(g)) &&
		    type(x) == ACAT &&
		    is_word(type(prev)) && vspace(g) + hspace(g) == 0 &&
		    units(gap(g)) == FIXED_UNIT &&
		    mode(gap(g)) == EDGE_MODE && !mark(gap(g)) &&
		    word_font(prev) == word_font(y) &&
		    word_colour(prev) == word_colour(y) &&
		    word_underline_colour(prev) == word_underline_colour(y) &&
		    word_texture(prev) == word_texture(y) &&
		    word_outline(prev) == word_outline(y) &&
		    word_language(prev) == word_language(y) &&
		    word_baselinemark(prev) == word_baselinemark(y) &&
		    word_strut(prev) == word_strut(y) &&
		    word_ligatures(prev) == word_ligatures(y) &&
		    underline(prev) == underline(y) &&
		    NextDown(NextDown(Up(prev))) == link
		    )
		{
		  unsigned typ;
		  debug3(DSF, DD, "compressing %s and %s at %s",
		    EchoObject(prev), EchoObject(y), EchoFilePos(&fpos(prev)));
		  if( StringLength(string(prev)) + StringLength(string(y))
		      >= MAX_BUFF )
		    Error(12, 2, "word %s%s is too long", FATAL, &fpos(prev),
		      string(prev), string(y));
		  typ = type(prev) == QWORD || type(y) == QWORD ? QWORD : WORD;
		  y = MakeWordTwo(typ, string(prev), string(y), &fpos(prev));
		  word_font(y) = word_font(prev);
		  word_colour(y) = word_colour(prev);
		  word_underline_colour(y) = word_underline_colour(prev);
		  word_texture(y) = word_texture(prev);
		  word_outline(y) = word_outline(prev);
		  word_language(y) = word_language(prev);
		  word_baselinemark(y) = word_baselinemark(prev);
		  word_strut(y) = word_strut(prev);
		  word_ligatures(y) = word_ligatures(prev);
		  word_hyph(y) = word_hyph(prev);
		  underline(y) = underline(prev);
		  FontWordSize(y);
		  Link(Up(prev), y);
		  DisposeChild(Up(prev));
		  DisposeChild(Up(g));
		  DisposeChild(link);
		  prev = y;
		  link = Up(prev);
		  continue;
		}

		FontWordSize(y);
		debug4(DSF, DD, "FontWordSize( %s ) font %d = %s,%s",
		EchoObject(y), word_font(y),
		EchoLength(back(y, COLM)), EchoLength(fwd(y, COLM)));
	      }
	    }
	    else y = MinSize(y, dim, extras);

	    if( is_indefinite(type(y)) )
	    {
	      /* error if preceding gap has mark */
	      if( g != nilobj && mark(gap(g)) )
	      {	Error(12, 3, "^ deleted (it may not precede this object)",
		  WARN, &fpos(y));
		mark(gap(g)) = FALSE;
	      }

	      /* error if next unit is used in preceding gap */
	      if( g != nilobj && units(gap(g)) == NEXT_UNIT )
	      {	Error(12, 4, "gap replaced by 0i (%c unit not allowed here)",
		  WARN, &fpos(y), CH_UNIT_WD);
		units(gap(g)) = FIXED_UNIT;
		width(gap(g)) = 0;
	      }
	    }
	    else
	    {
	      /* calculate running total length */
	      if( prev == nilobj )  b = back(y, dim), f = 0;
	      else
	      { FULL_LENGTH tmp;
		tmp = MinGap(fwd(prev,dim), back(y,dim), fwd(y, dim), &gap(g));
		assert(g!=nilobj && mode(gap(g))!=NO_MODE, "MinSize: NO_MODE!");
		if( units(gap(g)) == FIXED_UNIT && mode(gap(g)) == TAB_MODE )
		{
		  f = find_max(width(gap(g)) + back(y, dim), f + tmp);
		}
		else
		{
		  f = f + tmp;
		}
		if( units(gap(g)) == FRAME_UNIT && width(gap(g)) > FR )
		    will_expand = TRUE;
		if( units(gap(g)) == AVAIL_UNIT && mark(gap(g)) && width(gap(g)) > 0 )
		  Error(12, 9, "mark alignment incompatible with centring or right justification",
		    WARN, &fpos(g));
		/* ***
		if( units(gap(g)) == AVAIL_UNIT && width(gap(g)) >= FR )
		    will_expand = TRUE;
		*** */
		if( mark(gap(g)) )  b += f, f = 0;
	      }
	      prev = y;
	    }
	    debug2(DSF,DD,"  b = %s, f = %s",EchoLength(b),EchoLength(f));
	  }
	} /* end for */

	if( prev == nilobj )  b = f = 0;
	else f += fwd(prev, dim);
	back(x, dim) = find_min(MAX_FULL_LENGTH, b);
	fwd(x, dim)  = find_min(MAX_FULL_LENGTH, f);

	if( type(x) == ACAT && will_expand )  fwd(x, COLM) = MAX_FULL_LENGTH;
      }
      else
      {
	/********************************************************************/
	/*                                                                  */
	/*  Calculate sizes perpendicular to join direction                 */
	/*                                                                  */
	/*  Loop invariant:                                                 */
	/*                                                                  */
	/*     if found, (b, f) is the size of x, from the last // or from  */
	/*     the start, up to link exclusive.  Else no children yet.      */
	/*     If dble_found, a previous // exists, and (0, dble_fwd) is    */
	/*     the size of x from the start up to that //.                  */
	/*                                                                  */
	/********************************************************************/

	dble_found = found = FALSE;  dble_fwd = 0;
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link)
	    ;
	  debug4(DSF, DD, "  %s in %s, y = %s %s", dimen(dim),
	    Image(type(x)), Image(type(y)), EchoObject(y));
	  if( is_index(type(y)) )
	  { if( dim == ROWM )
	    { link = PrevDown(link);
	      MoveLink(NextDown(link), *extras, PARENT);
	    }
	    continue;
	  }
	  else if( type(y) == type(x) )
	  { link = PrevDown(link);
	    TransferLinks(Down(y), y, NextDown(link));
	    DisposeChild(Up(y));
	    continue;
	  }
	  else if( type(y) == GAP_OBJ )
	  { assert( found, "MinSize/VCAT/perp: !found!" );
	    if( !join(gap(y)) )
	    {
	      /* found // or || operator, so end current group */
	      dble_found = TRUE;
	      dble_fwd = find_max(dble_fwd, b + f);
	      debug1(DSF, DD, "  endgroup, dble_fwd: %s", EchoLength(dble_fwd));
	      found = FALSE;
	    }
	  }
	  else /* found object */
	  {
	    /* calculate size of subobject y */
	    if( is_word(type(y)) )
	    { if( dim == COLM )  FontWordSize(y);
	    }
	    else y = MinSize(y, dim, extras);
	    if( found )
	    { b = find_max(b, back(y, dim));
	      f = find_max(f, fwd(y, dim));
	    }
	    else
	    { b = back(y, dim);
	      f = fwd(y, dim);
	      found = TRUE;
	    }
	    debug2(DSF,DD, "  b: %s, f: %s", EchoLength(b), EchoLength(f));
	  }
	} /* end for */
	assert( found, "MinSize/VCAT/perp: !found (2)!" );

	/* finish off last group */
	if( dble_found )
	{ back(x, dim) = 0;
	  dble_fwd = find_max(dble_fwd, b + f);
	  fwd(x, dim) = find_min(MAX_FULL_LENGTH, dble_fwd);
	  debug1(DSF, DD, "  end group, dble_fwd: %s", EchoLength(dble_fwd));
	}
	else
	{ back(x, dim) = b;
	  fwd(x, dim)  = f;
	}
      } /* end else */
      break;


    case COL_THR:

      assert( dim == COLM, "MinSize/COL_THR: dim!" );
      if( thr_state(x) == NOTSIZED )
      {	assert( Down(x) != x, "MinSize/COL_THR: Down(x)!" );

	/* first size all the non-spanning members of the thread */
	debug1(DSF, DD,  "[[ starting sizing %s", Image(type(x)));
	b = f = 0;
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link)
	    ;
	  assert( type(y) != GAP_OBJ, "MinSize/COL_THR: GAP_OBJ!" );
	  if( type(y) != START_HVSPAN && type(y) != START_HSPAN &&
	      type(y) != HSPAN && type(y) != VSPAN )
	  { y = MinSize(y, dim, extras);
	    b = find_max(b, back(y, dim));
	    f = find_max(f, fwd(y, dim));
	  }
	}
	back(x, dim) = b;
	fwd(x, dim)  = f;
	thr_state(x) = SIZED;
	debug3(DSF, DD,  "][ middle sizing %s (%s,%s)", Image(type(x)),
	  EchoLength(back(x, dim)), EchoLength(fwd(x, dim)));

	/* now size all the spanning members of the thread           */
	/* these will use back(x, dim) and fwd(x, dim) during sizing */
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link)
	    ;
	  assert( type(y) != GAP_OBJ, "MinSize/COL_THR: GAP_OBJ!" );
	  if( type(y) == START_HVSPAN || type(y) == START_HSPAN ||
	      type(y) == HSPAN || type(y) == VSPAN )
	  { y = MinSize(y, dim, extras);
	    b = find_max(b, back(y, dim));
	    f = find_max(f, fwd(y, dim));
	  }
	}
	back(x, dim) = b;
	fwd(x, dim)  = f;
	debug3(DSF, DD,  "]] end sizing %s (%s,%s)", Image(type(x)),
	  EchoLength(back(x, dim)), EchoLength(fwd(x, dim)));
      }
      break;


    case ROW_THR:

      assert( dim == ROWM, "MinSize/ROW_THR: dim!" );
      if( thr_state(x) == NOTSIZED )
      {	assert( Down(x) != x, "MinSize/ROW_THR: Down(x)!" );

	/* first size all the non-spanning members of the thread */
	debug1(DSF, DD,  "[[ starting sizing %s", Image(type(x)));
	b = f = 0;
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link)
	    ;
	  assert( type(y) != GAP_OBJ, "MinSize/ROW_THR: GAP_OBJ!" );
	  if( type(y) != START_HVSPAN && type(y) != START_VSPAN &&
	      type(y) != HSPAN        && type(y) != VSPAN )
	  { y = MinSize(y, dim, extras);
	    debug5(DSF, DD, "   MinSize(%s) has size %s,%s -> %s,%s",
	      Image(type(y)), EchoLength(back(y, dim)), EchoLength(fwd(y, dim)),
	      EchoLength(b), EchoLength(f));
	    b = find_max(b, back(y, dim));
	    f = find_max(f, fwd(y, dim));
	  }
	}
	back(x, dim) = b;
	fwd(x, dim)  = f;
	thr_state(x) = SIZED;
	debug3(DSF, DD,  "][ middle sizing %s (%s,%s)", Image(type(x)),
	  EchoLength(back(x, dim)), EchoLength(fwd(x, dim)));

	/* now size all the spanning members of the thread           */
	/* these will use back(x, dim) and fwd(x, dim) during sizing */
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link)
	    ;
	  assert( type(y) != GAP_OBJ, "MinSize/ROW_THR: GAP_OBJ!" );
	  if( type(y) == START_HVSPAN || type(y) == START_VSPAN ||
	      type(y) == HSPAN ||        type(y) == VSPAN )
	  { y = MinSize(y, dim, extras);
	    back(x, dim) = find_max(back(x, dim), back(y, dim));
	    fwd(x, dim) = find_max(fwd(x, dim), fwd(y, dim));
	    debug5(DSF, DD, "   MinSize(%s) has size %s,%s -> %s,%s",
	      Image(type(y)), EchoLength(back(y, dim)), EchoLength(fwd(y, dim)),
	      EchoLength(back(x, dim)), EchoLength(fwd(x, dim)));
	  }
	}
	debug3(DSF, DD,  "]] end sizing %s (%s,%s)", Image(type(x)),
	  EchoLength(back(x, dim)), EchoLength(fwd(x, dim)));
      }
      break;


    case INCGRAPHIC:
    case SINCGRAPHIC:

      /* open file and hunt for %%BoundingBox line */
      if( dim == ROWM )  break;
      Child(y, Down(x))
        ;
      fp = OpenIncGraphicFile(string(y), type(x), &full_name, &fpos(y), &cp);
      incgraphic_ok(x) = PS_FindBoundingBox(fp, &fpos(y),
	  &back(y, COLM), &back(y, ROWM), &fwd(y, COLM), &fwd(y, ROWM));
      b = (fwd(y, COLM) - back(y, COLM)) * PT;
      b = find_min(MAX_FULL_LENGTH, find_max(0, b));
      back(x, COLM) = fwd(x, COLM) = b / 2;
      b = (fwd(y, ROWM) - back(y, ROWM)) * PT;
      b = find_min(MAX_FULL_LENGTH, find_max(0, b));
      back(x, ROWM) = fwd(x, ROWM) = b / 2;
      if( fp != NULL )
      {
	fclose(fp);
        if( cp )  StringRemove(AsciiToFull(LOUT_EPS));
      }
      DisposeObject(full_name);
      break;


    default:
    
      assert1(FALSE, "MinSize", Image(type(x)));
      break;


  } /* end switch */
  debugcond6(DSF, D, dim == COLM && --debug_depth < debug_depth_max,
    "%*s] MinSize(COLM, %s %d) = (%s, %s)", debug_depth*2, " ", Image(type(x)),
    (int) x, EchoLength(back(x, dim)), EchoLength(fwd(x, dim)));
  debug1(DSF, DD,  "] MinSize returning, x = %s", EchoObject(x));
  debug3(DSF, DD, "  (%s size is %s, %s)", dimen(dim),
		EchoLength(back(x, dim)), EchoLength(fwd(x, dim)) );
  ifdebug(DSF, DDD, DebugObject(x));

  assert(back(x, dim) >= 0, "MinSize: back(x, dim) < 0!");
  assert(fwd(x, dim) >= 0, "MinSize: fwd(x, dim) < 0!");

  return x;
} /* end MinSize */
