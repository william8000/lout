/*@z14.c:Fill Service:Declarations@*******************************************/
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
/*  FILE:         z14.c                                                      */
/*  MODULE:       Fill Service                                               */
/*  EXTERNS:      FillObject()                                               */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define TOO_TIGHT_BAD	1048576	/* 2^20; badness of a too tight line         */
#define TOO_LOOSE_BAD	65536	/* 2^16; the max badness of a too loose line */
#define	TIGHT_BAD	4096	/* 2^12; the max badness of a tight line     */
#define	LOOSE_BAD	4096	/* 2^12; the max badness of a loose line     */
#define	HYPH_BAD	2048	/* 2^11; threshold for calling hyphenation   */
#define SQRT_TOO_LOOSE	256	/* 2^ 8; sqrt(TOO_LOOSE_BAD)                 */
#define	SQRT_TIGHT_BAD	64	/* 2^ 6; sqrt(TIGHT_BAD)                     */
#define	SQRT_LOOSE_BAD	64	/* 2^ 6; sqrt(LOOSE_BAD)                     */
#define MAX_EXPAND	1
#define MAX_SHRINK	3


typedef struct {
  OBJECT llink;			/* link to gap before left end of interval   */
  OBJECT rlink;			/* link to gap before right end of interval  */
  LENGTH nat_width;		/* natural width of interval                 */
  LENGTH space_width;		/* natural width of spaces in the interval   */
  int	 badness;		/* badness of this interval		     */
  unsigned char   class;	/* badness class of this interval	     */
  unsigned char	 tab_count;	/* number of gaps with tab mode in interval  */
  LENGTH tab_pos;		/* if tab_count > 0, this holds the position */
				/*  of the left edge of the object following */
				/*  the rightmost tab gap in the interval    */
  LENGTH width_to_tab;		/* if tab_count > 0, the interval width up   */
				/*  to but not including the rightmost tab   */
} INTERVAL;

#define unbreakable(g, hyph_allowed)					\
(width(g)==0 && (!hyph_allowed || (mode(g)!=HYPH_MODE && mode(g)!=ADD_HYPH)))


/*****************************************************************************/
/*                                                                           */
/*  Badness classes                                                          */
/*                                                                           */
/*****************************************************************************/

#define TOO_LOOSE	0	/* interval is too loose		     */
#define LOOSE		1	/* interval is loose but not too loose	     */
#define TIGHT		2	/* interval is tight but not too tight	     */
#define TOO_TIGHT	3	/* interval is too tight 		     */
#define TAB_OVERLAP	4	/* interval has a tab and left part overlaps */
#define AT_END		5	/* interval ends at right end of paragraph   */
#define ZERO_AT_LEFT	6	/* interval has a zero-width gap at left     */
#define ZERO_AT_RIGHT	7	/* interval has a zero-width gap at right    */
#define EMPTY_INTERVAL	8	/* interval is empty                         */

/*@::SetIntervalBadness()@****************************************************/
/*                                                                           */
/*  SetIntervalBadness(I)                                                    */
/*                                                                           */
/*  Private, calculates the badness and badness class of an interval.        */
/*  Does not take into account any zero-width gap at either end.             */
/*                                                                           */
/*****************************************************************************/

#define SetIntervalBadness(I, max_width, etc_width)			\
{ OBJECT g; int badness;						\
  LENGTH col_width;							\
  if( I.llink == x )							\
  { col_width = max_width;						\
    I.badness = 0;							\
  }									\
  else									\
  { col_width = etc_width;						\
    Child(g, I.llink);							\
    I.badness = save_badness(g);					\
  }									\
									\
  if( I.tab_count > 0 && I.width_to_tab > I.tab_pos )			\
  { I.class = TAB_OVERLAP;						\
    I.badness += TOO_TIGHT_BAD;						\
  }									\
  else if( MAX_EXPAND*(col_width-I.nat_width) > 2*I.space_width )	\
  { I.class = I.tab_count > 0 ? LOOSE : TOO_LOOSE;			\
    badness = (SQRT_TOO_LOOSE*(col_width - I.nat_width)) / col_width;	\
    I.badness += badness * badness;					\
  }									\
  else if( I.nat_width <= col_width )					\
  { I.class = LOOSE;							\
    badness = (SQRT_LOOSE_BAD*(col_width - I.nat_width)) / col_width;	\
    I.badness += badness * badness;					\
  }									\
  else if( MAX_SHRINK*(I.nat_width-col_width) <= I.space_width )	\
  { I.class = TIGHT;							\
    badness = (SQRT_TIGHT_BAD*(col_width - I.nat_width)) / col_width;	\
    I.badness += badness * badness;					\
  }									\
  else { I.class = TOO_TIGHT;  I.badness += TOO_TIGHT_BAD; }		\
} /* end macro SetIntervalBadness */

/*@::CorrectOversizeError()@**************************************************/
/*                                                                           */
/*  CorrectOversizeError(x, link, y, etc_width)                              */
/*                                                                           */
/*  Child y of x, whose link is link, has caused an oversize error, either   */
/*  because it is wider than etc_width, or because it is joined by zero-     */
/*  width gaps on the left to other objects with oversize total size.        */
/*  In the first case, the correction is to replace the object by an         */
/*  empty object; in the second case, the correction is to widen the gap.    */
/*                                                                           */
/*****************************************************************************/

static CorrectOversizeError(x, link, y, etc_width, hyph_allowed)
OBJECT x, link, y;  LENGTH etc_width;  BOOLEAN hyph_allowed;
{ OBJECT tmp, g;  BOOLEAN done = FALSE;

  if( PrevDown(link) != x ) /* make any preceding unbreakable gap breakable */
  { Child(g, PrevDown(link));
    assert( type(g) == GAP_OBJ, "CorrectOversizeError: left gap!" );
    if( unbreakable(gap(g), hyph_allowed) )
    { done = TRUE;  SetGap(gap(g), FALSE, TRUE, FIXED_UNIT, EDGE_MODE, 1);
      Error(WARN, &fpos(g), "line break may occur here due to wide object");
    }
  }
  if( !done ) /* else replace the wide object by an empty object */
  { Error(WARN, &fpos(y), "%s object deleted (too wide for %s paragraph)",
		EchoLength(size(y, COL)), EchoLength(etc_width));
    tmp = MakeWord(WORD, STR_EMPTY, &fpos(x));
    back(tmp, COL) = fwd(tmp, COL) = back(tmp, ROW) = fwd(tmp, ROW) = 0;
    word_font(tmp) = 0;  Link(link, tmp);  DisposeChild(link);
  }
} /* end CorrectOversizeError */


/*@::MoveRightToGap()@********************************************************/
/*                                                                           */
/*  MoveRightToGap(I, x, rlink, right, max_width, etc_width, hyph_word)      */
/*                                                                           */
/*  Private.  Shared by IntervalInit and IntervalShiftRightEnd, for moving   */
/*  to the next gap to the right, setting save_space(newg), checking for     */
/*  hyphenation case, and setting the interval badness.                      */
/*                                                                           */
/*****************************************************************************/

#define MoveRightToGap(I,x,rlink,right,max_width,etc_width,hyph_word)	\
{ OBJECT newg, foll;							\
  BOOLEAN zero_at_right = FALSE;					\
									\
  /* search onwards to find newg, the next true breakpoint */		\
  NextDefiniteWithGap(x, rlink, foll, newg);				\
									\
  /* set right link and calculate badness of the new interval */	\
  if( rlink != x )							\
  { 									\
    /* set save_space(newg) now so that it is OK to forget right */	\
    if( mode(gap(newg)) == TAB_MODE )					\
    { save_space(newg) = ActualGap(0, back(foll,COL), fwd(foll,COL),	\
	  &gap(newg), etc_width, 0) - back(foll, COL);			\
    }									\
    else								\
    { save_space(newg) = ActualGap(fwd(right, COL), back(foll, COL),	\
	  fwd(foll,COL), &gap(newg), etc_width,				\
	  I.nat_width - fwd(right,COL))					\
	  - back(foll, COL) - fwd(right, COL);				\
    }									\
									\
    /* if interval ends with hyphen, add hyph_word to nat_width */	\
    /* NB ADD_HYPH is possible after a restart                  */	\
    if( hyph_allowed &&							\
	(mode(gap(newg)) == HYPH_MODE || mode(gap(newg)) == ADD_HYPH) )	\
    { if( is_word(type(right)) && 					\
	 !(string(right)[StringLength(string(right))-1] == CH_HYPHEN) )	\
      {									\
	/* make sure hyph_word exists and is of the right font */	\
	if( hyph_word == nil )						\
	{ hyph_word = MakeWord(WORD, STR_HYPHEN, &fpos(x));		\
	  word_font(hyph_word) = 0;					\
	}								\
	if( word_font(hyph_word) != font(save_style(x)) )		\
	{ word_font(hyph_word) = font(save_style(x));			\
	  FposCopy(fpos(hyph_word), fpos(x));				\
	  FontWordSize(hyph_word);					\
	}								\
									\
	mode(gap(newg)) = ADD_HYPH;					\
	I.nat_width += size(hyph_word, COL);				\
	debug0(DOF, DD, "   adding hyph_word from nat_width\n");	\
      }									\
    }									\
    else if( unbreakable(gap(newg), hyph_allowed) ) zero_at_right=TRUE;	\
									\
    I.rlink = Up(newg);							\
  }									\
  else I.rlink = x;							\
  SetIntervalBadness(I, max_width, etc_width);				\
  if( zero_at_right )  I.class = ZERO_AT_RIGHT;				\
}

/*@::IntervalInit(), IntervalShiftRightEnd()@*********************************/
/*                                                                           */
/*  IntervalInit(I, x, max_width, etc_width, hyph_word)                      */
/*                                                                           */
/*  Set I to the first interval of x.                                        */
/*                                                                           */
/*****************************************************************************/

#define IntervalInit(I, x, max_width, etc_width, hyph_word)		\
{ OBJECT rlink, right;							\
  I.llink = x;								\
									\
  FirstDefinite(x, rlink, right);					\
  if( rlink == x )  I.class = AT_END, I.rlink = x;			\
  else									\
  { 									\
    /* have first definite object, so set interval width etc. */	\
    I.nat_width = size(right, COL);					\
    I.space_width = 0;							\
    I.tab_count = 0;							\
									\
    /* move to gap, check hyphenation there etc. */			\
    MoveRightToGap(I,x,rlink,right,max_width,etc_width,hyph_word); 	\
  }									\
} /* end macro IntervalInit */


/*****************************************************************************/
/*                                                                           */
/*  IntervalShiftRightEnd(I, x, hyph_word, max_width, etc_width)             */
/*                                                                           */
/*  Shift the right end of interval I one place to the right.                */
/*                                                                           */
/*****************************************************************************/

#define IntervalShiftRightEnd(I, x, hyph_word, max_width, etc_width) 	\
{ OBJECT rlink, g, right;						\
  assert( I.class != AT_END, "IntervalShiftRightEnd: AT_END!" );	\
  rlink = I.rlink;							\
  if( rlink == x ) I.class = AT_END;					\
  else									\
  {									\
    /* I is optimal here so save its badness and left endpoint */	\
    Child(g, rlink);							\
    assert( type(g) == GAP_OBJ, "IntervalShiftRightEnd: type(g)!" );	\
    save_badness(g) = I.badness;					\
    save_prev(g) = I.llink;						\
									\
    /* if hyphenation case, must take away width of hyph_word */	\
    if( mode(gap(g)) == ADD_HYPH )					\
    { I.nat_width -= size(hyph_word,COL);				\
      debug0(DOF, DD, "   subtracting hyph_word from nat_width");	\
    }									\
									\
    /* find definite object which must lie just to the right of g */	\
    NextDefinite(x, rlink, right);					\
    assert( rlink != x, "IntervalShiftRightEnd: rlink == x!" );		\
									\
    /* modify I to reflect the addition of g and right */		\
    if( mode(gap(g)) == TAB_MODE )					\
    { I.tab_count++;							\
      I.tab_pos = save_space(g);					\
      I.width_to_tab = I.nat_width;					\
      I.nat_width = save_space(g) + size(right, COL);			\
      I.space_width = 0;						\
    }									\
    else								\
    { I.nat_width += save_space(g) + size(right, COL);			\
      I.space_width += save_space(g);					\
    }									\
									\
    /* now shift one step to the right */				\
    MoveRightToGap(I, x, rlink, right, max_width, etc_width,hyph_word);	\
  }									\
} /* end macro IntervalShiftRightEnd */


/*@::IntervalShiftLeftEnd(), IntervalBadness()@*******************************/
/*                                                                           */
/*  IntervalShiftLeftEnd(I, x, max_width, etc_width)                         */
/*                                                                           */
/*  Shift the left end of interval I one place to the right.                 */
/*                                                                           */
/*****************************************************************************/

#define IntervalShiftLeftEnd(I, x, max_width, etc_width)		\
{ OBJECT llink, left, lgap, y;						\
  debug1(DOF, DDD, "IntervalShiftLeftEnd(%s)", IntervalPrint(I, x));	\
  assert( I.class != AT_END, "IntervalShiftLeftEnd: AT_END!" );		\
									\
  /* find left, the leftmost definite object of I */			\
  llink = I.llink;							\
  NextDefinite(x, llink, left);						\
  assert( llink != x, "IntervalShiftLeftEnd: llink == x!" );		\
									\
  /* find lgap, the first true breakpoint following left */		\
  NextDefiniteWithGap(x, llink, y, lgap);				\
  assert( llink != x, "IntervalShiftLeftEnd: llink == x!" );		\
									\
  /* calculate width and badness of interval minus left and lgap */	\
  if( mode(gap(lgap)) == TAB_MODE )					\
  { assert( I.tab_count > 0 || Up(lgap) == I.rlink,			\
			"IntervalShiftLeftEnd: tab_count <= 0!" );	\
    I.tab_count--;							\
    if( I.tab_count == 0 )  I.nat_width -= save_space(lgap);		\
  }									\
  else /* take from nat_width, or if tab, from width_to_tab */		\
  { if( I.tab_count == 0 )						\
    { I.nat_width -= save_space(lgap) + size(left, COL);		\
      I.space_width -= save_space(lgap);				\
    }									\
    else if( I.tab_count == 1 )						\
    { I.width_to_tab -= save_space(lgap) + size(left, COL);		\
    }									\
    /* else no changes since tabs hide them */				\
  }									\
  I.llink = Up(lgap);							\
  if( I.llink == I.rlink )  I.class = EMPTY_INTERVAL;			\
  else									\
  { SetIntervalBadness(I, max_width, etc_width);			\
    if( unbreakable(gap(lgap), hyph_allowed) )  I.class = ZERO_AT_LEFT;	\
  }									\
  debug1(DOF, DDD, "IShiftLeftEnd returning %s", IntervalPrint(I, x));	\
} /* end macro IntervalShiftLeftEnd */


/*****************************************************************************/
/*                                                                           */
/*  IntervalBadness(I)                                                       */
/*                                                                           */
/*  Return the badness of interval I.                                        */
/*                                                                           */
/*****************************************************************************/

#define IntervalBadness(I)	(I.badness)


/*@IntervalClass(), IntervalPrint()@******************************************/
/*                                                                           */
/*  IntervalClass(I)                                                         */
/*                                                                           */
/*  Return the badness class of interval I.                                  */
/*                                                                           */
/*****************************************************************************/

#define IntervalClass(I)	(I.class)


#if DEBUG_ON
/*****************************************************************************/
/*                                                                           */
/*  IntervalPrint(I, x)                                                      */
/*                                                                           */
/*  Return string image of the contents of interval I of ACAT x.             */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *IntervalPrint(I, x)
INTERVAL I;  OBJECT x;
{ static char *class_name[] =
    { "TOO_LOOSE", "LOOSE", "TIGHT", "TOO_TIGHT", "TAB_OVERLAP", "AT_END",
      "ZERO_AT_LEFT", "ZERO_AT_RIGHT" };
  static FULL_CHAR res[300];
  OBJECT link, y, g, prev, z; int i;
  if( I.llink == I.rlink )  return AsciiToFull("[]");
  StringCopy(res, AsciiToFull("["));
  g = nil;
  for( link = NextDown(I.llink);  link != I.rlink;  link = NextDown(link) )
  { assert(link != x, "IntervalPrint: link == x!");
    Child(y, link);
    assert(y != x, "IntervalPrint: y == x!");
    if( type(y) == GAP_OBJ )
    { g = y;
      if( Down(g) != g )
      {	Child(z, Down(g));
	StringCat(res, STR_SPACE);
	StringCat(res, EchoCatOp(ACAT, mark(gap(g)), join(gap(g)))),
	StringCat(res, is_word(type(z)) ? string(z) : Image(type(z)));
	StringCat(res, STR_SPACE);
      }
      else for( i = 1;  i <= hspace(g) + vspace(g); i++ )
	     StringCat(res, STR_SPACE);
    }
    else StringCat(res, is_word(type(z)) ? string(z) : Image(type(z)));
  }
  StringCat(res, AsciiToFull("] n"));
  StringCat(res, EchoLength(I.nat_width));
  StringCat(res, AsciiToFull(", "));
  StringCat(res, EchoLength(I.space_width));
  StringCat(res, AsciiToFull(" ("));
  StringCat(res, AsciiToFull(class_name[I.class]));
  StringCat(res, AsciiToFull(" "));
  StringCat(res, StringInt(I.badness));
  StringCat(res, AsciiToFull(")"));
  if( I.tab_count > 0 )
  { StringCat(res, AsciiToFull(" <"));
    StringCat(res, StringInt(I.tab_count));
    StringCat(res, STR_SPACE);
    StringCat(res, EchoLength(I.width_to_tab));
    StringCat(res, AsciiToFull(":"));
    StringCat(res, EchoLength(I.tab_pos));
    StringCat(res, AsciiToFull(">"));
  }
  return res;
} /* end IntervalPrint */
#endif


/*@::FillObject()@************************************************************/
/*                                                                           */
/*  FillObject(x, c)                                                         */
/*                                                                           */
/*  Break ACAT x into lines using optimal breakpoints.                       */
/*                                                                           */
/*****************************************************************************/

OBJECT FillObject(x, c)
OBJECT x;  CONSTRAINT *c;
{ INTERVAL I, BestI;  OBJECT res, gp, tmp, z, y, link, prev;
  LENGTH max_width, etc_width, outdent_margin, f;
  static OBJECT hyph_word = nil;
  BOOLEAN can_hyphenate;    /* TRUE when it is possible to call Hyphenate() */
  BOOLEAN hyph_allowed;	    /* TRUE when hyphenation of words is permitted  */
  assert( type(x) == ACAT, "FillObject: type(x) != ACAT!" );

  /* set max_width (width of 1st line) and etc_width (width of later lines) */
  max_width = min(fc(*c), bfc(*c));
  if( display_style(save_style(x)) == DISPLAY_OUTDENT )
  { outdent_margin = 2 * FontSize(font(save_style(x)), x);
    etc_width = max_width - outdent_margin;
  }
  else etc_width = max_width;
  assert( size(x, COL) > max_width, "FillObject: initial size!" );

  /* add &1rt {} to end of paragraph */
  gp = New(GAP_OBJ);  hspace(gp) = 1;  vspace(gp) = 0;
  SetGap(gap(gp), FALSE, TRUE, AVAIL_UNIT, TAB_MODE, 1*FR);
  tmp = MakeWord(WORD, STR_GAP_RJUSTIFY, &fpos(x));
  Link(gp, tmp);  Link(x, gp);
  tmp = MakeWord(WORD, STR_EMPTY, &fpos(x));
  back(tmp, COL) = fwd(tmp, COL) = back(tmp, ROW) = fwd(tmp, ROW) = 0;
  word_font(tmp) = 0;
  Link(x, tmp);
  debug2(DOF, D, "FillObject(x, %s); %s",
	EchoConstraint(c), EchoStyle(&save_style(x)));
  ifdebug(DOF, DD, DebugObject(x); fprintf(stderr, "\n\n") );

  /* initially we can hyphenate if hyphenation is on, but not first pass */
  if( hyph_style(save_style(x)) == HYPH_UNDEF )
    Error(FATAL, &fpos(x), "hyphen or nohyphen option missing");
  can_hyphenate = (hyph_style(save_style(x)) == HYPH_ON);
  hyph_allowed = FALSE;

  /* initialize I to first interval, BestI to best ending here, and run */
  RESTART:
  IntervalInit(I, x, max_width, etc_width, hyph_word);  BestI = I;
  while( IntervalClass(I) != AT_END )
  {
    debug1(DOF, D, "loop:  %s", IntervalPrint(I, x));
    switch( IntervalClass(I) )
    {

      case TOO_LOOSE:
      
	/* too loose, so save best and shift right end */
	if( IntervalBadness(BestI) < IntervalBadness(I) )  I = BestI;
	debug1(DOF, D, "BestI: %s\n", IntervalPrint(I, x));
	/* NB no break */


      case ZERO_AT_RIGHT:

	IntervalShiftRightEnd(I, x, hyph_word, max_width, etc_width);
	BestI = I;
	break;


      case LOOSE:
      case TIGHT:
      
	/* reasonable, so check best and shift left end */
	if( IntervalBadness(I) < IntervalBadness(BestI) )  BestI = I;
	/* NB no break */


      case ZERO_AT_LEFT:
      case TAB_OVERLAP:
      case TOO_TIGHT:
      
	/* too tight, or zero-width gap at left end, so shift left end */
	IntervalShiftLeftEnd(I, x, max_width, etc_width);
	break;


      case EMPTY_INTERVAL:

	PrevDefinite(x, I.llink, y);
	if( can_hyphenate )
	{ x = Hyphenate(x);
	  can_hyphenate = FALSE;
	  hyph_allowed = TRUE;
	}
	else CorrectOversizeError(x, I.llink, y, etc_width, hyph_allowed);
	goto RESTART;
	break;


      default:
      
	Error(INTERN, &fpos(x), "FillObject: unknown interval class!");
	break;

    }
  }

  /* do end processing */
  ifdebug(DOF, D,
    debug0(DOF, D, "final result:");
    debug1(DOF, D, "%s", IntervalPrint(BestI, x));
    while( BestI.llink != x )
    { BestI.rlink = BestI.llink;
      Child(gp, BestI.rlink);
      BestI.llink = save_prev(gp);
      debug1(DOF, D, "%s", IntervalPrint(BestI, x));
    }
  );

  if( I.llink == x )
  { /* since line did not fit initally, this must mean either that a large  */
    /* word was discarded, or else that the line was only slightly tight    */
    res = x;
    back(res, COL) = 0;  fwd(res, COL) = max_width;
  }
  else if( can_hyphenate && IntervalBadness(BestI) > HYPH_BAD )
  { x = Hyphenate(x);
    can_hyphenate = FALSE;
    hyph_allowed = TRUE;
    goto RESTART;
  }
  else
  { OBJECT lgap, llink;
    res = New(VCAT);
    back(res, COL) = 0;  fwd(res, COL) = max_width;
    ReplaceNode(res, x);
    llink = I.llink;

    /* break the lines of x */
    while( llink != x )
    { y = New(ACAT);
      FposCopy(fpos(y), fpos(x));
      StyleCopy(save_style(y), save_style(x));
      if( Down(res) != res &&
		(display_style(save_style(y)) == DISPLAY_ADJUST ||
		 display_style(save_style(y)) == DISPLAY_OUTDENT) )
	 display_style(save_style(y)) = DO_ADJUST;
      back(y, COL) = 0;
      fwd(y, COL) = max_width;

      /* if outdented paragraphs, add 2.0f @Wide & to front of new line */
      if( display_style(save_style(x)) == DISPLAY_OUTDENT )
      {
	OBJECT t1, t2, z;
	t1 = MakeWord(WORD, STR_EMPTY, &fpos(x));
	back(t1, COL) = fwd(t1, COL) = back(t1, ROW) = fwd(t1, ROW) = 0;
	word_font(t1) = 0;
	t2 = New(WIDE);
	SetConstraint(constraint(t2), MAX_LEN, outdent_margin, MAX_LEN);
	back(t2, COL) = 0;  fwd(t2, COL) = outdent_margin;
	Link(t2, t1);
	Link(y, t2);
	z = New(GAP_OBJ);
	hspace(z) = vspace(z) = 0;
	SetGap(gap(z), FALSE, TRUE, FIXED_UNIT, EDGE_MODE, 0);
	Link(y, z);
      }

      /* move the line to below y */
      TransferLinks(NextDown(llink), x, y);

      /* add hyphen to end of previous line, if lgap is ADD_HYPH */
      Child(lgap, llink);
      if( mode(gap(lgap)) == ADD_HYPH )
      { OBJECT z = New(GAP_OBJ);
	debug0(DOF, DD, "   adding hyphen\n");
	hspace(z) = vspace(z) = 0;
	SetGap(gap(z), FALSE, TRUE, FIXED_UNIT, EDGE_MODE, 0);
	Link(x, z);
	z = MakeWord(WORD, STR_HYPHEN, &fpos(y));
	word_font(z) = font(save_style(x));
	FontWordSize(z);
	Link(x, z);
      }

      /* attach y to res, recycle lgap for gap separating the two lines */
      Link(NextDown(res), y);
      MoveLink(llink, NextDown(res), PARENT);
      hspace(lgap) = 0;
      vspace(lgap) = 1;
      GapCopy(gap(lgap), line_gap(save_style(x)));

      /* move on to previous line */
      llink = save_prev(lgap);
    }

    /* attach first line, x, to res */
    Link(NextDown(res), x);
    back(x, COL) = 0;
    fwd(x, COL) = max_width;
    if( display_style(save_style(x)) == DISPLAY_ADJUST ||
	display_style(save_style(x)) == DISPLAY_OUTDENT )
	  display_style(save_style(x)) = DO_ADJUST;

    /* delete the final &1rt {} from the last line, to help clines */
    Child(y, LastDown(res));
    assert( Down(y) != LastDown(y), "FillObject: empty last line!" );
    Child(z, LastDown(y));
    assert( type(z)==WORD && string(z)[0] == '\0', "FillObject: last word!" );
    DisposeChild(LastDown(y));
    Child(z, LastDown(y));
    assert( type(z) == GAP_OBJ, "FillObject: last gap_obj!" );
    DisposeChild(LastDown(y));

    /* recalculate the width of the last line, since it is smaller */
    FirstDefinite(y, link, z);
    assert( link != y, "FillObject: last line is empty!" );
    f = back(z, COL);  prev = z;
    NextDefiniteWithGap(y, link, z, gp);
    while( link != y )
    {
      f += MinGap(fwd(prev, COL), back(y, COL), fwd(y, COL), &gap(gp));
      prev = z;
      NextDefiniteWithGap(y, link, z, gp);
    }
    fwd(y, COL) = f + fwd(prev, COL);

    /* make last line DO_ADJUST if it is oversize */
    if( size(y, COL) > max_width )  display_style(save_style(y)) = DO_ADJUST;
  }

  debug0(DOF, D, "FillObject exiting");
  return res;
} /* end FillObject */
