/*@z14.c:Fill Service:Declarations@*******************************************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.08)                       */
/*  COPYRIGHT (C) 1991, 1996 Jeffrey H. Kingston                             */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@cs.usyd.edu.au)                                */
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
#define	HYPH_BAD	1024	/* 2^10; threshold for calling hyphenation   */
#define SQRT_TOO_LOOSE	256	/* 2^ 8; sqrt(TOO_LOOSE_BAD)                 */
#define	SQRT_TIGHT_BAD	64	/* 2^ 6; sqrt(TIGHT_BAD)                     */
#define	SQRT_LOOSE_BAD	64	/* 2^ 6; sqrt(LOOSE_BAD)                     */
#define MAX_EXPAND	1
#define MAX_SHRINK	3


typedef struct {
  OBJECT	llink;		/* link to gap before left end of interval   */
  OBJECT	rlink;		/* link to gap before right end of interval  */
  OBJECT	cwid;		/* link to current line width in multi case  */
  int		nat_width;	/* natural width of interval                 */
  int		space_width;	/* natural width of spaces in the interval   */
  int		badness;	/* badness of this interval		     */
  unsigned char	class;		/* badness class of this interval	     */
  unsigned char	tab_count;	/* number of gaps with tab mode in interval  */
  int		tab_pos;	/* if tab_count > 0, this holds the position */
				/*  of the left edge of the object following */
				/*  the rightmost tab gap in the interval    */
  int width_to_tab;		/* if tab_count > 0, the interval width up   */
				/*  to but not including the rightmost tab   */
} INTERVAL;


/*****************************************************************************/
/*                                                                           */
/*  static BOOLEAN DoNotBreak(g, hyph_allowed, nonzero_breaks, use_unbreak)  */
/*                                                                           */
/*  TRUE iff gap g is unbreakable i.e. unsuitable for replacement by a line  */
/*  break.  Parameter hyph_allowed is true if hyphenation is allowed, and    */
/*  nonzero_breaks is true if breaks are allowed only at gaps of non-zero    */
/*  width.                                                                   */
/*                                                                           */
/*  A gap is always breakable if hyphenation is on and it has one of the     */
/*  two hyphenation modes.  Otherwise, it is unbreakable if breaks are       */
/*  allowed only at gaps of non-zero width and this gap has zero width,      */
/*  or if the gap is marked as unbreakable, because it or a nearby gap has   */
/*  overstrike mode.                                                         */
/*                                                                           */
/*****************************************************************************/

#define DoNotBreak(g, hyph_allowed, nonzero_breaks, use_unbreakable)	\
(									\
  use_unbreakable ? unbreakable(g) :					\
  hyph_allowed && (mode(gap(g))==HYPH_MODE || mode(gap(g))==ADD_HYPH) ?	\
  FALSE : ((nonzero_breaks && width(gap(g))==0) || mode(gap(g))==OVER_MODE) \
)

/* *** old value
  hyph_allowed && (mode(g)==HYPH_MODE || mode(g)==ADD_HYPH) ? FALSE :
  (nonzero_breaks && width(g) == 0) || mode(g) == OVER_MODE
*** */

/*** old value
(width(g)==0 && (!hyph_allowed || (mode(g)!=HYPH_MODE && mode(g)!=ADD_HYPH)))
***/


/*****************************************************************************/
/*                                                                           */
/*  Badness classes                                                          */
/*                                                                           */
/*****************************************************************************/

#define TOO_LOOSE	  0	/* interval is too loose		     */
#define LOOSE		  1	/* interval is loose but not too loose	     */
#define TIGHT		  2	/* interval is tight but not too tight	     */
#define TOO_TIGHT	  3	/* interval is too tight 		     */
#define TAB_OVERLAP	  4	/* interval has a tab and left part overlaps */
#define AT_END		  5	/* interval ends at right end of paragraph   */
#define UNBREAKABLE_LEFT  6	/* interval has an unbreakable gap at left   */
#define UNBREAKABLE_RIGHT 7	/* interval has an unbreakable gap at right  */
#define EMPTY_INTERVAL	  8	/* interval is empty                         */

/*@::SetIntervalBadness()@****************************************************/
/*                                                                           */
/*  SetIntervalBadness(I)                                                    */
/*                                                                           */
/*  Private, calculates the badness and badness class of an interval.        */
/*  Does not take into account any unbreakable gap at either end.            */
/*                                                                           */
/*****************************************************************************/

#define SetIntervalBadness(I, max_width, etc_width)			\
{ OBJECT g; int badness;						\
  int col_width;							\
  if( I.llink == x )							\
  { col_width = (I.cwid!=nilobj) ? bfc(constraint(I.cwid)) : max_width;	\
    I.badness = 0;							\
  }									\
  else									\
  { col_width = (I.cwid!=nilobj) ? bfc(constraint(I.cwid)) : etc_width;	\
    Child(g, I.llink);							\
    I.badness = save_badness(g);					\
  }									\
									\
  if( col_width <= 0 )							\
  { if( I.nat_width == 0 )						\
    { I.class = TOO_LOOSE;						\
      I.badness += 0;							\
    }									\
    else								\
    { I.class = TIGHT;							\
      I.badness += TOO_TIGHT_BAD;					\
    }									\
  }									\
  else if( I.tab_count > 0 && I.width_to_tab > I.tab_pos )		\
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
  else if( BackEnd == POSTSCRIPT && allow_shrink &&			\
    MAX_SHRINK*(I.nat_width-col_width) <= I.space_width )		\
  { I.class = TIGHT;							\
    badness = (SQRT_TIGHT_BAD*(col_width - I.nat_width)) / col_width;	\
    I.badness += badness * badness;					\
  }									\
  else { I.class = TOO_TIGHT;  I.badness += TOO_TIGHT_BAD; }		\
} /* end macro SetIntervalBadness */


/*@::CorrectOversize()@*******************************************************/
/*                                                                           */
/*  CorrectOversize(x, link, y, etc_width, hyph_allowed, nonzero_breaks)     */
/*                                                                           */
/*  Child y of x, whose link is link, has caused an oversize error, either   */
/*  because it is wider than etc_width, or because it is joined by unbreak-  */
/*  able gaps on the left to other objects with oversize total size.         */
/*                                                                           */
/*****************************************************************************/

static void CorrectOversize(OBJECT x, OBJECT link, OBJECT y, int etc_width,
BOOLEAN hyph_allowed, BOOLEAN nonzero_breaks, BOOLEAN use_unbreakable)
{ OBJECT tmp, z, last_z, zlink, new_y, new_acat;
  CONSTRAINT c;  LENGTH total_size;
  SetConstraint(c, etc_width, etc_width, etc_width);

  /* determine the range of objects involved:  NextDown(zlink) ... link */
  last_z = nilobj;
  total_size = size(y, COLM);
  for( zlink = PrevDown(link);  zlink != x;  zlink = PrevDown(zlink) )
  { Child(z, zlink);
    if( type(z) != GAP_OBJ )
    { total_size += size(z, COLM);
      last_z = z;
    }
    else if( !DoNotBreak(z, hyph_allowed,nonzero_breaks,use_unbreakable) )
      break;
  }
  assert( !FitsConstraint(0, total_size, c), "CorrectOversize: fits!" );

  /* make y be a single object containing all the culprits, if not already */
  if( NextDown(zlink) != link )
  {
    /* create a new ACAT and put the culprits under it */
    New(new_acat, ACAT);
    FposCopy(fpos(new_acat), fpos(last_z));
    StyleCopy(save_style(new_acat), save_style(x));
    back(new_acat, COLM) = 0;
    fwd(new_acat, COLM) = total_size;
    back(new_acat, ROWM) = 0;
    fwd(new_acat, ROWM) = 0;
    TransferLinks(NextDown(zlink), NextDown(link), new_acat);

    /* create a new ONE_COL, link the new ACAT to it, and insert it */
    New(new_y, ONE_COL);
    FposCopy(fpos(new_y), fpos(new_acat));
    back(new_y, COLM) = back(new_acat, COLM);
    fwd(new_y, COLM) = fwd(new_acat, COLM);
    back(new_y, ROWM) = back(new_acat, ROWM);
    fwd(new_y, ROWM) = fwd(new_acat, ROWM);
    Link(new_y, new_acat);
    Link(NextDown(zlink), new_y);
    debug1(DOF, D, "new_y = %s", EchoObject(new_y));
    y = new_y;
  }

  if( BackEnd == POSTSCRIPT && InsertScale(y, &c) )
  { OBJECT prnt;
    Parent(prnt, Up(y));

    /* the problem has just been fixed, by inserting a @Scale above y */
    if( is_word(type(y)) )
    { Error(14, 1, "word %s horizontally scaled by factor %.2f (too wide for %s paragraph)",
        WARN, &fpos(y), string(y), (float) bc(constraint(prnt)) / SF,
	EchoLength(etc_width));
    }
    else
    { Error(14, 2, "%s object horizontally scaled by factor %.2f (too wide for %s paragraph)",
        WARN, &fpos(y), EchoLength(size(y, COLM)),
	(float) bc(constraint(prnt)) / SF, EchoLength(etc_width));
    }
  }
  else
  {
    /* fix the problem by replacing y by an empty object */
    if( size(y, COLM) <= 0 )
      Error(14, 3, "oversize object has size 0 or less", INTERN, &fpos(y));
    if( is_word(type(y)) )
    { Error(14, 4, "word %s deleted (too wide for %s paragraph)",
	WARN, &fpos(y), string(y), EchoLength(etc_width));
    }
    else
    { Error(14, 5, "%s object deleted (too wide for %s paragraph)",
	WARN, &fpos(y), EchoLength(size(y, COLM)), EchoLength(etc_width));
    }
    tmp = MakeWord(WORD, STR_EMPTY, &fpos(x));
    back(tmp, COLM) = fwd(tmp, COLM) = back(tmp, ROWM) = fwd(tmp, ROWM) = 0;
    word_font(tmp) = word_colour(tmp) = 0;
    word_language(tmp) = word_hyph(tmp) = 0;
    Link(link, tmp);  DisposeChild(link);
  }
} /* end CorrectOversize */


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
  BOOLEAN jn, unbreakable_at_right = FALSE;				\
  debug0(DOF, DD, "MoveRightToGap(I, x, rlink, right, -, -, -)");	\
									\
  /* search onwards to find newg, the next true breakpoint */		\
  NextDefiniteWithGap(x, rlink, foll, newg, jn);			\
									\
  /* set right link and calculate badness of the new interval */	\
  if( rlink != x )							\
  { 									\
    /* set save_space(newg) now so that it is OK to forget right */	\
    debug0(DOF, DD, "  MoveRightToGap setting save_space(newg)");	\
    if( I.cwid != nilobj )  etc_width = bfc(constraint(I.cwid));	\
    if( mode(gap(newg)) == TAB_MODE )					\
    { save_space(newg) = ActualGap(0, back(foll,COLM), fwd(foll,COLM),	\
	  &gap(newg), etc_width, 0) - back(foll, COLM);			\
    }									\
    else								\
    { save_space(newg) = ActualGap(fwd(right, COLM), back(foll, COLM),	\
	  fwd(foll,COLM), &gap(newg), etc_width,			\
	  I.nat_width - fwd(right,COLM))				\
	  - back(foll, COLM) - fwd(right, COLM);			\
    }									\
									\
    ifdebug(DOF, D,							\
      if( Down(newg) != newg )						\
      { OBJECT tmp;							\
	Child(tmp, Down(newg));						\
	debug5(DOF, D, "newg %s: %s %s, gap = %s, save_space = %s",	\
	Image(type(newg)), Image(type(tmp)), EchoObject(tmp),		\
	EchoGap(&gap(newg)), EchoLength(save_space(newg)));		\
      }									\
      else debug3(DOF, D, "newg %s: gap = %s, save_space = %s",		\
	Image(type(newg)), EchoGap(&gap(newg)),				\
	EchoLength(save_space(newg)));					\
    )									\
									\
    /* if interval ends with hyphen, add hyph_word to nat_width */	\
    /* NB ADD_HYPH is possible after a restart                  */	\
    if( hyph_allowed &&							\
	(mode(gap(newg)) == HYPH_MODE || mode(gap(newg)) == ADD_HYPH) )	\
    { if( is_word(type(right)) && 					\
	 !(string(right)[StringLength(string(right))-1] == CH_HYPHEN) )	\
      {									\
	/* make sure hyph_word exists and is of the right font */	\
	debug0(DOF, DD, "  MoveRightToGap checking hyph_word");		\
	if( hyph_word == nilobj )					\
	{ hyph_word = MakeWord(WORD, STR_HYPHEN, &fpos(x));		\
	  word_font(hyph_word) = 0;					\
	  word_colour(hyph_word) = colour(save_style(x));		\
	  word_language(hyph_word) = language(save_style(x));		\
	  word_hyph(hyph_word) = hyph_style(save_style(x)) == HYPH_ON;	\
	}								\
	if( word_font(hyph_word) != font(save_style(x)) )		\
	{ word_font(hyph_word) = font(save_style(x));			\
	  FposCopy(fpos(hyph_word), fpos(x));				\
	  FontWordSize(hyph_word);					\
	}								\
									\
	mode(gap(newg)) = ADD_HYPH;					\
	I.nat_width += size(hyph_word, COLM);				\
	debug0(DOF, DD, "   adding hyph_word from nat_width");		\
      }									\
    }									\
    else if( DoNotBreak(newg, hyph_allowed, nonzero_breaks,		\
		use_unbreakable) )					\
      unbreakable_at_right = TRUE;					\
									\
    I.rlink = Up(newg);							\
  }									\
  else I.rlink = x;							\
  SetIntervalBadness(I, max_width, etc_width);				\
  if( unbreakable_at_right )  I.class = UNBREAKABLE_RIGHT;		\
  else if( I.class == TIGHT && mode(gap(newg)) == TAB_MODE )		\
    I.class = TOO_TIGHT, I.badness = TOO_TIGHT_BAD;			\
  debug0(DOF, DD, "MoveRightToGap returning.");				\
}

/*@::IntervalInit(), IntervalShiftRightEnd()@*********************************/
/*                                                                           */
/*  IntervalInit(I, x, max_width, etc_width, hyph_word)                      */
/*                                                                           */
/*  Set I to the first interval of x.                                        */
/*                                                                           */
/*****************************************************************************/

#define IntervalInit(I, x, max_width, etc_width, hyph_word)		\
{ OBJECT rlink, right; BOOLEAN jn;					\
  debug0(DOF, DD, "IntervalInit(I, x, -, -, hyph_word)");		\
  I.llink = x;								\
									\
  FirstDefinite(x, rlink, right, jn);					\
  if( rlink == x )  I.class = AT_END, I.rlink = x;			\
  else									\
  { 									\
    /* have first definite object, so set interval width etc. */	\
    if( multi != nilobj )						\
    { Child(I.cwid, Down(multi));					\
    }									\
    else I.cwid = nilobj;						\
    I.nat_width = size(right, COLM);					\
    I.space_width = 0;							\
    I.tab_count = 0;							\
									\
    /* move to gap, check hyphenation there etc. */			\
    MoveRightToGap(I,x,rlink,right,max_width,etc_width,hyph_word); 	\
  }									\
  debug0(DOF, DD, "IntervalInit returning.");				\
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
    save_cwid(g) = I.cwid;						\
									\
    /* if hyphenation case, must take away width of hyph_word */	\
    if( mode(gap(g)) == ADD_HYPH )					\
    { I.nat_width -= size(hyph_word,COLM);				\
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
      I.nat_width = save_space(g) + size(right, COLM);			\
      I.space_width = 0;						\
    }									\
    else								\
    { I.nat_width += save_space(g) + size(right, COLM);			\
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
{ OBJECT llink, left, lgap, y;  BOOLEAN jn;				\
  debug1(DOF, DDD, "IntervalShiftLeftEnd(%s)", IntervalPrint(I, x));	\
  assert( I.class != AT_END, "IntervalShiftLeftEnd: AT_END!" );		\
									\
  /* find left, the leftmost definite object of I */			\
  llink = I.llink;							\
  NextDefinite(x, llink, left);						\
  assert( llink != x, "IntervalShiftLeftEnd: llink == x!" );		\
									\
  /* find lgap, the first true breakpoint following left */		\
  NextDefiniteWithGap(x, llink, y, lgap, jn);				\
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
    { I.nat_width -= save_space(lgap) + size(left, COLM);		\
      I.space_width -= save_space(lgap);				\
    }									\
    else if( I.tab_count == 1 )						\
    { I.width_to_tab -= save_space(lgap) + size(left, COLM);		\
    }									\
    /* else no changes since tabs hide them */				\
  }									\
  I.llink = Up(lgap);							\
  if( I.llink == I.rlink )  I.class = EMPTY_INTERVAL;			\
  else									\
  {									\
    if( save_cwid(lgap) != nilobj )					\
    { OBJECT tlink;							\
      tlink = NextDown(Up(save_cwid(lgap)));				\
      if( type(tlink) == ACAT )  I.cwid = save_cwid(lgap);		\
      else Child(I.cwid, tlink);					\
    }									\
    SetIntervalBadness(I, max_width, etc_width);			\
    if( DoNotBreak(lgap, hyph_allowed, nonzero_breaks,			\
      use_unbreakable) )						\
      I.class = UNBREAKABLE_LEFT;					\
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

static FULL_CHAR *IntervalPrint(INTERVAL I, OBJECT x)
{ static char *class_name[] =
    { "TOO_LOOSE", "LOOSE", "TIGHT", "TOO_TIGHT", "TAB_OVERLAP", "AT_END",
      "UNBREAKABLE_LEFT", "UNBREAKABLE_RIGHT" };
  OBJECT link, y, g, z; int i;
  static FULL_CHAR res[300];
  if( I.llink == I.rlink )  return AsciiToFull("[]");
  StringCopy(res, AsciiToFull(""));
  if( I.cwid != nilobj )
  { StringCat(res, AsciiToFull("!"));
    StringCat(res, EchoLength(bfc(constraint(I.cwid))));
    StringCat(res, AsciiToFull("!"));
  }
  StringCat(res, AsciiToFull("["));
  g = nilobj;
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
    else if( is_word(type(y)) )
	StringCat(res, string(y)[0] == '\0' ? AsciiToFull("{}") : string(y));
    else StringCat(res, Image(type(y)));
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
/*  FillObject(x, c, multi, nonzero_breaks, can_hyphenate, allow_shrink,     */
/*                          use_unbreakable, hyph_used)                      */
/*                                                                           */
/*  Break ACAT x into lines using optimal breakpoints.  Set hyph_used to     */
/*  TRUE if any hyphenation was done.                                        */
/*                                                                           */
/*    multi           If multi is not nilobj, ignore c and use the sequence  */
/*                    of constraints within multi for the successive lines.  */
/*                                                                           */
/*    nonzero_breaks  TRUE if line breaks at gaps of width zero are banned.  */
/*                                                                           */
/*    can_hyphenate   TRUE if hyphenation is permitted during this fill.     */
/*                                                                           */
/*    allow_shrink    TRUE if gaps may be shrunk as well as expanded.        */
/*                                                                           */
/*    use_unbreakable TRUE if the unbreakable() field is to be set and used  */
/*                    to prevent gaps hidden under overstruck objects from   */
/*                    becoming break points.                                 */
/*                                                                           */
/*****************************************************************************/

OBJECT FillObject(OBJECT x, CONSTRAINT *c, OBJECT multi,
  BOOLEAN nonzero_breaks, BOOLEAN can_hyphenate, BOOLEAN allow_shrink,
  BOOLEAN use_unbreakable, BOOLEAN *hyph_used)
{ INTERVAL I, BestI;  OBJECT res, gp, tmp, z, y, link, prev;
  int max_width, etc_width, outdent_margin, f;  BOOLEAN jn;
  static OBJECT hyph_word = nilobj;
  BOOLEAN hyph_allowed;	    /* TRUE when hyphenation of words is permitted  */
  assert( type(x) == ACAT, "FillObject: type(x) != ACAT!" );

  debug4(DOF, D, "FillObject(x, %s, can_hyph = %s, %s); %s",
    EchoConstraint(c), bool(can_hyphenate),
    multi == nilobj ? "nomulti" : "multi", EchoStyle(&save_style(x)));
  ifdebug(DOF, DD, DebugObject(x); fprintf(stderr, "\n\n") );

  *hyph_used = FALSE;

  if( multi == nilobj )
  {
    /* set max_width (width of 1st line), etc_width (width of later lines) */
    max_width = min(fc(*c), bfc(*c));
    if( display_style(save_style(x)) == DISPLAY_OUTDENT )
    { outdent_margin = 2 * FontSize(font(save_style(x)), x);
      etc_width = max_width - outdent_margin;
    }
    else etc_width = max_width;
    assert( size(x, COLM) > max_width, "FillObject: initial size!" );

    /* if column width is ridiculously small, exit with error message */
    if( max_width <= 3 * FontSize(font(save_style(x)), x) )
    {
      Error(14, 6, "paragraph deleted (assigned width %s is too narrow)",
         WARN, &fpos(x), EchoLength(max_width));
      res = MakeWord(WORD, STR_EMPTY, &fpos(x));
      word_font(res) = font(save_style(x));
      word_colour(res) = colour(save_style(x));
      word_language(res) = language(save_style(x));
      word_hyph(res) = hyph_style(save_style(x)) == HYPH_ON;
      back(res, COLM) = fwd(res, COLM) = 0;
      ReplaceNode(res, x);
      DisposeObject(x);
      return res;
    }
  }
  else max_width = etc_width = 0; /* not used really */

  /* add &1rt {} to end of paragraph */
  New(gp, GAP_OBJ);  hspace(gp) = 1;  vspace(gp) = 0;
  unbreakable(gp) = FALSE;
  SetGap(gap(gp), FALSE, TRUE, AVAIL_UNIT, TAB_MODE, 1*FR);
  tmp = MakeWord(WORD, STR_GAP_RJUSTIFY, &fpos(x));
  Link(gp, tmp);  Link(x, gp);
  tmp = MakeWord(WORD, STR_EMPTY, &fpos(x));
  back(tmp, COLM) = fwd(tmp, COLM) = back(tmp, ROWM) = fwd(tmp, ROWM) = 0;
  word_font(tmp) = 0;
  word_colour(tmp) = 0;
  word_language(tmp) = 0;
  word_hyph(tmp) = 0;
  Link(x, tmp);

  /* if use_unbreakable, run through x and set every overstrike gap and  */
  /* every gap in their shadows to be unbreakable                        */
  if( use_unbreakable )
  { int f, max_f;  OBJECT g;
    FirstDefinite(x, link, y, jn);
    assert( link != x, "FillObject/use_unbreakable:  link == x!" );
    f = max_f = size(y, COLM);  prev = y;
    NextDefiniteWithGap(x, link, y, g, jn);
    while( link != x )
    {
      /* implement the usual definition of being unbreakable */
      if( hyph_allowed && (mode(gap(g))==HYPH_MODE || mode(gap(g))==ADD_HYPH) )
	unbreakable(g) = FALSE;
      else if( nonzero_breaks && width(gap(g)) == 0 )
	unbreakable(g) = TRUE;
      else if( mode(gap(g)) == OVER_MODE )
	unbreakable(g) = TRUE;
      else
        unbreakable(g) = FALSE;

      /* add unbreakableness if gap is overshadowed by a previous one */
      f += MinGap(fwd(prev, COLM), back(y, COLM), fwd(y, COLM), &gap(g))
	     - fwd(prev, COLM) + back(y, COLM);
      if( f < max_f )
      { unbreakable(g) = (units(gap(g)) == FIXED_UNIT);
      }
      else
      { max_f = f;
      }

      /* on to next component and gap */
      prev = y;
      NextDefiniteWithGap(x, link, y, g, jn);
    }
  }

  /* initially we can hyphenate if hyphenation is on, but not first pass */
  if( hyph_style(save_style(x)) == HYPH_UNDEF )
    Error(14, 7, "hyphen or nohyphen option missing", FATAL, &fpos(x));
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
	if( IntervalBadness(BestI) <= IntervalBadness(I) )  I = BestI;
	debug1(DOF, D, "BestI: %s\n", IntervalPrint(I, x));
	/* NB no break */


      case UNBREAKABLE_RIGHT:

	IntervalShiftRightEnd(I, x, hyph_word, max_width, etc_width);
	BestI = I;
	break;


      case LOOSE:
      case TIGHT:
      
	/* reasonable, so check best and shift left end */
	if( IntervalBadness(I) < IntervalBadness(BestI) )  BestI = I;
	/* NB no break */


      case UNBREAKABLE_LEFT:
      case TAB_OVERLAP:
      case TOO_TIGHT:
      
	/* too tight, or unbreakable gap at left end, so shift left end */
	IntervalShiftLeftEnd(I, x, max_width, etc_width);
	break;


      case EMPTY_INTERVAL:

	PrevDefinite(x, I.llink, y);
	if( can_hyphenate )
	{ x = Hyphenate(x);
	  can_hyphenate = FALSE;
	  hyph_allowed = TRUE;
	  *hyph_used = TRUE;
	}
	else CorrectOversize(x, I.llink, y,
	  (I.cwid!=nilobj) ? bfc(constraint(I.cwid)) : etc_width,
	  hyph_allowed, nonzero_breaks, use_unbreakable);
	goto RESTART;


      default:
      
	assert(FALSE, "FillObject: IntervalClass(I)");
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
  { /* The result has only one line.  Since the line did not fit initially, */
    /* this must mean either that a large word was discarded or else that   */
    /* the line was only slightly tight                                     */
    if( multi == nilobj )
    { res = x;
      back(res, COLM) = 0;  fwd(res, COLM) = max_width;
    }
    else
    { New(res, VCAT);
      adjust_cat(res) = FALSE;
      ReplaceNode(res, x);
      Link(res, x);
    }
  }
  else if( can_hyphenate && IntervalBadness(BestI) > HYPH_BAD )
  { x = Hyphenate(x);
    can_hyphenate = FALSE;
    hyph_allowed = TRUE;
    *hyph_used = TRUE;
    goto RESTART;
  }
  else
  { OBJECT lgap, llink;
    New(res, VCAT);
    adjust_cat(res) = FALSE;
    back(res, COLM) = 0;  fwd(res, COLM) = max_width;
    ReplaceNode(res, x);
    llink = I.llink;

    /* break the lines of x */
    while( llink != x )
    { New(y, ACAT);
      FposCopy(fpos(y), fpos(x));
      StyleCopy(save_style(y), save_style(x));
      if( Down(res) != res &&
		(display_style(save_style(y)) == DISPLAY_ADJUST ||
		 display_style(save_style(y)) == DISPLAY_OUTDENT) )
	 display_style(save_style(y)) = DO_ADJUST;
      back(y, COLM) = 0;
      fwd(y, COLM) = max_width;

      /* if outdented paragraphs, add 2.0f @Wide & to front of new line */
      if( display_style(save_style(x)) == DISPLAY_OUTDENT )
      {
	OBJECT t1, t2, z;
	t1 = MakeWord(WORD, STR_EMPTY, &fpos(x));
	back(t1, COLM) = fwd(t1, COLM) = back(t1, ROWM) = fwd(t1, ROWM) = 0;
	word_font(t1) = 0;
	word_colour(t1) = 0;
	word_language(t1) = 0;
	word_hyph(t1) = 0;
	New(t2, WIDE);
	SetConstraint(constraint(t2), MAX_LEN, outdent_margin, MAX_LEN);
	back(t2, COLM) = 0;  fwd(t2, COLM) = outdent_margin;
	Link(t2, t1);
	Link(y, t2);
	New(z, GAP_OBJ);
	unbreakable(z) = TRUE;
	hspace(z) = vspace(z) = 0;
	SetGap(gap(z), FALSE, TRUE, FIXED_UNIT, EDGE_MODE, 0);
	Link(y, z);
      }

      /* move the line to below y */
      TransferLinks(NextDown(llink), x, y);

      /* add hyphen to end of previous line, if lgap is ADD_HYPH */
      Child(lgap, llink);
      if( mode(gap(lgap)) == ADD_HYPH )
      { OBJECT z;  BOOLEAN under;

	/* work out whether the hyphen needs to be underlined */
	Child(z, LastDown(x));
	under = underline(z);

	/* add zero-width gap object */
        New(z, GAP_OBJ);
	unbreakable(z) = TRUE;
	debug0(DOF, DD, "   adding hyphen\n");
	hspace(z) = vspace(z) = 0;
	underline(z) = under;
	SetGap(gap(z), FALSE, TRUE, FIXED_UNIT, EDGE_MODE, 0);
	Link(x, z);

	/* add hyphen */
	z = MakeWord(WORD, STR_HYPHEN, &fpos(y));
	word_font(z) = font(save_style(x));
	word_colour(z) = colour(save_style(x));
	word_language(z) = language(save_style(x));
	word_hyph(z) = hyph_style(save_style(x)) == HYPH_ON;
	underline(z) = under;
	FontWordSize(z);
	Link(x, z);
      }

      /* attach y to res, recycle lgap for gap separating the two lines */
      Link(NextDown(res), y);
      MoveLink(llink, NextDown(res), PARENT);
      hspace(lgap) = 0;
      vspace(lgap) = 1;
      GapCopy(gap(lgap), line_gap(save_style(x)));
      if( Down(lgap) != lgap )  DisposeChild(Down(lgap));

      /* move on to previous line */
      llink = save_prev(lgap);
    }

    /* attach first line, x, to res */
    Link(NextDown(res), x);
    back(x, COLM) = 0;
    fwd(x, COLM) = max_width;
    if( display_style(save_style(x)) == DISPLAY_ADJUST ||
	display_style(save_style(x)) == DISPLAY_OUTDENT )
	  display_style(save_style(x)) = DO_ADJUST;

    /* if last line contains only the {} from final &1rt {}, delete the line */
    /* and the preceding gap                                                 */
    Child(y, LastDown(res));
    if( Down(y) == LastDown(y) )
    { DisposeChild(LastDown(res));
      assert( Down(res) != LastDown(res), "almost empty paragraph!" );
      DisposeChild(LastDown(res));
    }

    /* else delete the final &1rt {} from the last line, to help clines */
    else
    { Child(z, LastDown(y));
      assert( type(z)==WORD && string(z)[0]=='\0', "FillObject: last word!" );
      DisposeChild(LastDown(y));
      Child(z, LastDown(y));
      assert( type(z) == GAP_OBJ, "FillObject: last gap_obj!" );
      DisposeChild(LastDown(y));
    }

    /* recalculate the width of the last line, since it may now be smaller */
    assert( LastDown(res) != res, "FillObject: empty paragraph!" );
    Child(y, LastDown(res));
    FirstDefinite(y, link, z, jn);
    assert( link != y, "FillObject: last line is empty!" );
    f = back(z, COLM);  prev = z;
    NextDefiniteWithGap(y, link, z, gp, jn);
    while( link != y )
    {
      f += MinGap(fwd(prev, COLM), back(z, COLM), fwd(z, COLM), &gap(gp));
      prev = z;
      NextDefiniteWithGap(y, link, z, gp, jn);
    }
    fwd(y, COLM) = min(MAX_LEN, f + fwd(prev, COLM));

    /* make last line DO_ADJUST if it is oversize */
    if( size(y, COLM) > max_width )  display_style(save_style(y)) = DO_ADJUST;
  }

  debug0(DOF, D, "FillObject exiting");
  return res;
} /* end FillObject */
