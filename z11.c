/*@z11.c:Style Service:EchoStyle()@*******************************************/
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
/*  FILE:         z11.c                                                      */
/*  MODULE:       Style Service                                              */
/*  EXTERNS:      EchoStyle(), SpaceChange(), BreakChange()                  */
/*                                                                           */
/*****************************************************************************/
#include "externs"


#if DEBUG_ON
/*****************************************************************************/
/*                                                                           */
/*  FULL_CHAR *EchoStyle(style)                                              */
/*                                                                           */
/*  Returns a string showing the value of the style.                         */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *EchoStyle(STYLE *style)
{ static FULL_CHAR res[100];
  static char *hyphwords[] = { "hyph_undef", "hyph_off", "hyph_on" };
  static char *fillwords[] = { "fill_undef", "fill_off", "fill_on" };
  static char *displaywords[] = { "undef", "adjust", "outdent", "left",
			     "centre", "right", "do" };
  static char *capswords[] = { "nosmallcaps", "smallcaps" };

  StringCopy(res, AsciiToFull("["));
  StringCat(res, EchoCatOp(VCAT,mark(line_gap(*style)),join(line_gap(*style))));
  StringCat(res, EchoGap(&line_gap(*style)));
  StringCat(res, AsciiToFull(", "));
  StringCat(res, font(*style) == 0 ?
		   AsciiToFull("nofont") : FontFamilyAndFace(font(*style)));
  StringCat(res, AsciiToFull(" ("));
  StringCat(res, EchoGap(&space_gap(*style)));
  StringCat(res, AsciiToFull("), "));
  StringCat(res, AsciiToFull(hyph_style(*style) < 3 ?
		    hyphwords[hyph_style(*style)] : "?"));
  StringCat(res, AsciiToFull(":"));
  StringCat(res, AsciiToFull(fill_style(*style) < 3 ?
		    fillwords[fill_style(*style)] : "?"));
  StringCat(res, AsciiToFull(":"));
  StringCat(res, AsciiToFull(display_style(*style) < 7 ?
		    displaywords[display_style(*style)] : "?"));
  StringCat(res, AsciiToFull(":"));
  StringCat(res, AsciiToFull(small_caps(*style) < 2 ?
		    capswords[small_caps(*style)] : "?"));
  StringCat(res, AsciiToFull("]"));
  return res;
} /* end EchoStyle */
#endif


/*@::SpaceChange()@***********************************************************/
/*                                                                           */
/*  SpaceChange(style, x)                                                    */
/*                                                                           */
/*  Change the current break style as indicated by object x.                 */
/*                                                                           */
/*****************************************************************************/

void SpaceChange(STYLE *style, OBJECT x)
{ GAP res_gap;  unsigned gap_inc;
  debug2(DSS, D, "SpaceChange(%s, %s)", EchoStyle(style), EchoObject(x));
  if( !is_word(type(x)) )
  { Error(11, 1, "invalid left parameter of %s", WARN, &fpos(x), KW_SPACE);
  }
  else
  { GetGap(x, style, &res_gap, &gap_inc);
    if( gap_inc != GAP_ABS && units(res_gap) != units(space_gap(*style)) )
    { Error(11, 2, "spacing %s is not compatible with current spacing",
	WARN, &fpos(x), string(x));
    }
    else
    { units(space_gap(*style)) = units(res_gap);
      mode(space_gap(*style))  = mode(res_gap);
      width(space_gap(*style)) = gap_inc == GAP_ABS ? width(res_gap) :
	     gap_inc == GAP_INC ? width(space_gap(*style)) + width(res_gap) :
	     max(width(space_gap(*style)) - width(res_gap), 0);
    }
  }
  debug1(DSS, D, "SpaceChange returning %s", EchoStyle(style));
} /* end SpaceChange */


/*@::BreakChange()@***********************************************************/
/*                                                                           */
/*  BreakChange(style, x)                                                    */
/*                                                                           */
/*  Change the current break style as indicated by object x.                 */
/*                                                                           */
/*****************************************************************************/

static void changebreak(STYLE *style, OBJECT x)
{ GAP res_gap;  unsigned gap_inc;
  if( beginsbreakstyle(string(x)[0]) )
  {
    /* should be a new break style option */
    if( StringEqual(string(x), STR_BREAK_HYPHEN) )
	hyph_style(*style) = HYPH_ON;
    else if( StringEqual(string(x), STR_BREAK_NOHYPHEN) )
	hyph_style(*style) = HYPH_OFF;
    else if( StringEqual(string(x), STR_BREAK_ADJUST) )
	fill_style(*style) = FILL_ON, display_style(*style) = DISPLAY_ADJUST;
    else if( StringEqual(string(x), STR_BREAK_OUTDENT) )
	fill_style(*style) = FILL_ON, display_style(*style) = DISPLAY_OUTDENT;
    else if( StringEqual(string(x), STR_BREAK_RAGGED) )
	fill_style(*style) = FILL_ON, display_style(*style) = DISPLAY_LEFT;
    else if( StringEqual(string(x), STR_BREAK_CRAGGED) )
	fill_style(*style) = FILL_ON, display_style(*style) = DISPLAY_CENTRE;
    else if( StringEqual(string(x), STR_BREAK_RRAGGED) )
	fill_style(*style) = FILL_ON, display_style(*style) = DISPLAY_RIGHT;
    else if( StringEqual(string(x), STR_BREAK_LINES) )
	fill_style(*style) = FILL_OFF, display_style(*style) = DISPLAY_LEFT;
    else if( StringEqual(string(x), STR_BREAK_CLINES) )
	fill_style(*style) = FILL_OFF, display_style(*style) = DISPLAY_CENTRE;
    else if( StringEqual(string(x), STR_BREAK_RLINES) )
	fill_style(*style) = FILL_OFF, display_style(*style) = DISPLAY_RIGHT;
    else Error(11, 3, "unknown option to %s symbol (%s)",
	   WARN, &fpos(x), KW_BREAK, string(x));
  }
  else /* should be a new inter-line gap */
  { GetGap(x, style, &res_gap, &gap_inc);
    if( gap_inc != GAP_ABS && units(res_gap) != units(line_gap(*style)) )
      Error(11, 4, "line spacing %s is not compatible with current spacing",
        WARN, &fpos(x), string(x));
    else
    { units(line_gap(*style)) = units(res_gap);
      mode(line_gap(*style))  = mode(res_gap);
      width(line_gap(*style)) = gap_inc == GAP_ABS ? width(res_gap) :
	gap_inc == GAP_INC ? width(line_gap(*style)) + width(res_gap) :
	max(width(line_gap(*style)) - width(res_gap), 0);
    }
  }
} /* end changebreak */

void BreakChange(STYLE *style, OBJECT x)
{ OBJECT link, y;
  debug2(DSS, D, "BreakChange(%s, %s)", EchoStyle(style), EchoObject(x));
  switch( type(x) )
  {
    case NULL_CLOS: break;

    case WORD:
    case QWORD:	if( !StringEqual(string(x), STR_EMPTY) )
		  changebreak(style, x);
		break;


    case ACAT:	for( link = Down(x);  link != x;  link = NextDown(link) )
		{ Child(y, link);
		  if( type(y) == GAP_OBJ || type(y) == NULL_CLOS )  continue;
		  else if( is_word(type(y)) )
		  { if( !StringEqual(string(y), STR_EMPTY) )
		      changebreak(style, y);
		  }
		  else Error(11, 5, "invalid left parameter of %s",
			 WARN, &fpos(x), KW_BREAK);
		}
		break;


    default:	Error(11, 6, "invalid left parameter of %s",
		  WARN, &fpos(x), KW_BREAK);
		break;
  }
  debug1(DSS, D, "BreakChange returning %s", EchoStyle(style));
} /* end BreakChange */
