/*@z11.c:Style Service:SpaceChange(), BreakChange(), EchoStyle()@*************/
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
/*  FILE:         z11.c                                                      */
/*  MODULE:       Style Service                                              */
/*  EXTERNS:      SpaceChange(), BreakChange(), EchoStyle()                  */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  SpaceChange(style, x)                                                    */
/*                                                                           */
/*  Change the current break style as indicated by object x.                 */
/*                                                                           */
/*****************************************************************************/

SpaceChange(style, x)
STYLE *style;  OBJECT x;
{ GAP res_gap;  unsigned gap_inc;
  debug2(DSS, D, "SpaceChange(%s, %s)", EchoStyle(style), EchoObject(null, x));
  if( type(x) != WORD )
  { Error(WARN, &fpos(x), "invalid left parameter to %s", KW_SPACE);
  }
  else
  { GetGap(x, style, &res_gap, &gap_inc);
    if( gap_inc != ABS && units(res_gap) != units(space_gap(*style)) )
    { Error(WARN, &fpos(x), "space %s incompatible with enclosing", string(x));
    }
    else
    { units(space_gap(*style)) = units(res_gap);
      mode(space_gap(*style))  = mode(res_gap);
      width(space_gap(*style)) = gap_inc == ABS ? width(res_gap) :
	     gap_inc == INC ? width(space_gap(*style)) + width(res_gap) :
	     max(width(space_gap(*style)) - width(res_gap), 0);
    }
  }
  debug1(DSS, D, "SpaceChange returning %s", EchoStyle(style));
} /* end SpaceChange */


/*****************************************************************************/
/*                                                                           */
/*  BreakChange(style, x)                                                    */
/*                                                                           */
/*  Change the current break style as indicated by object x.                 */
/*                                                                           */
/*****************************************************************************/

static changebreak(style, x)
STYLE *style;  OBJECT x;
{ int i; GAP res_gap;  unsigned gap_inc;
  if( string(x)[0] >= 'a' && string(x)[0] <= 'z' )
  {
    /* should be a new break style option */
    if( strcmp(string(x), "hyphen") == 0 )
	hyph_style(*style) = HYPH_ON;
    else if( strcmp(string(x), "nohyphen") == 0 )
	hyph_style(*style) = HYPH_OFF;
    else if( strcmp(string(x), "adjust") == 0 )
	fill_style(*style) = FILL_ON, display_style(*style) = DISPLAY_ADJUST;
    else if( strcmp(string(x), "outdent") == 0 )
	fill_style(*style) = FILL_ON, display_style(*style) = DISPLAY_OUTDENT;
    else if( strcmp(string(x), "ragged") == 0 )
	fill_style(*style) = FILL_ON, display_style(*style) = DISPLAY_LEFT;
    else if( strcmp(string(x), "cragged") == 0 )
	fill_style(*style) = FILL_ON, display_style(*style) = DISPLAY_CENTRE;
    else if( strcmp(string(x), "ragged") == 0 )
	fill_style(*style) = FILL_ON, display_style(*style) = DISPLAY_RIGHT;
    else if( strcmp(string(x), "lines") == 0 )
	fill_style(*style) = FILL_OFF, display_style(*style) = DISPLAY_LEFT;
    else if( strcmp(string(x), "clines") == 0 )
	fill_style(*style) = FILL_OFF, display_style(*style) = DISPLAY_CENTRE;
    else if( strcmp(string(x), "rlines") == 0 )
	fill_style(*style) = FILL_OFF, display_style(*style) = DISPLAY_RIGHT;
    else Error(WARN, &fpos(x), "invalid %s option %s", KW_BREAK, string(x));
  }
  else
  {
    /* should be a new inter-line gap */
    GetGap(x, style, &res_gap, &gap_inc);
    if( gap_inc != ABS && units(res_gap) != units(line_gap(*style)) )
    { Error(WARN, &fpos(x),
		    "line spacing %s incompatible with enclosing", string(x));
    }
    else
    { units(line_gap(*style)) = units(res_gap);
      mode(line_gap(*style))  = mode(res_gap);
      width(line_gap(*style)) = gap_inc == ABS ? width(res_gap) :
	gap_inc == INC ? width(line_gap(*style)) + width(res_gap) :
	max(width(line_gap(*style)) - width(res_gap), 0);
    }
  }
} /* end changebreak */

BreakChange(style, x)
STYLE *style;  OBJECT x;
{ OBJECT link, y;
  debug2(DSS, D, "BreakChange(%s, %s)", EchoStyle(style), EchoObject(null, x));
  switch( type(x) )
  {
    case WORD:
    
      changebreak(style, x);
      break;


    case ACAT:
    
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	if( type(y) == GAP_OBJ )  continue;
	else if( type(y) == WORD )  changebreak(style, y);
	else Error(WARN, &fpos(x), "invalid left parameter of %s", KW_BREAK);
      }
      break;


    default:
    
      Error(WARN, &fpos(x), "invalid left parameter of %s", KW_BREAK);
      break;

  }
  debug1(DSS, D, "BreakChange returning %s", EchoStyle(style));
} /* end BreakChange */


#if DEBUG_ON
/*****************************************************************************/
/*                                                                           */
/*  unsigned char *EchoStyle(style)                                          */
/*                                                                           */
/*  Returns a string showing the value of the style.                         */
/*                                                                           */
/*****************************************************************************/

unsigned char *EchoStyle(style)
STYLE *style;
{ char buff1[100], buff2[100], buff3[100], buff4[100];
  static char res[100];
  static char *hyphwords[] = { "hyph_undef", "hyph_off", "hyph_on" };
  static char *fillwords[] = { "fill_undef", "fill_off", "fill_on" };
  static char *displaywords[] = { "undef", "adjust", "outdent", "left",
			     "centre", "right", "do" };
  strcpy(buff1, EchoCatOp(VCAT,mark(line_gap(*style)),join(line_gap(*style))));
  strcpy(buff2, EchoGap(&line_gap(*style)));
  strcpy(buff3, EchoGap(&space_gap(*style)));
  sprintf(buff4, "%s:%s:%s",
	hyph_style(*style) < 3 ? hyphwords[hyph_style(*style)] : "?",
	fill_style(*style) < 3 ? fillwords[fill_style(*style)] : "?",
	display_style(*style) < 7 ? displaywords[display_style(*style)] : "?");
  sprintf(res, "[%s%s, %d (%s), %s]", buff1, buff2, font(*style), buff3, buff4);
  return (unsigned char *) res;
} /* end EchoStyle */
#endif
