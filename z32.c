/*@z32.c:Counter Service:Next()@**********************************************/
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
/*  FILE:         z32.c                                                      */
/*  MODULE:       Counter Service                                            */
/*  EXTERNS:      Next()                                                     */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define isdigit(x)	( (x) >= '0' && (x) <= '9' )

/*****************************************************************************/
/*                                                                           */
/*  OBJECT Next(x, inc, done)                                                */
/*                                                                           */
/*  Return x with its value incremented by inc (if possible).                */
/*  Set *done to TRUE if successful, leave *done unchanged otherwise.        */
/*                                                                           */
/*****************************************************************************/

OBJECT Next(x, inc, done)
OBJECT x; int inc; BOOLEAN *done;
{ OBJECT y, link;  int l, r, n, len;
  unsigned char buff[MAX_LINE + 1];
  debug3(DCS, DD, "Next( %s, %d, %s )", EchoObject(null, x), inc, bool(*done));
  switch( type(x) )
  {
    case WORD:
    
      len = strlen(string(x));
      for( r = len - 1;  r >= 0 && !isdigit(string(x)[r]);  r--);
      if( r < 0 ) break;
      for( l = r-1;  l >= 0 && isdigit(string(x)[l]);  l-- );
      sscanf(&string(x)[l+1], "%d", &n);
      string(x)[l+1] = '\0';
      sprintf(buff, "%s%d%s", string(x), n+inc, &string(x)[r+1]);
      if( strlen(buff) >= MAX_LINE )
	Error(FATAL, &fpos(x), "word %s is too long", buff);
      y = MakeWord(buff, &fpos(x));
      word_font(y) = word_font(x);
      MergeNode(y, x);  x = y;
      *done = TRUE;
      break;


    case INCGRAPHIC:
    case SINCGRAPHIC:
    case GAP_OBJ:
    case CLOSURE:
    case NULL_CLOS:
    case CROSS:
    
      break;


    case ONE_COL:
    case ONE_ROW:
    case WIDE:
    case HIGH:
    case HCONTRACT:
    case VCONTRACT:
    case HEXPAND:
    case VEXPAND:
    case PADJUST:
    case HADJUST:
    case VADJUST:
    case HSCALE:
    case VSCALE:
    case ROTATE:
    case SCALE:
    case SPLIT:
    case GRAPHIC:
    
      Child(y, LastDown(x));
      y = Next(y, inc, done);
      break;


    case ACAT:
    
      /* ***
      x = ReplaceWithTidy(x);
      *** */
      if( type(x) == WORD )
      {	len = strlen(string(x));
	for( r = len-1; r >= 0 && !isdigit(string(x)[r]); r--);
	if( r < 0 ) break;
	for( l = r-1;  l >= 0 && isdigit(string(x)[l]);  l-- );
	sscanf(&string(x)[l+1], "%d", &n);
	string(x)[l+1] = '\0';
	sprintf(buff,"%s%d%s",string(x),n+inc,&string(x)[r+1]);
	y = MakeWord(buff, &fpos(x));
	word_font(y) = word_font(x);
	MergeNode(y, x);  x = y;
	*done = TRUE;
      }
      else
      {	assert( type(x) == ACAT, "Next: ACAT!" );
	link = LastDown(x);
	while( link != x && !*done )
	{ Child(y, link);
	  if( is_index(type(y)) )  continue;
	  y = Next(y, inc, done);
	  if( !*done )  link = PrevDown(link);
	}
      }
      break;


    case COL_THR:
    case ROW_THR:
    case HCAT:
    case VCAT:
    
      link = LastDown(x);
      while( link != x && !*done )
      {	Child(y, link);
	if( is_index(type(y)) )  continue;
	y = Next(y, inc, done);
	if( !*done )  link = PrevDown(link);
      }
      break;


    default:
    
      Error(INTERN,&fpos(x), "Next: type(x) = %s", Image(type(x)));
      break;

  } /* end switch */
  debug1(DCS, DD, "Next returning %s", EchoObject(null, x));
  return x;
} /* end Next */
