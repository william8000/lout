/*@z07.c:Object Service:SplitIsDefinite(), DisposeObject()@*******************/
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
/*  FILE:         z07.c                                                      */
/*  MODULE:       Object Service                                             */
/*  EXTERNS:      MakeWord(), MakeWordTwo(), DisposeObject(), CopyObject(),  */
/*                SplitIsDefinite()                                          */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN SplitIsDefinite(x)                                               */
/*                                                                           */
/*  Return TRUE if x is a definite SPLIT object (both children definite)     */
/*                                                                           */
/*****************************************************************************/

BOOLEAN SplitIsDefinite(x)
OBJECT x;
{ OBJECT y1, y2;
  assert( type(x) == SPLIT, "SplitIsDefinite: x not a SPLIT!" );
  Child(y1, DownDim(x, COL));
  Child(y2, DownDim(x, ROW));
  return is_definite(type(y1)) && is_definite(type(y2));
} /* end SplitIsDefinite */


/*****************************************************************************/
/*                                                                           */
/*  DisposeObject(x)                                                         */
/*                                                                           */
/*  Dispose object x recursively, leaving intact any shared descendants.     */
/*                                                                           */
/*****************************************************************************/

DisposeObject(x)
OBJECT x;
{ debug2(DOS,D,"[DisposeObject( %d ), type = %s, x =", (int) x, Image(type(x)));
  ifdebug(DOS, DD, DebugObject(x));
  assert( Up(x) == x, "DisposeObject: x has a parent!" );
  while( Down(x) != x )  DisposeChild(Down(x));   Dispose(x);
  debug0(DOS, D, "]DisposeObject returning.");
} /* end DisposeObject */


/*@::MakeWord(), MakeWordTwo()@***********************************************/
/*                                                                           */
/*  OBJECT MakeWord(typ, str, pos)                                           */
/*                                                                           */
/*  Return an unsized WORD or QWORD made from the given string and fpos.     */
/*                                                                           */
/*****************************************************************************/

OBJECT MakeWord(typ, str, pos)
unsigned typ;  FULL_CHAR *str;  FILE_POS *pos;
{ OBJECT res = NewWord(typ, StringLength(str), pos);
  StringCopy(string(res), str);
  FposCopy(fpos(res), *pos);
  debug4(DOS, DD, "MakeWord(%s, %s, %s) returning %s",
    Image(typ), str, EchoFilePos(pos), EchoObject(res));
  return res;
} /* end MakeWord */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT MakeWordTwo(typ, str1, str2, pos)                                 */
/*                                                                           */
/*  Return an unsized WORD or QWORD made from the two strings and fpos.      */
/*                                                                           */
/*****************************************************************************/

OBJECT MakeWordTwo(typ, str1, str2, pos)
unsigned typ;  FULL_CHAR *str1, *str2;  FILE_POS *pos;
{ int len1 = StringLength(str1);
  int len2 = StringLength(str2);
  OBJECT res = NewWord(typ, len1 + len2, pos);
  StringCopy(string(res), str1);
  StringCopy(&string(res)[len1], str2);
  FposCopy(fpos(res), *pos);
  debug5(DOS, DD, "MakeWordTwo(%s, %s, %s, %s) returning %s",
    Image(typ), str1, str2, EchoFilePos(pos), EchoObject(res));
  return res;
} /* end MakeWordTwo */


/*@::CopyObject()@************************************************************/
/*                                                                           */
/*  OBJECT CopyObject(x, pos)                                                */
/*                                                                           */
/*  Make a copy of unsized object x, setting all file positions to *pos.     */
/*                                                                           */
/*****************************************************************************/

OBJECT CopyObject(x, pos)
OBJECT x;  FILE_POS *pos;
{ OBJECT y, link, res, tmp;

  debug2(DOS, DD, "CopyObject(%s, %s)", EchoObject(x), EchoFilePos(pos));
  switch( type(x) )
  {

    case WORD:
    case QWORD:
    
      res = NewWord(type(x), StringLength(string(x)), pos);
      StringCopy(string(res), string(x));
      break;


    case GAP_OBJ:
    
      res = New(GAP_OBJ);
      mark(gap(res)) = mark(gap(x));
      join(gap(res)) = join(gap(x));
      if( Down(x) != x )
      {	Child(y, Down(x));
	tmp = CopyObject(y, pos);
	Link(res, tmp);
      }
      else
      {	hspace(res) = hspace(x);
	vspace(res) = vspace(x);
      }
      break;


    /* case HEAD: */
    case NULL_CLOS:
    case CROSS:
    case ONE_COL:
    case ONE_ROW:
    case WIDE:
    case HIGH:
    case HSHIFT:
    case VSHIFT:
    case HSCALE:
    case VSCALE:
    case SCALE:
    case HCONTRACT:
    case VCONTRACT:
    case HEXPAND:
    case VEXPAND:
    case PADJUST:
    case HADJUST:
    case VADJUST:
    case ROTATE:
    case CASE:
    case YIELD:
    case BACKEND:
    case XCHAR:
    case FONT:
    case SPACE:
    case BREAK:
    case COLOUR:
    case LANGUAGE:
    case CURR_LANG:
    case NEXT:
    case OPEN:
    case TAGGED:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case GRAPHIC:
    case VCAT:
    case HCAT:
    case ACAT:
    
      res = New(type(x));
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	tmp = CopyObject(y, pos);
	Link(res, tmp);
      }
      break;


    case FILTERED:

      res = New(type(x));
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	Link(res, y);	/* do not copy children of FILTERED */
      }
      break;


    case ENV:
    
      res = x;  /* do not copy environments */
      break;


    case PAR:
    
      res = New(PAR);
      actual(res) = actual(x);
      assert( Down(x) != x, "CopyObject: PAR child!" );
      Child(y, Down(x));
      tmp = CopyObject(y, pos);
      Link(res, tmp);
      break;


    case CLOSURE:
    
      res = New(CLOSURE);
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	assert( type(y) != CLOSURE, "CopyObject: CLOSURE!" );
	tmp = CopyObject(y, pos);
	Link(res, tmp);
      }
      actual(res) = actual(x);
      StyleCopy(save_style(res), save_style(x));
      break;


    default:
    
      Error(7, 1, "CopyObject: %s", INTERN, pos, Image(type(x)));
      break;

  } /* end switch */
  if( pos == no_fpos )  FposCopy(fpos(res), fpos(x));
  else FposCopy(fpos(res), *pos);
  debug1(DOS, DD, "CopyObject returning %s", EchoObject(res));
  return res;
} /* end CopyObject */
