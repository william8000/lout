/*@z07.c:Object Service:SplitIsDefinite(), DisposeObject()@*******************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.11)                       */
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
/*  FILE:         z07.c                                                      */
/*  MODULE:       Object Service                                             */
/*  EXTERNS:      MakeWord(), MakeWordTwo(), MakeWordThree(),                */
/*                DisposeObject(), CopyObject(),                             */
/*                SplitIsDefinite(), InsertObject()                          */
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

BOOLEAN SplitIsDefinite(OBJECT x)
{ OBJECT y1, y2;
  assert( type(x) == SPLIT, "SplitIsDefinite: x not a SPLIT!" );
  Child(y1, DownDim(x, COLM));
  Child(y2, DownDim(x, ROWM));
  return is_definite(type(y1)) && is_definite(type(y2));
} /* end SplitIsDefinite */


/*****************************************************************************/
/*                                                                           */
/*  DisposeObject(x)                                                         */
/*                                                                           */
/*  Dispose object x recursively, leaving intact any shared descendants.     */
/*                                                                           */
/*****************************************************************************/

int DisposeObject(OBJECT x)
{ debug2(DOS,DD,"[DisposeObject( %ld ), type = %s, x =", (long) x, Image(type(x)));
  ifdebug(DOS, DD, DebugObject(x));
  assert( Up(x) == x, "DisposeObject: x has a parent!" );
  while( Down(x) != x )  DisposeChild(Down(x));   Dispose(x);
  debug0(DOS, DD, "]DisposeObject returning.");
  return 0;
} /* end DisposeObject */


/*@::MakeWord(), MakeWordTwo()@***********************************************/
/*                                                                           */
/*  OBJECT MakeWord(typ, str, pos)                                           */
/*                                                                           */
/*  Return an unsized WORD or QWORD made from the given string and fpos.     */
/*                                                                           */
/*****************************************************************************/

OBJECT MakeWord(unsigned typ, FULL_CHAR *str, FILE_POS *pos)
{ OBJECT res;
  NewWord(res, typ, StringLength(str), pos);
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

OBJECT MakeWordTwo(unsigned typ, FULL_CHAR *str1, FULL_CHAR *str2, FILE_POS *pos)
{ int len1 = StringLength(str1);
  int len2 = StringLength(str2);
  OBJECT res;
  debug4(DOS, DD, "MakeWordTwo(%s, %s, %s, %s)",
    Image(typ), str1, str2, EchoFilePos(pos));
  NewWord(res, typ, len1 + len2, pos);
  StringCopy(string(res), str1);
  StringCopy(&string(res)[len1], str2);
  FposCopy(fpos(res), *pos);
  debug5(DOS, DD, "MakeWordTwo(%s, %s, %s, %s) returning %s",
    Image(typ), str1, str2, EchoFilePos(pos), EchoObject(res));
  return res;
} /* end MakeWordTwo */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT MakeWordThree(s1, s2, s3)                                         */
/*                                                                           */
/*  Return an unsized WORD containing these three strings.                   */
/*                                                                           */
/*****************************************************************************/

OBJECT MakeWordThree(FULL_CHAR *s1, FULL_CHAR *s2, FULL_CHAR *s3)
{ int len1 = StringLength(s1);
  int len2 = StringLength(s2);
  int len3 = StringLength(s3);
  OBJECT res;
  debug3(DOS, DD, "MakeWordThree(%s, %s, %s)", s1, s2, s3);
  NewWord(res, WORD, len1 + len2 + len3, no_fpos);
  StringCopy(string(res), s1);
  StringCopy(&string(res)[len1], s2);
  StringCopy(&string(res)[len1 + len2], s3);
  debug4(DOS, DD, "MakeWordThree(%s, %s, %s) returning %s",
    s1, s2, s3, EchoObject(res));
  return res;
} /* end MakeWordThree */


/*@::CopyObject()@************************************************************/
/*                                                                           */
/*  OBJECT CopyObject(x, pos)                                                */
/*                                                                           */
/*  Make a copy of unsized object x, setting all file positions to *pos.     */
/*                                                                           */
/*****************************************************************************/

OBJECT CopyObject(OBJECT x, FILE_POS *pos)
{ OBJECT y, link, res, tmp;

  debug2(DOS, DD, "CopyObject(%s, %s)", EchoObject(x), EchoFilePos(pos));
  switch( type(x) )
  {

    case WORD:
    case QWORD:
    
      NewWord(res, type(x), StringLength(string(x)), pos);
      StringCopy(string(res), string(x));
      break;


    case GAP_OBJ:
    
      New(res, GAP_OBJ);
      mark(gap(res)) = mark(gap(x));
      join(gap(res)) = join(gap(x));
      hspace(res) = hspace(x);
      vspace(res) = vspace(x);
      if( Down(x) != x )
      {	Child(y, Down(x));
	tmp = CopyObject(y, pos);
	Link(res, tmp);
      }
      break;


    /* case HEAD: */
    case NULL_CLOS:
    case PAGE_LABEL:
    case CROSS:
    case FORCE_CROSS:
    case ONE_COL:
    case ONE_ROW:
    case WIDE:
    case HIGH:
    case HSHIFT:
    case VSHIFT:
    case HSCALE:
    case VSCALE:
    case HCOVER:
    case VCOVER:
    case SCALE:
    case KERN_SHRINK:
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
    case YUNIT:
    case ZUNIT:
    case BREAK:
    case UNDERLINE:
    case COLOUR:
    case LANGUAGE:
    case CURR_LANG:
    case COMMON:
    case RUMP:
    case INSERT:
    case NEXT:
    case PLUS:
    case MINUS:
    case OPEN:
    case TAGGED:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case GRAPHIC:
    case VCAT:
    case HCAT:
    case ACAT:
    case ENV_OBJ:
    
      New(res, type(x));
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	tmp = CopyObject(y, pos);
	Link(res, tmp);
      }
      break;


    case FILTERED:

      New(res, type(x));
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	Link(res, y);	/* do not copy children of FILTERED */
      }
      break;


    case ENV:
    
      res = x;  /* do not copy environments */
      break;


    case PAR:
    
      New(res, PAR);
      actual(res) = actual(x);
      assert( Down(x) != x, "CopyObject: PAR child!" );
      Child(y, Down(x));
      tmp = CopyObject(y, pos);
      Link(res, tmp);
      break;


    case CLOSURE:
    
      New(res, CLOSURE);
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
    
      assert1(FALSE, "CopyObject:", Image(type(x)));
      break;

  } /* end switch */
  if( pos == no_fpos )  FposCopy(fpos(res), fpos(x));
  else FposCopy(fpos(res), *pos);
  debug1(DOS, DD, "CopyObject returning %s", EchoObject(res));
  return res;
} /* end CopyObject */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT InsertObject(OBJECT x, OBJECT *ins, STYLE *style)                 */
/*                                                                           */
/*  Search through manifested object x for an ACAT where ins may be          */
/*  attached.  If successful, set *ins to nilobj after the attachment.       */
/*                                                                           */
/*****************************************************************************/

OBJECT InsertObject(OBJECT x, OBJECT *ins, STYLE *style)
{ OBJECT link, y, g, res;
  debug2(DOS, D, "InsertObject(%s, %s)", EchoObject(x), EchoObject(*ins));
  switch( type(x) )
  {
    case WORD:
    case QWORD:

      New(res, ACAT);
      FposCopy(fpos(res), fpos(x));
      ReplaceNode(res, x);
      Link(res, x);
      StyleCopy(save_style(res), *style);
      adjust_cat(res) = padjust(*style);
      res = InsertObject(res, ins, style);
      break;


    case NULL_CLOS:
    case HEAD:
    case CROSS:
    case FORCE_CROSS:
    case PAGE_LABEL:
    case CLOSURE:
    case INCGRAPHIC:
    case SINCGRAPHIC:

      res = x;
      break;


    case HCAT:
    case VCAT:
    case COL_THR:
    case ROW_THR:
    case SPLIT:

      for( link = Down(x);  link != x && *ins != nilobj;  link = NextDown(link) )
      { Child(y, link);
	y = InsertObject(y, ins, style);
      }
      res = x;
      break;


    case ONE_COL:
    case ONE_ROW:
    case PADJUST:
    case HADJUST:
    case VADJUST:
    case HCONTRACT:
    case VCONTRACT:
    case HEXPAND:
    case VEXPAND:
    case HSCALE:
    case VSCALE:
    case HCOVER:
    case VCOVER:
    case GRAPHIC:
    case ROTATE:
    case SCALE:
    case KERN_SHRINK:
    case WIDE:
    case HIGH:
    case HSHIFT:
    case VSHIFT:

      Child(y, LastDown(x));
      y = InsertObject(y, ins, style);
      res = x;
      break;


    case ACAT:

      New(g, GAP_OBJ);
      SetGap(gap(g), FALSE, FALSE, TRUE, FIXED_UNIT, EDGE_MODE, 0);
      hspace(g) = vspace(g) = 0;
      Link(Down(x), g);
      Link(Down(x), *ins);
      *ins = nilobj;
      res = x;
      break;


    default:
    
      assert1(FALSE, "InsertObject:", Image(type(x)));
      res = x;
      break;

  }
  debug2(DOS, D, "InsertObject returning (%s) %s",
    *ins == nilobj ? "success" : "failure", EchoObject(res));
  return res;
} /* end InsertObject */
