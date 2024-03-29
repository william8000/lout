/*@z07.c:Object Service:SplitIsDefinite(), DisposeObject()@*******************/
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
/*  FILE:         z07.c                                                      */
/*  MODULE:       Object Service                                             */
/*  EXTERNS:      MakeWord(), MakeWordTwo(), MakeWordThree(),                */
/*                DisposeObject(), CopyObject(),                             */
/*                SplitIsDefinite(), InsertObject()                          */
/*                                                                           */
/*****************************************************************************/
#include "externs.h"


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
  Child(y1, DownDim(x, COLM))
    ;
  Child(y2, DownDim(x, ROWM))
    ;
  return is_definite(type(y1)) && is_definite(type(y2));
} /* end SplitIsDefinite */


/*****************************************************************************/
/*                                                                           */
/*  DisposeSplitObject(x)                                                    */
/*                                                                           */
/*  Dispose SPLIT object x, taking care to handle COL_THR and ROW_THR        */
/*  children properly.                                                       */
/*                                                                           */
/*****************************************************************************/

static void DisposeSplitObject(OBJECT x)
{ int i, count;
  OBJECT y, link, uplink;
  debug1(DOS, DDD, "[ DisposeSplitObject( %ld )", (long) x);
  assert(type(x) == SPLIT, "DisposeSplitObject: type(x) != SPLIT!");
  assert(Down(x) != x, "DisposeSplitObject: x has no children!")
  assert(LastDown(x) != Down(x), "DisposeSplitObject: x has one child!")
  assert(LastDown(x) == NextDown(Down(x)), "DisposeSplitObject: children!")

  /* handle first child */
  CountChild(y, Down(x), count)
    ;
  if( type(y) == COL_THR )
  {
    /* find corresponding child link out of y and delete that link */
    for( link = Down(y), uplink = Up(y), i = 1;
         link != y && uplink != y && i < count;
         link = NextDown(link), uplink = NextUp(uplink), i++ )
      ;
    assert( link != y && uplink != y, "DisposeSplitObject: link (a)!" );
    DisposeChild(link);
  }
  DisposeChild(Down(x));

  /* handle second child */
  CountChild(y, LastDown(x), count)
    ;
  if( type(y) == ROW_THR )
  {
    /* find corresponding child link out of y and delete that link */
    for( link = Down(y), uplink = Up(y), i = 1;
         link != y && uplink != y && i < count;
         link = NextDown(link), uplink = NextUp(uplink), i++ )
      ;
    assert( link != y && uplink != y, "DisposeSplitObject: link (b)!" );
    DisposeChild(link);
  }
  DisposeChild(LastDown(x));
  debug0(DOS, DDD, "] DisposeSplitObject returning");
} /* end DisposeSplitObject */


/*****************************************************************************/
/*                                                                           */
/*  DisposeObject(x)                                                         */
/*                                                                           */
/*  Dispose object x recursively, leaving intact any shared descendants.     */
/*  We return a useless integer so that we can use this in expresssions.     */
/*                                                                           */
/*  If x is a SPLIT object then one or both of its children could be         */
/*  COL_THR or ROW_THR objects.  If such thread object is has this SPLIT     */
/*  as its ith parent, then we need to dispose its ith child.                */
/*                                                                           */
/*****************************************************************************/

int DisposeObject(OBJECT x)
{ debug2(DOS,DDD,"[DisposeObject( %ld ), type = %s, x =", (long) x, Image(type(x)));
  ifdebug(DOS, DDD, DebugObject(x));
  if ( x == NULL ) return 0;
  assert( Up(x) == x, "DisposeObject: x has a parent!" );
  if( type(x) == SPLIT )
    DisposeSplitObject(x);
  else
  { while( x != NULL && Down(x) != x )  DisposeChild(Down(x));
    if ( x != NULL ) Dispose(x);
  }
  debug0(DOS, DDD, "]DisposeObject returning.");
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
  debug4(DOS, DDD, "MakeWord(%s, %s, %s) returning %s",
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
  debug4(DOS, DDD, "MakeWordTwo(%s, %s, %s, %s)",
    Image(typ), str1, str2, EchoFilePos(pos));
  NewWord(res, typ, len1 + len2, pos);
  StringCopy(string(res), str1);
  StringCopy(&string(res)[len1], str2);
  FposCopy(fpos(res), *pos);
  debug5(DOS, DDD, "MakeWordTwo(%s, %s, %s, %s) returning %s",
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
  debug3(DOS, DDD, "MakeWordThree(%s, %s, %s)", s1, s2, s3);
  NewWord(res, WORD, len1 + len2 + len3, no_fpos);
  StringCopy(string(res), s1);
  StringCopy(&string(res)[len1], s2);
  StringCopy(&string(res)[len1 + len2], s3);
  debug4(DOS, DDD, "MakeWordThree(%s, %s, %s) returning %s",
    s1, s2, s3, EchoObject(res));
  return res;
} /* end MakeWordThree */


/*@::CopyObject()@************************************************************/
/*                                                                           */
/*  OBJECT CopyObject(x, pos)                                                */
/*                                                                           */
/*  Make a copy of unsized object x, setting all file positions to *pos,     */
/*  unless *pos is no_fpos, in which case set all file positions to what     */
/*  they are in the object being copied.                                     */
/*                                                                           */
/*****************************************************************************/

OBJECT CopyObject(OBJECT x, FILE_POS *pos)
{ OBJECT y, link, res, tmp;

  debug2(DOS, DDD, "[ CopyObject(%s, %s)", EchoObject(x), EchoFilePos(pos));
  switch( type(x) )
  {

    case WORD:
    case QWORD:
    
      NewWord(res, type(x), StringLength(string(x)), pos);
      StringCopy(string(res), string(x));
      break;


    case GAP_OBJ:
    
      New(res, GAP_OBJ);
      GapCopy(gap(res), gap(x));
      hspace(res) = hspace(x);
      vspace(res) = vspace(x);
      if( Down(x) != x )
      {	Child(y, Down(x))
          ;
	tmp = CopyObject(y, pos);
	Link(res, tmp);
      }
      break;


    /* case HEAD: */
    case NULL_CLOS:
    case PAGE_LABEL:
    case CROSS:
    case FORCE_CROSS:
    case BEGIN_HEADER:
    case END_HEADER:
    case SET_HEADER:
    case CLEAR_HEADER:
    case ONE_COL:
    case ONE_ROW:
    case WIDE:
    case HIGH:
    case HSHIFT:
    case VSHIFT:
    case HMIRROR:
    case VMIRROR:
    case HSCALE:
    case VSCALE:
    case HCOVER:
    case VCOVER:
    case SCALE:
    case KERN_SHRINK:
    case HCONTRACT:
    case VCONTRACT:
    case HLIMITED:
    case VLIMITED:
    case HEXPAND:
    case VEXPAND:
    case START_HVSPAN:
    case START_HSPAN:
    case START_VSPAN:
    case HSPAN:
    case VSPAN:
    case PADJUST:
    case HADJUST:
    case VADJUST:
    case ROTATE:
    case BACKGROUND:
    case RAW_VERBATIM:
    case VERBATIM:
    case CASE:
    case YIELD:
    case BACKEND:
    case XCHAR:
    case FONT:
    case SPACE:
    case YUNIT:
    case ZUNIT:
    case SET_CONTEXT:
    case GET_CONTEXT:
    case BREAK:
    case UNDERLINE:
    case UNDERLINE_COLOUR:
    case COLOUR:
    case TEXTURE:
    case OUTLINE:
    case LANGUAGE:
    case CURR_LANG:
    case CURR_FAMILY:
    case CURR_FACE:
    case CURR_YUNIT:
    case CURR_ZUNIT:
    case COMMON:
    case RUMP:
    case MELD:
    case INSERT:
    case ONE_OF:
    case NEXT:
    case PLUS:
    case MINUS:
    case OPEN:
    case TAGGED:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case PLAIN_GRAPHIC:
    case GRAPHIC:
    case LINK_SOURCE:
    case LINK_DEST:
    case LINK_DEST_NULL:
    case LINK_URL:
    case VCAT:
    case HCAT:
    case ACAT:
    case ENV_OBJ:
    
      New(res, type(x));
      back(res, COLM) = back(res, ROWM) = fwd(res, COLM) = fwd(res, ROWM) = 0;
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link)
          ;
	tmp = CopyObject(y, pos);
	Link(res, tmp);
      }
      break;


    case FILTERED:

      New(res, type(x));
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link)
          ;
	Link(res, y);	/* do not copy children of FILTERED */
      }
      debug3(DFH, D, "copying FILTERED %d into %d %s",
	(int) x, (int) res, EchoObject(res));
      break;


    case ENV:
    
      res = x;  /* do not copy environments */
      break;


    case PAR:
    
      New(res, PAR);
      actual(res) = actual(x);
      assert( Down(x) != x, "CopyObject: PAR child!" );
      Child(y, Down(x))
        ;
      tmp = CopyObject(y, pos);
      Link(res, tmp);
      break;


    case CLOSURE:
    
      New(res, CLOSURE);
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link)
          ;
	assert( type(y) != CLOSURE, "CopyObject: CLOSURE!" );
	tmp = CopyObject(y, pos);
	Link(res, tmp);
      }
      actual(res) = actual(x);
      StyleCopy(save_style(res), save_style(x));
      break;


    default:
    
      assert1(FALSE, "CopyObject:", Image(type(x)));
      res = nilobj;
      break;

  } /* end switch */
  if( pos == no_fpos )  FposCopy(fpos(res), fpos(x));
  else FposCopy(fpos(res), *pos);
  debug1(DOS, DDD, "] CopyObject returning %s", EchoObject(res));
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
    case BEGIN_HEADER:
    case END_HEADER:
    case SET_HEADER:
    case CLEAR_HEADER:
    case HEAD:
    case CROSS:
    case FORCE_CROSS:
    case PAGE_LABEL:
    case CLOSURE:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case HSPAN:
    case VSPAN:

      res = x;
      break;


    case HCAT:
    case VCAT:
    case COL_THR:
    case ROW_THR:
    case SPLIT:

      for( link = Down(x);  link != x && *ins != nilobj;  link = NextDown(link) )
      { Child(y, link)
          ;
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
    case HLIMITED:
    case VLIMITED:
    case HEXPAND:
    case VEXPAND:
    case HMIRROR:
    case VMIRROR:
    case HSCALE:
    case VSCALE:
    case HCOVER:
    case VCOVER:
    case PLAIN_GRAPHIC:
    case GRAPHIC:
    case LINK_SOURCE:
    case LINK_DEST:
    case LINK_DEST_NULL:
    case LINK_URL:
    case ROTATE:
    case BACKGROUND:
    case SCALE:
    case KERN_SHRINK:
    case WIDE:
    case HIGH:
    case HSHIFT:
    case VSHIFT:
    case START_HVSPAN:
    case START_HSPAN:
    case START_VSPAN:

      Child(y, LastDown(x))
        ;
      y = InsertObject(y, ins, style);
      res = x;
      break;


    case ACAT:

      New(g, GAP_OBJ);
      SetGap(gap(g), FALSE, FALSE, TRUE, FIXED_UNIT, EDGE_MODE, 0);
      hspace(g) = vspace(g) = 0;
      underline(g) = UNDER_OFF;
      Link(Down(x), g);
      Link(Down(x), *ins);
      underline(*ins) = UNDER_OFF;
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


/*****************************************************************************/
/*                                                                           */
/*  Meld(x, y)                                                               */
/*                                                                           */
/*  Return the meld of x with y.                                             */
/*                                                                           */
/*****************************************************************************/
#define NO_DIR	0
#define X_DIR	1
#define Y_DIR	2
#define XY_DIR	3
#define MAX_MELD 32

OBJECT Meld(OBJECT x, OBJECT y)
{ OBJECT res;
  char table[MAX_MELD][MAX_MELD], dir[MAX_MELD][MAX_MELD];
  OBJECT xcomp[MAX_MELD], ycomp[MAX_MELD];
  OBJECT xgaps[MAX_MELD], ygaps[MAX_MELD];
  BOOLEAN is_equal;
  OBJECT link, z = nilobj, g;  BOOLEAN jn;
  int xlen, ylen, xi, yi;
  debug2(DOS, D, "Meld(%s, %s)", EchoObject(x), EchoObject(y));
  assert(type(x) == ACAT, "Meld: type(x) != ACAT");
  assert(type(y) == ACAT, "Meld: type(y) != ACAT");

  /* initialize xcomp, xgaps, xlen */
  debug0(DOS, DD, "  initializing xcomp[]");
  xlen = 0;
  xcomp[xlen] = nilobj;
  xlen++;
  g = nilobj;
  FirstDefinite(x, link, z, jn);
  while( link != x )
  { if( xlen >= MAX_MELD )
      Error(7, 1, "%s: maximum paragraph length (%d) exceeded", FATAL, &fpos(x),
	KW_MELD, MAX_MELD-1);
    assert( type(z) != ACAT, "Meld: xcomp is ACAT!");
    if( g == nilobj || width(gap(g)) != 0 )
    {
      debug3(DOS, DD, "  initializing xcomp[%d] to %s %s",
        xlen, Image(type(z)), EchoObject(z));
      xcomp[xlen] = z;
      xgaps[xlen] = g;
      xlen++;
    }
    else
    {
      debug3(DOS, DD, "  extending xcomp[%d] with %s %s",
        xlen-1, Image(type(z)), EchoObject(z));
      if( type(xcomp[xlen-1]) != ACAT )
      {
	New(res, ACAT);
	StyleCopy(save_style(res), save_style(x));
	Link(res, xcomp[xlen-1]);
	xcomp[xlen-1] = res;
      }
      Link(xcomp[xlen-1], g);
      Link(xcomp[xlen-1], z);
    }
    NextDefiniteWithGap(x, link, z, g, jn)
  }

  /* initialize ycomp, ygaps, ylen */
  debug0(DOS, DD, "  initializing ycomp[]");
  ylen = 0;
  ycomp[ylen] = nilobj;
  ylen++;
  g = nilobj;
  FirstDefinite(y, link, z, jn);
  while( link != y )
  { if( ylen >= MAX_MELD )
      Error(7, 1, "%s: maximum paragraph length (%d) exceeded", FATAL, &fpos(y),
	KW_MELD, MAX_MELD-1);
    assert( type(z) != ACAT, "Meld: ycomp is ACAT!");
    if( g == nilobj || width(gap(g)) != 0 )
    {
      debug3(DOS, DD, "  initializing ycomp[%d] to %s %s",
        ylen, Image(type(z)), EchoObject(z));
      ycomp[ylen] = z;
      ygaps[ylen] = g;
      ylen++;
    }
    else
    {
      debug3(DOS, DD, "  extending ycomp[%d] with %s %s",
        ylen-1, Image(type(z)), EchoObject(z));
      if( type(ycomp[ylen-1]) != ACAT )
      {
	New(res, ACAT);
	StyleCopy(save_style(res), save_style(x));
	Link(res, ycomp[ylen-1]);
	ycomp[ylen-1] = res;
      }
      Link(ycomp[ylen-1], g);
      Link(ycomp[ylen-1], z);
    }
    NextDefiniteWithGap(y, link, z, g, jn)
  }

  /* initialize table and dir */
  debug0(DOS, DD, "  initializing table[]");
  table[0][0] = 0;
  dir[0][0] = NO_DIR;
  for( xi = 1;  xi < xlen;  xi++ )
  { table[xi][0] = 0;
    dir[xi][0] = X_DIR;
  }
  for( yi = 1;  yi < ylen;  yi++ )
  { table[0][yi] = 0;
    dir[0][yi] = Y_DIR;
  }
  for( xi = 1;  xi < xlen;  xi++ )
  {
    for( yi = 1;  yi < ylen;  yi++ )
    {
      is_equal = EqualManifested(xcomp[xi], ycomp[yi]);
      if( is_equal )
      {
	table[xi][yi] = 1 + table[xi - 1][yi - 1];
	dir[xi][yi] = XY_DIR;
        debug3(DOS, DD, "  assigning (XY) table[%d][%d] = %d", xi, yi,
	  table[xi][yi]);
      }
      else if( table[xi - 1][yi] > table[xi][yi - 1] )
      {
	table[xi][yi] = table[xi - 1][yi];
	dir[xi][yi] = X_DIR;
        debug3(DOS, DD, "  assigning (X) table[%d][%d] = %d", xi, yi,
	  table[xi][yi]);
      }
      else
      {
	table[xi][yi] = table[xi][yi - 1];
	dir[xi][yi] = Y_DIR;
        debug3(DOS, DD, "  assigning (Y) table[%d][%d] = %d", xi, yi,
	  table[xi][yi]);
      }
    }
  }

  /* traverse table from [xlen-l][ylen-1] back to [0][0], finding who's in */
  debug0(DOS, DD, "  traversing table[]");
  New(res, ACAT);
  StyleCopy(save_style(res), save_style(x));
  for( xi = xlen - 1, yi = ylen - 1;  dir[xi][yi] != NO_DIR; )
  {
    switch( dir[xi][yi] )
    {
      case XY_DIR:

        debug3(DOS, DD, "  at table[%d][%d] (XY) linking %s",
	  xi, yi, EchoObject(xcomp[xi]));
	if( type(xcomp[xi]) != ACAT )
	{
          Link(Down(res), xcomp[xi]);
	}
	else
	  TransferLinks(Down(xcomp[xi]), xcomp[xi], Down(res));
        g = xgaps[xi];
        xi--;
        yi--;
	break;

    
      case Y_DIR:

        debug3(DOS, DD, "  at table[%d][%d] (ydec) linking %s",
	  xi, yi, EchoObject(ycomp[yi]));
	if( type(ycomp[yi]) != ACAT )
	{
          Link(Down(res), ycomp[yi]);
	}
	else
	  TransferLinks(Down(ycomp[yi]), ycomp[yi], Down(res));
        g = ygaps[yi];
        yi--;
	break;


      case X_DIR:

        debug3(DOS, DD, "  at table[%d][%d] (xdec) linking %s",
	  xi, yi, EchoObject(xcomp[xi]));
	if( type(xcomp[xi]) != ACAT )
	{
          Link(Down(res), xcomp[xi]);
	}
	else
	  TransferLinks(Down(xcomp[xi]), xcomp[xi], Down(res));
        g = xgaps[xi];
        xi--;
    }

    /* add gap if not last time; either g or one we make up */
    if( dir[xi][yi] != NO_DIR )
    {
      if( g == nilobj )
      {
	OBJECT tmp;
	New(g, GAP_OBJ);
	hspace(g) = 1;  vspace(g) = 0;
	FposCopy(fpos(g), *no_fpos);
	SetGap(gap(g), FALSE, FALSE, TRUE, FIXED_UNIT, EDGE_MODE,
	  width(space_gap(save_style(res))));
	tmp = MakeWord(WORD, AsciiToFull("1s"), &fpos(g));
	Link(g, tmp);
        Link(Down(res), g);
      }
      else
      {
	assert(Up(g) == LastUp(g), "Meld: g!" );
        Link(Down(res), g);
      }
    }
  }

  debug1(DOS, D, "Meld returning %s", EchoObject(res));
  return res;
}


/*****************************************************************************/
/*                                                                           */
/*  static BOOLEAN EqualChildren(x, y)                                       */
/*                                                                           */
/*  Return TRUE if manifested objects x and y have equal children.           */
/*                                                                           */
/*****************************************************************************/

static BOOLEAN EqualChildren(OBJECT x, OBJECT y)
{ OBJECT xl, yl, xc, yc;
  xl = Down(x), yl = Down(y);
  for( ; xl != x && yl != y;  xl = NextDown(xl), yl = NextDown(yl) )
  {
    Child(xc, xl)
      ;
    Child(yc, yl)
      ;
    if( !EqualManifested(xc, yc) )
      return FALSE;
  }
  return xl == x && yl == y;
}


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN EqualManifested(x, y)                                            */
/*                                                                           */
/*  Return TRUE if manifested objects x and y are equal.                     */
/*                                                                           */
/*****************************************************************************/

BOOLEAN EqualManifested(OBJECT x, OBJECT y)
{ OBJECT xc, yc;

  if( is_word(type(x)) && is_word(type(y)) )
  {
    return StringEqual(string(x), string(y));
  }
  else if( type(x) != type(y) )
  {
    return FALSE;
  }
  else switch( type(x) )
  {
    case GAP_OBJ:

      /* objects are equal if the two gaps are equal */
      return GapEqual(gap(x), gap(y));
      break;


    case CLOSURE:

      /* objects are equal if it's the same symbol and same parameters */
      if( actual(x) != actual(y) )
	return FALSE;
      return EqualChildren(x, y);
      break;


    case PAGE_LABEL:
    case NULL_CLOS:
    case CROSS:
    case FORCE_CROSS:
    case HEAD:
    case SPLIT:
    case HSPANNER:
    case VSPANNER:
    case COL_THR:
    case ROW_THR:
    case ACAT:
    case HCAT:
    case VCAT:
    case HMIRROR:
    case VMIRROR:
    case HSCALE:
    case VSCALE:
    case BEGIN_HEADER:
    case SET_HEADER:
    case END_HEADER:
    case CLEAR_HEADER:
    case ONE_COL:
    case ONE_ROW:
    case HCOVER:
    case VCOVER:
    case HCONTRACT:
    case VCONTRACT:
    case HEXPAND:
    case VEXPAND:
    case START_HSPAN:
    case START_VSPAN:
    case START_HVSPAN:
    case HSPAN:
    case VSPAN:
    case KERN_SHRINK:
    case BACKGROUND:
    case GRAPHIC:
    case PLAIN_GRAPHIC:
    case LINK_DEST:
    case LINK_DEST_NULL:
    case LINK_URL:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case PAR:

      /* objects are equal if the children are equal */
      return EqualChildren(x, y);
      break;


    case LINK_SOURCE:

      /* objects are equal if right children are equal */
      Child(xc, LastDown(x))
        ;
      Child(yc, LastDown(y))
        ;
      return EqualManifested(xc, yc);
      break;


    case WIDE:
    case HIGH:

      /* objects are equal if constraints and children are equal */
      return EqualConstraint(constraint(x), constraint(y)) &&
	     EqualChildren(x, y);
      break;


    case HSHIFT:
    case VSHIFT:

      /* objects are equal if constraints and children are equal */
      return shift_type(x) == shift_type(y) &&
	     GapEqual(shift_gap(x), shift_gap(y)) && EqualChildren(x, y);
      break;


    case SCALE:

      /* objects are equal if constraints and children are equal */
      return bc(constraint(x)) == bc(constraint(y)) &&
	     fc(constraint(x)) == fc(constraint(y)) &&
	     EqualChildren(x, y);
      break;


    case ROTATE:

      /* objects are equal if angle is equal and children are equal */
      return sparec(constraint(x)) == sparec(constraint(y)) &&
	     EqualChildren(x, y);
      break;


    default:

      Error(7, 2, "EqualUnsized: type == %s", FATAL, &fpos(x), Image(type(x)));
      return FALSE;
      break;
  }
}
