/*@z04.c:Token Service:NewToken(), CopyTokenList()@***************************/
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
/*  FILE:         z04.c                                                      */
/*  MODULE:       Token Service                                              */
/*  EXTERNS:      NewToken(), CopyTokenList(), EchoCatOp(), EchoToken()      */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  OBJECT NewToken(xtype, xfpos, xvspace, xhspace, xprec, xactual)          */
/*                                                                           */
/*  Returns a new non-WORD token initialised as the parameters indicate.     */
/*                                                                           */
/*****************************************************************************/

OBJECT NewToken(unsigned char xtype, FILE_POS *xfpos, unsigned char xvspace,
unsigned char xhspace, unsigned char xprec, OBJECT xactual)
{ OBJECT res;
  debug1(DTS, DDD, "NewToken(%s, ...)", Image(xtype));
  res = New(xtype);  FposCopy(fpos(res), *xfpos);
  vspace(res) = xvspace;  hspace(res) = xhspace;
  precedence(res) = xprec;  actual(res) = xactual;
  debug1(DTS, DDD, "NewToken returning %s", EchoToken(res));
  return res;
} /* end NewToken */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT CopyTokenList(x, pos)                                             */
/*                                                                           */
/*  Returns a copy of the list of tokens pointed to by x.                    */
/*  All file positions in the copy are set to *pos.                          */
/*                                                                           */
/*****************************************************************************/

OBJECT CopyTokenList(OBJECT x, FILE_POS *pos)
{ OBJECT y, z, res;
  res = nilobj;  y = x;
  if( x != nilobj ) do
  { if( is_word(type(y)) )
    { z = MakeWord(type(y), string(y), pos);
      vspace(z) = vspace(y);  hspace(z) = hspace(y);
    }
    else z = NewToken(type(y), pos,vspace(y),hspace(y),precedence(y),actual(y));
    res = Append(res, z, PARENT);
    y = succ(y, PARENT);
  } while( y != x );
  return res;
} /* end CopyTokenList */

/*@::EchoCatOp(), EchoToken()@************************************************/
/*                                                                           */
/*  FULL_CHAR *EchoCatOp(xtype, xmark, xjoin)                                */
/*                                                                           */
/*  Return the catenation operator with this type, mark and join.            */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *EchoCatOp(unsigned xtype, BOOLEAN xmark, BOOLEAN xjoin)
{ switch( xtype )
  {
    case VCAT:	return	(xmark ? xjoin ? KW_VCAT_MJ : KW_VCAT_MN
			       : xjoin ? KW_VCAT_NJ : KW_VCAT_NN);

    case HCAT:	return	(xmark ? xjoin ? KW_HCAT_MJ : KW_HCAT_MN
			       : xjoin ? KW_HCAT_NJ : KW_HCAT_NN);

    case ACAT:	return	(xmark ? xjoin ? KW_ACAT_MJ : AsciiToFull("??")
			       : xjoin ? KW_ACAT_NJ : AsciiToFull("??") );

    default:	Error(4, 1, "EchoCatOp: %d", INTERN, no_fpos, xtype);
		return STR_EMPTY;

  } /* end switch */
} /* end EchoCatOp */


#if DEBUG_ON
/*****************************************************************************/
/*                                                                           */
/*  FULL_CHAR *EchoToken(x)                                                  */
/*                                                                           */
/*  Return an image of token x.  Do not worry about preceding space.         */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *EchoToken(OBJECT x)
{ switch( type(x) )
  {
    case WORD:
    
      return string(x);


    case QWORD:
    
      return StringQuotedWord(x);


    case TSPACE:
    case TJUXTA:
    case USE:
    case GSTUB_EXT:
    case GSTUB_INT:
    case GSTUB_NONE:
    
      return Image(type(x));


    case UNEXPECTED_EOF:
    case BEGIN:
    case END:
    case ENV:
    case CLOS:
    case LBR:
    case RBR:
    case NULL_CLOS:
    case PAGE_LABEL:
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
    case UNDERLINE:
    case COLOUR:
    case LANGUAGE:
    case CURR_LANG:
    case COMMON:
    case RUMP:
    case NEXT:
    case OPEN:
    case TAGGED:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case GRAPHIC:
    case ACAT:
    case HCAT:
    case VCAT:
    case CLOSURE:
    case PREPEND:
    case SYS_PREPEND:
    case DATABASE:
    case SYS_DATABASE:
    case LUSE:
    case LVIS:
    
      return actual(x) != nilobj ? SymName(actual(x)) : Image(type(x));


    default:
    
      Error(4, 2, "EchoToken: %s", INTERN, &fpos(x), Image(type(x)));
      return STR_EMPTY;
  }
} /* end EchoToken */
#endif
