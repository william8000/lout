/*@z26.c:Echo Service:BeginString(), AppendString(), EndString(), Image()@****/
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
/*  FILE:         z26.c                                                      */
/*  MODULE:       Echo Service                                               */
/*  EXTERNS:      BeginString(), AppendString(), EndString(), Image()        */
/*                                                                           */
/*****************************************************************************/
#include "externs"

#if DEBUG_ON
#define	MULTI	  7			/* max no of simultaneous calls      */

static	unsigned char buff[MULTI][MAX_LINE];	/* buffers for strings       */
static	int	curr = 1;		/* current buffer in use             */
static	int	bp;			/* next free space in buff[curr]     */
static	BOOLEAN	instring = FALSE;	/* TRUE while making a string        */


/*****************************************************************************/
/*                                                                           */
/*  BeginString()                                                            */
/*                                                                           */
/*  Locate a clear buffer into which debug output may be accumulated.        */
/*                                                                           */
/*****************************************************************************/

BeginString()
{ if( instring ) Error(INTERN, no_fpos, "BeginString: currently in string!");
  instring = TRUE;  curr = (curr + 1) % MULTI;
  assert( 0 <= curr && curr < MULTI, "BeginString: curr!" );
  strcpy(buff[curr], "");  bp = 0;
}


/*****************************************************************************/
/*                                                                           */
/*  AppendString(str, p1, p2, p3, p4, p5, p6)                                */
/*                                                                           */
/*  Sprintf str to the current buffer, if space is available there.          */
/*                                                                           */
/*****************************************************************************/

/*VARARGS1*/
AppendString(str, p1, p2, p3, p4, p5, p6)
unsigned char *str;  int p1, p2, p3, p4, p5, p6;
{ int len;
  if( !instring ) Error(INTERN, no_fpos, "AppendString: no current string!");
  assert( 0 <= curr && curr < MULTI, "BeginString: curr!" );
  if( bp == MAX_LINE ) return;		/* no space, do nothing */

  len = strlen(str);
  if( len + bp >= MAX_LINE )
  { strcpy( &buff[curr][MAX_LINE/2], " ... <too long to print>" );
    bp = MAX_LINE;
  }
  else
  { sprintf( &buff[curr][bp], str, p1, p2, p3, p4, p5, p6 );
    while( buff[curr][bp] != '\0' )  bp++;
    if( bp >= MAX_LINE )  Error(INTERN, no_fpos, "AppendString abort");
  }
} /* end AppendString */


/*****************************************************************************/
/*                                                                           */
/*  unsigned char *EndString()                                               */
/*                                                                           */
/*  Return the string constructed by previous AppendString operations.       */
/*                                                                           */
/*****************************************************************************/

unsigned char *EndString()
{ if( !instring ) Error(INTERN, no_fpos, "EndString: no current string!");
  assert( 0 <= curr && curr < MULTI, "BeginString: curr!" );
  instring = FALSE;
  return buff[curr];
} /* end Endstring */
#endif


/*****************************************************************************/
/*                                                                           */
/*  unsigned char *EchoLength(len)                                           */
/*                                                                           */
/*  Echo a length.                                                           */
/*                                                                           */
/*****************************************************************************/

unsigned char *EchoLength(len)
int len;
{ static unsigned char buff[6][20];
  static int i = 0;
  i = (i + 1) % 6;
  sprintf(buff[i], "%.3fc", (float) len/CM);
  return buff[i];
} /* end EchoLength */


/*@@**************************************************************************/
/*                                                                           */
/*  unsigned char *Image(c)                                                  */
/*                                                                           */
/*  Returns the string value of type c.                                      */
/*                                                                           */
/*****************************************************************************/

unsigned char *Image(c)
unsigned int c;
{ static unsigned char b[20];
  switch(c)
  {

    case LINK:		return (unsigned char *) "link";

    case SPLIT:		return (unsigned char *) "split";
    case HEAD:		return (unsigned char *) "head";
    case PAR:		return (unsigned char *) "par";
    case WORD:		return (unsigned char *) "word";
    case GAP_OBJ:	return (unsigned char *) "gap_obj";
    case ROW_THR:	return (unsigned char *) "row_thr";
    case COL_THR:	return (unsigned char *) "col_thr";
    case CLOSURE:	return (unsigned char *) "closure";
    case NULL_CLOS:	return (unsigned char *) KW_NULL;
    case CROSS:		return (unsigned char *) KW_CROSS;
    case ONE_COL:	return (unsigned char *) KW_ONE_COL;
    case ONE_ROW:	return (unsigned char *) KW_ONE_ROW;
    case WIDE:		return (unsigned char *) KW_WIDE;
    case HIGH:		return (unsigned char *) KW_HIGH;
    case HSCALE:	return (unsigned char *) KW_HSCALE;
    case VSCALE:	return (unsigned char *) KW_VSCALE;
    case HCONTRACT:	return (unsigned char *) KW_HCONTRACT;
    case VCONTRACT:	return (unsigned char *) KW_VCONTRACT;
    case HEXPAND:	return (unsigned char *) KW_HEXPAND;
    case VEXPAND:	return (unsigned char *) KW_VEXPAND;
    case PADJUST:	return (unsigned char *) KW_PADJUST;
    case HADJUST:	return (unsigned char *) KW_HADJUST;
    case VADJUST:	return (unsigned char *) KW_VADJUST;
    case ROTATE:	return (unsigned char *) KW_ROTATE;
    case SCALE:		return (unsigned char *) KW_SCALE;
    case CASE:		return (unsigned char *) KW_CASE;
    case YIELD:		return (unsigned char *) KW_YIELD;
    case FONT:		return (unsigned char *) KW_FONT;
    case SPACE:		return (unsigned char *) KW_SPACE;
    case BREAK:		return (unsigned char *) KW_BREAK;
    case NEXT:		return (unsigned char *) KW_NEXT;
    case ENV:		return (unsigned char *) KW_ENV;
    case CLOS:		return (unsigned char *) KW_CLOS;
    case LVIS:		return (unsigned char *) KW_LVIS;
    case OPEN:		return (unsigned char *) KW_OPEN;
    case TAGGED:	return (unsigned char *) KW_TAGGED;
    case INCGRAPHIC:	return (unsigned char *) KW_INCGRAPHIC;
    case SINCGRAPHIC:	return (unsigned char *) KW_SINCGRAPHIC;
    case GRAPHIC:	return (unsigned char *) KW_GRAPHIC;
    case ACAT:		return (unsigned char *) "acat";
    case HCAT:		return (unsigned char *) "hcat";
    case VCAT:		return (unsigned char *) "vcat";

    case TSPACE:	return (unsigned char *) "tspace";
    case TJUXTA:	return (unsigned char *) "tjuxta";
    case LBR:		return (unsigned char *) "lbr";
    case RBR:		return (unsigned char *) "rbr";
    case BEGIN:		return (unsigned char *) KW_BEGIN;
    case END:		return (unsigned char *) KW_END;
    case USE:		return (unsigned char *) KW_USE;
    case GSTUB_NONE:	return (unsigned char *) "gstub_none";
    case GSTUB_INT:	return (unsigned char *) "gstub_int";
    case GSTUB_EXT:	return (unsigned char *) "gstub_ext";
    case INCLUDE:	return (unsigned char *) KW_INCLUDE;
    case SYS_INCLUDE:	return (unsigned char *) KW_SYSINCLUDE;
    case PREPEND:	return (unsigned char *) KW_PREPEND;
    case SYS_PREPEND:	return (unsigned char *) KW_SYSPREPEND;
    case DATABASE:	return (unsigned char *) KW_DATABASE;
    case SYS_DATABASE:	return (unsigned char *) KW_SYSDATABASE;
    case START:	 	return (unsigned char *) "start";

    case DEAD:		return (unsigned char *) "dead";
    case UNATTACHED:	return (unsigned char *) "unattached";
    case RECEPTIVE:	return (unsigned char *) "receptive";
    case RECEIVING:	return (unsigned char *) "receiving";
    case RECURSIVE:	return (unsigned char *) "recursive";
    case PRECEDES:	return (unsigned char *) "precedes";
    case FOLLOWS:	return (unsigned char *) "follows";
    case CROSS_FOLL:	return (unsigned char *) "cross_foll";
    case GALL_FOLL:	return (unsigned char *) "gall_foll";
    case CROSS_TARG:	return (unsigned char *) "cross_targ";
    case GALL_TARG:	return (unsigned char *) "gall_targ";
    case GALL_PREC:	return (unsigned char *) "gall_prec";
    case CROSS_PREC:	return (unsigned char *) "cross_prec";
    case EXPAND_IND:	return (unsigned char *) "expand_ind";
    case THREAD:	return (unsigned char *) "thread";
    case CROSS_SYM:	return (unsigned char *) "cross_sym";
    case CR_ROOT:	return (unsigned char *) "cr_root";
    case MACRO:		return (unsigned char *) KW_MACRO;
    case LOCAL:		return (unsigned char *) "local";
    case LPAR:		return (unsigned char *) "lpar";
    case NPAR:		return (unsigned char *) "npar";
    case RPAR:		return (unsigned char *) "rpar";
    case CR_LIST:	return (unsigned char *) "cr_list";
    case EXT_GALL:	return (unsigned char *) "ext_gall";
    case DISPOSED:	return (unsigned char *) "disposed";

    case BACK:		return (unsigned char *) "back";
    case ON:		return (unsigned char *) "on";
    case FWD:		return (unsigned char *) "fwd";

    case PROMOTE:	return (unsigned char *) "promote";
    case CLOSE:		return (unsigned char *) "close";
    case BLOCK:		return (unsigned char *) "block";
    case CLEAR:		return (unsigned char *) "clear";

    case ABS:		return (unsigned char *) "abs";
    case INC:		return (unsigned char *) "inc";
    case DEC:		return (unsigned char *) "dec";

    default:		sprintf(b, "??(%d)", c);
			return b;
  } /* end switch */
} /* end Image */
