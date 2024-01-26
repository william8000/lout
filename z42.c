/*@z42.c:Colour Service:ColourChange, ColourCommand@**************************/
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
/*  FILE:         z42.c                                                      */
/*  MODULE:       Colour Service                                             */
/*  EXTERNS:      ColourChange(), ColourCommand()                            */
/*                                                                           */
/*****************************************************************************/
#include "externs.h"
#define INIT_COLOUR_NUM	100


/*****************************************************************************/
/*                                                                           */
/*  COLOUR_TABLE                                                             */
/*                                                                           */
/*  A symbol table permitting access to colour records by number or name.    */
/*  The table will automatically enlarge to accept any number of entries,    */
/*  but there is an arbitrary limit of 65535 colours imposed so that colour  */
/*  numbers can be stored in 16 bit fields.                                  */
/*                                                                           */
/*     ctab_new(newsize)                 New empty table, newsize capacity   */
/*     ctab_insert(x, &S)                Insert new colour object x into S   */
/*     ctab_retrieve(str, S)             Retrieve colour object of name str  */
/*     ctab_num(S, num)                  Retrieve colour object, number num  */
/*     ctab_debug(S, fp)                 Debug print of table S to file fp   */
/*                                                                           */
/*****************************************************************************/

typedef struct
{ int coltab_size;				/* size of table             */
  int coltab_count;				/* number of colours held    */
  struct coltab_rec
  {	OBJECT	by_number;			/* colour record by number   */
	OBJECT	by_name_hash;			/* colour record by name     */
  } coltab[1];
} *COLOUR_TABLE;

#define	ctab_size(S)	(S)->coltab_size
#define	ctab_count(S)	(S)->coltab_count
#define	ctab_num(S, i)	(S)->coltab[i].by_number
#define	ctab_name(S, i)	(S)->coltab[i].by_name_hash

#define hash(pos, str, S)						\
{ FULL_CHAR *p = str;							\
  pos = *p++;								\
  while( *p ) pos += *p++;						\
  pos = pos % ctab_size(S);						\
}

static COLOUR_TABLE ctab_new(int newsize)
{ COLOUR_TABLE S;  int i;
  ifdebug(DMA, D, DebugRegisterUsage(MEM_COLOUR_TAB, 1,
    2*sizeof(int) + newsize * sizeof(struct coltab_rec)));
  S = (COLOUR_TABLE) malloc(2*sizeof(int) + newsize * sizeof(struct coltab_rec));
  if( S == (COLOUR_TABLE) NULL )
    Error(42, 1, "ran out of memory when enlarging colour table",
      FATAL, no_fpos);
  ctab_size(S) = newsize;
  ctab_count(S) = 0;
  for( i = 0;  i < newsize;  i++ )
  { ctab_num(S, i) = ctab_name(S, i) = nilobj;
  }
  return S;
} /* end ctab_new */

static void ctab_insert(OBJECT x, COLOUR_TABLE *S);

static COLOUR_TABLE ctab_rehash(COLOUR_TABLE S, int newsize)
{ COLOUR_TABLE NewS;  int i;
  NewS = ctab_new(newsize);
  for( i = 1;  i <= ctab_count(S);  i++ )
     ctab_insert(ctab_num(S, i), &NewS);
  for( i = 0;  i < ctab_size(S);  i++ )
  { if( ctab_name(S, i) != nilobj )  DisposeObject(ctab_name(S, i));
  }
  ifdebug(DMA, D, DebugRegisterUsage(MEM_COLOUR_TAB, -1,
    -(2*sizeof(int) + ctab_size(S) * sizeof(struct coltab_rec))));
  free(S);
  return NewS;
} /* end ctab_rehash */

static void ctab_insert(OBJECT x, COLOUR_TABLE *S)
{ int pos, num;					
  if( ctab_count(*S) == ctab_size(*S) - 1 )	/* one less since 0 unused */
    *S = ctab_rehash(*S, 2*ctab_size(*S));
  num = ++ctab_count(*S);
  if( num > MAX_COLOUR )
    Error(42, 2, "too many colours (maximum is %d)",
      FATAL, &fpos(x), MAX_COLOUR);
  hash(pos, string(x), *S);
  if( ctab_name(*S, pos) == nilobj )  New(ctab_name(*S, pos), ACAT);
  Link(ctab_name(*S, pos), x);
  word_colour(x) = num;
  ctab_num(*S, num) = x;
} /* end ctab_insert */

static OBJECT ctab_retrieve(FULL_CHAR *str, COLOUR_TABLE S)
{ OBJECT x, link, y;  int pos;
  hash(pos, str, S);
  x = ctab_name(S, pos);
  if( x == nilobj )  return nilobj;
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link)
      ;
    if( StringEqual(str, string(y)) )  return y;
  }
  return nilobj;
} /* end ctab_retrieve */

#if DEBUG_ON
static void ctab_debug(COLOUR_TABLE S, FILE *fp)
{ int i;  OBJECT x, link, y;
  fprintf(fp, "  table size: %d;  current number of colours: %d%s",
    ctab_size(S), ctab_count(S), STR_NEWLINE);
  for( i = 0;  i < ctab_size(S);  i++ )
  { x = ctab_num(S, i);
    fprintf(fp, "  ctab_num(S, %d) = %s%s", i,
      x == nilobj ? AsciiToFull("<nilobj>") :
      is_word(type(x)) ? string(x) : AsciiToFull("not WORD!"), STR_NEWLINE);
  }
  fprintf(fp, "%s", STR_NEWLINE);
  for( i = 0;  i < ctab_size(S);  i++ )
  { x = ctab_name(S, i);
    fprintf(fp, "ctab_name(S, %d) =", i);
    if( x == nilobj )
      fprintf(fp, " <nilobj>");
    else if( type(x) != ACAT )
      fprintf(fp, " not ACAT!");
    else for( link = Down(x);  link != x;  link = NextDown(link) )
    { Child(y, link)
        ;
      fprintf(fp, " %s",
	is_word(type(y)) ? string(y) : AsciiToFull("not-WORD!"));
    }
    fprintf(fp, "%s", STR_NEWLINE);
  }
} /* end ctab_debug */
#endif


static COLOUR_TABLE col_tab;

/*****************************************************************************/
/*                                                                           */
/*  ColourInit(void)                                                         */
/*                                                                           */
/*  Initialize this module.                                                  */
/*                                                                           */
/*****************************************************************************/

void ColourInit(void)
{ col_tab = ctab_new(INIT_COLOUR_NUM);
} /* end ColourInit */


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN ColChange(OBJECT x, char *keyword, COLOUR_NUM *res)              */
/*                                                                           */
/*  Interpret x as a colour change.  If successful, return TRUE and          */
/*  set *res (or leave it alone if x was nochange).  Else return FALSE.      */
/*                                                                           */
/*****************************************************************************/

static BOOLEAN ColChange(OBJECT x, unsigned char *keyword, COLOUR_NUM *res)
{ OBJECT cname;
  debug2(DCO, D, "ColChange(%s, %s)", EchoObject(x), keyword);

  /* if argument is not a word, fail and exit */
  if( !is_word(type(x)) )
  { Error(42, 3, "%s ignored (illegal left parameter)", WARN, &fpos(x),
      keyword);
    debug0(DCO, D, "ColChange returning (colour unchanged)");
    return FALSE;
  }

  /* if argument is nochange, do nothing */
  if( StringEqual(string(x), STR_COLOUR_NOCHANGE) ||
      StringEqual(string(x), STR_EMPTY) )
  { debug0(DCO, D, "ColourChange returning (colour nochange)");
    return TRUE;
  }

  /* retrieve colour command if present, else insert it */
  cname = ctab_retrieve(string(x), col_tab);
  if( cname == nilobj )
  { cname = MakeWord(type(x), string(x), &fpos(x));
    ctab_insert(cname, &col_tab);
    *res = word_colour(cname);
  }
  else *res = word_colour(cname);

  debug1(DCO, D, "ColChange returning (colour = %s)", string(cname));
  ifdebug(DCO, DD, ctab_debug(col_tab, stderr));
  return TRUE;
}


/*****************************************************************************/
/*                                                                           */
/*  ColourChange(style, x)                                                   */
/*                                                                           */
/*  Change the current style to contain the colour of colour command x.      */
/*  Set the underline colour at the same time.                               */
/*                                                                           */
/*****************************************************************************/

void ColourChange(STYLE *style, OBJECT x)
{
  if( ColChange(x, KW_COLOUR, &colour(*style)) )
    underline_colour(*style) = colour(*style);
}

/* *** old version
void ColourChange(STYLE *style, OBJECT x)
{ OBJECT cname;
  debug2(DCO, D, "ColourChange(%s, %s)", EchoStyle(style), EchoObject(x));

  ** if argument is not a word, fail and exit **
  if( !is_word(type(x)) )
  { Error(42, 3, "%s ignored (illegal left parameter)", WARN, &fpos(x),
      KW_COLOUR);
    debug0(DCO, D, "ColourChange returning (colour unchanged)");
    return;
  }

  ** if argument is nochange, do nothing **
  if( StringEqual(string(x), STR_COLOUR_NOCHANGE) ||
      StringEqual(string(x), STR_EMPTY) )
  { debug0(DCO, D, "ColourChange returning (colour nochange)");
    return;
  }

  ** retrieve colour command if present, else insert it **
  { cname = ctab_retrieve(string(x), col_tab);
    if( cname == nilobj )
    { cname = MakeWord(type(x), string(x), &fpos(x));
      ctab_insert(cname, &col_tab);
      colour(*style) = word_colour(cname);
    }
    else colour(*style) = word_colour(cname);
  }

  debug1(DCO, D, "ColourChange returning (colour = %s)", string(cname));
  ifdebug(DCO, DD, ctab_debug(col_tab, stderr));
}
*** */


/*****************************************************************************/
/*                                                                           */
/*  void ColourUnderlineChange(STYLE *style, OBJECT x)                       */
/*                                                                           */
/*  Change the underline colour of *style as indicated by x.                 */
/*                                                                           */
/*****************************************************************************/

void ColourUnderlineChange(STYLE *style, OBJECT x)
{
  ColChange(x, KW_UNDERLINE_COLOUR, &underline_colour(*style));
}


/*@::ColourCommand()@*********************************************************/
/*                                                                           */
/*  FULL_CHAR *ColourCommand(cnum)                                           */
/*                                                                           */
/*  Return the PostScript command for producing colour cnum.                 */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *ColourCommand(COLOUR_NUM cnum)
{ FULL_CHAR *res;
  debug1(DCO, D, "ColourCommand(%d)", cnum);
  assert( cnum > 0 && cnum <= ctab_count(col_tab), "ColourCommand: number" );

  res = string(ctab_num(col_tab, cnum));

  debug1(DCO, D, "ColourCommand returning %s", res);
  return res;
} /* end ColourCommand */
