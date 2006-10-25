/*@z52.c:Texture Service:TextureChange, TextureCommand@***********************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.32)                       */
/*  COPYRIGHT (C) 1991, 2006 Jeffrey H. Kingston                             */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@it.usyd.edu.au)                                */
/*  School of Information Technologies                                       */
/*  The University of Sydney 2006                                            */
/*  AUSTRALIA                                                                */
/*                                                                           */
/*  This program is free software; you can redistribute it and/or modify     */
/*  it under the terms of the GNU General Public License as published by     */
/*  the Free Software Foundation; either Version 2, or (at your option)      */
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
/*  FILE:         z52.c                                                      */
/*  MODULE:       Texture Service                                            */
/*  EXTERNS:      TextureChange(), TextureCommand()                          */
/*                                                                           */
/*****************************************************************************/
#include "externs.h"
#define INIT_TEXTURE_NUM	100


/*****************************************************************************/
/*                                                                           */
/*  TEXTURE_TABLE                                                            */
/*                                                                           */
/*  A symbol table permitting access to texture records by number or name.   */
/*  The table will automatically enlarge to accept any number of entries,    */
/*  but there is an arbitrary limit of 65535 textures imposed so that texture*/
/*  numbers can be stored in 16 bit fields.                                  */
/*                                                                           */
/*     ptab_new(newsize)                 New empty table, newsize capacity   */
/*     ptab_insert(x, &S)                Insert new texture object x into S  */
/*     ptab_retrieve(str, S)             Retrieve texture object of name str */
/*     ptab_num(S, num)                  Retrieve texture object, number num */
/*     ptab_debug(S, fp)                 Debug print of table S to file fp   */
/*                                                                           */
/*****************************************************************************/

typedef struct
{ int pattab_size;				/* size of table             */
  int pattab_count;				/* number of textures held   */
  struct pattab_rec
  {	OBJECT	by_number;			/* texture record by number  */
	OBJECT	by_name_hash;			/* texture record by name    */
  } pattab[1];
} *TEXTURE_TABLE;

#define	ptab_size(S)	(S)->pattab_size
#define	ptab_count(S)	(S)->pattab_count
#define	ptab_num(S, i)	(S)->pattab[i].by_number
#define	ptab_name(S, i)	(S)->pattab[i].by_name_hash

#define hash(pos, str, S)						\
{ FULL_CHAR *p = str;							\
  pos = *p++;								\
  while( *p ) pos += *p++;						\
  pos = pos % ptab_size(S);						\
}

static TEXTURE_TABLE ptab_new(int newsize)
{ TEXTURE_TABLE S;  int i;
  ifdebug(DMA, D, DebugRegisterUsage(MEM_TEXTURE_TAB, 1,
    2*sizeof(int) + newsize * sizeof(struct pattab_rec)));
  S = (TEXTURE_TABLE) malloc(2*sizeof(int) + newsize * sizeof(struct pattab_rec));
  if( S == (TEXTURE_TABLE) NULL )
    Error(42, 1, "ran out of memory when enlarging texture table",
      FATAL, no_fpos);
  ptab_size(S) = newsize;
  ptab_count(S) = 0;
  for( i = 0;  i < newsize;  i++ )
  { ptab_num(S, i) = ptab_name(S, i) = nilobj;
  }
  return S;
} /* end ptab_new */

static void ptab_insert(OBJECT x, TEXTURE_TABLE *S);

static TEXTURE_TABLE ptab_rehash(TEXTURE_TABLE S, int newsize)
{ TEXTURE_TABLE NewS;  int i;
  NewS = ptab_new(newsize);
  for( i = 1;  i <= ptab_count(S);  i++ )
     ptab_insert(ptab_num(S, i), &NewS);
  for( i = 0;  i < ptab_size(S);  i++ )
  { if( ptab_name(S, i) != nilobj )  DisposeObject(ptab_name(S, i));
  }
  ifdebug(DMA, D, DebugRegisterUsage(MEM_TEXTURE_TAB, -1,
    -(2*sizeof(int) + ptab_size(S) * sizeof(struct pattab_rec))));
  free(S);
  return NewS;
} /* end ptab_rehash */

static void ptab_insert(OBJECT x, TEXTURE_TABLE *S)
{ int pos, num;					
  if( ptab_count(*S) == ptab_size(*S) - 1 )	/* one less since 0 unused */
    *S = ptab_rehash(*S, 2*ptab_size(*S));
  num = ++ptab_count(*S);
  if( num > MAX_TEXTURE )
    Error(42, 2, "too many textures (maximum is %d)",
      FATAL, &fpos(x), MAX_TEXTURE);
  hash(pos, string(x), *S);
  if( ptab_name(*S, pos) == nilobj )  New(ptab_name(*S, pos), ACAT);
  Link(ptab_name(*S, pos), x);
  word_texture(x) = num;
  ptab_num(*S, num) = x;
} /* end ptab_insert */

static OBJECT ptab_retrieve(FULL_CHAR *str, TEXTURE_TABLE S)
{ OBJECT x, link, y;  int pos;
  hash(pos, str, S);
  x = ptab_name(S, pos);
  if( x == nilobj )  return nilobj;
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link);
    if( StringEqual(str, string(y)) )  return y;
  }
  return nilobj;
} /* end ptab_retrieve */

#if DEBUG_ON
static void ptab_debug(TEXTURE_TABLE S, FILE *fp)
{ int i;  OBJECT x, link, y;
  fprintf(fp, "  table size: %d;  current number of textures: %d%s",
    ptab_size(S), ptab_count(S), STR_NEWLINE);
  for( i = 0;  i < ptab_size(S);  i++ )
  { x = ptab_num(S, i);
    fprintf(fp, "  ptab_num(S, %d) = %s%s", i,
      x == nilobj ? AsciiToFull("<nilobj>") :
      is_word(type(x)) ? string(x) : AsciiToFull("not WORD!"), STR_NEWLINE);
  }
  fprintf(fp, "%s", STR_NEWLINE);
  for( i = 0;  i < ptab_size(S);  i++ )
  { x = ptab_name(S, i);
    fprintf(fp, "ptab_name(S, %d) =", i);
    if( x == nilobj )
      fprintf(fp, " <nilobj>");
    else if( type(x) != ACAT )
      fprintf(fp, " not ACAT!");
    else for( link = Down(x);  link != x;  link = NextDown(link) )
    { Child(y, link);
      fprintf(fp, " %s",
	is_word(type(y)) ? string(y) : AsciiToFull("not-WORD!"));
    }
    fprintf(fp, "%s", STR_NEWLINE);
  }
} /* end ptab_debug */
#endif


static TEXTURE_TABLE pat_tab;

/*****************************************************************************/
/*                                                                           */
/*  UseTexture                                                               */
/*                                                                           */
/*  When set to FALSE (by z01.c), means to ignore texture changing commands. */
/*                                                                           */
/*****************************************************************************/

BOOLEAN UseTexture = TRUE;


/*****************************************************************************/
/*                                                                           */
/*  TextureInit()                                                            */
/*                                                                           */
/*  Initialize this module.                                                  */
/*                                                                           */
/*****************************************************************************/

void TextureInit(void)
{ OBJECT cname;

  /* initial texture table is empty */
  pat_tab = ptab_new(INIT_TEXTURE_NUM);

  /* insert "LoutTextureSolid" as texture number 1 */
  cname = MakeWord(WORD, AsciiToFull("LoutTextureSolid"), no_fpos);
  ptab_insert(cname, &pat_tab);
} /* end TextureInit */


/*****************************************************************************/
/*                                                                           */
/*  TextureChange(style, x)                                                  */
/*                                                                           */
/*  Change the current style to contain the texture of texture command x.    */
/*                                                                           */
/*****************************************************************************/

void TextureChange(STYLE *style, OBJECT x)
{ OBJECT cname;
  debug2(DTX, D, "TextureChange(%s, %s)", EchoStyle(style), EchoObject(x));

  /* if argument is not a word, fail and exit */
  if( !is_word(type(x)) )
  { Error(42, 3, "%s ignored (illegal left parameter)", WARN, &fpos(x),
      KW_TEXTURE);
    debug0(DTX, D, "TextureChange returning (texture unchanged)");
    return;
  }

  /* if not using textures, do nothing */
  if( !UseTexture )
  { debug0(DTX, D, "TextureChange returning (not UseTexture)");
    return;
  }

  /* if argument is nochange, do nothing */
  if( StringEqual(string(x), STR_TEXTURE_NOCHANGE) ||
      StringEqual(string(x), STR_EMPTY) )
  { debug0(DTX, D, "TextureChange returning (texture nochange)");
    return;
  }

  /* retrieve texture command if present, else insert it */
  { cname = ptab_retrieve(string(x), pat_tab);
    if( cname == nilobj )
    { cname = MakeWord(type(x), string(x), &fpos(x));
      ptab_insert(cname, &pat_tab);
      texture(*style) = word_texture(cname);
    }
    else texture(*style) = word_texture(cname);
  }

  debug2(DTX, D, "TextureChange returning %d (texture = %s)",
   texture(*style), string(cname));
  ifdebug(DTX, DD, ptab_debug(pat_tab, stderr));
} /* TextureChange */


/*@::TextureCommand()@********************************************************/
/*                                                                           */
/*  FULL_CHAR *TextureCommand(pnum)                                          */
/*                                                                           */
/*  Return the PostScript command for producing texture pnum.                */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *TextureCommand(TEXTURE_NUM pnum)
{ FULL_CHAR *res;
  debug1(DTX, D, "TextureCommand(%d)", pnum);
  assert( pnum > 0 && pnum <= ptab_count(pat_tab), "TextureCommand: number" );

  res = string(ptab_num(pat_tab, pnum));

  debug1(DTX, D, "TextureCommand returning %s", res);
  return res;
} /* end TextureCommand */
