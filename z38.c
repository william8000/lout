/*@z38.c:Encoding Vectors:Declarations@***************************************/
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
/*  FILE:         z38.c                                                      */
/*  MODULE:       Encoding Vectors                                           */
/*  EXTERNS:      EvLoad(), EvRetrieve(), EvName(), EvPrintEncodings(),      */
/*                EvPrintResources()                                         */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define MAX_EV		 20		/* max number of encoding vectors    */
#define MAX_CHAR	256		/* max chars represented in one char */
#define	MAX_HASH	353		/* size of hash table                */

typedef struct evec {
  OBJECT	file_name;		/* name of file containing the vec   */
  FILE_NUM	fnum;			/* the file number of this file      */
  BOOLEAN	must_print;		/* TRUE if this vec must be printed  */
  OBJECT	name;			/* PostScript name of encoding vec   */
  OBJECT	vector[MAX_CHAR];	/* character names                   */
  FULL_CHAR	hash_table[MAX_HASH];	/* character hash table for inverse  */
} *EVEC;

static	EVEC	ev_table[MAX_EV];	/* the encoding vectors              */
static	int	evtop = 0;		/* first free slot in ev_table[]     */

#define hash(str, pos)							\
{ FULL_CHAR *p = str;							\
  for( pos = 2 * *p++;  *p;  pos += *p++);				\
  pos = pos % MAX_HASH;							\
}

/*@::EvLoad()@****************************************************************/
/*                                                                           */
/*  ENCODING EvLoad(file_name, must_print)                                   */
/*                                                                           */
/*  Declare file_name to be an encoding vector file.  A file may be so       */
/*  declared more than once.  If must_print is true on any declaration,      */
/*  the encoding vector must be down-loaded to the PostScript interpreter.   */
/*                                                                           */
/*****************************************************************************/

ENCODING EvLoad(OBJECT file_name, BOOLEAN must_print)
{ int i;  FILE *fp;  EVEC ev;
  FULL_CHAR buff[30], enc; unsigned int code, pos;
  debug2(DEV, D, "EvLoad(%s, %s)", EchoObject(file_name), bool(must_print));
  enc = 0;
  while( enc < evtop &&
	 !StringEqual(string(ev_table[enc]->file_name), string(file_name)) )
    enc++;
  if( enc < evtop )
  {
    /* encoding vector already loaded, so only need to record must_print */
    ev_table[enc]->must_print = ev_table[enc]->must_print || must_print;
    Dispose(file_name);
  }
  else
  { 
    /* initialize new slot in ev_table[] for a new encoding vector */
    if( evtop++ == MAX_EV )
      Error(38, 1, "too many encoding vectors", FATAL, &fpos(file_name));
    ifdebug(DMA, D, DebugRegisterUsage(MEM_EVECS, 1, sizeof(struct evec)));
    ev_table[enc] = ev = (EVEC) malloc( sizeof(struct evec) );
    if( ev == (EVEC) NULL )
      Error(38, 2, "run out of memory when loading encoding vector",
	FATAL, &fpos(file_name));
    ev->must_print = must_print;
    ev->file_name  = file_name;
    for( i = 0;  i < MAX_CHAR; i++ )  ev->vector[i] = nilobj;
    for( i = 0;  i < MAX_HASH; i++ )  ev->hash_table[i] = 0;

    /* define and open the file */
    ev->fnum = DefineFile(string(file_name), STR_EMPTY, &fpos(file_name),
      ENCODING_FILE, ENCODING_PATH);
    fp = OpenFile(ev->fnum, FALSE, FALSE);
    if( fp == NULL )
      Error(38, 3, "cannot open encoding vector file %s",
	FATAL, PosOfFile(ev->fnum), FileName(ev->fnum));

    /* invent a PostScript name for the encoding vector */
    StringCopy(buff, AsciiToFull("vec"));
    StringCat(buff, StringInt(evtop));
    ev->name = MakeWord(WORD, buff, no_fpos);

    /* read character names and insert (name, pos) pairs into hash table */
    for( code = 0;  fscanf(fp, "%s", buff) == 1;  code++ )
    { if( code >= MAX_CHAR )
	Error(38, 4, "too many character names in encoding vector file %s",
	  FATAL, PosOfFile(ev->fnum), FileName(ev->fnum));
      hash(buff, pos);
      while( (i = ev->hash_table[pos]) != 0 )
      {	if( StringEqual(string(ev->vector[i]), buff) )
	{ ev->vector[code] = ev->vector[i];
	  break;
	}
	pos = (pos + 1) % MAX_HASH;
      }
      if( i == 0 )
      {	ev->vector[code] = MakeWord(WORD, buff, no_fpos);
	ev->hash_table[pos] = (FULL_CHAR) code;
      }
    }
    if( code != MAX_CHAR )
      Error(38, 5, "too few character names in encoding vector file %s",
	FATAL, PosOfFile(ev->fnum), FileName(ev->fnum));
    fclose(fp);
  }
  debug1(DEV, D, "EvLoad returning %d", enc);
  return enc;
} /* end EvLoad */


/*@::EvRetrieve(), EvName(), EvPrintEncodings()@******************************/
/*                                                                           */
/*  FULL_CHAR EvRetrieve(str, enc)                                           */
/*                                                                           */
/*  Returns the character code corresponding to character name str in        */
/*  ENCODING enc, or 0 if not found.                                         */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR EvRetrieve(FULL_CHAR *str, ENCODING enc)
{ unsigned int pos;  FULL_CHAR code;  EVEC ev;
  ev = ev_table[enc];
  hash(str, pos);
  while( (code = ev->hash_table[pos]) != 0 )
  { if( StringEqual(string(ev->vector[code]), str) )  return code;
    pos = (pos + 1) % MAX_HASH;
  }
  return '\0';
} /* end EvRetrieve */


/*****************************************************************************/
/*                                                                           */
/*  FULL_CHAR *EvName(enc)                                                   */
/*                                                                           */
/*  Returns the PostScript name of ENCODING enc                              */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *EvName(ENCODING enc)
{ assert( enc < evtop, "EvName: enc out of range!" );
  return string(ev_table[enc]->name);
} /* end EvName */


/*****************************************************************************/
/*                                                                           */
/*  EvPrintEncodings(fp)                                                     */
/*                                                                           */
/*  Print all encoding vectors in PostScript form on file fp.                */
/*                                                                           */
/*****************************************************************************/

void EvPrintEncodings(FILE *fp)
{ ENCODING enc;  EVEC ev;  int i;
  for( enc = 0;  enc < evtop;  enc++ )  if( ev_table[enc]->must_print )
  { ev = ev_table[enc];
    fprintf(fp, "%%%%BeginResource encoding %s\n", string(ev->name));
    fprintf(fp, "/%s [\n", string(ev->name));
    for( i = 0;  i < MAX_CHAR;  i++ )
      fprintf(fp, "/%s%c", string(ev->vector[i]), (i+1) % 8 != 0 ? ' ' : '\n');
    fprintf(fp, "] def\n");
    fprintf(fp, "%%%%EndResource\n\n");
  }
} /* end EvPrintEncodings */


/*****************************************************************************/
/*                                                                           */
/*  EvPrintResources(fp)                                                     */
/*                                                                           */
/*  Print all encoding vectors in PostScript form on file fp.                */
/*                                                                           */
/*****************************************************************************/

void EvPrintResources(FILE *fp)
{ ENCODING enc;  EVEC ev;
  for( enc = 0;  enc < evtop;  enc++ )  if( ev_table[enc]->must_print )
  { ev = ev_table[enc];
    fprintf(fp, "%%%%+ encoding %s\n", string(ev->name));
  }
} /* end EvPrintResources */

