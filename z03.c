/*@z03.c:File Service:Declarations, no_fpos@******************************** */
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
/*  FILE:         z03.c                                                      */
/*  MODULE:       File Service                                               */
/*  EXTERNS:      InitFiles(), AddToPath(), DefineFile(), FirstFile(),       */
/*                NextFile(), FileNum(), FileName(), EchoFilePos(),          */
/*                PosOfFile(), OpenFile(), OpenIncGraphicFile()              */
/*                EchoFileFrom()                                             */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define MAX_TYPES	 11			/* number of file types      */
#define MAX_PATHS	  8			/* number of search paths    */
#define	INIT_TAB 	  3			/* initial file table size   */

#define	file_number(x)	word_font(x)		/* file number of file x     */
#define	updated(x)	fwd(x, COL)		/* TRUE when x is updated    */
#define	path(x)		back(x, COL)		/* search path for file x    */


/*****************************************************************************/
/*                                                                           */
/*  FILE_TABLE                                                               */
/*                                                                           */
/*  A symbol table permitting access to file records by number or name.      */
/*  The table will automatically enlarge to accept any number of entries,    */
/*  but there is an arbitrary limit of 65535 files imposed so that file      */
/*  numbers can be stored in 16 bit fields.                                  */
/*                                                                           */
/*     ftab_new(newsize)                 New empty table, newsize capacity   */
/*     ftab_insert(x, &S)                Insert new file object x into S     */
/*     ftab_retrieve(str, S)             Retrieve file object of name str    */
/*     ftab_num(S, num)                  Retrieve file object of number num  */
/*     ftab_debug(S, fp)                 Debug print of table S to file fp   */
/*                                                                           */
/*****************************************************************************/

typedef struct
{ int filetab_size;				/* size of table             */
  int filetab_count;				/* number of files in table  */
  struct filetab_rec
  {	OBJECT	by_number;			/* file record by number     */
	OBJECT	by_name_hash;			/* file record by name hash  */
  } filetab[1];
} *FILE_TABLE;

#define	ftab_size(S)	(S)->filetab_size
#define	ftab_count(S)	(S)->filetab_count
#define	ftab_num(S, i)	(S)->filetab[i].by_number
#define	ftab_name(S, i)	(S)->filetab[i].by_name_hash

#define hash(pos, str, S)						\
{ FULL_CHAR *p = str;							\
  pos = *p++;								\
  while( *p ) pos += *p++;						\
  pos = pos % ftab_size(S);						\
}

static FILE_TABLE ftab_new(newsize)
int newsize;
{ FILE_TABLE S;  int i;
  S = (FILE_TABLE) malloc(2*sizeof(int) + newsize * sizeof(struct filetab_rec));
  if( S == (FILE_TABLE) NULL )
    Error(3, 1, "run out of memory when enlarging file table", FATAL, no_fpos);
  ftab_size(S) = newsize;
  ftab_count(S) = 0;
  for( i = 0;  i < newsize;  i++ )
  { ftab_num(S, i) = ftab_name(S, i) = nil;
  }
  return S;
} /* end ftab_new */

static FILE_TABLE ftab_rehash(S, newsize)
FILE_TABLE S;  int newsize;
{ FILE_TABLE NewS;  int i;
  NewS = ftab_new(newsize);
  for( i = 1;  i <= ftab_count(S);  i++ )
     ftab_insert(ftab_num(S, i), &NewS);
  for( i = 0;  i < ftab_size(S);  i++ )
  { if( ftab_name(S, i) != nil )  DisposeObject(ftab_name(S, i));
  }
  free(S);
  return NewS;
} /* end ftab_rehash */

static ftab_insert(x, S)
OBJECT x;  FILE_TABLE *S;
{ int pos, num;					
  if( ftab_count(*S) == ftab_size(*S) - 1 )	/* one less since 0 unused */
    *S = ftab_rehash(*S, 2*ftab_size(*S));
  num = ++ftab_count(*S);
  if( num > MAX_FILES )
    Error(3, 2, "too many files (maximum is %d)",
      FATAL, &fpos(x), MAX_FILES);
  hash(pos, string(x), *S);
  if( ftab_name(*S, pos) == nil )  ftab_name(*S, pos) = New(ACAT);
  Link(ftab_name(*S, pos), x);
  file_number(x) = num;
  ftab_num(*S, num) = x;
} /* end ftab_insert */

static OBJECT ftab_retrieve(str, S)
FULL_CHAR *str;  FILE_TABLE S;
{ OBJECT x, link, y;  int pos;
  hash(pos, str, S);
  x = ftab_name(S, pos);
  if( x == nil )  return nil;
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link);
    if( StringEqual(str, string(y)) )  return y;
  }
  return nil;
} /* end ftab_retrieve */

#if DEBUG_ON
static ftab_debug(S, fp)
FILE_TABLE S;  FILE *fp;
{ int i;  OBJECT x, link, y;
  fprintf(fp, "  table size: %d;  current number of files: %d\n",
    ftab_size(S), ftab_count(S));
  for( i = 0;  i < ftab_size(S);  i++ )
  { x = ftab_num(S, i);
    fprintf(fp, "  ftab_num(S, %d) = %s\n", i,
      x == nil ? AsciiToFull("<nil>") :
      !is_word(type(x)) ? AsciiToFull("not WORD!") : string(x) );
  }
  fprintf(fp, "\n");
  for( i = 0;  i < ftab_size(S);  i++ )
  { x = ftab_name(S, i);
    fprintf(fp, "  ftab_name(S, %d) =", i);
    if( x == nil )
      fprintf(fp, " <nil>");
    else if( type(x) != ACAT )
      fprintf(fp, " not ACAT!");
    else for( link = Down(x);  link != x;  link = NextDown(link) )
    { Child(y, link);
      fprintf(fp, " %s",
	is_word(type(y)) ? string(y) : AsciiToFull("not-WORD!"));
    }
    fprintf(fp, "\n");
  }
} /* end ftab_debug */

static	char	*file_types[]		/* the type names for debug  */
		= { "source", "include", "incgraphic", "database", "index",
		    "font", "prepend", "hyph", "hyphpacked", "encoding",
		    "filter" };
#endif


static	FILE_TABLE	file_tab;		/* the file table            */
static	OBJECT		file_type[MAX_TYPES];	/* files of each type        */
static	OBJECT		file_path[MAX_PATHS];	/* the search paths          */


/*****************************************************************************/
/*                                                                           */
/*  no_fpos                                                                  */
/*                                                                           */
/*  A null file position value.                                              */
/*                                                                           */
/*****************************************************************************/

static FILE_POS no_file_pos = {0, 0, 0, 0, 0};
FILE_POS *no_fpos = &no_file_pos;


/*@::InitFiles(), AddToPath(), DefineFile()@**********************************/
/*                                                                           */
/*  InitFiles()                                                              */
/*                                                                           */
/*  Initialize this module.                                                  */
/*                                                                           */
/*****************************************************************************/

InitFiles()
{ int i;
  for( i = 0;  i < MAX_TYPES; i++ )  file_type[i]  = New(ACAT);
  for( i = 0;  i < MAX_PATHS; i++ )  file_path[i] = New(ACAT);
  file_tab = ftab_new(INIT_TAB);
} /* end InitFiles */


/*****************************************************************************/
/*                                                                           */
/*  AddToPath(fpath, dirname)                                                */
/*                                                                           */
/*  Add the directory dirname to the end of search path fpath.               */
/*                                                                           */
/*****************************************************************************/

AddToPath(fpath, dirname)
int fpath; FULL_CHAR *dirname;
{ OBJECT x;
  x = MakeWord(WORD, dirname, no_fpos);
  Link(file_path[fpath], x);
} /* end AddToPath */


/*****************************************************************************/
/*                                                                           */
/*  FILE_NUM DefineFile(str, suffix, xfpos, ftype, fpath)                    */
/*                                                                           */
/*  Declare a file whose name is str plus suffix and whose fpos is xfpos.    */
/*  The file type is ftype, and its search path is fpath.                    */
/*                                                                           */
/*****************************************************************************/

FILE_NUM DefineFile(str, suffix, xfpos, ftype, fpath)
FULL_CHAR *str, *suffix; FILE_POS *xfpos;  int ftype, fpath;
{ register FULL_CHAR *p;
  register int i;
  OBJECT fname;
  assert( ftype < MAX_TYPES, "DefineFile: ftype!" );
  debug5(DFS, D, "DefineFile(%s, %s,%s, %s, %d)",
    str, suffix, EchoFilePos(xfpos), file_types[ftype], fpath);
  if( ftype == SOURCE_FILE && (i = StringLength(str)) >= 3 )
  {
    /* check that file name does not end in ".li" or ".ld" */
    if( StringEqual(&str[i-StringLength(DATA_SUFFIX)], DATA_SUFFIX) )
      Error(3, 3, "database file %s where source file expected",
        FATAL, xfpos, str);
    if( StringEqual(&str[i-StringLength(INDEX_SUFFIX)], INDEX_SUFFIX) )
      Error(3, 4, "database index file %s where source file expected",
        FATAL, xfpos, str);
  }
  if( StringLength(str) + StringLength(suffix) >= MAX_WORD )
    Error(3, 5, "file name %s%s is too long", FATAL, no_fpos, str, suffix);
  fname = MakeWordTwo(WORD, str, suffix, xfpos);
  Link(file_type[ftype], fname);
  path(fname) = fpath;
  updated(fname) = FALSE;
  ftab_insert(fname, &file_tab);
  debug1(DFS, D, "DefineFile returning %s", string(fname));
  ifdebug(DFS, DD, ftab_debug(file_tab, stderr));
  return file_number(fname);
} /* end DefineFile */


/*@::FirstFile(), NextFile(), FileNum()@**************************************/
/*                                                                           */
/*  FILE_NUM FirstFile(ftype)                                                */
/*                                                                           */
/*  Returns first file of type ftype, else NO_FILE.                          */
/*                                                                           */
/*****************************************************************************/

FILE_NUM FirstFile(ftype)
int ftype;
{ FILE_NUM i;
  OBJECT link, y;
  debug1(DFS, D, "FirstFile( %s )", file_types[ftype]);
  link = Down(file_type[ftype]);
  if( type(link) == ACAT )  i = NO_FILE;
  else
  { Child(y, link);
    i = file_number(y);
  }
  debug1(DFS, D, "FirstFile returning %s", i==NO_FILE ? STR_NONE : FileName(i));
  return i;
} /* end FirstFile */


/*****************************************************************************/
/*                                                                           */
/*  FILE_NUM NextFile(i)                                                     */
/*                                                                           */
/*  Returns the next file after file i of the type of i, else NO_FILE.       */
/*                                                                           */
/*****************************************************************************/

FILE_NUM NextFile(i)
FILE_NUM i;
{ OBJECT link, y;
  debug1(DFS, D, "NextFile( %s )", FileName(i));
  link = NextDown(Up(ftab_num(file_tab, i)));
  if( type(link) == ACAT )  i = NO_FILE;
  else
  { Child(y, link);
    i = file_number(y);
  }
  debug1(DFS, D, "NextFile returning %s", i==NO_FILE ? STR_NONE : FileName(i));
  return i;
} /* end NextFile */


/*****************************************************************************/
/*                                                                           */
/*  FILE_NUM FileNum(str, suffix)                                            */
/*                                                                           */
/*  Return the number of the file with name str plus suffix, else NO_FILE.   */
/*                                                                           */
/*****************************************************************************/

FILE_NUM FileNum(str, suffix)
FULL_CHAR *str, *suffix;
{ register FULL_CHAR *p;
  register int i;  OBJECT fname;
  FULL_CHAR buff[MAX_BUFF];
  debug2(DFS, D, "FileNum(%s, %s)", str, suffix);
  if( StringLength(str) + StringLength(suffix) >= MAX_BUFF )
    Error(3, 6, "file name %s%s is too long", FATAL, no_fpos, str, suffix);
  StringCopy(buff, str);
  StringCat(buff, suffix);
  fname = ftab_retrieve(buff, file_tab);
  i = fname == nil ? NO_FILE : file_number(fname);
  debug1(DFS, D, "FileNum returning %s",
    i == NO_FILE ? STR_NONE : FileName( (FILE_NUM) i));
  return (FILE_NUM) i;
} /* end FileNum */


/*@::FileName(), EchoFilePos(), PosOfFile()@**********************************/
/*                                                                           */
/*  FULL_CHAR *FileName(fnum)                                                */
/*                                                                           */
/*  Return the string name of this file.  This is as given to DefineFile     */
/*  until OpenFile is called, after which it is the full path name.          */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *FileName(fnum)
FILE_NUM fnum;
{ OBJECT x;
  x = ftab_num(file_tab, fnum);
  assert( x != nil, "FileName: x == nil!" );
  if( Down(x) != x )  Child(x, Down(x));
  return string(x);
} /* end FileName */


/*****************************************************************************/
/*                                                                           */
/*  FULL_CHAR *EchoFilePos(pos)                                              */
/*                                                                           */
/*  Returns a string reporting the value of file position pos.               */
/*                                                                           */
/*****************************************************************************/

static FULL_CHAR buff[2][MAX_BUFF];  static bp = 1;

static append_fpos(pos)
FILE_POS *pos;
{ OBJECT x;
  x = ftab_num(file_tab, file_num(*pos));
  assert( x != nil, "EchoFilePos: file_tab entry is nil!" );
  if( file_num(fpos(x)) > 0 )
  { append_fpos( &fpos(x) );
    if( StringLength(buff[bp]) + 2 >= MAX_BUFF )
      Error(3, 7, "file position %s... is too long to print",
        FATAL, no_fpos, buff[bp]);
    StringCat(buff[bp], STR_SPACE);
    StringCat(buff[bp], AsciiToFull("/"));
  }
  if( StringLength(buff[bp]) + StringLength(string(x)) + 13 >= MAX_BUFF )
    Error(3, 8, "file position %s... is too long to print",
      FATAL, no_fpos, buff[bp]);
  StringCat(buff[bp], STR_SPACE);
  StringCat(buff[bp], STR_QUOTE);
  StringCat(buff[bp], string(x));
  StringCat(buff[bp], STR_QUOTE);
  if( line_num(*pos) != 0 )
  { StringCat(buff[bp], STR_SPACE);
    StringCat(buff[bp], StringInt( (int) line_num(*pos)));
    StringCat(buff[bp], AsciiToFull(","));
    StringCat(buff[bp], StringInt( (int) col_num(*pos)));
  }
} /* end append_fpos */

FULL_CHAR *EchoFilePos(pos)
FILE_POS *pos;
{ bp = (bp + 1) % 2;
  StringCopy(buff[bp], STR_EMPTY);
  if( file_num(*pos) > 0 )  append_fpos(pos);
  return buff[bp];
} /* end EchoFilePos */


/*@::EchoFileSource(), EchoFileLIne(), PosOfFile()@***************************/
/*                                                                           */
/*  FULL_CHAR *EchoFileSource(fnum)                                          */
/*                                                                           */
/*  Returns a string reporting the "file source" information for file fnum.  */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *EchoFileSource(fnum)
FILE_NUM fnum;
{ OBJECT x, nextx;
  bp = (bp + 1) % 2;
  StringCopy(buff[bp], STR_EMPTY);
  if( fnum > 0 )
  { StringCat(buff[bp], STR_SPACE);
    StringCat(buff[bp], AsciiToFull(condcatgets(MsgCat, 3, 9, "file")));
    /* for estrip's benefit: Error(3, 9, "file"); */
    StringCat(buff[bp], STR_SPACE);
    x = ftab_num(file_tab, fnum);
    StringCat(buff[bp], STR_QUOTE);
    StringCat(buff[bp], string(x));
    StringCat(buff[bp], STR_QUOTE);
    if( file_num(fpos(x)) > 0 )
    { StringCat(buff[bp], AsciiToFull(" ("));
      for(;;)
      { nextx = ftab_num(file_tab, file_num(fpos(x)));
	StringCat(buff[bp], AsciiToFull(condcatgets(MsgCat, 3, 10, "from")));
	/* for estrip's benefit: Error(3, 10, "from"); */
	StringCat(buff[bp], STR_SPACE);
        StringCat(buff[bp], STR_QUOTE);
        StringCat(buff[bp], string(nextx));
        StringCat(buff[bp], STR_QUOTE);
	StringCat(buff[bp], STR_SPACE);
        StringCat(buff[bp],  AsciiToFull(condcatgets(MsgCat, 3, 11, "line")));
	/* for estrip's benefit: Error(3, 11, "line"); */
	StringCat(buff[bp], STR_SPACE);
        StringCat(buff[bp], StringInt( (int) line_num(fpos(x))));
	if( file_num(fpos(nextx)) == 0 )  break;
	StringCat(buff[bp], AsciiToFull(", "));
	x = nextx;
      }
      StringCat(buff[bp], AsciiToFull(")"));
    }
  }
  return buff[bp];
} /* end EchoFileSource */


/*****************************************************************************/
/*                                                                           */
/*  FULL_CHAR *EchoFileLine(pos)                                             */
/*                                                                           */
/*  Returns a string reporting the "line source" information for pos.        */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *EchoFileLine(pos)
FILE_POS *pos;
{ bp = (bp + 1) % 2;
  StringCopy(buff[bp], STR_EMPTY);
  if( file_num(*pos) > 0 && line_num(*pos) != 0 )
  { StringCat(buff[bp], StringInt( (int) line_num(*pos)));
    StringCat(buff[bp], AsciiToFull(","));
    StringCat(buff[bp], StringInt( (int) col_num(*pos)));
  }
  return buff[bp];
} /* end EchoFileLIne */


/*****************************************************************************/
/*                                                                           */
/*  FILE_POS *PosOfFile(fnum)                                                */
/*                                                                           */
/*  Returns a pointer to the file position where file fnum was encountered.  */
/*                                                                           */
/*****************************************************************************/

FILE_POS *PosOfFile(fnum)
FILE_NUM fnum;
{ OBJECT  x = ftab_num(file_tab, fnum);
  assert( x != nil, "PosOfFile: file_tab entry is nil!" );
  return &fpos(x);
}

/*@::SearchPath()@************************************************************/
/*                                                                           */
/*  static FILE *SearchPath(str, fpath, check_ld, check_lt, full_name, xfpos)*/
/*                                                                           */
/*  Search the given path for a file whose name is str.  If found, open      */
/*  it; return the resulting FILE *.                                         */
/*                                                                           */
/*  If check_ld is TRUE, it means that the file to be opened is a .li file   */
/*  and OpenFile() is required to check whether the corresponding .ld file   */
/*  is present.  If it is, then the search must stop.                        */
/*                                                                           */
/*  If check_lt is TRUE, it means that the file to be opened is a source     */
/*  file and OpenFile() is required to check for a .lt suffix version if     */
/*  the file does not open.                                                  */
/*                                                                           */
/*  Also return the full path name in object *full_name if different from    */
/*  the existing name, else nil.                                             */
/*                                                                           */
/*****************************************************************************/

static FILE *SearchPath(str, fpath, check_ld, check_lt, full_name, xfpos)
FULL_CHAR *str;  OBJECT fpath;  BOOLEAN check_ld, check_lt;
OBJECT *full_name;  FILE_POS *xfpos;
{ FULL_CHAR buff[MAX_BUFF];  OBJECT link, y;  FILE *fp, *fp2;
  debug4(DFS, DD, "SearchPath(%s, %s, %s, %s, -)", str, EchoObject(fpath),
	bool(check_ld), bool(check_lt));
  *full_name = nil;

  /* if file name is "stdin" just return it */
  if( StringEqual(str, STR_STDIN) )
  { fp = stdin;
    debug0(DFS, DD, "  opened stdin");
  }

  /* else if file name is a full path name, ignore fpath */
  else if( StringBeginsWith(str, AsciiToFull("/")) )
  { fp = StringFOpen(str, "r");
    debug1(DFS, DD, fp==null ? "  failed on %s" : "  succeeded on %s", str);

    /* if check_lt, see if str.lout exists as well as or instead of str */
    if( check_lt )
    { StringCopy(buff, str);
      StringCat(buff, SOURCE_SUFFIX);
      fp2 = StringFOpen(buff, "r");
      debug1(DFS, DD, fp2==null ? "  failed on %s" : "  succeeded on %s",buff);
      if( fp2 != null )
      {	if( fp != null )
	  Error(3, 12, "files %s and %s both exist", FATAL, xfpos, str, buff);
	fp = fp2;
	*full_name = MakeWord(WORD, buff, xfpos);
      }
    }
  }

  /* else prepend each element of fpath to str in turn and attempt to open */
  else
  { fp = null;
    for( link = Down(fpath);  fp==null && link!=fpath; link = NextDown(link) )
    { Child(y, link);

      /* set buff to hold the full path name and attempt to open it */
      if( StringLength(string(y)) == 0 )
      { StringCopy(buff, str);
	fp = StringFOpen(str, "r");
	debug1(DFS,DD, fp==null ? "  failed on %s" : "  succeeded on %s", str);
      }
      else
      {	if( StringLength(string(y)) + 1 + StringLength(str) >= MAX_BUFF )
	  Error(3, 13, "file path name %s/%s is too long",
	    FATAL, &fpos(y), string(y), str);
	StringCopy(buff, string(y));
	StringCat(buff, AsciiToFull("/"));
	StringCat(buff, str);
	fp = StringFOpen(buff, "r");
	debug1(DFS,DD, fp==null ? "  failed on %s" : "  succeeded on %s",buff);
	if( fp != null ) *full_name = MakeWord(WORD, buff, xfpos);
      }

      /* if failed to find .li file, exit if corresponding .ld file is found */
      if( fp == null && check_ld )
      {	StringCopy(&buff[StringLength(buff) - StringLength(INDEX_SUFFIX)],
	  DATA_SUFFIX);
	fp = StringFOpen(buff, "r");
	debug1(DFS,DD,fp==null ? "  failed on %s" : "  succeeded on %s", buff);
	if( fp != null )
	{ fclose(fp);
	  debug0(DFS, D, "SearchPath returning null (adjacent .ld file)");
	  return null;
	}
      }

      /* if check_lt, see if buff.lout exists as well as or instead of buff */
      if( check_lt )
      {	StringCat(buff, SOURCE_SUFFIX);
	fp2 = StringFOpen(buff, "r");
	debug1(DFS,DD,fp2==null ? "  failed on %s" : "  succeeded on %s",buff);
	StringCopy(&buff[StringLength(buff) - StringLength(SOURCE_SUFFIX)],
	  STR_EMPTY);
	if( fp2 != null )
	{ if( fp != null )
	    Error(3, 14, "files %s and %s%s both exist",
	      FATAL, xfpos, buff, buff, SOURCE_SUFFIX);
	  fp = fp2;
	  *full_name = MakeWord(WORD, buff, xfpos);
	}
      }
    }
  }
  debug1(DFS, DD, "SearchPath returning (fp %s null)", fp==null ? "==" : "!=");
  return fp;
} /* end SearchPath */


/*@::OpenFile(), OpenIncGraphicFile()@****************************************/
/*                                                                           */
/*  FILE *OpenFile(fnum, check_ld, check_lt)                                 */
/*                                                                           */
/*  Open for reading the file whose number is fnum.  This involves           */
/*  searching for it along its path if not previously opened.                */
/*                                                                           */
/*  If check_ld is TRUE, it means that the file to be opened is a .li file   */
/*  and OpenFile() is required to check whether the corresponding .ld file   */
/*  is present.  If it is, then the search must stop.                        */
/*                                                                           */
/*  If check_lt is TRUE, it means that the file to be opened is a source     */
/*  file and OpenFile() is required to check for a .lout suffix version      */
/*  if the file does not open without it.                                    */
/*                                                                           */
/*****************************************************************************/

FILE *OpenFile(fnum, check_ld, check_lt)
FILE_NUM fnum;  BOOLEAN check_ld, check_lt;
{ FILE *fp;  OBJECT fname, full_name, y;
  ifdebug(DPP, D, ProfileOn("OpenFile"));
  debug2(DFS, D, "OpenFile(%s, %s)", FileName(fnum), bool(check_ld));
  fname = ftab_num(file_tab, fnum);
  if( Down(fname) != fname )
  { Child(y, Down(fname));
    fp = StringFOpen(string(y), "r");
    debug1(DFS,DD,fp==null ? "  failed on %s" : "  succeeded on %s", string(y));
  }
  else
  { fp = SearchPath(string(fname), file_path[path(fname)],
	   check_ld, check_lt, &full_name, &fpos(fname));
    if( full_name != nil )  Link(fname, full_name);
  }
  ifdebug(DPP, D, ProfileOff("OpenFile"));
  debug1(DFS, D, "OpenFile returning (fp %s null)", fp==null ? "==" : "!=");
  return fp;
} /* end OpenFile */


/*****************************************************************************/
/*                                                                           */
/*  FILE *OpenIncGraphicFile(str, typ, full_name, xfpos)                     */
/*                                                                           */
/*  Open for reading the @IncludeGraphic file str; typ is INCGRAPHIC or      */
/*  SINCGRAPHIC.  Return the full name in full_name.                         */
/*                                                                           */
/*****************************************************************************/

FILE *OpenIncGraphicFile(str, typ, full_name, xfpos)
FULL_CHAR *str;  unsigned char typ;  OBJECT *full_name;  FILE_POS *xfpos;
{ FILE *fp;  int p;
  debug2(DFS, D, "OpenIncGraphicFile(%s, %s, -)", str, Image(typ));
  assert( typ == INCGRAPHIC || typ == SINCGRAPHIC, "OpenIncGraphicFile!" );
  p = (typ == INCGRAPHIC ? INCLUDE_PATH : SYSINCLUDE_PATH);
  fp = SearchPath(str, file_path[p], FALSE, FALSE, full_name, xfpos);
  if( *full_name == nil )  *full_name = MakeWord(WORD, str, xfpos);
  debug2(DFS, D, "OpenIncGraphicFile returning (fp %s null, *full_name = %s)",
    fp==null ? "==" : "!=", string(*full_name));
  return fp;
} /* end OpenIncGraphicFile */


/*****************************************************************************/
/*                                                                           */
/*  FileSetUpdated(fnum)                                                     */
/*                                                                           */
/*  Declare that file fnum has been updated.                                 */
/*                                                                           */
/*****************************************************************************/

FileSetUpdated(fnum)
FILE_NUM fnum;
{ updated(ftab_num(file_tab, fnum)) = TRUE;
} /* end FileSetUpdated */


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN FileTestUpdated(fnum)                                            */
/*                                                                           */
/*  Test whether file fnum has been declared to be updated.                  */
/*                                                                           */
/*****************************************************************************/

BOOLEAN FileTestUpdated(fnum)
FILE_NUM fnum;
{ return (BOOLEAN) updated(ftab_num(file_tab, fnum));
} /* end FileTestUpdated */
