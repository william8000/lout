/*@z03.c:File Service:Declarations, no_fpos@******************************** */
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
/*  FILE:         z03.c                                                      */
/*  MODULE:       File Service                                               */
/*  EXTERNS:      InitFiles(), AddToPath(), DefineFile(), FirstFile(),       */
/*                NextFile(), FileNum(), FileName(), EchoFilePos(),          */
/*                PosOfFile(), OpenFile(), OpenIncGraphicFile()              */
/*                EchoFileFrom()                                             */
/*                                                                           */
/*****************************************************************************/
#include "externs.h"
#if USE_STAT
#include <sys/types.h>
#include <sys/stat.h>
#endif

#define	INIT_TAB 	  3			/* initial file table size   */


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

static FILE_TABLE ftab_new(int newsize)
{ FILE_TABLE S;  int i;
  ifdebug(DMA, D, DebugRegisterUsage(MEM_FILES, 1,
    2*sizeof(int) + newsize * sizeof(struct filetab_rec)));
  S = (FILE_TABLE) malloc(2*sizeof(int) + newsize * sizeof(struct filetab_rec));
  if( S == (FILE_TABLE) NULL )
    Error(3, 1, "run out of memory when enlarging file table", FATAL, no_fpos);
  ftab_size(S) = newsize;
  ftab_count(S) = 0;
  for( i = 0;  i < newsize;  i++ )
  { ftab_num(S, i) = ftab_name(S, i) = nilobj;
  }
  return S;
} /* end ftab_new */

static void ftab_insert(OBJECT x, FILE_TABLE *S);

static FILE_TABLE ftab_rehash(FILE_TABLE S, int newsize)
{ FILE_TABLE NewS;  int i;
  NewS = ftab_new(newsize);
  for( i = 1;  i <= ftab_count(S);  i++ )
     ftab_insert(ftab_num(S, i), &NewS);
  for( i = 0;  i < ftab_size(S);  i++ )
  { if( ftab_name(S, i) != nilobj )  DisposeObject(ftab_name(S, i));
  }
  ifdebug(DMA, D, DebugRegisterUsage(MEM_FILES, -1,
    -(2*sizeof(int) + ftab_size(S) * sizeof(struct filetab_rec))));
  free(S);
  return NewS;
} /* end ftab_rehash */

static void ftab_insert(OBJECT x, FILE_TABLE *S)
{ int pos, num;					
  if( ftab_count(*S) == ftab_size(*S) - 1 )	/* one less since 0 unused */
    *S = ftab_rehash(*S, 2*ftab_size(*S));
  num = ++ftab_count(*S);
  if( num > MAX_FILES )
    Error(3, 2, "too many files (maximum is %d)",
      FATAL, &fpos(x), MAX_FILES);
  hash(pos, string(x), *S);
  if( ftab_name(*S, pos) == nilobj )  New(ftab_name(*S, pos), ACAT);
  Link(ftab_name(*S, pos), x);
  file_number(x) = num;
  ftab_num(*S, num) = x;
} /* end ftab_insert */

static OBJECT ftab_retrieve(FULL_CHAR *str, FILE_TABLE S)
{ OBJECT x, link, y;  int pos;
  hash(pos, str, S);
  x = ftab_name(S, pos);
  if( x == nilobj )  return nilobj;
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link)
      ;
    if( StringEqual(str, string(y)) )  return y;
  }
  return nilobj;
} /* end ftab_retrieve */

#if DEBUG_ON
static void ftab_debug(FILE_TABLE S)
{ int i;  OBJECT x, link, y;
  FULL_CHAR buff[MAX_BUFF];
  debug2(DFS, D, "  table size: %d;  current number of files: %d",
    ftab_size(S), ftab_count(S));
  for( i = 0;  i < ftab_size(S);  i++ )
  { x = ftab_num(S, i);
    debug2(DFS, D, "  ftab_num(S, %d) = %s", i,
      x == nilobj ? AsciiToFull("<nilobj>") :
      !is_word(type(x)) ? AsciiToFull("not WORD!") : string(x) );
  }
  debug0(DFS, D, "");
  for( i = 0;  i < ftab_size(S);  i++ )
  { x = ftab_name(S, i);
    if( x == nilobj )
    {
      debug1(DFS, D, "  ftab_name(S, %d) = <nilobj>", i);
    }
    else if( type(x) != ACAT )
    {
      debug1(DFS, D, "  ftab_name(S, %d) = not ACAT!>", i);
    }
    else
    {
      StringCopy(buff, STR_EMPTY);
      for( link = Down(x);  link != x;  link = NextDown(link) )
      { Child(y, link)
          ;
	StringCat(buff, STR_SPACE);
	StringCat(buff, is_word(type(y)) ? string(y):AsciiToFull("not-WORD!"));
      }
      debug2(DFS, D, "  ftab_name(S, %d) =%s", i, buff);
    }
  }
} /* end ftab_debug */

static	char	*file_types[]		/* the type names for debug  */
		= { "source", "include", "incgraphic", "database", "index",
		    "font", "prepend", "hyph", "hyphpacked",
		    "mapping", "filter" };
#endif


static	OBJECT		empty_path;		/* file path with just "" in */
static	FILE_TABLE	file_tab;		/* the file table            */
static	OBJECT		file_type[MAX_TYPES];	/* files of each type        */
static	OBJECT		file_path[MAX_PATHS];	/* the search paths          */
/* ***don't need these any more
static	char		*file_mode[MAX_TYPES] =
{ READ_TEXT, READ_TEXT, READ_TEXT, READ_BINARY, READ_TEXT,
  READ_TEXT, READ_TEXT, READ_BINARY, READ_TEXT, READ_TEXT };
*** */


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

void InitFiles(void)
{ int i;  OBJECT tmp;
  for( i = 0;  i < MAX_TYPES; i++ )  New(file_type[i], ACAT);
  for( i = 0;  i < MAX_PATHS; i++ )  New(file_path[i], ACAT);
  file_tab = ftab_new(INIT_TAB);
  New(empty_path, ACAT);
  tmp = MakeWord(WORD, STR_EMPTY, no_fpos);
  Link(empty_path, tmp);
} /* end InitFiles */


/*****************************************************************************/
/*                                                                           */
/*  AddToPath(fpath, dirname)                                                */
/*                                                                           */
/*  Add the directory dirname to the end of search path fpath.               */
/*                                                                           */
/*****************************************************************************/

void AddToPath(int fpath, OBJECT dirname)
{ Link(file_path[fpath], dirname);
} /* end AddToPath */


/*****************************************************************************/
/*                                                                           */
/*  FILE_NUM DefineFile(str, suffix, xfpos, ftype, fpath)                    */
/*                                                                           */
/*  Declare a file whose name is str plus suffix and whose fpos is xfpos.    */
/*  The file type is ftype, and its search path is fpath.                    */
/*                                                                           */
/*  If ignore_dup is TRUE, any repeated definition of the same file with     */
/*  the same type will be ignored.  The result in that case will be the      */
/*                                                                           */
/*****************************************************************************/

FILE_NUM DefineFile(FULL_CHAR *str, FULL_CHAR *suffix,
FILE_POS *xfpos, int ftype, int fpath)
{ register int i;
  OBJECT fname;
  assert( ftype < MAX_TYPES, "DefineFile: ftype!" );
  debug5(DFS, DD, "DefineFile(%s, %s,%s, %s, %d)",
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
  line_count(fname) = 0;
  type_of_file(fname) = ftype;
  used_suffix(fname) = FALSE;
  ftab_insert(fname, &file_tab);
  debug1(DFS, DD, "DefineFile returning %s", string(fname));
  ifdebug(DFS, DD, ftab_debug(file_tab));
  return file_number(fname);
} /* end DefineFile */


/*@::FirstFile(), NextFile(), FileNum()@**************************************/
/*                                                                           */
/*  FILE_NUM FirstFile(ftype)                                                */
/*                                                                           */
/*  Returns first file of type ftype, else NO_FILE.                          */
/*                                                                           */
/*****************************************************************************/

FILE_NUM FirstFile(int ftype)
{ FILE_NUM i;
  OBJECT link, y;
  debug1(DFS, DD, "FirstFile( %s )", file_types[ftype]);
  link = Down(file_type[ftype]);
  if( type(link) == ACAT )  i = NO_FILE;
  else
  { Child(y, link)
      ;
    i = file_number(y);
  }
  debug1(DFS, DD, "FirstFile returning %s", i==NO_FILE ? STR_NONE : FileName(i));
  return i;
} /* end FirstFile */


/*****************************************************************************/
/*                                                                           */
/*  FILE_NUM NextFile(i)                                                     */
/*                                                                           */
/*  Returns the next file after file i of the type of i, else NO_FILE.       */
/*                                                                           */
/*****************************************************************************/

FILE_NUM NextFile(FILE_NUM i)
{ OBJECT link, y;
  debug1(DFS, DD, "NextFile( %s )", FileName(i));
  link = NextDown(Up(ftab_num(file_tab, i)));
  if( type(link) == ACAT )  i = NO_FILE;
  else
  { Child(y, link)
      ;
    i = file_number(y);
  }
  debug1(DFS, DD, "NextFile returning %s", i==NO_FILE ? STR_NONE : FileName(i));
  return i;
} /* end NextFile */


/*****************************************************************************/
/*                                                                           */
/*  FILE_NUM FileNum(str, suffix)                                            */
/*                                                                           */
/*  Return the number of the file with name str plus suffix, else NO_FILE.   */
/*                                                                           */
/*****************************************************************************/

FILE_NUM FileNum(FULL_CHAR *str, FULL_CHAR *suffix)
{ register int i;  OBJECT fname;
  FULL_CHAR buff[MAX_BUFF];
  debug2(DFS, DD, "FileNum(%s, %s)", str, suffix);
  if( StringLength(str) + StringLength(suffix) >= MAX_BUFF )
    Error(3, 6, "file name %s%s is too long", FATAL, no_fpos, str, suffix);
  StringCopy(buff, str);
  StringCat(buff, suffix);
  fname = ftab_retrieve(buff, file_tab);
  i = fname == nilobj ? NO_FILE : file_number(fname);
  debug1(DFS, DD, "FileNum returning %s",
    i == NO_FILE ? STR_NONE : FileName( (FILE_NUM) i));
  return (FILE_NUM) i;
} /* end FileNum */


/*****************************************************************************/
/*                                                                           */
/*  FILE_NUM DatabaseFileNum(FILE_POS *xfpos)                                */
/*                                                                           */
/*  Return a suitable database file number for writing something out whose   */
/*  file position is xfpos.                                                  */
/*                                                                           */
/*****************************************************************************/
 
FILE_NUM DatabaseFileNum(FILE_POS *xfpos)
{ OBJECT x;
  FILE_NUM fnum;  FULL_CHAR *str;
  debug2(DFS, D, "DatabaseFileNum(%s %s)", EchoFilePos(xfpos),
    EchoFileSource(file_num(*xfpos)));
  x = ftab_num(file_tab, file_num(*xfpos));
  switch( type_of_file(x) )
  {
    case SOURCE_FILE:
    case INCLUDE_FILE:

      /* return the corresponding database file (may need to be defined) */
      str = FileName(file_num(*xfpos));
      fnum = FileNum(str, DATA_SUFFIX);
      if( fnum == NO_FILE )
      { debug0(DFS, DD, "  calling DefineFile from DatabaseFileNum");
	fnum = DefineFile(str, DATA_SUFFIX, xfpos, DATABASE_FILE, SOURCE_PATH);
      }
      break;


    case DATABASE_FILE:

      /* return the enclosing source file (recursively if necessary) */
      if( file_num(fpos(x)) == NO_FILE )
      {
	/* xfpos lies in a cross-reference database file; use itself */
	/* ***
	Error(3, 18, "DatabaseFileNum: database file position unknown",
	  INTERN, no_fpos);
	*** */
	fnum = file_num(*xfpos);
      }
      else
      {
	/* xfpos lies in a user-defined database file; use its source */
        fnum = DatabaseFileNum(&fpos(x));
      }
      break;


    case FILTER_FILE:

      /* return the enclosing source file (recursively if necessary) */
      if( file_num(fpos(x)) == NO_FILE )
	Error(3, 7, "DatabaseFileNum: filter file position unknown",
	  INTERN, no_fpos);
      fnum = DatabaseFileNum(&fpos(x));
      break;


    default:

      Error(3, 8, "DatabaseFileNum: unexpected file type", INTERN, no_fpos);
      fnum = NO_FILE;
      break;

  }
  debug2(DFS, D, "DatabaseFileNum returning %d (%s)", fnum,
    fnum == NO_FILE ? AsciiToFull("NO_FILE") : FileName(fnum));
  return fnum;
} /* end DatabaseFileNum */
 
 
/*@::FileName(), EchoFilePos(), PosOfFile()@**********************************/
/*                                                                           */
/*  FULL_CHAR *FileName(fnum)                                                */
/*  FULL_CHAR *FullFileName(fnum)                                            */
/*                                                                           */
/*  Return the string name of this file.  This is as given to DefineFile     */
/*  until OpenFile is called, after which it is the full path name.          */
/*                                                                           */
/*  FullFileName is the same except it will add a .lt to the file name       */
/*  if that was needed when the file was opened for reading.                 */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *FileName(FILE_NUM fnum)
{ OBJECT x;
  x = ftab_num(file_tab, fnum);
  assert( x != nilobj, "FileName: x == nilobj!" );
  if( Down(x) != x )
    Child(x, Down(x))
      ;
  return string(x);
} /* end FileName */


FULL_CHAR *FullFileName(FILE_NUM fnum)
{ OBJECT x;
  static FULL_CHAR ffbuff[2][MAX_BUFF];
  static int ffbp = 1;

  x = ftab_num(file_tab, fnum);
  assert( x != nilobj, "FileName: x == nilobj!" );
  if( used_suffix(x) )
  {
    if( Down(x) != x )
      Child(x, Down(x))
        ;
    ffbp = (ffbp + 1) % 2;
    StringCopy(ffbuff[ffbp], string(x));
    StringCat(ffbuff[ffbp], SOURCE_SUFFIX);
    return ffbuff[ffbp];
  }
  else
  {
    if( Down(x) != x )
      Child(x, Down(x))
        ;
    return string(x);
  }
} /* end FullFileName */


/*****************************************************************************/
/*                                                                           */
/*  int FileType(FILE_NUM fnum)                                              */
/*                                                                           */
/*  Return the type of file fnum.                                            */
/*                                                                           */
/*****************************************************************************/

int FileType(FILE_NUM fnum)
{ OBJECT x;
  x = ftab_num(file_tab, fnum);
  assert( x != nilobj, "FileType: x == nilobj!" );
  if( Down(x) != x )
    Child(x, Down(x))
      ;
  return type_of_file(x);
} /* end FileType */


/*****************************************************************************/
/*                                                                           */
/*  FULL_CHAR *EchoFilePos(pos)                                              */
/*                                                                           */
/*  Returns a string reporting the value of file position pos.               */
/*                                                                           */
/*****************************************************************************/

static FULL_CHAR buff[2][MAX_BUFF];  static int bp = 1;

static void append_fpos(FILE_POS *pos)
{ OBJECT x;
  x = ftab_num(file_tab, file_num(*pos));
  assert( x != nilobj, "EchoFilePos: file_tab entry is nilobj!" );
  if( file_num(fpos(x)) > 0 )
  { append_fpos( &fpos(x) );
    if( StringLength(buff[bp]) + 2 >= MAX_BUFF )
      Error(3, 9, "file position %s... is too long to print",
        FATAL, no_fpos, buff[bp]);
    StringCat(buff[bp], STR_SPACE);
    StringCat(buff[bp], STR_DIR);
  }
  if( StringLength(buff[bp]) + StringLength(string(x)) + 13 >= MAX_BUFF )
    Error(3, 10, "file position %s... is too long to print",
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

FULL_CHAR *EchoFilePos(FILE_POS *pos)
{ bp = (bp + 1) % 2;
  StringCopy(buff[bp], STR_EMPTY);
  if( file_num(*pos) > 0 )  append_fpos(pos);
  return buff[bp];
} /* end EchoFilePos */

FULL_CHAR *EchoAltFilePos(FILE_POS *pos)
{
  bp = (bp + 1) % 2;
  StringCopy(buff[bp], STR_EMPTY);
  if( file_num(*pos) > 0 )
  {
    /* *** x = ftab_num(file_tab, file_num(*pos)); *** */
    StringCat(buff[bp], FullFileName(file_num(*pos)));
    if( line_num(*pos) != 0 )
    { StringCat(buff[bp], AsciiToFull(":"));
      StringCat(buff[bp], StringInt( (int) line_num(*pos)));
      StringCat(buff[bp], AsciiToFull(":"));
      StringCat(buff[bp], StringInt( (int) col_num(*pos)));
    }
  }
  return buff[bp];
} /* end EchoFilePos */


/*@::EchoFileSource(), EchoFileLine(), PosOfFile()@***************************/
/*                                                                           */
/*  FULL_CHAR *EchoFileSource(fnum)                                          */
/*                                                                           */
/*  Returns a string reporting the "file source" information for file fnum.  */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *EchoFileSource(FILE_NUM fnum)
{ OBJECT x, nextx;
  bp = (bp + 1) % 2;
  StringCopy(buff[bp], STR_EMPTY);
  if( fnum > 0 )
  { StringCat(buff[bp], STR_SPACE);
    x = ftab_num(file_tab, fnum);
    assert( x != nilobj, "EchoFileSource: x == nilobj!" );
    if( type_of_file(x) == FILTER_FILE )
    { StringCat(buff[bp], AsciiToFull(condcatgets(MsgCat, 3, 11, "filter")));
      /* for estrip's benefit: Error(3, 11, "filter"); */
      StringCat(buff[bp], STR_SPACE);
    }
    StringCat(buff[bp], AsciiToFull(condcatgets(MsgCat, 3, 12, "file")));
    /* for estrip's benefit: Error(3, 12, "file"); */
    StringCat(buff[bp], STR_SPACE);
    /* *** x = ftab_num(file_tab, fnum); *** */
    StringCat(buff[bp], STR_QUOTE);
    StringCat(buff[bp], FullFileName(fnum));
    StringCat(buff[bp], STR_QUOTE);
    if( file_num(fpos(x)) > 0 )
    { StringCat(buff[bp], AsciiToFull(" ("));
      for(;;)
      { nextx = ftab_num(file_tab, file_num(fpos(x)));
	StringCat(buff[bp], AsciiToFull(condcatgets(MsgCat, 3, 13, "from")));
	/* for estrip's benefit: Error(3, 13, "from"); */
	StringCat(buff[bp], STR_SPACE);
        StringCat(buff[bp], STR_QUOTE);
        StringCat(buff[bp], string(nextx));
        StringCat(buff[bp], STR_QUOTE);
	StringCat(buff[bp], STR_SPACE);
        StringCat(buff[bp],  AsciiToFull(condcatgets(MsgCat, 3, 14, "line")));
	/* for estrip's benefit: Error(3, 14, "line"); */
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

FULL_CHAR *EchoFileLine(FILE_POS *pos)
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

FILE_POS *PosOfFile(FILE_NUM fnum)
{ OBJECT  x = ftab_num(file_tab, fnum);
  assert( x != nilobj, "PosOfFile: file_tab entry is nilobj!" );
  return &fpos(x);
}

/*@::SearchPath()@************************************************************/
/*                                                                           */
/*  static FILE *SearchPath(str, fpath, check_ld, check_lt, full_name, xfpos,*/
/*                          read_mode)                                       */
/*                                                                           */
/*  Search the given path for a file whose name is str.  If found, open      */
/*  it with mode read_mode; return the resulting FILE *.                     */
/*                                                                           */
/*  If check_ld is TRUE, it means that the file to be opened is a .li file   */
/*  and OpenFile() is required to check whether the corresponding .ld file   */
/*  is present.  If it is, then the search must stop.  Furthermore, if the   */
/*  .li file is out of date wrt the .ld file, it is to be removed.           */
/*                                                                           */
/*  If check_lt is TRUE, it means that the file to be opened is a source     */
/*  file and OpenFile() is required to check for a .lt suffix version.       */
/*                                                                           */
/*  Also return the full path name in object *full_name if different from    */
/*  the existing name, else nilobj.                                          */
/*                                                                           */
/*  Set *used_source_suffix to TRUE if the .lt source suffix had to be       */
/*  added in order to find the file.                                         */
/*                                                                           */
/*****************************************************************************/

static FILE *SearchPath(FULL_CHAR *str, OBJECT fpath, BOOLEAN check_ld,
BOOLEAN check_lt, OBJECT *full_name, FILE_POS *xfpos, char *read_mode,
BOOLEAN *used_source_suffix)
{ FULL_CHAR buff[MAX_BUFF], buff2[MAX_BUFF];
  OBJECT link, y = nilobj, cpath;  FILE *fp, *fp2;
  debug4(DFS, DD, "[ SearchPath(%s, %s, %s, %s, -)", str, EchoObject(fpath),
	bool_str(check_ld), bool_str(check_lt));

  *used_source_suffix = FALSE;

  /* if file name is "stdin" just return it */
  if( StringEqual(str, STR_STDIN) )
  {
    debug0(DFS, DD, "] SearchPath returning stdin");
    *full_name = nilobj;
    return stdin;
  }

  /* use fpath if relative file name, use empty_path if absolute filename */
  cpath = StringBeginsWith(str, STR_DIR) ? empty_path : fpath;

  /* try opening each path name in the search path */
  fp = null;
  for( link = Down(cpath);  fp == null && link != cpath;  link = NextDown(link) )
  { Child(y, link)
      ;

    /* set buff to the full path name */
    if( StringLength(string(y)) == 0 )
    { StringCopy(buff, str);
    }
    else
    { if( StringLength(string(y)) + StringLength(STR_DIR) +
	  StringLength(str) >= MAX_BUFF )
	Error(3, 15, "file path name %s%s%s is too long",
	  FATAL, &fpos(y), string(y), STR_DIR, str);
      StringCopy(buff, string(y));
      StringCat(buff, STR_DIR);
      StringCat(buff, str);
    }

    /* try opening the full path name */
    fp = StringFOpen(buff, read_mode);
    debug1(DFS, DD, fp == null ? "  fail %s" : "  succeed %s", buff);

    /* if failed to find .li file, exit if corresponding .ld file */
    if( check_ld && fp == null )
    {
        StringCopy(buff2, buff);
        StringCopy(&buff2[StringLength(buff2) - StringLength(INDEX_SUFFIX)],
	  DATA_SUFFIX);
        fp2 = StringFOpen(buff2, READ_FILE);
        debug1(DFS, DD, fp2 == null ? "  fail %s" : "  succeed %s", buff2);
        if( fp2 != null )
        { fclose(fp2);
	  debug0(DFS, DD, "] SearchPath returning null (adjacent .ld file)");
	  *full_name = nilobj;
	  return null;
        }
    }

#if USE_STAT
    /*****************************************************************/
    /*                                                               */
    /*  If your compiler won't compile this bit, it is probably      */
    /*  because you either don't have the stat() system call on      */
    /*  your system (it is not ANSI C), or because it can't be       */
    /*  found in the header files declared at the top of this file.  */
    /*                                                               */
    /*  The simple correct thing to do is to set the USESTAT macro   */
    /*  in the makefile to 0.  You won't lose much.                  */
    /*                                                               */
    /*****************************************************************/

    /* if found .li file, compare dates with corresponding .ld file  */
    if( check_ld && fp != null )
    {
      struct stat indexstat, datastat;
      StringCopy(buff2, buff);
      StringCopy(&buff2[StringLength(buff2) - StringLength(INDEX_SUFFIX)],
	DATA_SUFFIX);
      debug2(DFS, DD, "SearchPath comparing dates of .li %s and .ld %s",
	buff, buff2);
      if( stat( (char *) buff, &indexstat) == 0 &&
	  stat( (char *) buff2, &datastat) == 0 )
      {
	debug2(DFS, DD, "SearchPath mtimes are .li %d and .ld %d",
	  (int) indexstat.st_mtime, (int) datastat.st_mtime);
	if( datastat.st_mtime > indexstat.st_mtime )
	{ fclose(fp);
	  debug1(DFS, DD, "SearchPath calling StringRemove(%s)", buff);
	  StringRemove(buff);
	  debug0(DFS, DD, "] SearchPath returning null (.li out of date)");
	  *full_name = nilobj;
	  return null;
	}
      }
    }
#endif

    /* if check_lt, see if buff.lt exists as well as or instead of buff */
    if( check_lt )
    {
      StringCopy(buff2, buff);
      StringCat(buff2, SOURCE_SUFFIX);
      fp2 = StringFOpen(buff2, READ_FILE);
      debug1(DFS, DD, fp2 == null ? "  fail %s" : "  succeed %s", buff2);
      if( fp2 != null )
      { if( fp != null )
	  Error(3, 16, "files %s and %s both exist", FATAL, xfpos,buff,buff2);
	fp = fp2;
	*used_source_suffix = TRUE;
      }
    }

  }
  debug1(DFS, DD, "] SearchPath returning (fp %s null)", fp==null ? "==" : "!=");
  *full_name = (fp == null || StringLength(string(y)) == 0) ? nilobj :
    MakeWord(WORD, buff, xfpos);
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
/*  is present.  If it is, then the search must stop.  Furthermore, if the   */
/*  .li file is out of date wrt the .ld file, it is to be removed.           */
/*                                                                           */
/*  If check_lt is TRUE, it means that the file to be opened is a source     */
/*  file and OpenFile() is required to check for a .lt suffix version        */
/*  if the file does not open without it.                                    */
/*                                                                           */
/*****************************************************************************/

FILE *OpenFile(FILE_NUM fnum, BOOLEAN check_ld, BOOLEAN check_lt)
{ FILE *fp;  OBJECT fname, full_name, y;  BOOLEAN used_source_suffix;
  ifdebug(DPP, D, ProfileOn("OpenFile"));
  debug2(DFS, DD, "[ OpenFile(%s, %s)", FileName(fnum), bool_str(check_ld));
  fname = ftab_num(file_tab, fnum);
  if( Down(fname) != fname )
  { Child(y, Down(fname))
      ;
    /* fp = StringFOpen(string(y), file_mode[type_of_file(fname)]); */
    fp = StringFOpen(string(y), READ_FILE);
    debug1(DFS,DD,fp==null ? "  failed on %s" : "  succeeded on %s", string(y));
  }
  else
  {
    /* ***
    fp = SearchPath(string(fname), file_path[path(fname)], check_ld,
	   check_lt, &full_name, &fpos(fname), file_mode[type_of_file(fname)],
	   &used_source_suffix);
    *** */
    fp = SearchPath(string(fname), file_path[path(fname)], check_ld, check_lt,
	   &full_name, &fpos(fname), READ_FILE, &used_source_suffix);
    if( full_name != nilobj )  Link(fname, full_name);
    used_suffix(fname) = used_source_suffix;
  }
  ifdebug(DPP, D, ProfileOff("OpenFile"));
  debug1(DFS, DD, "] OpenFile returning (fp %s null)", fp==null ? "==" : "!=");
  return fp;
} /* end OpenFile */


/*****************************************************************************/
/*                                                                           */
/*  FILE *OpenIncGraphicFile(str, typ, full_name, xfpos, compressed)         */
/*                                                                           */
/*  Open for reading the @IncludeGraphic file str; typ is INCGRAPHIC or      */
/*  SINCGRAPHIC; xfpos is the file position of the file name.                */
/*                                                                           */
/*  Return the full name in full_name.  Set compressed to TRUE if the file   */
/*  was a compressed file.                                                   */
/*                                                                           */
/*****************************************************************************/
#define MAX_COMPRESSED 6
static char *compress_suffixes[MAX_COMPRESSED]
  = { ".gz", "-gz", ".z", "-z", "_z", ".Z" };

FILE *OpenIncGraphicFile(FULL_CHAR *str, unsigned char typ,
OBJECT *full_name, FILE_POS *xfpos, BOOLEAN *compressed)
{ FILE *fp = NULL;  int p, i, res;  BOOLEAN used_source_suffix;
  FULL_CHAR sort_name[128];  int sort_start = 0, sort_end = 0;
  static FULL_CHAR last_sort_name[128];  static FILE *last_ceps_fp = NULL;
  debug2(DFS, DD, "OpenIncGraphicFile(%s, %s, -)", str, Image(typ));
  assert( typ == INCGRAPHIC || typ == SINCGRAPHIC, "OpenIncGraphicFile!" );
  p = (typ == INCGRAPHIC ? INCLUDE_PATH : SYSINCLUDE_PATH);
  if (str[0] == '@')
  {
    /* parse @name@start:end */
    int len = 0;
    i = 1;
    while (str[i] != '\0' && str[i] != '@' && len < sizeof(sort_name) - 1)
    {
      sort_name[len++] = str[i++];
    }
    sort_name[len++] = '\0';
    if (str[i] == '@')
    {
      i++;
      while (str[i] >= '0' && str[i] <= '9')
      {
	sort_start = sort_start * 10 + str[i] - '0';
	i++;
      }
    }
    if (str[i] == ':')
    {
      i++;
      while (str[i] >= '0' && str[i] <= '9')
      {
	sort_end = sort_end * 10 + str[i] - '0';
	i++;
      }
    }
    if (sort_start <= 0 || sort_end <= 0 || sort_start > sort_end)
    {
      sort_start = sort_end =0;
    }
    if (sort_start > 0)
    {
      str = sort_name;
      if (last_ceps_fp != NULL)
      {
	if (strcmp((char *)sort_name, (char *)last_sort_name) == 0)
	{
	  fp = last_ceps_fp;
	  *full_name = nilobj;
	}
	else
	{
	  fclose(last_ceps_fp);
	  last_ceps_fp = NULL;
	}
      }
    }
  }
  if ( fp == NULL )
  {
    fp = SearchPath(str, file_path[p], FALSE, FALSE, full_name, xfpos,
	   READ_FILE, &used_source_suffix);
  }
  if( *full_name == nilobj )  *full_name = MakeWord(WORD, str, xfpos);

  if( fp == null )
  {
    /* if file didn't open, nothing more to do */
    *compressed = FALSE;
    fp = null;
  }
  else if (sort_start > 0)
  {
    FILE *ceps_fp;
    int ch;
    int sort_pos, header_pos;
    ceps_fp = fp;
    fp = fopen(LOUT_EPS, WRITE_FILE);
    if ( fp == NULL )
    {
      Error(3, 17, "Could not create %s", WARN, xfpos, LOUT_EPS);
    }
    else
    {
      fseek(ceps_fp, sort_start, SEEK_SET);
      sort_pos = sort_start;
      ch = fgetc(ceps_fp);
      if (ch == '\0')
      {
	sort_pos++;
	ch = fgetc(ceps_fp);
      }
      /* copy the document comments */
      while (sort_pos < sort_end && ch == '%')
      {
	while (sort_pos < sort_end && ch != '\n' && ch != '\r' && ch != '\0' && ch != EOF)
	{
	  putc(ch, fp);
	  ch = getc(ceps_fp);
	  sort_pos++;
	}
	while (sort_pos < sort_end && (ch == '\n' || ch == '\r'))
	{
	  putc(ch, fp);
	  ch = getc(ceps_fp);
	  sort_pos++;
	}
      }
      /* copy the header */
      fseek(ceps_fp, 0L, SEEK_SET);
      header_pos = 0;
      ch = fgetc(ceps_fp);
      while (ch != '\0' && ch != EOF)
      {
	putc(ch, fp);
	ch = getc(ceps_fp);
	header_pos++;
      }
      /* copy the rest of the ad */
      fseek(ceps_fp, sort_pos, SEEK_SET);
      ch = fgetc(ceps_fp);
      while (sort_pos < sort_end && ch != '\0' && ch != EOF)
      {
	putc(ch, fp);
	ch = getc(ceps_fp);
	sort_pos++;
      }
      /* copy the trailer */
      fseek(ceps_fp, header_pos, SEEK_SET);
      ch = fgetc(ceps_fp);
      while (ch != '\0' && ch != EOF)
      {
	putc(ch, fp);
	ch = getc(ceps_fp);
      }
      /* clean up the files */
      if (last_ceps_fp == NULL)
      {
        last_ceps_fp = ceps_fp;
        strcpy((char *)last_sort_name, (char *)sort_name);
      }
      fclose(fp);
      fp = fopen(LOUT_EPS, READ_FILE);
      *compressed = TRUE;
    }
  }
  else
  {
    /* if file is compressed, uncompress it into file LOUT_EPS */
    for( i = 0;  i < MAX_COMPRESSED; i++ )
    { if( StringEndsWith(string(*full_name), AsciiToFull(compress_suffixes[i])) )
        break;
    }
    if( i < MAX_COMPRESSED )
    { char buff[MAX_BUFF];
      fclose(fp);
      sprintf(buff, UNCOMPRESS_COM, (char *) string(*full_name), LOUT_EPS);
      if( SafeExecution )
      {
        Error(3, 17, "safe execution prohibiting command: %s", WARN, xfpos, buff);
        *compressed = FALSE;
        fp = null;
      }
      else
      {
        res = system(buff);
	if( res != -1 )  /* junk code to keep the compiler happy */
	{
	  fp = fopen(LOUT_EPS, READ_FILE);
	  *compressed = TRUE;
	}
	else
	{
	  *compressed = FALSE;
	  fp = null;
	}
      }
    }
    else
    {
      enum magic_enum { MAGIC_BUF_LEN = 4 };
      char magic_buf[ MAGIC_BUF_LEN ];
      int have_magic;
      have_magic = (fread(magic_buf, 1, MAGIC_BUF_LEN, fp) == MAGIC_BUF_LEN);
      if(  have_magic &&
           memcmp(magic_buf, "%PDF", 4) == 0 )
      {
        /* process a PDF file */
        /* treat it like a compressed file */
        /* convert it to EPS with the pdftops program */
        /*   from the open source xpdf package */
        char buff[ MAX_BUFF ];
        fclose(fp);
        sprintf(buff, PDFTOPS_COM, (char *) string(*full_name), LOUT_EPS);
        if( SafeExecution )
        {
          Error(3, 17, "safe execution prohibiting command: %s", WARN, xfpos,buff);
          *compressed = FALSE;
          fp = null;
        }
        else
        {
          system(buff);
          fp = fopen(LOUT_EPS, READ_FILE);
          *compressed = TRUE;
        }
      }
      else if (  have_magic &&
                 ((magic_buf[0] == 'I' && magic_buf[1] == 'I') || /* TIFF, Intel format */
                  (magic_buf[0] == 'M' && magic_buf[1] == 'M') || /* TIFF, Motorola format */
                  (magic_buf[0] == 'G' && magic_buf[1] == 'I' && magic_buf[2] == 'F') || /* GIF */
                  ((magic_buf[0]&0xff) == 0xff && (magic_buf[1]&0xff) == 0xd8) || /* JPEG */
                  (magic_buf[1] == 'P' && magic_buf[2] == 'N' && magic_buf[3] == 'G') /* PNG */ ) )
      {
	/* process bitmap files with ImageMagick convert */
        char buff[ MAX_BUFF ];
        fclose(fp);
        sprintf(buff, CONVERT_COM, (char *) string(*full_name), LOUT_EPS);
        if( SafeExecution )
        {
          Error(3, 17, "safe execution prohibiting command: %s", WARN, xfpos,buff);
          *compressed = FALSE;
          fp = null;
        }
        else
        {
          system(buff);
          fp = fopen(LOUT_EPS, READ_FILE);
          *compressed = TRUE;
        }	
      }
      else
      {
        /* normal uncompressed EPS file */
        rewind(fp);
        *compressed = FALSE;
      }
    }
  }

  debug2(DFS, DD, "OpenIncGraphicFile returning (fp %s null, *full_name = %s)",
    fp==null ? "==" : "!=", string(*full_name));
  return fp;
} /* end OpenIncGraphicFile */


/*****************************************************************************/
/*                                                                           */
/*  FileSetUpdated(fnum, newlines)                                           */
/*                                                                           */
/*  Declare that file fnum has been updated, and that it now contains        */
/*  newlines lines.                                                          */
/*                                                                           */
/*****************************************************************************/

void FileSetUpdated(FILE_NUM fnum, int newlines)
{ 
  debug2(DFS, DD, "FileSetUpdated(%s, %d)", FileName(fnum), newlines);
  updated(ftab_num(file_tab, fnum)) = TRUE;
  line_count(ftab_num(file_tab, fnum)) = newlines;
  debug0(DFS, DD, "FileSetUpdated returning");
} /* end FileSetUpdated */


/*****************************************************************************/
/*                                                                           */
/*  int FileGetLineCount(FILE_NUM fnum)                                      */
/*                                                                           */
/*  Return the number of lines so far written to file fnum.                  */
/*                                                                           */
/*****************************************************************************/

int FileGetLineCount(FILE_NUM fnum)
{ int res;
  debug1(DFS, DD, "FileGetLineCount(%s)", FileName(fnum));
  res = line_count(ftab_num(file_tab, fnum));
  debug1(DFS, DD, "FileGetLineCount returning %d", res);
  return res;
} /* end FileGetLineCount */


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN FileTestUpdated(fnum)                                            */
/*                                                                           */
/*  Test whether file fnum has been declared to be updated.                  */
/*                                                                           */
/*****************************************************************************/

BOOLEAN FileTestUpdated(FILE_NUM fnum)
{ return (BOOLEAN) updated(ftab_num(file_tab, fnum));
} /* end FileTestUpdated */
