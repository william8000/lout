/*@z03.c:File Service:Declarations, no_fpos@******************************** */
/*                                                                           */
/*  LOUT: A HIGH-LEVEL LANGUAGE FOR DOCUMENT FORMATTING (VERSION 2.05)       */
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
/*  FILE:         z03.c                                                      */
/*  MODULE:       File Service                                               */
/*  EXTERNS:      InitFiles(), AddToPath(), DefineFile(), FirstFile(),       */
/*                NextFile(), FileNum(), FileName(), EchoFilePos(),          */
/*                PosOfFile(), OpenFile(), OpenIncGraphicFile(),             */
/*                ReadFromFile(), AppendToFile(), CloseFiles()               */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define MAX_TYPES	 10			/* number of file types      */
#define MAX_PATHS	  7			/* number of search paths    */
#define	TAB_MASK	0xFF			/* mask forces <= MAX_FILES  */

#define	file_number(x)	word_font(x)		/* file number of file x     */
#define	updated(x)	broken(x)		/* TRUE when x is updated    */
#define	path(x)		back(x, COL)		/* search path for file x    */

static	int	file_count;			/* total number of files     */
static	OBJECT	fvec[MAX_FILES] = { nil };	/* the file table            */
static	OBJECT	file_list[MAX_TYPES];		/* files of each type        */
static	OBJECT	file_path[MAX_PATHS];		/* the search paths          */
#if DEBUG_ON
static	char	*file_types[]		/* the type names for debug  */
		= { "source", "include", "incgraphic", "database", "index",
		    "font", "prepend", "hyph", "hyphpacked", "encoding" };
#endif


/*****************************************************************************/
/*                                                                           */
/*  no_fpos                                                                  */
/*                                                                           */
/*  A null file position value.                                              */
/*                                                                           */
/*****************************************************************************/

static FILE_POS no_file_pos = {0, 0, 0};
FILE_POS *no_fpos = &no_file_pos;

/*****************************************************************************/
/*                                                                           */
/*  #define hash(str, val)                                                   */
/*                                                                           */
/*  Hash the string str and return its value in val.                         */
/*                                                                           */
/*****************************************************************************/

#define hash(str, val)							\
{ p = str;								\
  val = *p++;								\
  while( *p ) val += *p++;						\
  val = (val * 8) & TAB_MASK;						\
}

/*@::InitFiles(), AddToPath(), DefineFile()@**********************************/
/*                                                                           */
/*  InitFiles()                                                              */
/*                                                                           */
/*  Initialize this module.                                                  */
/*                                                                           */
/*****************************************************************************/

InitFiles()
{ int i;
  for( i = 0;  i < MAX_TYPES; i++ )  file_list[i]  = New(ACAT);
  for( i = 0;  i < MAX_PATHS; i++ )  file_path[i] = New(ACAT);
  fvec[0] = file_list[0];	/* so that no files will be given slot 0 */
  file_count = 1;
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
  assert( ftype < MAX_TYPES, "DefineFile: ftype!" );
  debug5(DFS, D, "DefineFile(%s, %s,%s, %s, %d)",
    str, suffix, EchoFilePos(xfpos), file_types[ftype], fpath);
  if( ftype == SOURCE_FILE && (i = StringLength(str)) >= 3 )
  {
    /* check that file name does not end in ".li" or ".ld" */
    if( StringEqual(&str[i-StringLength(DATA_SUFFIX)], DATA_SUFFIX) )
      Error(FATAL, xfpos,
	"database file %s where source file expected", str);
    if( StringEqual(&str[i-StringLength(INDEX_SUFFIX)], INDEX_SUFFIX) )
      Error(FATAL, xfpos,
	"database index file %s where source file expected", str);
  }
  if( ++file_count >= MAX_FILES ) Error(FATAL, xfpos, "too many file names");
  hash(str, i);
  while( fvec[i] != nil )
    if( ++i >= MAX_FILES ) i = 0;
  if( StringLength(str) + StringLength(suffix) >= MAX_LINE )
    Error(FATAL, no_fpos, "file name %s%s too long", str, suffix);
  fvec[i] = MakeWordTwo(WORD, str, suffix, xfpos);
  Link(file_list[ftype], fvec[i]);
  file_number(fvec[i]) = i;
  path(fvec[i]) = fpath;
  debug1(DFS, D, "DefineFile returning %s",
    i == NO_FILE ? STR_NONE : FileName( (FILE_NUM) i));
  return (FILE_NUM) i;
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
  link = Down(file_list[ftype]);
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
  debug1(DFS, D, "NextFile( %s )", EchoObject(fvec[i]));
  link = NextDown(Up(fvec[i]));
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
  register int i;
  FULL_CHAR buff[MAX_LINE];
  debug2(DFS, D, "FileNum(%s, %s)", str, suffix);
  hash(str, i);
  if( StringLength(str) + StringLength(suffix) >= MAX_LINE )
    Error(FATAL, no_fpos, "file name %s%s too long", str, suffix);
  StringCopy(buff, str);
  StringCat(buff, suffix);
  while( fvec[i] != nil && !StringEqual(string(fvec[i]), buff) )
    if( ++i >= MAX_FILES ) i = 0;
  if( fvec[i] == nil ) i = 0;
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
  assert( fnum > 0 && fvec[fnum] != nil, "FileName: fvec[fnum] == nil!" );
  x = fvec[fnum];  if( Down(x) != x )  Child(x, Down(x));
  return string(x);
} /* end FileName */


/*****************************************************************************/
/*                                                                           */
/*  FULL_CHAR *EchoFilePos(pos)                                              */
/*                                                                           */
/*  Returns a string reporting the value of file position pos.               */
/*                                                                           */
/*****************************************************************************/

static FULL_CHAR buff[2][MAX_LINE];  static bp = 1;

static append_fpos(pos)
FILE_POS *pos;
{ OBJECT x;
  x = fvec[file_num(*pos)];
  assert( x != nil, "EchoFilePos: fvec[] entry is nil!" );
  if( file_num(fpos(x)) > 0 )
  { append_fpos( &fpos(x) );
    if( StringLength(buff[bp]) + 2 >= MAX_LINE )
      Error(FATAL,no_fpos,"file position %s... is too long to print", buff[bp]);
    StringCat(buff[bp], STR_SPACE);
    StringCat(buff[bp], AsciiToFull("/"));
  }
  if( StringLength(buff[bp]) + StringLength(string(x)) + 13 >= MAX_LINE )
    Error(FATAL, no_fpos, "file position %s... is too long to print", buff[bp]);
  StringCat(buff[bp], STR_SPACE);
  StringCat(buff[bp], STR_QUOTE);
  StringCat(buff[bp], string(x));
  StringCat(buff[bp], STR_QUOTE);
  if( line_num(*pos) != 0 )
  { StringCat(buff[bp], STR_SPACE);
    StringCat(buff[bp], StringInt(line_num(*pos)));
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


/*****************************************************************************/
/*                                                                           */
/*  FILE_POS *PosOfFile(fnum)                                                */
/*                                                                           */
/*  Returns a pointer to the file position where file fnum was encountered.  */
/*                                                                           */
/*****************************************************************************/

FILE_POS *PosOfFile(fnum)
FILE_NUM fnum;
{ OBJECT  x = fvec[fnum];
  assert( x != nil, "PosOfFile: fvec[] entry is nil!" );
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
/*  Also return the full path name in object *full_name if reqd, else nil.   */
/*                                                                           */
/*****************************************************************************/

static FILE *SearchPath(str, fpath, check_ld, check_lt, full_name, xfpos)
FULL_CHAR *str;  OBJECT fpath;  BOOLEAN check_ld, check_lt;
OBJECT *full_name;  FILE_POS *xfpos;
{ 
  FULL_CHAR buff[MAX_LINE];  OBJECT link, y;  FILE *fp;
  debug4(DFS, DD, "SearchPath(%s, %s, %s, %s, -)", str, EchoObject(fpath),
	bool(check_ld), bool(check_lt));
  *full_name = nil;
  if( StringEqual(str, STR_STDIN) )
  { fp = stdin;
    debug0(DFS, DD, "  opened stdin");
  }
  else if( StringBeginsWith(str, AsciiToFull("/")) )
  { fp = StringFOpen(str, "r");
    debug1(DFS, DD, fp==null ? "  failed on %s" : "  succeeded on %s", str);
  }
  else
  { fp = null;
    for( link = Down(fpath);  fp==null && link != fpath; link = NextDown(link) )
    { Child(y, link);
      if( StringLength(string(y)) == 0 )
      { StringCopy(buff, str);
	fp = StringFOpen(str, "r");
	debug1(DFS, DD, fp==null ? "  failed on %s" : "  succeeded on %s", str);
      }
      else
      {	if( StringLength(string(y)) + 1 + StringLength(str) >= MAX_LINE )
	  Error(FATAL, &fpos(y), "file path name %s/%s is too long",
		string(y), str);
	StringCopy(buff, string(y));
	StringCat(buff, AsciiToFull("/"));
	StringCat(buff, str);
	fp = StringFOpen(buff, "r");
	debug1(DFS, DD, fp==null ? "  failed on %s" : "  succeeded on %s",buff);
	if( fp != null ) *full_name = MakeWord(WORD, buff, xfpos);
      }
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
      if( fp == null && check_lt )
      {	StringCopy(&buff[StringLength(buff)], SOURCE_SUFFIX);
	fp = StringFOpen(buff, "r");
	debug1(DFS,DD,fp==null ? "  failed on %s" : "  succeeded on %s", buff);
	StringCopy(&buff[StringLength(buff) - StringLength(SOURCE_SUFFIX)], STR_EMPTY);
	if( fp != null ) *full_name = MakeWord(WORD, buff, xfpos);
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
{ FILE *fp;  OBJECT full_name, y;
  ifdebug(DPP, D, ProfileOn("OpenFile"));
  debug2(DFS, D, "OpenFile(%s, %s)", FileName(fnum), bool(check_ld));
  if( Down(fvec[fnum]) != fvec[fnum] )
  { Child(y, Down(fvec[fnum]));
    fp = StringFOpen(string(y), "r");
    debug1(DFS,DD,fp==null ? "  failed on %s" : "  succeeded on %s", string(y));
  }
  else
  { fp = SearchPath(string(fvec[fnum]), file_path[path(fvec[fnum])],
	   check_ld, check_lt, &full_name, &fpos(fvec[fnum]));
    if( full_name != nil )  Link(fvec[fnum], full_name);
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


/*@::ReadFromFile()@**********************************************************/
/*                                                                           */
/*  OBJECT ReadFromFile(fnum, pos, sym)                                      */
/*                                                                           */
/*  Read an object from file fnum starting at position pos.                  */
/*  The object may include @Env operators defining its environment.          */
/*  If sym != nil, sym is the symbol which is to be read in.                 */
/*                                                                           */
/*****************************************************************************/

OBJECT ReadFromFile(fnum, pos, sym)
FILE_NUM fnum; long pos;  OBJECT sym;
{ OBJECT t, res; int ipos;
  ifdebug(DPP, D, ProfileOn("ReadFromFile"));
  ifdebug(DFS, D, ipos = (int) pos);
  debug3(DFS, D, "ReadFromFile(%s, %d, %s)", FileName(fnum), ipos,SymName(sym));
  LexPush(fnum, (int) pos, DATABASE_FILE);
  SwitchScope(sym);
  t = LexGetToken();
  if( type(t) != LBR )
  { debug1(DFS, D, "  following because type(t) = %s", Image(type(t)));
    Error(FATAL, &fpos(t),"syntax error (missing %s) in database file", KW_LBR);
  }
  res = Parse(&t, StartSym, FALSE, FALSE);
  if( t != nil || type(res) != CLOSURE )
  { debug1(DFS, D, "  following because of %s", t != nil ? "t" : "type(res)");
    Error(FATAL, &fpos(res), "syntax error in database file");
  }
  UnSwitchScope(sym);
  LexPop();
  debug1(DFS, D, "ReadFromFile returning %s", EchoObject(res));
  ifdebug(DPP, D, ProfileOff("ReadFromFile"));
  return res;
} /* end ReadFromFile */


static FILE_NUM	last_write_fnum = NO_FILE;
static FILE	*last_write_fp  = null;


/*@::WriteClosure()@**********************************************************/
/*                                                                           */
/*  static WriteClosure(x)                                                   */
/*                                                                           */
/*  Write closure x to file last_write_fp, without enclosing braces and      */
/*  without any environment attached.                                        */
/*                                                                           */
/*****************************************************************************/

static BOOLEAN need_lvis(sym)		/* true if @LVis needed before sym */
OBJECT sym;
{ return !visible(sym) &&
	 enclosing(sym) != StartSym &&
	 type(enclosing(sym)) == LOCAL;
} /* end need_lvis */

static WriteClosure(x)
OBJECT x;
{ OBJECT y, link, z, sym;
  BOOLEAN npar_seen, name_printed;
  static WriteObject();

  sym = actual(x);  npar_seen = FALSE;  name_printed = FALSE;
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link);
    if( type(y) == PAR )  switch( type(actual(y)) )
    {
      case LPAR:
      
	assert( Down(y) != y, "WriteObject/CLOSURE: LPAR!" );
	Child(z, Down(y));
	WriteObject(z, (int) precedence(sym));
	StringFPuts(STR_SPACE, last_write_fp);
	break;


      case NPAR:
      
	assert( Down(y) != y, "WriteObject/CLOSURE: NPAR!" );
	Child(z, Down(y));
	if( !name_printed )
	{ if( need_lvis(sym) )
	  { StringFPuts(KW_LVIS, last_write_fp);
	    StringFPuts(STR_SPACE, last_write_fp);
	  }
	  StringFPuts(SymName(sym), last_write_fp);
	  name_printed = TRUE;
	}
	StringFPuts(STR_NEWLINE, last_write_fp);
	StringFPuts(STR_SPACE, last_write_fp);
	StringFPuts(STR_SPACE, last_write_fp);
	StringFPuts(STR_SPACE, last_write_fp);
	StringFPuts(SymName(actual(y)), last_write_fp);
	StringFPuts(STR_SPACE, last_write_fp);
	StringFPuts(KW_LBR, last_write_fp);
	StringFPuts(STR_SPACE, last_write_fp);
	WriteObject(z, NO_PREC);
	StringFPuts(STR_SPACE, last_write_fp);
	StringFPuts(KW_RBR, last_write_fp);
	npar_seen = TRUE;
	break;


      case RPAR:
      
	assert( Down(y) != y, "WriteObject/CLOSURE: RPAR!" );
	Child(z, Down(y));
	if( !name_printed )
	{ if( need_lvis(sym) )
	  { StringFPuts(KW_LVIS, last_write_fp);
	    StringFPuts(STR_SPACE, last_write_fp);
	  }
	  StringFPuts(SymName(sym), last_write_fp);
	  name_printed = TRUE;
	}
	StringFPuts(npar_seen ? STR_NEWLINE : STR_SPACE, last_write_fp);
	if( has_body(sym) )
	{
	  StringFPuts(KW_LBR, last_write_fp);
	  StringFPuts(STR_SPACE, last_write_fp);
	  WriteObject(z, NO_PREC);
	  StringFPuts(STR_SPACE, last_write_fp);
	  StringFPuts(KW_RBR, last_write_fp);
	}
	else WriteObject(z, (int) precedence(sym));
	break;


      default:
      
	Error(INTERN, &fpos(y), "WriteClosure: %s", Image(type(actual(y))) );
	break;

    } /* end switch */
  } /* end for each parameter */
  if( !name_printed )
  { if( need_lvis(sym) )
    { StringFPuts(KW_LVIS, last_write_fp);
      StringFPuts(STR_SPACE, last_write_fp);
    }
    StringFPuts(SymName(sym), last_write_fp);
    name_printed = TRUE;
  }
} /* end WriteClosure */


/*@::WriteObject()@***********************************************************/
/*                                                                           */
/*  static WriteObject(x, outer_prec)                                        */
/*                                                                           */
/*  Write object x to file last_write_fp, assuming it is a subobject of an   */
/*  object and the precedence of operators enclosing it is outer_prec.       */
/*                                                                           */
/*****************************************************************************/

static WriteObject(x, outer_prec)
OBJECT x;  int outer_prec;
{ OBJECT link, y, gap_obj, sym, env;  FULL_CHAR *name;
  int prec, i, last_prec;  BOOLEAN braces_needed;
  switch( type(x) )
  {

    case WORD:

      if( StringLength(string(x)) == 0 && outer_prec > ACAT_PREC )
      { StringFPuts(KW_LBR, last_write_fp);
	StringFPuts(KW_RBR, last_write_fp);
      }
      else StringFPuts(string(x), last_write_fp);
      break;

    
    case QWORD:

      StringFPuts(StringQuotedWord(x), last_write_fp);
      break;

    
    case VCAT:  prec = VCAT_PREC;  goto ETC;
    case HCAT:  prec = HCAT_PREC;  goto ETC;
    case ACAT:  prec = ACAT_PREC;  goto ETC;

      ETC:
      if( prec < outer_prec )  StringFPuts(KW_LBR, last_write_fp);
      last_prec = prec;
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	if( type(y) == GAP_OBJ )
	{ if( Down(y) == y )
	  { assert( type(x) == ACAT, "WriteObject: Down(y) == y!" );
	    for( i = 1;  i <= vspace(y);  i++ )
	      StringFPuts(STR_NEWLINE, last_write_fp);
	    for( i = 1;  i <= hspace(y);  i++ )
	      StringFPuts(STR_SPACE,  last_write_fp);
	    last_prec = (vspace(y) + hspace(y) == 0) ? JUXTA_PREC : ACAT_PREC;
	  }
	  else
	  { Child(gap_obj, Down(y));
	    StringFPuts(type(x)==ACAT ? STR_SPACE : STR_NEWLINE, last_write_fp);
	    StringFPuts(EchoCatOp(type(x), mark(gap(y)), join(gap(y))),
	      last_write_fp);
	    if( !is_word(type(gap_obj)) || StringLength(string(gap_obj)) != 0 )
		WriteObject(gap_obj, FORCE_PREC);
	    StringFPuts(STR_SPACE, last_write_fp);
	    last_prec = prec;
	  }
	}
	else
	{ if( type(x) == ACAT )
	  { OBJECT next_gap;  int next_prec;
	    if( NextDown(link) != x )
	    { Child(next_gap, NextDown(link));
	      assert( type(next_gap) == GAP_OBJ, "WriteObject: next_gap!" );
	      next_prec = (vspace(next_gap) + hspace(next_gap) == 0)
				? JUXTA_PREC : ACAT_PREC;
	    }
	    else next_prec = prec;
	    WriteObject(y, max(last_prec, next_prec));
	  }
	  else WriteObject(y, prec);
	}
      }
      if( prec < outer_prec )  StringFPuts(KW_RBR, last_write_fp);
      break;


    case ENV:

      if( Down(x) == x )
      { /* do nothing */
      }
      else if( Down(x) == LastDown(x) )
      {	Child(y, Down(x));
	assert( type(y) == CLOSURE, "WriteObject: ENV/CLOSURE!" );
	assert( LastDown(y) != y, "WriteObject: ENV/LastDown(y)!" );
	Child(env, LastDown(y));
	assert( type(env) == ENV, "WriteObject: ENV/env!" );
	WriteObject(env, NO_PREC);
	StringFPuts(KW_LBR, last_write_fp);
	WriteClosure(y);
	StringFPuts(KW_RBR, last_write_fp);
	StringFPuts(STR_NEWLINE, last_write_fp);
      }
      else
      {	Child(env, LastDown(x));
	assert( type(env) == ENV, "WriteObject: ENV/ENV!" );
	WriteObject(env, NO_PREC);
	Child(y, Down(x));
	assert( type(y) == CLOSURE, "WriteObject: ENV/ENV+CLOSURE!" );
	WriteObject(y, NO_PREC);
      }
      break;


    case CLOSURE:

      sym = actual(x);  env = nil;
      if( LastDown(x) != x )
      {	Child(y, LastDown(x));
	if( type(y) == ENV )  env = y;
      }

      braces_needed = env != nil ||
	(precedence(sym) <= outer_prec && (has_lpar(sym) || has_rpar(sym)));

      /* print environment */
      if( env != nil )
      {	StringFPuts(KW_ENV, last_write_fp);
      	StringFPuts(STR_NEWLINE, last_write_fp);
	WriteObject(env, NO_PREC);
      }

      /* print left brace if needed */
      if( braces_needed )  StringFPuts(KW_LBR, last_write_fp);
	
      /* print the closure proper */
      WriteClosure(x);

      /* print closing brace if needed */
      if( braces_needed )  StringFPuts(KW_RBR, last_write_fp);

      /* print closing environment if needed */
      if( env != nil )
      { StringFPuts(STR_NEWLINE, last_write_fp);
	StringFPuts(KW_CLOS, last_write_fp);
      	StringFPuts(STR_NEWLINE, last_write_fp);
      }
      break;


    case CROSS:

      Child(y, Down(x));
      assert( type(y) == CLOSURE, "WriteObject/CROSS: type(y) != CLOSURE!" );
      StringFPuts(SymName(actual(y)), last_write_fp);
      StringFPuts(KW_CROSS, last_write_fp);
      Child(y, LastDown(x));
      WriteObject(y, FORCE_PREC);
      break;


    case NULL_CLOS:	name = KW_NULL;		goto SETC;
    case ONE_COL:	name = KW_ONE_COL;	goto SETC;
    case ONE_ROW:	name = KW_ONE_ROW;	goto SETC;
    case WIDE:		name = KW_WIDE;		goto SETC;
    case HIGH:		name = KW_HIGH;		goto SETC;
    case HSCALE:	name = KW_HSCALE;	goto SETC;
    case VSCALE:	name = KW_VSCALE;	goto SETC;
    case SCALE:		name = KW_SCALE;	goto SETC;
    case HCONTRACT:	name = KW_HCONTRACT;	goto SETC;
    case VCONTRACT:	name = KW_VCONTRACT;	goto SETC;
    case HEXPAND:	name = KW_HEXPAND;	goto SETC;
    case VEXPAND:	name = KW_VEXPAND;	goto SETC;
    case PADJUST:	name = KW_PADJUST;	goto SETC;
    case HADJUST:	name = KW_HADJUST;	goto SETC;
    case VADJUST:	name = KW_VADJUST;	goto SETC;
    case ROTATE:	name = KW_ROTATE;	goto SETC;
    case CASE:		name = KW_CASE;		goto SETC;
    case YIELD:		name = KW_YIELD;	goto SETC;
    case XCHAR:		name = KW_XCHAR;	goto SETC;
    case FONT:		name = KW_FONT;		goto SETC;
    case SPACE:		name = KW_SPACE;	goto SETC;
    case BREAK:		name = KW_BREAK;	goto SETC;
    case NEXT:		name = KW_NEXT;		goto SETC;
    case OPEN:		name = KW_OPEN;		goto SETC;
    case TAGGED:	name = KW_TAGGED;	goto SETC;
    case INCGRAPHIC:	name = KW_INCGRAPHIC;	goto SETC;
    case SINCGRAPHIC:	name = KW_SINCGRAPHIC;	goto SETC;
    case GRAPHIC:	name = KW_GRAPHIC;	goto SETC;

      /* print left parameter, if present */
      SETC:
      if( DEFAULT_PREC <= outer_prec )  StringFPuts(KW_LBR, last_write_fp);
      if( Down(x) != LastDown(x) )
      {	Child(y, Down(x));
	WriteObject(y, DEFAULT_PREC);
	StringFPuts(STR_SPACE, last_write_fp);
      }

      /* print the name of the symbol */
      StringFPuts(name, last_write_fp);

      /* print right parameter, if present */
      if( LastDown(x) != x )
      {	Child(y, LastDown(x));
	StringFPuts(STR_SPACE, last_write_fp);
	if( type(x) == OPEN )
	{ StringFPuts(KW_LBR, last_write_fp);
	  WriteObject(y, NO_PREC);
	  StringFPuts(KW_RBR, last_write_fp);
	}
	else WriteObject(y, DEFAULT_PREC);
      }
      if( DEFAULT_PREC <= outer_prec )
	StringFPuts(KW_RBR, last_write_fp);
      break;


    default:

      Error(INTERN, &fpos(x), "WriteObject: type(x) = %s", Image(type(x)));
      break;

  } /* end switch */
} /* end WriteObject */


/*@::AppendToFile(), CloseFiles()@********************************************/
/*                                                                           */
/*  AppendToFile(x, fnum, pos)                                               */
/*                                                                           */
/*  Append object x to file fnum, returning its fseek position in *pos.      */
/*  Record the fact that this file has been updated.                         */
/*                                                                           */
/*****************************************************************************/

AppendToFile(x, fnum, pos)
OBJECT x;  FILE_NUM fnum;  int *pos;
{ FULL_CHAR buff[MAX_LINE], *str;
  debug2(DFS, D, "AppendToFile( %s, %s )", EchoObject(x), FileName(fnum));

  /* open file fnum for writing */
  if( last_write_fnum != fnum )
  { if( last_write_fnum != NO_FILE )  fclose(last_write_fp);
    str = FileName(fnum);
    if( StringLength(str) + StringLength(NEW_DATA_SUFFIX) >= MAX_LINE )
      Error(FATAL, PosOfFile(fnum), "file name %s%s is too long",
	str, NEW_DATA_SUFFIX);
    StringCopy(buff, str);  StringCat(buff, NEW_DATA_SUFFIX);
    last_write_fp = StringFOpen(buff, "a");
    if( last_write_fp == null )  Error(FATAL, &fpos(fvec[fnum]),
		"cannot append to database file %s", buff);
    last_write_fnum = fnum;
  }

  /* write x out and record the fact that fnum has changed */
  *pos = (int) ftell(last_write_fp);
  StringFPuts(KW_LBR, last_write_fp);
  WriteObject(x, NO_PREC);
  StringFPuts(KW_RBR, last_write_fp);
  StringFPuts(STR_NEWLINE, last_write_fp);
  StringFPuts(STR_NEWLINE, last_write_fp);
  updated(fvec[fnum]) = TRUE;
  debug0(DFS, D, "AppendToFile returning.");
} /* end AppendToFile */


/*****************************************************************************/
/*                                                                           */
/*  CloseFiles()                                                             */
/*                                                                           */
/*  Close all files and move new versions to the names of old versions.      */
/*                                                                           */
/*****************************************************************************/

CloseFiles()
{ FILE_NUM fnum;  FULL_CHAR buff[MAX_LINE];
  ifdebug(DPP, D, ProfileOn("CloseFiles"));
  debug0(DFS, D, "CloseFiles()");

  /* close off last file opened by AppendToFile above */
  if( last_write_fnum != NO_FILE )  fclose(last_write_fp);

  /* get rid of old database files */
  for( fnum = FirstFile(SOURCE_FILE);  fnum != NO_FILE;  fnum = NextFile(fnum) )
  { StringCopy(buff, FileName(fnum));
    StringCat(buff, DATA_SUFFIX);  StringUnlink(buff);
  }

  /* move any new database files to the old names, if updated */
  for( fnum = FirstFile(DATABASE_FILE); fnum != NO_FILE; fnum = NextFile(fnum) )
  { if( updated(fvec[fnum]) )
    { StringCopy(buff, string(fvec[fnum]));
      StringCat(buff, NEW_DATA_SUFFIX);
      debug1(DFS, D, "unlink(%s)", string(fvec[fnum]));
      StringUnlink(string(fvec[fnum])); /* may fail if no old version */
      debug2(DFS, D, "link(%s, %s)", buff, string(fvec[fnum]));
      if( StringLink(buff, string(fvec[fnum])) != 0 )
        Error(INTERN, no_fpos, "link(%s, %s) failed", buff, string(fvec[fnum]));
      debug1(DFS, D, "unlink(%s)", buff);
      if( StringUnlink(buff) != 0 )  Error(INTERN, no_fpos, "unlink(%s)", buff);
    }
  }
  debug0(DFS, D, "CloseFiles returning.");
  ifdebug(DPP, D, ProfileOff("CloseFiles"));
} /* end CloseFiles */
