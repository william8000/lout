/*@z03.c:File Service:DefineFile(), FirstFile()@**************************** */
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
/*  FILE:         z03.c                                                      */
/*  MODULE:       File Service                                               */
/*  EXTERNS:      InitFiles(), AddToPath(), DefineFile(), FirstFile(),       */
/*                NextFile(), FileNum(), FileName(), EchoFilePos(),          */
/*                OpenFile(), ReadFromFile(), AppendToFile(), CloseFiles()   */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define MAX_TYPES	  9			/* number of file types      */
#define MAX_PATHS	  6			/* number of search paths    */
#define	TAB_MASK	0xFF			/* mask forces <= MAX_FILES  */

#define	file_number(x)	word_font(x)		/* file number of file x     */
#define	updated(x)	broken(x)		/* TRUE when x is updated    */
#define	path(x)		back(x, COL)		/* search path for file x    */

static	int	file_count;			/* total number of files     */
static	OBJECT	fvec[MAX_FILES] = { nil };	/* the file table            */
static	OBJECT	file_list[MAX_TYPES];		/* files of each type        */
static	OBJECT	file_path[MAX_PATHS];		/* the search paths          */
#ifdef DEBUG_ON
static	char	*file_types[]		/* the type names for debug  */
		= { "source", "include", "incgraphic", "database",
		    "index", "font", "prepend", "hyph", "hyphpacked" };
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


/*****************************************************************************/
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


/*@@**************************************************************************/
/*                                                                           */
/*  AddToPath(fpath, dirname)                                                */
/*                                                                           */
/*  Add the directory dirname to the end of search path fpath.               */
/*                                                                           */
/*****************************************************************************/

AddToPath(fpath, dirname)
int fpath; unsigned char *dirname;
{ OBJECT x;
  x = MakeWord(dirname, no_fpos);
  Link(file_path[fpath], x);
} /* end AddToPath */


/*****************************************************************************/
/*                                                                           */
/*  FILE_NUM DefineFile(x, ftype, fpath)                                     */
/*                                                                           */
/*  Declare file x, which is a WORD object containing the file name.         */
/*  ftype is the file's type; fpath is its search path.                      */
/*                                                                           */
/*****************************************************************************/

FILE_NUM DefineFile(x, ftype, fpath)
OBJECT x;  int ftype, fpath;
{ register unsigned char *p;
  register int i;
  assert( type(x) == WORD, "DefineFile: type(x) != WORD!" );
  assert( ftype < MAX_TYPES, "DefineFile: ftype!" );
  debug3(DFS, D, "DefineFile( %s, %s, %d )",
    EchoObject(null,x), file_types[ftype], fpath);
  if( ftype == SOURCE_FILE && (i = strlen(string(x))) >= 3 )
  {
    /* check that file name does not end in ".li" or ".ld" */
    if( strcmp(&string(x)[i-strlen(DATA_SUFFIX)], DATA_SUFFIX) == 0 )
      Error(FATAL, &fpos(x),
	"database file %s where source file expected", string(x));
    if( strcmp(&string(x)[i-strlen(INDEX_SUFFIX)], INDEX_SUFFIX) == 0 )
      Error(FATAL, &fpos(x),
	"database index file %s where source file expected", string(x));
  }
  if( ++file_count >= MAX_FILES ) Error(FATAL, &fpos(x), "too many file names");
  hash(string(x), i);
  while( fvec[i] != nil )
    if( ++i >= MAX_FILES ) i = 0;
  fvec[i] = x;
  Link(file_list[ftype], x);
  file_number(x) = i;
  path(x) = fpath;
  debug1(DFS, D, "DefineFile returning %s",
    i == NO_FILE ? (unsigned char *) "none" : FileName( (FILE_NUM) i));
  return (FILE_NUM) i;
} /* end DefineFile */


/*****************************************************************************/
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
  debug1(DFS, D, "FirstFile returning %s",
    i == NO_FILE ? (unsigned char *) "none" : FileName(i));
  return i;
} /* end FirstFile */


/*@@**************************************************************************/
/*                                                                           */
/*  FILE_NUM NextFile(i)                                                     */
/*                                                                           */
/*  Returns the next file after file i of i's type, else NO_FILE.            */
/*                                                                           */
/*****************************************************************************/

FILE_NUM NextFile(i)
FILE_NUM i;
{ OBJECT link, y;
  debug1(DFS, D, "NextFile( %s )", EchoObject(null, fvec[i]));
  link = NextDown(Up(fvec[i]));
  if( type(link) == ACAT )  i = NO_FILE;
  else
  { Child(y, link);
    i = file_number(y);
  }
  debug1(DFS, D, "NextFile returning %s",
    i == NO_FILE ? (unsigned char *) "none" : FileName(i));
  return i;
} /* end NextFile */


/*****************************************************************************/
/*                                                                           */
/*  FILE_NUM FileNum(str)                                                    */
/*                                                                           */
/*  Return the file number of the file with name str, else NO_FILE.          */
/*                                                                           */
/*****************************************************************************/

FILE_NUM FileNum(str)
unsigned char *str;
{ register unsigned char *p;
  register int i;
  debug1(DFS, D, "FileNum( %s )", str);
  hash(str, i);
  while( fvec[i] != nil && strcmp(string(fvec[i]), str) != 0 )
    if( ++i >= MAX_FILES ) i = 0;
  if( fvec[i] == nil ) i = 0;
  debug1(DFS, D, "FileNum returning %s",
    i == NO_FILE ? (unsigned char *) "none" : FileName( (FILE_NUM) i));
  return (FILE_NUM) i;
} /* end FileNum */


/*****************************************************************************/
/*                                                                           */
/*  unsigned char *FileName(fnum)                                            */
/*                                                                           */
/*  Return the string name of the file with this number.  This is the name   */
/*  provided by DefineFile until OpenFile is called, after which it is the   */
/*  full path name.                                                          */
/*                                                                           */
/*****************************************************************************/

unsigned char *FileName(fnum)
FILE_NUM fnum;
{ OBJECT x;
  assert( fnum > 0 , "FileName: num!" );
  assert( fvec[fnum] != nil, "FileName: fvec[fnum] == nil!" );
  x = fvec[fnum];
  if( Down(x) != x )  Child(x, Down(x));
  return string(x);
} /* end FileName */


/*@@**************************************************************************/
/*                                                                           */
/*  unsigned char *EchoFilePos(pos)                                          */
/*                                                                           */
/*  Returns a string reporting the value of file position pos.               */
/*                                                                           */
/*****************************************************************************/

static unsigned char buff[2][MAX_LINE];  static bp = 1;

static append_fpos(pos)
FILE_POS *pos;
{ OBJECT x;
  x = fvec[file_num(*pos)];
  assert( x != nil, "EchoFilePos: fvec[] entry is nil!" );
  if( file_num(fpos(x)) > 0 )
  { append_fpos( &fpos(x) );
    if( strlen(buff[bp]) + 2 >= MAX_LINE )
      Error(FATAL,no_fpos,"file position %s... is too long to print", buff[bp]);
    strcat(buff[bp], " /");
  }
  if( strlen(buff[bp]) + strlen(string(x)) + 13 >= MAX_LINE )
    Error(FATAL, no_fpos, "file position %s... is too long to print", buff[bp]);
  sprintf(&buff[bp][strlen(buff[bp])], " \"%s\"", string(x));
  if( line_num(*pos) != 0 )
    sprintf(&buff[bp][strlen(buff[bp])]," %d,%d",line_num(*pos), col_num(*pos));
} /* end append_fpos */

unsigned char *EchoFilePos(pos)
FILE_POS *pos;
{ bp = (bp + 1) % 2;
  strcpy(buff[bp], "");
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
{ OBJECT x;
  x = fvec[fnum];
  assert( x != nil, "PosOfFile: fvec[] entry is nil!" );
  return &fpos(x);
}


/*****************************************************************************/
/*                                                                           */
/*  FILE *SearchPath(str, fpath, check_ld, full_name, xfpos)                 */
/*                                                                           */
/*  Search the given path for a file whose name is str.  If found, open      */
/*  it; return the resulting FILE *.                                         */
/*                                                                           */
/*  If check_ld is TRUE, it means that the file to be opened is a .li file   */
/*  and OpenFile() is required to check whether the corresponding .ld file   */
/*  is present.  If it is, then the search must stop.                        */
/*                                                                           */
/*  Also return the full path name in object *full_name if reqd, else nil.   */
/*                                                                           */
/*****************************************************************************/

static FILE *SearchPath(str, fpath, check_ld, full_name, xfpos)
unsigned char *str;  OBJECT fpath;  BOOLEAN check_ld;
OBJECT *full_name;  FILE_POS *xfpos;
{ 
  unsigned char buff[MAX_LINE];  OBJECT link, y;  FILE *fp;
  debug3(DFS, DD, "SearchPath(%s, %s, %s, -)", str, EchoObject(null, fpath),
	bool(check_ld));
  *full_name = nil;
  if( strcmp(str, "-") == 0 )
  { fp = stdin;
    debug0(DFS, DD, "  opened stdin");
  }
  else if( str[0] == '/' )
  { fp = fopen(str, "r");
    debug1(DFS, DD, fp==null ? "  failed on %s" : "  succeeded on %s", str);
  }
  else
  { fp = null;
    for( link = Down(fpath);  fp==null && link != fpath; link = NextDown(link) )
    { Child(y, link);
      if( string(y)[0] == '\0' )
      { strcpy(buff, str);
	fp = fopen(str, "r");
	debug1(DFS, DD, fp==null ? "  failed on %s" : "  succeeded on %s", str);
      }
      else
      {	if( strlen(string(y)) + 1 + strlen(str) >= MAX_LINE )
	  Error(FATAL, &fpos(y), "file path name %s/%s is too long",
		string(y), str);
	sprintf(buff, "%s/%s", string(y), str);
	fp = fopen(buff, "r");
	debug1(DFS, DD, fp==null ? "  failed on %s" : "  succeeded on %s",buff);
	if( fp != null ) *full_name = MakeWord(buff, xfpos);
      }
      if( fp == null && check_ld )
      {	strcpy(&buff[strlen(buff) - strlen(INDEX_SUFFIX)], DATA_SUFFIX);
	fp = fopen(buff, "r");
	debug1(DFS,DD,fp==null ? "  failed on %s" : "  succeeded on %s", buff);
	if( fp != null )
	{ fclose(fp);
	  debug0(DFS, D, "SearchPath returning null (adjacent .ld file)");
	  return null;
	}
      }
    }
  }
  debug1(DFS, DD, "SearchPath returning (fp %s null)", fp==null ? "==" : "!=");
  return fp;
} /* end SearchPath */


/*****************************************************************************/
/*                                                                           */
/*  FILE *OpenFile(fnum, check_ld)                                           */
/*                                                                           */
/*  Open for reading the file whose number is fnum.  This involves           */
/*  searching for it along its path if not previously opened.                */
/*                                                                           */
/*  If check_ld is TRUE, it means that the file to be opened is a .li file   */
/*  and OpenFile() is required to check whether the corresponding .ld file   */
/*  is present.  If it is, then the search must stop.                        */
/*                                                                           */
/*****************************************************************************/

FILE *OpenFile(fnum, check_ld)
FILE_NUM fnum;  BOOLEAN check_ld;
{ FILE *fp;  OBJECT full_name, y;
  ifdebug(DPP, D, ProfileOn("OpenFile"));
  debug2(DFS, D, "OpenFile(%s, %s)", FileName(fnum), bool(check_ld));
  if( Down(fvec[fnum]) != fvec[fnum] )
  { Child(y, Down(fvec[fnum]));
    fp = fopen(string(y), "r");
    debug1(DFS,DD,fp==null ? "  failed on %s" : "  succeeded on %s", string(y));
  }
  else
  { fp = SearchPath(string(fvec[fnum]), file_path[path(fvec[fnum])],
		check_ld, &full_name, &fpos(fvec[fnum]));
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
unsigned char *str;  unsigned char typ;  OBJECT *full_name;  FILE_POS *xfpos;
{ FILE *fp;  int p;
  debug2(DFS, D, "OpenIncGraphicFile(%s, %s, -)", str, Image(typ));
  assert( typ == INCGRAPHIC || typ == SINCGRAPHIC, "OpenIncGraphicFile!" );
  p = (typ == INCGRAPHIC ? INCLUDE_PATH : SYSINCLUDE_PATH);
  fp = SearchPath(str, file_path[p], FALSE, full_name, xfpos);
  if( *full_name == nil )  *full_name = MakeWord(str, xfpos);
  debug2(DFS, D, "OpenIncGraphicFile returning (fp %s null, *full_name = %s)",
    fp==null ? "==" : "!=", string(*full_name));
  return fp;
} /* end OpenIncGraphicFile */


/*@@**************************************************************************/
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
  debug1(DFS, D, "ReadFromFile returning %s", EchoObject(null, res));
  ifdebug(DPP, D, ProfileOff("ReadFromFile"));
  return res;
} /* end ReadFromFile */


static FILE_NUM	last_write_fnum = NO_FILE;
static FILE	*last_write_fp  = null;


/*****************************************************************************/
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
	fputs(" ", last_write_fp);
	break;


      case NPAR:
      
	assert( Down(y) != y, "WriteObject/CLOSURE: NPAR!" );
	Child(z, Down(y));
	if( !name_printed )
	{ if( need_lvis(sym) )
	  { fputs(KW_LVIS, last_write_fp);
	    fputs(" ", last_write_fp);
	  }
	  fputs(SymName(sym), last_write_fp);
	  name_printed = TRUE;
	}
	fputs("\n   ", last_write_fp);
	fputs(SymName(actual(y)), last_write_fp);
	fprintf(last_write_fp, " %s ", KW_LBR);
	WriteObject(z, NO_PREC);
	fprintf(last_write_fp, " %s", KW_RBR);
	npar_seen = TRUE;
	break;


      case RPAR:
      
	assert( Down(y) != y, "WriteObject/CLOSURE: RPAR!" );
	Child(z, Down(y));
	if( !name_printed )
	{ if( need_lvis(sym) )
	  { fputs(KW_LVIS, last_write_fp);
	    fputs(" ", last_write_fp);
	  }
	  fputs(SymName(sym), last_write_fp);
	  name_printed = TRUE;
	}
	fputs(npar_seen ? "\n" : " ", last_write_fp);
	if( has_body(sym) )
	{ fputs(KW_LBR, last_write_fp);
	  fputs(" ", last_write_fp);
	  WriteObject(z, NO_PREC);
	  fputs(" ", last_write_fp);
	  fputs(KW_RBR, last_write_fp);
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
    { fputs(KW_LVIS, last_write_fp);
      fputs(" ", last_write_fp);
    }
    fputs(SymName(sym), last_write_fp);
    name_printed = TRUE;
  }
} /* end WriteClosure */

/*****************************************************************************/
/*                                                                           */
/*  static WriteObject(x, outer_prec)                                        */
/*                                                                           */
/*  Write object x to file last_write_fp, assuming it is a subobject of an   */
/*  object and the precedence of operators enclosing it is outer_prec.       */
/*                                                                           */
/*****************************************************************************/

static WriteObject(x, outer_prec)
OBJECT x;  int outer_prec;
{ OBJECT link, y, gap_obj, sym, env;  unsigned char *name;
  int prec, i, last_prec;  BOOLEAN braces_needed;
  switch( type(x) )
  {

    case WORD:

      if( strlen(string(x)) == 0 && outer_prec > ACAT_PREC )
      {	fputs(KW_LBR, last_write_fp);
	fputs(KW_RBR, last_write_fp);
      }
      else fputs(string(x), last_write_fp);
      break;

    
    case VCAT:  prec = VCAT_PREC;  goto ETC;
    case HCAT:  prec = HCAT_PREC;  goto ETC;
    case ACAT:  prec = ACAT_PREC;  goto ETC;

      ETC:
      if( prec < outer_prec )  fputs(KW_LBR, last_write_fp);
      last_prec = prec;
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	if( type(y) == GAP_OBJ )
	{ if( Down(y) == y )
	  { assert( type(x) == ACAT, "WriteObject: Down(y) == y!" );
	    for( i = 1;  i <= vspace(y);  i++ )  fputs("\n", last_write_fp);
	    for( i = 1;  i <= hspace(y);  i++ )  fputs(" ",  last_write_fp);
	    last_prec = (vspace(y) + hspace(y) == 0) ? JUXTA_PREC : ACAT_PREC;
	  }
	  else
	  { Child(gap_obj, Down(y));
	    fprintf(last_write_fp, type(x) == ACAT ? " %s" : "\n%s",
		EchoCatOp( (unsigned) type(x), mark(gap(y)), join(gap(y))));
	    if( type(gap_obj) != WORD || strlen(string(gap_obj)) != 0 )
		WriteObject(gap_obj, FORCE_PREC);
	    fputs(" ", last_write_fp);
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
      if( prec < outer_prec )  fputs(KW_RBR, last_write_fp);
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
	fputs(KW_LBR, last_write_fp);
	WriteClosure(y);
	fputs(KW_RBR, last_write_fp);
	fputs("\n",   last_write_fp);
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
      {	fputs(KW_ENV, last_write_fp);
      	fputs("\n", last_write_fp);
	WriteObject(env, NO_PREC);
      }

      /* print left brace if needed */
      if( braces_needed )  fputs(KW_LBR, last_write_fp);
	
      /* print the closure proper */
      WriteClosure(x);

      /* print closing brace if needed */
      if( braces_needed )  fputs(KW_RBR, last_write_fp);

      /* print closing environment if needed */
      if( env != nil )
      { fputs("\n", last_write_fp);
	fputs(KW_CLOS, last_write_fp);
      	fputs("\n", last_write_fp);
      }
      break;


    case CROSS:

      Child(y, Down(x));
      assert( type(y) == CLOSURE, "WriteObject/CROSS: type(y) != CLOSURE!" );
      fputs(SymName(actual(y)), last_write_fp);
      fputs(KW_CROSS, last_write_fp);
      Child(y, LastDown(x));
      WriteObject(y, FORCE_PREC);
      break;


    case NULL_CLOS:	name = (unsigned char *) KW_NULL;	goto SETC;
    case ONE_COL:	name = (unsigned char *) KW_ONE_COL;	goto SETC;
    case ONE_ROW:	name = (unsigned char *) KW_ONE_ROW;	goto SETC;
    case WIDE:		name = (unsigned char *) KW_WIDE;	goto SETC;
    case HIGH:		name = (unsigned char *) KW_HIGH;	goto SETC;
    case HSCALE:	name = (unsigned char *) KW_HSCALE;	goto SETC;
    case VSCALE:	name = (unsigned char *) KW_VSCALE;	goto SETC;
    case SCALE:		name = (unsigned char *) KW_SCALE;	goto SETC;
    case HCONTRACT:	name = (unsigned char *) KW_HCONTRACT;	goto SETC;
    case VCONTRACT:	name = (unsigned char *) KW_VCONTRACT;	goto SETC;
    case HEXPAND:	name = (unsigned char *) KW_HEXPAND;	goto SETC;
    case VEXPAND:	name = (unsigned char *) KW_VEXPAND;	goto SETC;
    case PADJUST:	name = (unsigned char *) KW_PADJUST;	goto SETC;
    case HADJUST:	name = (unsigned char *) KW_HADJUST;	goto SETC;
    case VADJUST:	name = (unsigned char *) KW_VADJUST;	goto SETC;
    case ROTATE:	name = (unsigned char *) KW_ROTATE;	goto SETC;
    case CASE:		name = (unsigned char *) KW_CASE;	goto SETC;
    case YIELD:		name = (unsigned char *) KW_YIELD;	goto SETC;
    case FONT:		name = (unsigned char *) KW_FONT;	goto SETC;
    case SPACE:		name = (unsigned char *) KW_SPACE;	goto SETC;
    case BREAK:		name = (unsigned char *) KW_BREAK;	goto SETC;
    case NEXT:		name = (unsigned char *) KW_NEXT;	goto SETC;
    case OPEN:		name = (unsigned char *) KW_OPEN;	goto SETC;
    case TAGGED:	name = (unsigned char *) KW_TAGGED;	goto SETC;
    case INCGRAPHIC:	name = (unsigned char *) KW_INCGRAPHIC;	goto SETC;
    case SINCGRAPHIC:	name = (unsigned char *) KW_SINCGRAPHIC;goto SETC;
    case GRAPHIC:	name = (unsigned char *) KW_GRAPHIC;	goto SETC;

      /* print left parameter, if present */
      SETC:
      if( DEFAULT_PREC <= outer_prec )  fputs(KW_LBR, last_write_fp);
      if( Down(x) != LastDown(x) )
      {	Child(y, Down(x));
	WriteObject(y, DEFAULT_PREC);
	fputs(" ", last_write_fp);
      }

      /* print the symbol's name */
      fputs(name, last_write_fp);

      /* print right parameter, if present */
      if( LastDown(x) != x )
      {	Child(y, LastDown(x));
	fputs(" ", last_write_fp);
	if( type(x) == OPEN )
	{ fputs(KW_LBR, last_write_fp);
	  WriteObject(y, NO_PREC);
	  fputs(KW_RBR, last_write_fp);
	}
	else WriteObject(y, DEFAULT_PREC);
      }
      if( DEFAULT_PREC <= outer_prec )  fputs(KW_RBR, last_write_fp);
      break;


    default:

      Error(INTERN, &fpos(x), "WriteObject: type(x) = %s", Image(type(x)));
      break;

  } /* end switch */
} /* end WriteObject */


/*****************************************************************************/
/*                                                                           */
/*  AppendToFile(x, fnum, pos)                                               */
/*                                                                           */
/*  Append object x to file fnum, returning its fseek position in *pos.      */
/*  Record the fact that this file has been updated.                         */
/*                                                                           */
/*****************************************************************************/

AppendToFile(x, fnum, pos)
OBJECT x;  FILE_NUM fnum;  int *pos;
{ unsigned char buff[MAX_LINE], *str;
  ifdebug(DPP, D, ProfileOn("AppendToFile"));
  debug2(DFS, D, "AppendToFile( %s, %s )", EchoObject(null, x), FileName(fnum));

  /* open file fnum for writing */
  if( last_write_fnum != fnum )
  { if( last_write_fnum != NO_FILE )  fclose(last_write_fp);
    str = FileName(fnum);
    if( strlen(str) + strlen(NEW_DATA_SUFFIX) >= MAX_LINE )
      Error(FATAL, PosOfFile(fnum), "file name %s%s is too long",
	str, NEW_DATA_SUFFIX);
    sprintf(buff, "%s%s", str, NEW_DATA_SUFFIX);
    last_write_fp = fopen(buff, "a");
    if( last_write_fp == null )  Error(FATAL, &fpos(fvec[fnum]),
		"cannot append to database file %s", buff);
    last_write_fnum = fnum;
  }

  /* write x out */
  *pos = (int) ftell(last_write_fp);
  fputs(KW_LBR, last_write_fp);
  WriteObject(x, NO_PREC);
  fprintf(last_write_fp, "%s\n\n", KW_RBR);

  /* record the fact that fnum has changed */
  updated(fvec[fnum]) = TRUE;
  ifdebug(DPP, D, ProfileOff("AppendToFile"));
  debug0(DFS, D, "AppendToFile returning.");
} /* end AppendToFile */


/*@@**************************************************************************/
/*                                                                           */
/*  CloseFiles()                                                             */
/*                                                                           */
/*  Close all files and move new versions to the names of old versions.      */
/*                                                                           */
/*****************************************************************************/

CloseFiles()
{ FILE_NUM fnum;
  unsigned char buff[MAX_LINE];
  ifdebug(DPP, D, ProfileOn("CloseFiles"));
  debug0(DFS, D, "CloseFiles()");

  /* close off last file opened by AppendToFile above */
  if( last_write_fnum != NO_FILE )  fclose(last_write_fp);

  /* get rid of old database files */
  for( fnum = FirstFile(SOURCE_FILE);  fnum != NO_FILE;  fnum = NextFile(fnum) )
  { sprintf(buff, "%s%s", FileName(fnum), DATA_SUFFIX);
    unlink(buff);
  }

  /* move any new database files to the old names, if updated */
  for( fnum = FirstFile(DATABASE_FILE); fnum != NO_FILE; fnum = NextFile(fnum) )
  { if( updated(fvec[fnum]) )
    { sprintf(buff, "%s%s", string(fvec[fnum]), NEW_DATA_SUFFIX);
      debug1(DFS, D, "unlink(%s)", string(fvec[fnum]));
      unlink(string(fvec[fnum])); /* may fail if old version does not exist */
      debug2(DFS, D, "link(%s, %s)", buff, string(fvec[fnum]));
      if( link(buff, string(fvec[fnum])) != 0 )
        Error(INTERN, no_fpos, "link(%s, %s) failed", buff, string(fvec[fnum]));
      debug1(DFS, D, "unlink(%s)", buff);
      if( unlink(buff) != 0 )
       	Error(INTERN, no_fpos, "unlink(%s) failed", buff);
    }
  }
  debug0(DFS, D, "CloseFiles returning.");
  ifdebug(DPP, D, ProfileOff("CloseFiles"));
} /* end CloseFiles */
