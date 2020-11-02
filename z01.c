/*@z01.c:Supervise:StartSym, AllowCrossDb, etc.@******************************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.41)                       */
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
/*  FILE:         z01.c                                                      */
/*  MODULE:       Supervise                                                  */
/*  EXTERNS:      main(), StartSym, GalleySym, ForceGalleySym, InputSym,     */
/*                PrintSym, AllowCrossDb                                     */
/*                                                                           */
/*****************************************************************************/
#include "externs.h"
#include <signal.h>

/* On DOS/Win32 we need to set binary mode on stdout (Uwe) */
#if OS_DOS
#include <io.h>
#include <fcntl.h>
#ifdef __DJGPP__
#define _setmode(fd, mode)     setmode((fd), (mode))
#define _fileno(stream)        fileno((stream))
#endif
#endif

/*****************************************************************************/
/*                                                                           */
/*  MemCheck - check this memory location                                    */
/*                                                                           */
/*****************************************************************************/

POINTER MemCheck = 0;


/*****************************************************************************/
/*                                                                           */
/*  StartSym        the symbol table entry for \Start (overall scope)        */
/*  GalleySym       the symbol table entry for @Galley                       */
/*  ForceGalleySym  the symbol table entry for @ForceGalley                  */
/*  InputSym        the symbol table entry for @LInput                       */
/*  PrintSym        the symbol table entry for \Print (root target)          */
/*  OptGallSym      the symbol table entry for @OptGall (optimal galley rec) */
/*  FilterInSym     the symbol table entry for @FilterIn                     */
/*  FilterOutSym    the symbol table entry for @FilterOut                    */
/*  FilterErrSym    the symbol table entry for @FilterErr                    */
/*                                                                           */
/*****************************************************************************/

OBJECT StartSym, GalleySym, ForceGalleySym, InputSym, PrintSym, OptGallSym,
       FilterInSym, FilterOutSym, FilterErrSym, VerbatimSym, RawVerbatimSym;


/*****************************************************************************/
/*                                                                           */
/*  BackEnd             The back end (PostScript, PDF, etc.) to use          */
/*  CommandOptions      Command-line options (ACAT of objects)               */
/*  UseCollate          Use local collation sequence rather than ASCII       */
/*  AllowCrossDb        Allow references to OldCrossDb and NewCrossDb        */
/*  InMemoryDbIndexes   True if cr database index file is to be in-memory    */
/*  Kern                Do kerning                                           */
/*  SafeExecution       Execute safely, i.e. prohibit system() calls         */
/*  AltErrorFormat      Use alternative error message format                 */
/*  InitializeAll       TRUE if this is an initializing run.                 */
/*  MsgCat              category for locale-specific messages                */
/*  TotalWordCount      total number of words printed                        */
/*                                                                           */
/*****************************************************************************/

BACK_END BackEnd;
OBJECT CommandOptions;
BOOLEAN UseCollate;
BOOLEAN AllowCrossDb;
BOOLEAN InMemoryDbIndexes;
BOOLEAN Kern;
BOOLEAN SafeExecution;
BOOLEAN	AltErrorFormat;
BOOLEAN InitializeAll;
#if LOCALE_ON
nl_catd MsgCat;
#endif
int TotalWordCount;


/*****************************************************************************/
/*                                                                           */
/*  static OBJECT load(xstr, xpredefined, xleft, xright, xindef, xprec)      */
/*                                                                           */
/*  Load a predefined operator with these attributes into the symbol table.  */
/*  If the operator has parameters, load symbols for those also.             */
/*                                                                           */
/*****************************************************************************/

static OBJECT load(FULL_CHAR *xstr, unsigned xpre,
BOOLEAN xleft, BOOLEAN xright, BOOLEAN xindef, unsigned char xprec)
{ OBJECT s;
  s = InsertSym(xstr, LOCAL, no_fpos, xprec, xindef, FALSE, xpre,
	StartSym, nilobj);
  if( xleft )  InsertSym( AsciiToFull("pa"), LPAR, no_fpos, DEFAULT_PREC,
    FALSE, FALSE, 0, s, nilobj);
  if( xright )  InsertSym( AsciiToFull("pb"), RPAR, no_fpos, DEFAULT_PREC,
    FALSE, FALSE, 0, s, nilobj);
  if( xleft && xright && xpre != PLUS && xpre != MINUS )
    right_assoc(s) = TRUE;
  return s;
} /* end load */


/*****************************************************************************/
/*                                                                           */
/*  void PrintUsage(fp)                                                      */
/*  static void PrintVersion(lib, fp)                                        */
/*                                                                           */
/*  Print usage information / version information on file fp.                */
/*                                                                           */
/*****************************************************************************/
#define lputnl fputs( (char *) STR_NEWLINE, fp)
#define lput0(fmt)                { fprintf(fp,fmt);                 lputnl; }
#define lput1(fmt, p1)            { fprintf(fp,fmt, p1);             lputnl; }
#define lput2(fmt, p1, p2)        { fprintf(fp,fmt, p1, p2);         lputnl; }
#define lput3(fmt, p1, p2, p3)    { fprintf(fp,fmt, p1, p2, p3);     lputnl; }
#define lput4(fmt, p1, p2, p3, p4){ fprintf(fp,fmt, p1, p2, p3, p4); lputnl; }

void PrintUsage(FILE *fp)
{
  lputnl;
  lput0("usage:  lout options files"					    );
  lputnl;
  lput0("  -a              alternative error format:  file:line:col ..."    );
  lput0("  -c file         use file.li instead of lout.li for crossrefs"    );
  lput0("  -C directory    add directory to LCM file search path"	    );
  lput0("  -d<various>     debug the Lout run (if enabled in binary)"	    );
  lput0("  -D directory    add directory to database file search path"	    );
  lput0("  -e file         error messages to file instead of stderr"	    );
  lput0("  -EPS            EPS (Encapsulated PostScript) output"	    );
  lput0("  -F directory    add directory to font metrics file search path"  );
  lput0("  -h file         use hyphenation file"                            );
  lput0("  -H directory    add directory to hyphenation file search path"   );
  lput0("  -i file         like @SysInclude { file }; not recommended"	    );
  lput0("  -I directory    add directory to include file search path"	    );
  lput0("  -k              suppress all kerning"                            );
  lput0("  -l              ASCII collation order when sorting indexes etc." );
  lput0("  -L              locale collation order when sorting indexes etc.");
  lput0("  -m<addr>        monitor address during Lout run (for debugging)" );
  lput0("  -M              save memory (don't read in database indexes)"    );
  lput0("  -o file         output to file instead of stdout"		    );
  lput0("  -p              plain text output instead of PostScript"	    );
  lput0("  -P              like -p but with form-feed char between pages"   );
  lput0("  -PDF or -Z      PDF (Adobe Portable Document Format) output"     );
  lput0("  -r<integer>     run Lout <integer> times; print on last run only");
  lput0("  -s              suppress access to cross reference database"	    );
  lput0("  -S              safe execution (disable calls to system(3))"	    );
  lput0("  -t              ignore texture changes, always use solid colour" );
  lput0("  -u              print this usage message on stderr and exit"	    );
  lput0("  -U              unsafe execution (allow calls to system(3))"	    );
  lput0("  -V              print version and configuration information"	    );
  lput0("  -w              print total number of words in output"	    );
  lput0("  -x              initializing run, not for ordinary use"	    );
  lput0("  -Z              PDF (Adobe Portable Document Format) output"     );
  lput0("  --option{value} set option e.g. --'@InitialFont{Times Base 10p}'");
  lput0("  -               a file name denoting standard input"		    );
  lputnl;
} /* end PrintUsage */

static void PrintVersion(FULL_CHAR *lib, FILE *fp)
{
  lput1("%s", LOUT_VERSION);
  lput2("%-28s %s",
    "Basser Lout written by:", "Jeffrey H. Kingston (jeff@it.usyd.edu.au)");
  lput2("%-28s %s",
    "Free source available from:", "ftp://ftp.it.usyd.edu.au/jeff/lout");
  lput3("%-28s %s %s",
    "This executable compiled:", __TIME__, __DATE__);
  lput4("%-28s %s%s%s", "System include directory:", lib, STR_DIR, INCL_DIR);
  lput4("%-28s %s%s%s", "System database directory:", lib, STR_DIR, DATA_DIR);
  lput1("Database index files created afresh automatically:%s",
    USE_STAT ? " yes" : " no");
  lput1("Safe execution (disabling system()) is default:%s",
    SAFE_DFT ? " yes" : " no");
  lput1("strcoll() used for sorting by default:%s", COLLATE ? " yes" : " no");
  lput1("PDF compression on:%s", PDF_COMPRESSION ? " yes" : " no");
  lput1("Debugging (-d, -dd, -ddd flags) available:%s", DEBUG_ON ?" yes":" no");
  lputnl;
  lput0("Basser Lout comes with ABSOLUTELY NO WARRANTY.");
  lput0("This is free software, and you are welcome to");
  lput0("redistribute it under certain conditions.  For");
  lput0("details on both points, consult the GNU General");
  lput0("Public License (distributed with this software).");
} /* end PrintVersion */


/*@::GetArg(), main()@********************************************************/
/*                                                                           */
/*  GetArg(argv, argc, i)                                                    */
/*                                                                           */
/*  Get the next argument from the command line and return it.               */
/*  Return NULL if it isn't there.                                           */
/*                                                                           */
/*****************************************************************************/

static FULL_CHAR *GetArg(char *argv[], int argc, int *i)
{ if( !StringEqual(AsciiToFull(argv[*i]+2), STR_EMPTY) )
    return AsciiToFull(argv[*i]+2);
  else if( *i < argc-1 && *argv[*i + 1] != CH_HYPHEN )
    return AsciiToFull(argv[(*i)++ +1]);
  else
    return (FULL_CHAR *) NULL;
} /* end GetArg */


/*****************************************************************************/
/*                                                                           */
/*  void run(int argc, char *argv[], int *runs_to_do, FULL_CHAR *lib)        */
/*                                                                           */
/*  Carry out one run of Lout.  If *runs_to_do is -1, set it.                */
/*                                                                           */
/*****************************************************************************/

typedef enum {
  BE_PLAIN,
  BE_PS,
  BE_PDF
} BE_TYPE;

static void run(int argc, char *argv[], int run_num, int *runs_to_do,
  FULL_CHAR *lib)
{ int i, len;  FULL_CHAR *arg;
  OBJECT t, y, res, s;			/* current token, parser output      */
  BOOLEAN stdin_seen;			/* TRUE when stdin file seen         */
  int source_file_count;		/* number of source files in command */
  FULL_CHAR *cross_db;			/* name of cross reference database  */
  FULL_CHAR *outfile;			/* name of output file               */
  FILE *out_fp;
  long MemCheckLong;
  FULL_CHAR oname[MAX_BUFF], oval[MAX_BUFF], buff[MAX_BUFF], *p;
  int bp, runcount;  OBJECT z;
  BOOLEAN seen_wordcount, encapsulated;
  BE_TYPE be_type;

  /* initialise various modules, add current directory to search paths */
  TotalWordCount = 0;
  seen_wordcount = FALSE;
  be_type = BE_PS;
  PlainCharWidth = PLAIN_WIDTH;
  PlainCharHeight = PLAIN_HEIGHT;
  PlainFormFeed = FALSE;
  InitializeAll = FALSE;
  UseCollate = COLLATE;
  AllowCrossDb = TRUE;
  InMemoryDbIndexes = TRUE;
  encapsulated = FALSE;
  SafeExecution = SAFE_DFT ? TRUE : FALSE;
  Kern = TRUE;
  ErrorInit();
  MemInit();
  InitSym();
  LexInit();
  InitFiles();
  AddToPath(SOURCE_PATH,   MakeWord(WORD, STR_EMPTY, no_fpos));
  AddToPath(DATABASE_PATH, MakeWord(WORD, STR_EMPTY, no_fpos));
  AddToPath(INCLUDE_PATH,  MakeWord(WORD, STR_EMPTY, no_fpos));

  /* read command line */
  stdin_seen = FALSE;
  AltErrorFormat = FALSE;
  cross_db = CROSS_DB;
  outfile = STR_STDOUT;
  source_file_count = 0;
  New(CommandOptions, ACAT);
  for( i = 1;  i < argc;  i++ )
  {
    if( *argv[i] == CH_HYPHEN ) switch( *(argv[i]+1) )
    {
      case CH_FLAG_OUTFILE:
     
	/* read name of output file */
	if( (outfile = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 7, "usage: -o <filename>", FATAL_WITH_USAGE, no_fpos);
	if( StringEndsWith(outfile, SOURCE_SUFFIX) )
	  Error(1, 28, "-o: output file name %s ends with %s",
	    FATAL, no_fpos, outfile, SOURCE_SUFFIX);
	break;


      case CH_FLAG_RUNS:
     
	/* multiple runs */
	if( sscanf(argv[i]+2, "%d", &runcount) != 1 )
	  Error(1, 32, "usage: -r<integer>", FATAL_WITH_USAGE, no_fpos);
	if( runcount <= 0 || runcount > 20 )
	  Error(1, 33, "invalid value of -r", FATAL_WITH_USAGE, no_fpos);
	assert(*runs_to_do == -1 || *runs_to_do == runcount, "-r");
	*runs_to_do = runcount;
	break;


      case CH_FLAG_SUPPRESS:
     
	/* suppress references to OldCrossDb and NewCrossDb */
	AllowCrossDb = FALSE;
	break;


      case CH_FLAG_MEMCR:
     
	/* don't use in-memory database indexes */
	InMemoryDbIndexes = FALSE;
	break;


      case CH_FLAG_NOKERN:
     
	/* suppress kerning */
	Kern = FALSE;
	break;


      case CH_FLAG_NOCOLLATE:
     
	/* suppress local collation */
	UseCollate = FALSE;
	break;


      case CH_FLAG_COLLATE:
     
	/* invoke local collation */
	UseCollate = TRUE;
	break;


      case CH_FLAG_CROSS:
     
	/* read name of cross reference database */
	if( (cross_db = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 8, "usage: -c <filename>", FATAL_WITH_USAGE, no_fpos);
	break;


      case CH_FLAG_ERRFILE:
     
	/* read log file name */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 9, "usage: -e <filename>", FATAL_WITH_USAGE, no_fpos);
	ErrorSetFile(arg);
	break;


      case CH_FLAG_ALTERR:
     
	/* alternative error message format */
	AltErrorFormat = TRUE;
	break;


      case CH_FLAG_EPSFIRST:
     
	/* -EPS produces encapsulated PostScript output */
	if( !StringEqual(AsciiToFull(argv[i]+1), STR_EPS) )
	  Error(1, 10, "usage: -EPS", FATAL_WITH_USAGE, no_fpos);
	encapsulated = TRUE;
	break;


      case CH_FLAG_DIRPATH:
     
	/* add directory to database and sysdatabase paths */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 11, "usage: -D <directoryname>", FATAL_WITH_USAGE, no_fpos);
	AddToPath(DATABASE_PATH, MakeWord(WORD, arg, no_fpos));
	AddToPath(SYSDATABASE_PATH, MakeWord(WORD, arg, no_fpos));
	break;


      case CH_FLAG_ENCPATH:
     
	/* add directory to character mapping path */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 12, "usage: -C <directoryname>", FATAL_WITH_USAGE, no_fpos);
	AddToPath(MAPPING_PATH, MakeWord(WORD, arg, no_fpos));
	break;


      case CH_FLAG_FNTPATH:
     
	/* add directory to font path */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 13, "usage: -F <directoryname>", FATAL_WITH_USAGE, no_fpos);
	AddToPath(FONT_PATH, MakeWord(WORD, arg, no_fpos));
	break;


      case CH_FLAG_HYPPATH:
     
	/* add directory to hyph path */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 14, "usage: -H <directoryname>", FATAL_WITH_USAGE, no_fpos);
	AddToPath(HYPH_PATH, MakeWord(WORD, arg, no_fpos));
	break;


      case CH_FLAG_INCPATH:
     
	/* add directory to include and sysinclude paths */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 15, "usage: -I <directoryname>", FATAL_WITH_USAGE, no_fpos);
	AddToPath(INCLUDE_PATH, MakeWord(WORD, arg, no_fpos));
	AddToPath(SYSINCLUDE_PATH, MakeWord(WORD, arg, no_fpos));
	break;


      case CH_FLAG_INCLUDE:
     
	/* read sysinclude file and strip any .lt suffix */
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 16, "usage: -i <filename>", FATAL_WITH_USAGE, no_fpos);
	len = StringLength(arg) - StringLength(SOURCE_SUFFIX);
	if( len >= 0 && StringEqual(&arg[len], SOURCE_SUFFIX) )
	  StringCopy(&arg[len], STR_EMPTY);
	debug0(DFS, D, "  calling DefineFile from main (1)");
	DefineFile(arg, STR_EMPTY, no_fpos, SOURCE_FILE, SYSINCLUDE_PATH);
	break;


      case CH_FLAG_HYPHEN:
     
	/* declare hyphenation file */
	if( FirstFile(HYPH_FILE) != NO_FILE )
	  Error(1, 17, "two -h options illegal", FATAL_WITH_USAGE, no_fpos);
	if( (arg = GetArg(argv, argc, &i)) == NULL )
	  Error(1, 18, "usage: -h <filename>", FATAL_WITH_USAGE, no_fpos);
	debug0(DFS, D, "  calling DefineFile from main (2)");
	DefineFile(arg, STR_EMPTY, no_fpos, HYPH_FILE, INCLUDE_PATH);
	DefineFile(arg, HYPH_SUFFIX, no_fpos, HYPH_PACKED_FILE, INCLUDE_PATH);
	break;


      case CH_FLAG_NOTEXTURE:
     
	UseTexture = FALSE;
	break;


      case CH_FLAG_VERSION:
     
	PrintVersion(lib, stderr);
	exit(0);
	break;


      case CH_FLAG_WORDS:
     
	seen_wordcount = TRUE;
	break;


      case CH_FLAG_PDF:

	be_type = BE_PDF;
	break;


      case CH_FLAG_FFPLAIN:

	if( StringEqual(AsciiToFull(argv[i]+1), STR_PDF) )
	{
	  be_type = BE_PDF;
	  break;
	}
	PlainFormFeed = TRUE;
	/* NB NO BREAK */


      case CH_FLAG_PLAIN:
     
	be_type = BE_PLAIN;
	if( *(argv[i]+2) != '\0' )
	{ float len1, len2;  FULL_CHAR units1, units2;
	  if( sscanf(argv[i]+2, "%f%c%f%c",&len1,&units1,&len2,&units2) != 4 )
	  { Error(1, 19, "usage: lout -%c<length><length>",
	      FATAL, no_fpos, *(argv[i]+1));
	  }
	  switch( units1 )
	  {
	    case CH_UNIT_CM:	PlainCharWidth = len1 * CM; break;
	    case CH_UNIT_IN:	PlainCharWidth = len1 * IN; break;
	    case CH_UNIT_PT:	PlainCharWidth = len1 * PT; break;
	    case CH_UNIT_EM:	PlainCharWidth = len1 * EM; break;

	    default:	Error(1, 20, "lout -%c: units must be c, i, p, or m",
				  FATAL, no_fpos, *(argv[i]+1));
				break;
	  }
	  switch( units2 )
	  {
	    case CH_UNIT_CM:	PlainCharHeight = len2 * CM; break;
	    case CH_UNIT_IN:	PlainCharHeight = len2 * IN; break;
	    case CH_UNIT_PT:	PlainCharHeight = len2 * PT; break;
	    case CH_UNIT_EM:	PlainCharHeight = len2 * EM; break;

	    default:	Error(1, 21, "lout -%c: units must be c, i, p, or m",
				  FATAL, no_fpos, *(argv[i]+1));
				break;
	  }
	}
	break;


      case CH_FLAG_INITALL:

	InitializeAll = TRUE;
	AllowCrossDb = FALSE;
	break;


      case CH_FLAG_USAGE:

	PrintUsage(stderr);
	exit(0);
	break;


      case CH_FLAG_DEBUG:
     
	debug_init(AsciiToFull(argv[i]));
	break;


      case CH_FLAG_MEMCHECK:

	sscanf(argv[i], "-m%ld", &MemCheckLong);
	MemCheck = (POINTER) MemCheckLong;
	fprintf(stderr, "checking memory location %ld%s",
	  (long) MemCheck, (char *) STR_NEWLINE);
	break;


      case '\0':
     
	/* read stdin as file name */
	if( stdin_seen )
	  Error(1, 23, "standard input specified twice", FATAL, no_fpos);
	stdin_seen = TRUE;
	debug0(DFS, D, "  calling DefineFile from main (3)");
	DefineFile(STR_STDIN, STR_EMPTY, no_fpos, SOURCE_FILE, SOURCE_PATH);
	break;


      case CH_FLAG_OPTION:

	/* read command-line document option */
	if( sscanf(argv[i]+2, "%[^{ ] { %[^}] }", oname, oval) != 2 ||
	  StringLength(oname) == 0 || StringLength(oval) == 0 )
	  Error(1, 24, "error in command-line option %s", FATAL, no_fpos,
	    argv[i]+2);
	debug2(DSP, D, "  command line option [%s] [%s]", oname, oval);
	y = MakeWord(WORD, oname, no_fpos);
	Link(CommandOptions, y);
	New(y, ACAT);
	Link(CommandOptions, y);
	bp = 0;
	for( p = oval;  *p != '\0';  p++ )  switch( *p )
	{
	  case ' ':
	  case '\t':
	  case CH_CR:
	  case CH_LF:
	  case '{':
	  case '}':

	    if( bp > 0 )
	    { buff[bp++] = '\0';
	      if( Down(y) != y ) 
	      { OBJECT g;
		New(g, GAP_OBJ);
		hspace(g) = 1;  vspace(g) = 0;
		FposCopy(fpos(g), *no_fpos);
		Link(y, g);
	      }
	      z = MakeWord(WORD, buff, no_fpos);
	      Link(y, z);
	      bp = 0;
	    }
	    break;


	  default:

	    buff[bp++] = *p;
	    break;
	}
	if( bp > 0 )
	{ buff[bp++] = '\0';
	  if( Down(y) != y ) 
	  { OBJECT g;
	    New(g, GAP_OBJ);
	    hspace(g) = 1;  vspace(g) = 0;
	    FposCopy(fpos(g), *no_fpos);
	    Link(y, g);
	  }
	  z = MakeWord(WORD, buff, no_fpos);
	  Link(y, z);
	}
	if( Down(y) == y )
	  Error(1, 25, "error in command-line option %s", FATAL, no_fpos,
	    argv[i]+2);
	break;


      case CH_FLAG_SAFE:

	/* ensure safe execution by disabling system calls */
	SafeExecution = TRUE;
	break;

      case CH_FLAG_UNSAFE:

	/* allow unsafe execution */
	SafeExecution = FALSE;
	break;

      default:
     
	Error(1, 26, "unknown command line flag %s", FATAL_WITH_USAGE,
	  no_fpos, argv[i]);
	break;

    }
    else
    {   /* argument is source file, strip any .lout suffix and define it */
	arg = AsciiToFull(argv[i]);
	len = StringLength(arg) - StringLength(SOURCE_SUFFIX);
	if( len >= 0 && StringEqual(&arg[len], SOURCE_SUFFIX) )
	  StringCopy(&arg[len], STR_EMPTY);
	debug0(DFS, D, "  calling DefineFile from main (4)");
	DefineFile(AsciiToFull(argv[i]), STR_EMPTY, no_fpos,
	    SOURCE_FILE, SOURCE_PATH);
	source_file_count++;
    }
  } /* for */
  if( *runs_to_do == -1 )
    *runs_to_do = 1;

  if( UseCollate )
  {
    if (!setlocale (LC_COLLATE, ""))
      Error(1, 30, "unable to initialize collation", WARN, no_fpos);
  }

  /* start timing if required */
  ifdebug(DPP, D, ProfileOn("main"));

  /* sort out output file and back end */
  if( run_num == *runs_to_do )
  {
    /* last run, so open output file (or stdout if none specified) */
    if( StringEqual(outfile, STR_STDOUT) )
    {
#if OS_DOS
      /* For DOS/Win32 we need to set binary mode on stdout to prevent
	 PDF compressed streams and xrefs from being corrupted - Uwe 12/98 */
      if( be_type != BE_PLAIN && _setmode(_fileno(stdout), _O_BINARY) == -1 )
	Error(1, 31, "cannot set binary mode on stdout", FATAL, no_fpos);
#endif
      out_fp = stdout;
    }
    else
    { out_fp = StringFOpen(outfile, WRITE_FILE);
      if( out_fp == null )
	Error(1, 27, "cannot open output file %s", FATAL, no_fpos, outfile);
    }
    if( be_type == BE_PLAIN )
      BackEnd = Plain_BackEnd;
    else if( be_type == BE_PS )
      BackEnd = PS_BackEnd;
    else
      BackEnd = PDF_BackEnd;
    BackEnd->PrintInitialize(out_fp, encapsulated);
  }
  else
  {
    /* not last run, so use a null backend */
    if( be_type == BE_PLAIN )
      BackEnd = Plain_NullBackEnd;
    else
      BackEnd = PS_NullBackEnd;
    BackEnd->PrintInitialize(NULL, encapsulated);
  }

  /* initialize miscellaneous modules */
  ColourInit();
  TextureInit();
  LanguageInit();

  /* append default directories to file search paths */
  AddToPath(FONT_PATH,      MakeWordThree(lib, STR_DIR, AsciiToFull(FONT_DIR)));
  AddToPath(HYPH_PATH,      MakeWordThree(lib, STR_DIR, AsciiToFull(HYPH_DIR)));
  AddToPath(MAPPING_PATH,   MakeWordThree(lib, STR_DIR, AsciiToFull(MAPS_DIR)));
  AddToPath(SYSDATABASE_PATH,MakeWordThree(lib,STR_DIR, AsciiToFull(DATA_DIR)));
  AddToPath(DATABASE_PATH,  MakeWordThree(lib, STR_DIR, AsciiToFull(DATA_DIR)));
  AddToPath(SYSINCLUDE_PATH,MakeWordThree(lib, STR_DIR, AsciiToFull(INCL_DIR)));
  AddToPath(INCLUDE_PATH,   MakeWordThree(lib, STR_DIR, AsciiToFull(INCL_DIR)));

  /* use stdin if no source files were mentioned */
  if( source_file_count == 0 )
  { debug0(DFS, D, "  calling DefineFile from main (5)");
    DefineFile(STR_STDIN, STR_EMPTY, no_fpos, SOURCE_FILE, SOURCE_PATH);
  }

  /* load predefined symbols into symbol table */
  StartSym      = nilobj;  /* Not a mistake */
  StartSym      = load(KW_START,        0, FALSE,  FALSE,  TRUE,  NO_PREC     );
  GalleySym     = load(KW_GALLEY,       0, FALSE,  FALSE,  TRUE,  NO_PREC     );
  ForceGalleySym= load(KW_FORCE_GALLEY, 0, FALSE,  FALSE,  TRUE,  NO_PREC     );
  InputSym      = load(KW_INPUT,        0, FALSE,  FALSE,  TRUE,  NO_PREC     );
  PrintSym      = load(KW_PRINT,        0, FALSE,  FALSE,  TRUE,  NO_PREC     );
  FilterInSym   = load(KW_FILTERIN,     0, FALSE,  FALSE,  FALSE, NO_PREC     );
  FilterOutSym  = load(KW_FILTEROUT,    0, FALSE,  FALSE,  FALSE, NO_PREC     );
  FilterErrSym  = load(KW_FILTERERR,    0, FALSE,  FALSE,  FALSE, NO_PREC     );
  OptGallSym    = load(KW_OPTGALL,      0, FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  VerbatimSym   = load(KW_VERBATIM,VERBATIM,FALSE, TRUE,   FALSE, DEFAULT_PREC);
  RawVerbatimSym= load(KW_RAWVERBATIM,RAW_VERBATIM,FALSE,TRUE,FALSE,DEFAULT_PREC);


  load(KW_BEGIN,        BEGIN,          FALSE,  FALSE,  FALSE, BEGIN_PREC  );
  load(KW_END,          END,            FALSE,  FALSE,  FALSE, END_PREC    );
  load(KW_ENV,          ENV,            FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_ENVA,         ENVA,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_ENVB,         ENVB,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_ENVC,         ENVC,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_ENVD,         ENVD,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_CENV,         CENV,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_CLOS,         CLOS,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_LVIS,         LVIS,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_LUSE,         LUSE,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_LEO,          LEO,            FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_LBR,          LBR,            FALSE,  FALSE,  FALSE, LBR_PREC    );
  load(KW_RBR,          RBR,            FALSE,  FALSE,  FALSE, RBR_PREC    );
  load(KW_INCLUDE,      INCLUDE,        FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_SYSINCLUDE,   SYS_INCLUDE,    FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_PREPEND,      PREPEND,        FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_SYSPREPEND,   SYS_PREPEND,    FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_INCG_REPEATED,INCG_REPEATED,  FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_SINCG_REPEATED,SINCG_REPEATED,FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_DATABASE,     DATABASE,       FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_SYSDATABASE,  SYS_DATABASE,   FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_USE,          USE,            FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_NOT_REVEALED, NOT_REVEALED,   FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_CASE,         CASE,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_YIELD,        YIELD,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_BACKEND,      BACKEND,        FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_XCHAR,        XCHAR,          FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_FONT,         FONT,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_SPACE,        SPACE,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_YUNIT,        YUNIT,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ZUNIT,        ZUNIT,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_BREAK,        BREAK,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_UNDERLINE,    UNDERLINE,      FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_UNDERLINE_COLOUR, UNDERLINE_COLOUR,TRUE,TRUE, FALSE, DEFAULT_PREC);
  load(KW_UNDERLINE_COLOR, UNDERLINE_COLOUR,TRUE,TRUE,  FALSE, DEFAULT_PREC);
  load(KW_COLOUR,       COLOUR,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_COLOR,        COLOUR,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_TEXTURE,      TEXTURE,        TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_OUTLINE,      OUTLINE,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_LANGUAGE,     LANGUAGE,       TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_CURR_LANG,    CURR_LANG,      FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_CURR_FAMILY,  CURR_FAMILY,    FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_CURR_FACE,    CURR_FACE,      FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_CURR_YUNIT,   CURR_YUNIT,     FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_CURR_ZUNIT,   CURR_ZUNIT,     FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_COMMON,       COMMON,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_RUMP,         RUMP,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_MELD,         MELD,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_INSERT,       INSERT,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ONE_OF,       ONE_OF,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_NEXT,         NEXT,           FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_PLUS,         PLUS,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_MINUS,        MINUS,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_OPEN,         OPEN,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_TAGGED,       TAGGED,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_WIDE,         WIDE,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HIGH,         HIGH,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HSHIFT,       HSHIFT,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VSHIFT,       VSHIFT,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_BEGIN_HEADER, BEGIN_HEADER,   TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_END_HEADER,   END_HEADER,     FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_SET_HEADER,   SET_HEADER,     TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_CLEAR_HEADER, CLEAR_HEADER,   FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_ONE_COL,      ONE_COL,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ONE_ROW,      ONE_ROW,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HMIRROR,      HMIRROR,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VMIRROR,      VMIRROR,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HSCALE,       HSCALE,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VSCALE,       VSCALE,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HCOVER,       HCOVER,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VCOVER,       VCOVER,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_KERN_SHRINK,  KERN_SHRINK,    TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_SCALE,        SCALE,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HCONTRACT,    HCONTRACT,      FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VCONTRACT,    VCONTRACT,      FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HLIMITED,     HLIMITED,       FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VLIMITED,     VLIMITED,       FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HEXPAND,      HEXPAND,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VEXPAND,      VEXPAND,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_STARTHVSPAN,  START_HVSPAN,   FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_STARTHSPAN,   START_HSPAN,    FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_STARTVSPAN,   START_VSPAN,    FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HSPAN,        HSPAN,          FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_VSPAN,        VSPAN,          FALSE,  FALSE,  FALSE, DEFAULT_PREC);
  load(KW_PADJUST,      PADJUST,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HADJUST,      HADJUST,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VADJUST,      VADJUST,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ROTATE,       ROTATE,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_BACKGROUND,   BACKGROUND,     TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_INCGRAPHIC,   INCGRAPHIC,     FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_SINCGRAPHIC,  SINCGRAPHIC,    FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_PLAINGRAPHIC, PLAIN_GRAPHIC,  TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_GRAPHIC,      GRAPHIC,        TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_LINK_SOURCE,  LINK_SOURCE,    TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_LINK_DEST,    LINK_DEST,      TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_LINK_URL,     LINK_URL,       TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_CROSS,        CROSS,          TRUE,   TRUE,   FALSE, CROSSOP_PREC);
  load(KW_FORCE_CROSS,  FORCE_CROSS,    TRUE,   TRUE,   FALSE, CROSSOP_PREC);
  load(KW_NULL,         NULL_CLOS,      FALSE,  FALSE,  TRUE,  NO_PREC     );
  load(KW_PAGE_LABEL,   PAGE_LABEL,     FALSE,  TRUE,   TRUE,  DEFAULT_PREC);
  load(KW_SET_CONTEXT,  SET_CONTEXT,    TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_GET_CONTEXT,  GET_CONTEXT,    FALSE,   TRUE,   FALSE, DEFAULT_PREC);

#define setcat(s, mk, jn)  has_mark(s)=mk, has_join(s)=jn

  s=load(KW_VCAT_NN, VCAT, TRUE, TRUE, FALSE, VCAT_PREC); setcat(s,FALSE,FALSE);
  s=load(KW_VCAT_MN, VCAT, TRUE, TRUE, FALSE, VCAT_PREC); setcat(s,TRUE, FALSE);
  s=load(KW_VCAT_NJ, VCAT, TRUE, TRUE, FALSE, VCAT_PREC); setcat(s,FALSE,TRUE);
  s=load(KW_VCAT_MJ, VCAT, TRUE, TRUE, FALSE, VCAT_PREC); setcat(s,TRUE, TRUE);
  s=load(KW_HCAT_NN, HCAT, TRUE, TRUE, FALSE, HCAT_PREC); setcat(s,FALSE,FALSE);
  s=load(KW_HCAT_MN, HCAT, TRUE, TRUE, FALSE, HCAT_PREC); setcat(s,TRUE, FALSE);
  s=load(KW_HCAT_NJ, HCAT, TRUE, TRUE, FALSE, HCAT_PREC); setcat(s,FALSE,TRUE);
  s=load(KW_HCAT_MJ, HCAT, TRUE, TRUE, FALSE, HCAT_PREC); setcat(s,TRUE, TRUE);
  s=load(KW_ACAT_NJ, ACAT, TRUE, TRUE, FALSE, ACAT_PREC); setcat(s,FALSE,TRUE);
  s=load(KW_ACAT_MJ, ACAT, TRUE, TRUE, FALSE, ACAT_PREC); setcat(s,TRUE, TRUE);

  /* intialize fonts and load @FontDef symbol */
  FontInit();

  /* intialize current time and load @Moment symbol */
  InitTime();

  /* initialize filter module */
  FilterInit();

  /* initialize enviroment table module, etc. */
  CrossInitModule();
  EnvInit();
  DbInit();
  HyphInit();
  MapInit();
  ReadFromFileInit();
  PromoteInit();

  /* initialise scope chain to <StartSym> */
  PushScope(StartSym, FALSE, FALSE);

  /* initialise lexical analyser */
  LexPush(FirstFile(SOURCE_FILE), 0, SOURCE_FILE, 1, FALSE);

  /* alternative processing which tests running time of lexer+parser only */
  /* ***
  if( TRUE )
  {
    InitParser(cross_db);
    t = NewToken(BEGIN, no_fpos, 0, 0, BEGIN_PREC, StartSym);
    res = Parse(&t, StartSym, TRUE, FALSE);
    exit(0);
    return 0;
  }
  *** */

  /* process input files */
  InitParser(cross_db);
  t = NewToken(BEGIN, no_fpos, 0, 0, BEGIN_PREC, StartSym);
  res = Parse(&t, StartSym, TRUE, TRUE);
  DisposeObject(CommandOptions);
  debug0(DGT, D, "calling TransferEnd(res) from main()");
  TransferEnd(res);
  TransferClose();

  /* close various modules */
  BackEnd->PrintAfterLastPage();
  BackEnd->LinkCheck();
  CrossClose();
  CloseFiles();

  /* remove any leftover filter temporary files */
  FilterScavenge(TRUE);

  /* print word count, if required */
  if( seen_wordcount && run_num == *runs_to_do )
    Error(1, 29, "total of all words printed: %d", WARN,no_fpos,TotalWordCount);

  /* check for unbalanced error blocks */
  CheckErrorBlocks();

  /* wrapup */
  ifdebug(DST, D, CheckSymSpread() );
  ifdebug(ANY, D, DeleteEverySym() );
  debug0(DMA, D, "at end of run:");
  ifdebug(DMA, D, DebugMemory() );
  ifdebug(DPP, D, ProfileOff("main"));
  ifdebug(DPP, D, ProfilePrint());
  ifdebug(DET, D, EnvDebug());

} /* end run */


/*****************************************************************************/
/*                                                                           */
/*  main(argc, argv)                                                         */
/*                                                                           */
/*  Read command line, initialise everything, read definitions, read         */
/*  galleys, clean up and exit.                                              */
/*                                                                           */
/*****************************************************************************/

int main(int argc, char *argv[])
{ 
  FULL_CHAR *lib;			/* name of library directory         */
  int run_num, runs_to_do;
#if LOCALE_ON
  char catname[MAX_BUFF], *loc;
#endif

  /* find the name of the library directory, from envt or else from -D */
  lib = AsciiToFull(getenv("LOUTLIB"));
  if( lib == (FULL_CHAR *) NULL )
    lib = AsciiToFull(LIB_DIR);

  /* set locale if that's what we are doing */
#if LOCALE_ON
  loc = setlocale(LC_MESSAGES, "");
  if( loc == (char *) NULL )
  { Error(1, 6, "unable to initialize locale", WARN, no_fpos);
    loc = "C";
  }
  sprintf(catname, "%s/%s/%s/LC_MESSAGES/errors.%s",
    lib, LOCALE_DIR, loc, loc);
  MsgCat = catopen(catname, 0);
#endif

  run_num = 1;  runs_to_do = -1;
  do
  {
    if( run_num > 1 )
      Error(1, 34, "lout -r beginning run %d:", WARN, no_fpos, run_num);
    run(argc, argv, run_num, &runs_to_do, lib);
    run_num++;
  }
  while( run_num <= runs_to_do );

#if LOCALE_ON
  catclose(MsgCat);
#endif

  exit(0);
  return 0;
} /* end main */
