/*@z01.c:Supervise:StartSym, AllowCrossDb, Encapsulated, etc.@****************/
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
/*  FILE:         z01.c                                                      */
/*  MODULE:       Supervise                                                  */
/*  EXTERNS:      main(), StartSym, GalleySym, InputSym, PrintSym,           */
/*                AllowCrossDb, Encapsulated                                 */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  StartSym      the symbol table entry for \Start (overall scope)          */
/*  GalleySym     the symbol table entry for @Galley                         */
/*  InputSym      the symbol table entry for @LInput                         */
/*  PrintSym      the symbol table entry for \Print (root target)            */
/*                                                                           */
/*****************************************************************************/

OBJECT StartSym, GalleySym, InputSym, PrintSym;

/*****************************************************************************/
/*                                                                           */
/*  AllowCrossDb        Allow references to OldCrossDb and NewCrossDb        */
/*  Encapsulated        Produce a one-page encapsulated PostScript file      */
/*                                                                           */
/*****************************************************************************/

BOOLEAN AllowCrossDb;
BOOLEAN Encapsulated;


/*****************************************************************************/
/*                                                                           */
/*  static OBJECT load(xstr, xpredefined, xleft, xright, xindef, xprec)      */
/*                                                                           */
/*  Load a predefined operator with these attributes into the symbol table.  */
/*  If the operator has parameters, load symbols for those also.             */
/*                                                                           */
/*****************************************************************************/

static OBJECT load(xstr, xpre, xleft, xright, xindef, xprec)
FULL_CHAR *xstr;  unsigned  xpre;  BOOLEAN xleft, xright, xindef;
unsigned char xprec;
{ OBJECT s;
  s = InsertSym(xstr, LOCAL, no_fpos, xprec, xindef, FALSE, xpre, StartSym,nil);
  if( xleft )  InsertSym( AsciiToFull("pa"), LPAR, no_fpos, DEFAULT_PREC,
    FALSE, FALSE, 0, s, nil);
  if( xright )  InsertSym( AsciiToFull("pb"), RPAR, no_fpos, DEFAULT_PREC,
    FALSE, FALSE, 0, s, nil);
  if( xleft && xright )  right_assoc(s) = TRUE;
  return s;
} /* end load */


/*@::GetArg(), main()@********************************************************/
/*                                                                           */
/*  GetArg(arg, message)                                                     */
/*                                                                           */
/*  Get the next argument from the command line and store it in arg.         */
/*  Print message as a fatal error if it isn't there.                        */
/*                                                                           */
/*****************************************************************************/

#define GetArg(arg, message)						\
{ if( !StringEqual(AsciiToFull(argv[i]+2), STR_EMPTY) )			\
    arg = AsciiToFull(argv[i]+2);					\
  else if( i < argc-1 && *argv[i+1] != CH_HYPHEN )			\
    arg = AsciiToFull(argv[i++ +1]);					\
  else									\
    Error(FATAL, no_fpos, message);					\
} /* end GetArg */


/*****************************************************************************/
/*                                                                           */
/*  main(argc, argv)                                                         */
/*                                                                           */
/*  Read command line, initialise everything, read definitions, read         */
/*  galleys, clean up and exit.                                              */
/*                                                                           */
/*****************************************************************************/

main(argc, argv)
int argc; char *argv[];
{ int i, len;  FULL_CHAR *arg;
  OBJECT t, res, s;			/* current token, parser output      */
  BOOLEAN stdin_seen;			/* TRUE when stdin file seen         */
  FULL_CHAR *cross_db;			/* name of cross reference database  */
  FULL_CHAR *outfile;			/* name of output file               */
  FILE *out_fp;

  /* initialise various modules, add current directory to search paths */
  AllowCrossDb = TRUE;
  Encapsulated = FALSE;
  InitSym();
  LexInit();
  MemInit();
  InitFiles();
  AddToPath(SOURCE_PATH,   STR_EMPTY);
  AddToPath(DATABASE_PATH, STR_EMPTY);
  AddToPath(INCLUDE_PATH,  STR_EMPTY);

  /* read command line */
  stdin_seen = FALSE;
  cross_db = CROSS_DB;
  outfile = STR_STDOUT;
  for( i = 1;  i < argc;  i++ )
  { if( *argv[i] == CH_HYPHEN ) switch( *(argv[i]+1) )
    {
      case CH_FLAG_OUTFILE:
     
	/* read name of output file */
	GetArg(outfile, "usage: -o<filename>");
	break;


      case CH_FLAG_SUPPRESS:
     
	/* suppress references to OldCrossDb and NewCrossDb */
	AllowCrossDb = FALSE;
	break;


      case CH_FLAG_CROSS:
     
	/* read name of cross reference database */
	GetArg(cross_db, "usage: -c<filename>");
	break;


      case CH_FLAG_ERRFILE:
     
	/* read log file name */
	GetArg(arg, "usage: -e<filename>");
	ErrorInit(arg);
	break;


      case CH_FLAG_EPSFIRST:
     
	/* -EPS produces encapsulated PostScript output */
	if( !StringEqual(AsciiToFull(argv[i]+1), STR_EPS) )
	  Error(FATAL, no_fpos, "usage: -EPS");
	Encapsulated = TRUE;
	break;


      case CH_FLAG_DIRPATH:
     
	/* add directory to database and sysdatabase paths */
	GetArg(arg, "usage: -D<dirname>");
	AddToPath(DATABASE_PATH, arg);
	AddToPath(SYSDATABASE_PATH, arg);
	break;


      case CH_FLAG_ENCPATH:
     
	/* add directory to encoding path */
	GetArg(arg, "usage: -C<dirname>");
	AddToPath(ENCODING_PATH, arg);
	break;


      case CH_FLAG_FNTPATH:
     
	/* add directory to font path */
	GetArg(arg, "usage: -F<dirname>");
	AddToPath(FONT_PATH, arg);
	break;


      case CH_FLAG_INCPATH:
     
	/* add directory to include and sysinclude paths */
	GetArg(arg, "usage: -I<dirname>");
	AddToPath(INCLUDE_PATH, arg);
	AddToPath(SYSINCLUDE_PATH, arg);
	break;


      case CH_FLAG_INCLUDE:
     
	/* read sysinclude file and strip any .lt suffix */
	GetArg(arg, "usage: -i<filename>");
	len = StringLength(arg) - StringLength(SOURCE_SUFFIX);
	if( len >= 0 && StringEqual(&arg[len], SOURCE_SUFFIX) )
	  StringCopy(&arg[len], STR_EMPTY);
	DefineFile(arg, STR_EMPTY, no_fpos,
	  SOURCE_FILE, SYSINCLUDE_PATH);
	break;


      case CH_FLAG_HYPHEN:
     
	/* declare hyphenation file */
	if( FirstFile(HYPH_FILE) != NO_FILE )
	  Error(FATAL, no_fpos, "two -h options illegal");
	GetArg(arg, "usage: -h<filename>");
	DefineFile(arg, STR_EMPTY, no_fpos,
	  HYPH_FILE, INCLUDE_PATH);
	DefineFile(arg, HYPH_SUFFIX, no_fpos,
	  HYPH_PACKED_FILE, INCLUDE_PATH);
	break;


      case CH_FLAG_VERSION:
     
	fprintf(stderr, "%s\n", LOUT_VERSION);
	break;


      case CH_FLAG_USAGE:
     
	fprintf(stderr, "usage: lout [ -i<filename> ] files\n");
	exit(0);
	break;


      case CH_FLAG_DEBUG:
     
	debug_init(argv[i]);
	break;


      case '\0':
     
	/* read stdin as file name */
	if( stdin_seen )  Error(FATAL, no_fpos, "stdin read twice!");
	stdin_seen = TRUE;
	DefineFile(STR_STDIN, STR_EMPTY, no_fpos, SOURCE_FILE, SOURCE_PATH);
	break;


      default:
     
	Error(FATAL, no_fpos, "unknown command line flag %s", argv[i]);
	break;

    }
    else
    {   /* argument is source file, strip any .lout suffix and define it */
	arg = argv[i];
	len = StringLength(arg) - StringLength(SOURCE_SUFFIX);
	if( len >= 0 && StringEqual(&arg[len], SOURCE_SUFFIX) )
	  StringCopy(&arg[len], STR_EMPTY);
	DefineFile(AsciiToFull(argv[i]), STR_EMPTY, no_fpos,
	    SOURCE_FILE, SOURCE_PATH);
    }
  } /* for */

  /* define hyphenation file if not done already by -h flag */
  if( FirstFile(HYPH_FILE) == NO_FILE )
  { DefineFile(HYPH_FILENAME, STR_EMPTY, no_fpos, HYPH_FILE, SYSINCLUDE_PATH);
    DefineFile(HYPH_FILENAME, HYPH_SUFFIX, no_fpos,
      HYPH_PACKED_FILE, SYSINCLUDE_PATH);
  }

  /* start timing if required */
  ifdebug(DPP, D, ProfileOn("main"));

  /* open output file, or stdout if none specified, and initialize printer */
  if( StringEqual(outfile, STR_STDOUT) )  out_fp = stdout;
  else if( (out_fp = StringFOpen(outfile, "w")) == null )
    Error(FATAL, no_fpos, "cannot open output file %s", outfile);
  FontInit();
  PrintInit(out_fp);

  /* append default directories to file search paths */
  AddToPath(FONT_PATH,         AsciiToFull(FONT_DIR));
  AddToPath(ENCODING_PATH,     AsciiToFull(EVEC_DIR));
  AddToPath(SYSDATABASE_PATH,  AsciiToFull(DATA_DIR));
  AddToPath(DATABASE_PATH,     AsciiToFull(DATA_DIR));
  AddToPath(SYSINCLUDE_PATH,   AsciiToFull(INCL_DIR));
  AddToPath(INCLUDE_PATH,      AsciiToFull(INCL_DIR));

  /* use stdin if no source files were mentioned */
  if( FirstFile(SOURCE_FILE) == NO_FILE )
    DefineFile(STR_STDIN, STR_EMPTY, no_fpos, SOURCE_FILE, SOURCE_PATH);

  /* load predefined symbols into symbol table */
  StartSym = nil;  /* Not a mistake */
  StartSym  = load(KW_START,    0,     FALSE,  FALSE,  TRUE,  NO_PREC     );
  GalleySym = load(KW_GALLEY,   0,     FALSE,  FALSE,  TRUE,  NO_PREC     );
  InputSym  = load(KW_INPUT,    0,     FALSE,  FALSE,  TRUE,  NO_PREC     );
  PrintSym  = load(KW_PRINT,    0,     FALSE,  FALSE,  TRUE,  NO_PREC     );

  load(KW_BEGIN,       BEGIN,          FALSE,  FALSE,  FALSE, BEGIN_PREC  );
  load(KW_END,         END,            FALSE,  FALSE,  FALSE, END_PREC    );
  load(KW_ENV,         ENV,            FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_CLOS,        CLOS,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_LVIS,        LVIS,           FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_LBR,         LBR,            FALSE,  FALSE,  FALSE, LBR_PREC    );
  load(KW_RBR,         RBR,            FALSE,  FALSE,  FALSE, RBR_PREC    );
  load(KW_INCLUDE,     INCLUDE,        FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_SYSINCLUDE,  SYS_INCLUDE,    FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_PREPEND,     PREPEND,        FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_SYSPREPEND,  SYS_PREPEND,    FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_DATABASE,    DATABASE,       FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_SYSDATABASE, SYS_DATABASE,   FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_USE,         USE,            FALSE,  FALSE,  FALSE, NO_PREC     );
  load(KW_CASE,        CASE,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_YIELD,       YIELD,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_XCHAR,       XCHAR,          FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_FONT,        FONT,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_SPACE,       SPACE,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_BREAK,       BREAK,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_NEXT,        NEXT,           FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_OPEN,        OPEN,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_TAGGED,      TAGGED,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HIGH,        HIGH,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_WIDE,        WIDE,           TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ONE_COL,     ONE_COL,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ONE_ROW,     ONE_ROW,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HSCALE,      HSCALE,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VSCALE,      VSCALE,         FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_SCALE,       SCALE,          TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HCONTRACT,   HCONTRACT,      FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VCONTRACT,   VCONTRACT,      FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HEXPAND,     HEXPAND,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VEXPAND,     VEXPAND,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_PADJUST,     PADJUST,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_HADJUST,     HADJUST,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_VADJUST,     VADJUST,        FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_ROTATE,      ROTATE,         TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_INCGRAPHIC,  INCGRAPHIC,     FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_SINCGRAPHIC, SINCGRAPHIC,    FALSE,  TRUE,   FALSE, DEFAULT_PREC);
  load(KW_GRAPHIC,     GRAPHIC,        TRUE,   TRUE,   FALSE, DEFAULT_PREC);
  load(KW_CROSS,       CROSS,          TRUE,   TRUE,   FALSE, CROSSOP_PREC);
  load(KW_NULL,        NULL_CLOS,      FALSE,  FALSE,  TRUE,  NO_PREC     );

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

  /* intialize current time and load @Moment symbol */
  InitTime();

  /* initialise scope chain to <StartSym> */
  PushScope(StartSym, FALSE, FALSE);

  /* initialise lexical analyser */
  LexPush(FirstFile(SOURCE_FILE), 0, SOURCE_FILE);

  /* process input files */
  InitParser(cross_db);
  t = NewToken(BEGIN, no_fpos, 0, 0, BEGIN_PREC, StartSym);
  res = Parse(&t, StartSym, TRUE, TRUE);
  TransferEnd(res);
  TransferClose();

  /* close various  modules */
  PrintClose();
  CrossClose();
  CloseFiles();

  /* wrapup */
  ifdebug(DST, D, CheckSymSpread() );
  debug0(ANY, D, "commencing deletes");
  ifdebug(ANY, D, DeleteEverySym() );
  ifdebug(DMA, D, DebugMemory() );
  ifdebug(DPP, D, ProfileOff("main"));
  ifdebug(DPP, D, ProfilePrint());
  exit(0);
  return 0;
} /* end main */
