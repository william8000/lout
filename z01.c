/*@z01.c:Supervise:main()@****************************************************/
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
/*  FILE:         z01.c                                                      */
/*  MODULE:       Supervise                                                  */
/*  EXTERNS:      main()                                                     */
/*                                                                           */
/*****************************************************************************/
#include "externs"

/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN StringBeginsWith(str, pattern)                                   */
/*  BOOLEAN StringContains(str, pattern)                                     */
/*                                                                           */
/*  Check whether str begins with (or contains within it) pattern.  This     */
/*  could be done by the standard function "strstr" except that not all      */
/*  systems have it and in at least one case the implementation has a bug.   */
/*                                                                           */
/*****************************************************************************/

BOOLEAN StringBeginsWith(str, pattern)
unsigned char *str, *pattern;
{ unsigned char *sp, *pp;
  sp = str;  pp = pattern;
  while( *sp != '\0' && *pp != '\0' )
  { if( *sp++ != *pp++ )  return FALSE;
  }
  return (*pp == '\0');
} /* end StringBeginsWith */

BOOLEAN StringContains(str, pattern)
unsigned char *str, *pattern;
{ unsigned char *sp;
  for( sp = str;  *sp != '\0';  sp++ )
  { if( StringBeginsWith(sp, pattern) )  return TRUE;
  }
  return FALSE;
} /* end StringContains */


/*****************************************************************************/
/*                                                                           */
/*  StartSym      the symbol table entry for \Start (overall scope)          */
/*  GalleySym     the symbol table entry for @Galley                         */
/*  InputSym      the symbol table entry for @Input@                         */
/*  PrintSym      the symbol table entry for \Print (root target)            */
/*                                                                           */
/*****************************************************************************/

OBJECT StartSym, GalleySym, InputSym, PrintSym;

/*****************************************************************************/
/*                                                                           */
/*  AllowCrossDb        Allow references to OldCrossDb and NewCrossDb        */
/*                                                                           */
/*****************************************************************************/

BOOLEAN AllowCrossDb;

/*****************************************************************************/
/*                                                                           */
/*  Encapsulated        Produce a one-page encapsulated PostScript file      */
/*                                                                           */
/*****************************************************************************/

BOOLEAN Encapsulated;


/*****************************************************************************/
/*                                                                           */
/*  OBJECT load(xstr, xpredefined, xleft, xright, xindef, xprec)             */
/*                                                                           */
/*  Load a predefined operator with these attributes into the symbol table.  */
/*  If the operator has parameters, load symbols for those also.             */
/*                                                                           */
/*****************************************************************************/

static OBJECT load(xstr, xpre, xleft, xright, xindef, xprec)
unsigned char *xstr;  unsigned  xpre;  BOOLEAN xleft, xright, xindef;
unsigned char xprec;
{ OBJECT s;
  s = InsertSym(xstr, LOCAL, no_fpos, xprec, xindef, FALSE, xpre, StartSym,nil);
  if( xleft )
    InsertSym("pa", LPAR, no_fpos, DEFAULT_PREC, FALSE, FALSE, 0, s, nil);
  if( xright )
    InsertSym("pb", RPAR, no_fpos, DEFAULT_PREC, FALSE, FALSE, 0, s, nil);
  if( xleft && xright )  right_assoc(s) = TRUE;
  return s;
} /* end load */


/*@@**************************************************************************/
/*                                                                           */
/*  main(argc, argv)                                                         */
/*                                                                           */
/*  Read command line, initialise everything, read definitions, read         */
/*  galleys, clean up and exit.                                              */
/*                                                                           */
/*****************************************************************************/

main(argc, argv)
int argc; unsigned char *argv[];
{ int i;
  OBJECT t, res, s;				/* current token, parser o/p */
  BOOLEAN stdin_seen;				/* TRUE when stdin file seen */
  unsigned char *cross_db;			/* name of cross ref database*/
  unsigned char *outfile;			/* name of output file       */
  FILE *out_fp;

  /* initialise various modules, add current directory to search paths */
  AllowCrossDb = TRUE;
  Encapsulated = FALSE;
  InitSym();
  LexInit();
  MemInit();
  InitFiles();
  AddToPath(FONT_PATH,     "");
  AddToPath(SOURCE_PATH,   "");
  AddToPath(DATABASE_PATH, "");
  AddToPath(INCLUDE_PATH,  "");

  /* read command line */
  stdin_seen = FALSE;
  cross_db = (unsigned char *) CROSS_DB;
  outfile = (unsigned char *) "-";
  for( i = 1;  i < argc;  i++ )
  {  if( *argv[i] == '-' ) switch( *(argv[i]+1) )
     {
	case 'o':	/* read name of output file */
			if( *(argv[i]+2) == '\0' )
				Error(FATAL, no_fpos, "usage: -o<filename>");
			outfile = argv[i]+2;
			break;

	case 's':	/* suppress references to OldCrossDb and NewCrossDb */
			AllowCrossDb = FALSE;
			break;

	case 'c':	/* read name of cross reference database */
			cross_db = argv[i]+2;
			break;

	case 'e':	/* read log file name */
			if( *(argv[i]+2) == '\0' )
				Error(FATAL, no_fpos, "usage: -e<filename>");
			ErrorInit(argv[i]+2);
			break;

	case 'E':	/* -EPS produces encapsulated PostScript output */
			if( strcmp(argv[i]+1, "EPS") != 0 )
			  Error(FATAL, no_fpos, "usage: -EPS");
			Encapsulated = TRUE;
			break;

	case 'D':	/* add directory to database and sysdatabase paths */
			if( *(argv[i]+2) == '\0' )
				Error(FATAL, no_fpos, "usage: -D<dirname>");
			AddToPath(DATABASE_PATH, argv[i]+2);
			AddToPath(SYSDATABASE_PATH, argv[i]+2);
			break;

	case 'F':	/* add directory to font path */
			if( *(argv[i]+2) == '\0' )
				Error(FATAL, no_fpos, "usage: -F<dirname>");
			AddToPath(FONT_PATH, argv[i]+2);
			break;

	case 'I':	/* add directory to include and sysinclude paths */
			if( *(argv[i]+2) == '\0' )
				Error(FATAL, no_fpos, "usage: -I<dirname>");
			AddToPath(INCLUDE_PATH, argv[i]+2);
			AddToPath(SYSINCLUDE_PATH, argv[i]+2);
			break;

	case 'i':	/* read sysinclude file */
			if( *(argv[i]+2) == '\0' )
				Error(FATAL, no_fpos, "usage: -i<filename>");
			t = MakeWord(argv[i]+2, no_fpos);
			DefineFile(t, SOURCE_FILE, SYSINCLUDE_PATH);
			break;

	case 'h':	/* declare hyphenation file */
			if( FirstFile(HYPH_FILE) != NO_FILE )
			  Error(FATAL, no_fpos, "two -h options illegal");
			if( *(argv[i]+2) == '\0' )
			  Error(FATAL, no_fpos, "usage: -h<filename>");
			if( strlen(argv[i]+2) + strlen(HYPH_SUFFIX) >= MAX_LINE)			  Error(FATAL, no_fpos, "-h option too long");
			t = MakeWord(argv[i] + 2, no_fpos);
			DefineFile(t, HYPH_FILE, INCLUDE_PATH);
			t = MakeWordTwo(string(t), HYPH_SUFFIX, no_fpos);
			DefineFile(t, HYPH_PACKED_FILE, INCLUDE_PATH);
			break;

	case 'V':	fprintf(stderr, "%s\n", LOUT_VERSION);
			break;

	case 'd':	debug_init(argv[i]);
			break;

	case '\0':	/* read stdin as file name */
			if( stdin_seen )
				Error(FATAL, no_fpos, "stdin read twice!");
			stdin_seen = TRUE;
			t = MakeWord("-", no_fpos);
			DefineFile(t, SOURCE_FILE, SOURCE_PATH);
			break;

	default:	Error(FATAL, no_fpos,
				"unknown command line flag %s", argv[i]);
			break;
     }
     else DefineFile(MakeWord(argv[i], no_fpos), SOURCE_FILE, SOURCE_PATH);
  } /* for */

  /* define hyphenation file if not done already by -h flag */
  if( FirstFile(HYPH_FILE) == NO_FILE )
  { t = MakeWord(HYPH_FILENAME, no_fpos);
    DefineFile(t, HYPH_FILE, SYSINCLUDE_PATH);
    t = MakeWordTwo(HYPH_FILENAME, HYPH_SUFFIX, no_fpos);
    DefineFile(t, HYPH_PACKED_FILE, SYSINCLUDE_PATH);
  }

  /* start timing if required */
  ifdebug(DPP, D, ProfileOn("main"));

  /* open output file, or stdout if none specified, and initialize printer */
  if( strcmp(outfile, "-") == 0 )  out_fp = stdout;
  else if( (out_fp = fopen(outfile, "w")) == null )
    Error(FATAL, no_fpos, "cannot open output file %s", outfile);
  PrintInit(out_fp);

  /* append default directories to file search paths */
  AddToPath(FONT_PATH,         FONT_DIR);
  AddToPath(SYSDATABASE_PATH,  DATA_DIR);
  AddToPath(DATABASE_PATH,     DATA_DIR);
  AddToPath(SYSINCLUDE_PATH,   INCL_DIR);
  AddToPath(INCLUDE_PATH,      INCL_DIR);

  /* use stdin if no source files were mentioned */
  if( FirstFile(SOURCE_FILE) == NO_FILE )
    DefineFile(MakeWord("-", no_fpos), SOURCE_FILE, SOURCE_PATH);

  /* load predefined symbols into symbol table */
  StartSym = nil;
  StartSym  = load("\\Start",   0,     FALSE,  FALSE,  TRUE,  NO_PREC     );
  GalleySym = load(KW_GALLEY,   0,     FALSE,  FALSE,  TRUE,  NO_PREC     );
  InputSym  = load(KW_INPUT,    0,     FALSE,  FALSE,  TRUE,  NO_PREC     );
  PrintSym  = load("\\Print",   0,     FALSE,  FALSE,  TRUE,  NO_PREC     );

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
