/*****************************************************************************/
/*                                                                           */
/*  C2LOUT: A PROGRAM TO CONVERT C AND C++ SOURCE INTO LOUT (VERSION 1.0)    */
/*  COPYRIGHT (C) 1993 Jeffrey H. Kingston                                   */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@cs.usyd.edu.au)                                */
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
/*****************************************************************************/
#include <stdio.h>

#define C2LOUT_VERSION	"c2lout Version 3.11 (December 1996)"
#define	BOOLEAN		unsigned
#define	FALSE		0
#define	TRUE		1
#define	MAX_LINE	1024

/* print styles */
#define	NO_STYLE	0
#define	FIXED_STYLE	1
#define	VARYING_STYLE	2
#define	SYMBOL_STYLE	3

static char	file_name[MAX_LINE];	/* current input file name           */
static int	line_num;		/* current input line number         */
static int	line_pos;		/* current input column number       */
static FILE	*err_fp;		/* where error messages go           */

static BOOLEAN	headers_option;		/* TRUE if no -n option (headers)    */
static int	style_option;		/* value of -p option, or NO_STYLE   */
static char	*font_option;		/* value of -f option, else null     */
static char	*size_option;		/* value of -s option, else null     */
static char	*line_option;		/* value of -v option, else null     */
static char	*tabin_option;		/* value of -t option, else null     */
static char	*tabout_option;		/* value of -T option, else null     */

static BOOLEAN	tab_by_spacing;		/* TRUE if using space chars to tab  */
static int	tab_in;			/* tab interval, value of -t option  */
static float	tab_out;		/* tab interval width (-T option)    */
static char 	tab_unit;		/* unit of measurement for tab       */
static BOOLEAN	Scan();

extern void ProcessStandAlone(char *fname, FILE *in_fp, FILE *out_fp);

#define Error0(str)							\
{									\
  if( line_num > 0 )							\
    fprintf(err_fp, "c2lout %s %d,%d: ", file_name, line_num, line_pos);\
  else									\
    fprintf(err_fp, "c2lout: ");					\
  fprintf(err_fp, str);						\
  fprintf(err_fp, "\n");						\
}

#define Error1(str, arg)							\
{									\
  if( line_num > 0 )							\
    fprintf(err_fp, "c2lout %s %d,%d: ", file_name, line_num, line_pos);\
  else									\
    fprintf(err_fp, "c2lout: ");					\
  fprintf(err_fp, str, arg);						\
  fprintf(err_fp, "\n");						\
}

#define GetArg(arg, message, null_ok)					\
{ if( strcmp(argv[i]+2, "") != 0 )					\
    arg = argv[i]+2;							\
  else if( !null_ok && i < argc-1 && *argv[i+1] != '-' )		\
    arg = argv[++i];							\
  else if( null_ok )							\
    arg = (char *) NULL;						\
  else									\
  { Error0(message);							\
    exit(1);								\
  }									\
} /* end GetArg */


/*****************************************************************************/
/*                                                                           */
/*  main(argc, argv)                                                         */
/*                                                                           */
/*  Read command line and process each file in turn.                         */
/*                                                                           */
/*****************************************************************************/

void main(int argc, char *argv[])
{ FILE *in_fp, *out_fp = stdout;
  BOOLEAN at_least_one_file, raw_seen;  int i;
  char *infilename, *outfilename, *errfilename, *str;

  /* read command line */
  in_fp = out_fp = NULL;
  err_fp = stderr;
  line_num = 0;
  raw_seen = FALSE;
  tab_by_spacing = TRUE;
  style_option = NO_STYLE;
  tab_in = 8;
  tab_out = 3;
  tab_unit = 'f';
  at_least_one_file = FALSE;
  headers_option = TRUE;
  font_option = size_option = line_option =
    tabin_option = tabout_option = (char *) NULL;
  for( i = 1;  i < argc;  i++ )
  { if( *argv[i] == '-' ) switch( *(argv[i]+1) )
    {
      case 'r':

	if( i > 1 )
	{ Error0("-r must be first if it occurs at all");
	  exit(1);
	}
	raw_seen = TRUE;
	break;


      case 'i':
     
	/* read name of input file */
	if( !raw_seen )
	{ Error0("-i illegal with -r");
	  exit(1);
	}
	if( in_fp != NULL )
	{ Error0("-i seen twice");
	  exit(1);
	}
	GetArg(infilename, "usage: -i<filename>", FALSE);

	/* open the file */
	in_fp = fopen(infilename, "r");
	if( in_fp == NULL )
	{ Error1("cannot open input file %s", infilename);
	  exit(1);
	}

	/* initialize file position */
	strcpy(file_name, infilename);
	line_num = 1;
	line_pos = 0;
	break;


      case 'o':
     
	/* read name of output file */
	if( out_fp != NULL )
	{ Error0("-o seen twice");
	  exit(1);
	}
	GetArg(outfilename, "usage: -o<filename>", FALSE);
	out_fp = fopen(outfilename, "w");
	if( out_fp == NULL )
	{ Error1("cannot open output file %s", outfilename);
	  exit(1);
	}
	break;


      case 'e':
     
	/* read name of error file */
	GetArg(errfilename, "usage: -e<filename>", FALSE);
	err_fp = fopen(errfilename, "w");
	if( err_fp == NULL )
	{ Error1("cannot open error file %s", errfilename);
	  exit(1);
	}
	break;


      case 'p':
     
	/* read print style */
	if( raw_seen )
	{ Error0("-p illegal with -r option");
	  exit(1);
	}
	GetArg(str, "usage: -p<printstyle>", FALSE);
	if( style_option != NO_STYLE )
	{ Error0("-p option appears twice");
	  exit(1);
	}
	else if( strcmp(str, "fixed")   == 0 )
	{ style_option = FIXED_STYLE;
	}
	else if( strcmp(str, "varying") == 0 )
	{ style_option = VARYING_STYLE;
	  tab_by_spacing = FALSE;
	}
	else if( strcmp(str, "symbol")  == 0 )
	{ style_option = SYMBOL_STYLE;
	  tab_by_spacing = FALSE;
	}
	else
	{ Error1("unknown -p option %s", str);
	  exit(1);
	}
	break;


      case 'f':
     
	/* read font family */
	if( raw_seen )
	{ Error0("-f illegal with -r option");
	  exit(1);
	}
	GetArg(font_option, "usage: -f<font>", FALSE);
	break;


      case 's':
     
	/* read font size */
	if( raw_seen )
	{ Error0("-s illegal with -r option");
	  exit(1);
	}
	GetArg(size_option, "usage: -s<size>", FALSE);
	break;


      case 'v':
     
	/* read line spacing */
	if( raw_seen )
	{ Error0("-v illegal with -r option");
	  exit(1);
	}
	GetArg(line_option, "usage: -v<vsize>", FALSE);
	break;


      case 't':
     
	/* read tab interval */
	GetArg(tabin_option, "usage: -t<number>", TRUE);
	if( tabin_option != NULL && sscanf(tabin_option,"%d",&tab_in) != 1 )
	{ Error0("usage: -t<number>\n");
	  exit(1);
	}
	if( tab_in <= 0 )
	{ Error0("-t: tab interval must be greater than 0\n");
	  exit(1);
	}
	break;


      case 'T':
     
	/* read tab_out and tab_unit */
	GetArg(tabout_option, "usage: -T<number><unit>", TRUE);
	if( tabout_option != NULL )
	{ if( sscanf(tabout_option, "%f%c",&tab_out,&tab_unit) != 2 )
	  { Error0("usage: -T<number><unit>\n");
	    exit(1);
	  }
	  if( tab_out <= 0 || tab_out >= 50 )
	  { Error0("-T: unreasonably large or small tab interval");
	    exit(1);
	  }
	  if( tab_unit != 'c' && tab_unit != 'i' && tab_unit != 'p' &&
	      tab_unit != 'm' && tab_unit != 'f' && tab_unit != 's' &&
	      tab_unit != 'v' )
	  { Error0("-T: tab unit must be one of cipmfsv\n");
	    exit(1);
	  }
	  tab_by_spacing = FALSE;
	}
	break;


      case 'n':
     
	if( raw_seen )
	{ Error0("-n illegal with -r option");
	  exit(1);
	}
	headers_option = FALSE;
	break;


      case 'V':
     
	if( raw_seen )
	{ Error0("-V illegal with -r option");
	  exit(1);
	}
	Error1("%s", C2LOUT_VERSION);
	exit(0);
	break;


      case 'u':
     
	if( raw_seen )
	{ Error0("-u illegal with -r option");
	  exit(1);
	}
	Error0("usage: c2lout C-files   or   c2lout -r Lout-files");
	exit(0);
	break;


      default:
     
	Error1("unknown command line flag %s", argv[i]);
	exit(1);
	break;

    }
    else
    {
	/* argument is source file, so open it */
	if( raw_seen )
	{ Error0("file parameter illegal with -r flag!");
	  exit(1);
	}
	at_least_one_file = TRUE;
	in_fp = fopen(argv[i], "r");
	if( in_fp == NULL )
	{ Error1("cannot open input file %s", argv[i]);
	  exit(1);
	}

	/* initialize file position */
	strcpy(file_name, argv[i]);
	line_num = 1;
	line_pos = 0;

	ProcessStandAlone(argv[i], in_fp, out_fp == NULL ? stdout : out_fp);
    }
  } /* for */

  /* raw case: filter in_fp to out_fp using Scan() */
  if( raw_seen )
  {
    /* check that input and output files are open */
    if( in_fp == NULL )
    { Error0("-r: missing -i option");
      exit(1);
    }
    if( out_fp == NULL )
    { Error0("-r: missing -o option");
      exit(1);
    }

    /* scan the file and leave unchanged if an error occurred */
    if( !Scan(in_fp, out_fp) )  exit(1);
  }

  /* finish of non-raw case with end text */
  else if( at_least_one_file )
    fprintf(out_fp == NULL ? stdout : out_fp, "@End @Text\n");

  exit(0);
} /* end main */


/*****************************************************************************/
/*                                                                           */
/*  ProcessStandAlone(fname, in_fp, out_fp)                                  */
/*                                                                           */
/*  Process one file consisting completely of C code.                        */
/*                                                                           */
/*****************************************************************************/

void ProcessStandAlone(char *fname, FILE *in_fp, FILE *out_fp)
{ static BOOLEAN first = TRUE;
  char *style_str, *font_str, *size_str, *line_str, *face_str,
       *tabin_str, *tabout_str;

  /* print heading information on out_fp */
  if( first )
  {
    /* sort out the options' values */
    switch( style_option )
    {

      case NO_STYLE:
      case FIXED_STYLE:

	style_str  = "fixed";
	face_str   = "Base";
	font_str   = font_option   != NULL ? font_option   : "Courier";
	size_str   = size_option   != NULL ? size_option   : "9p";
	line_str   = line_option   != NULL ? line_option   : "1.1fx";
	tabin_str  = tabin_option  != NULL ? tabin_option  : "8";
	tabout_str = tabout_option != NULL ? tabout_option : "8s";
	break;


      case VARYING_STYLE:

	style_str  = "varying";
	face_str   = "Slope";
	font_str   = font_option   != NULL ? font_option   : "Times";
	size_str   = size_option   != NULL ? size_option   : "10p";
	line_str   = line_option   != NULL ? line_option   : "1.1fx";
	tabin_str  = tabin_option  != NULL ? tabin_option  : "8";
	tabout_str = tabout_option != NULL ? tabout_option : "3f";
	break;


      case SYMBOL_STYLE:

	style_str  = "symbol";
	face_str   = "Slope";
	font_str   = font_option   != NULL ? font_option   : "Times";
	size_str   = size_option   != NULL ? size_option   : "10p";
	line_str   = line_option   != NULL ? line_option   : "1.1fx";
	tabin_str  = tabin_option  != NULL ? tabin_option  : "8";
	tabout_str = tabout_option != NULL ? tabout_option : "3f";
	break;


      default:

	Error0("internal error in -p option");
	exit(1);
	break;
    }
		 

    /* now print the initial @Use clauses etc.*/
    fprintf(out_fp, "@SysInclude { cdoc }\n");
    fprintf(out_fp, "@Use { @CP\n");
    fprintf(out_fp, "    style  { %s }\n", style_str);
    fprintf(out_fp, "    font   { %s }\n", font_str);
    fprintf(out_fp, "    size   { %s }\n", size_str);
    fprintf(out_fp, "    line   { %s }\n", line_str);
    fprintf(out_fp, "    tabin  { %s }\n", tabin_str);
    fprintf(out_fp, "    tabout { %s }\n", tabout_str);
    fprintf(out_fp, "{}\n");
    fprintf(out_fp, "}\n");
    fprintf(out_fp, "@Document\n");
    fprintf(out_fp, "    @InitialFont { \"%s\" \"%s\" \"%s\" }\n",
      font_str, face_str, size_str);
    fprintf(out_fp, "    @InitialBreak { lines \"%s\" nohyphen }\n", line_str);
    fprintf(out_fp, "//\n");
    fprintf(out_fp, "@Text @Begin\n\n");
    first = FALSE;
  }
  else fprintf(out_fp, "@NP\n\n");

  /* print file name and contents */
  if( headers_option )
    fprintf(out_fp, "{ Times Bold \"+3p\" } @Font \"%s\"\n@DP\n", fname);
  if( !Scan(in_fp, out_fp) )  exit(1);

} /* end ProcessStandAlone */


/*****************************************************************************/
/*                                                                           */
/*  EmitTab(out_fp)                                                          */
/*                                                                           */
/*  Emit the equivalent of one tab character, depending on whether we        */
/*  are using spaces or Lout tab operators to simulate it.                   */
/*                                                                           */
/*****************************************************************************/

void EmitTab(FILE *out_fp)
{
  if( tab_by_spacing )
  { putc(' ', out_fp);
    while( line_pos % tab_in != 0 )
    { putc(' ', out_fp);
      line_pos++;
    }
  }
  else
  { while( line_pos % tab_in != 0 )
    { line_pos++;
    }
    fprintf(out_fp, " $>%.1f%ct ",
      (line_pos/tab_in) * tab_out, tab_unit);
  }
} /* end EmitTab */


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN Scan(in_fp, out_fp)                                              */
/*                                                                           */
/*  Scan the C source beginning in file in_fp and write it in modified       */
/*  Lout source form onto out_fp.  Return TRUE if successful.                */
/*                                                                           */
/*****************************************************************************/

/* states of scanner */
#define	C_REGULAR		 1
#define	C_SLASH			 2
#define	C_COMMENT		 3
#define	CPP_COMMENT		 4
#define	LOUT_INSERT		 5
#define	CPP_LOUT_INSERT		 6
#define	LOUT_INSERT_STAR	 7
#define	C_COMMENT_STAR		 8
#define	C_STRING		 9
#define	C_STRING_BACKSLASH	10
#define	C_CHAR			11
#define	C_CHAR_BACKSLASH	12

static BOOLEAN Scan(FILE *in_fp, FILE *out_fp)
{ int state, ch;
  state = C_REGULAR;
  while( (ch = getc(in_fp)) != EOF )
  {
    if( ch == '\n' )
    { line_num++;
      line_pos = 0;
    }
    else line_pos++;

    switch( state )
    {

      case C_REGULAR:		/* in ordinary C code */

	if( ch == '\f' )
	{ fprintf(out_fp, "\n@NP\n");
	}
	else if( ch == '\t' )
	{ EmitTab(out_fp);
	}
	else if( ch == '#' )
	{ fputs("$$", out_fp);
	}
	else if( ch == '\\' )
	{ fputs("\"\\\\\"", out_fp);
	}
	else if( ch == '{' )  /*}*/
	{ fputs("${", out_fp);   /*}*/
	}
	else if( ch == /*{*/ '}' )
	{ /*{*/ fputs("$}", out_fp);
	}
	else if( ch == '@' )
	{ Error0("@ character in C program text");
	  return FALSE;
	}
	else if( ch == '/' )
	{ state = C_SLASH;
	}
	else if( ch == '\'' )
	{ fputs("{@L \"", out_fp);
	  state = C_CHAR;
	}
	else if( ch == '"' )
	{ fputs("{@S \"", out_fp); /*}*/
	  state = C_STRING;
	}
	else
	{ putc(ch, out_fp);
	}
	break;


      case C_SLASH:	/* in ordinary C code just after / */

	if( ch == '*' )
	{ int nextch = getc(in_fp);
	  if( nextch == EOF )
	  { Error0("unexpected end-of-file");
	    return FALSE;
	  }
	  else if( nextch == '@' )
	  { state = LOUT_INSERT;
	  }
	  else
	  { ungetc(nextch, in_fp);
	    if( nextch == '\n' )  line_num--;
	    fputs("{@C \"/*", out_fp);
	    state = C_COMMENT;
	  }
	}
	else if( ch == '/' ) /* C++ comment */
	{ int nextch = getc(in_fp);
	  if( nextch == EOF )
	  { Error0("unexpected end-of-file");
	    return FALSE;
	  }
	  else if( nextch == '@' )
	  { state = CPP_LOUT_INSERT;
	  }
	  else
	  { ungetc(nextch, in_fp);
	    if( nextch == '\n' )  line_num--;
	    fputs("{@C \"//", out_fp);
	    state = CPP_COMMENT;
	  }
	}
	else
	{ putc('/', out_fp);
	  ungetc(ch, in_fp);
	  if( ch == '\n' )  line_num--;
	  state = C_REGULAR;
	}
	break;


      case C_COMMENT:		/* inside a C comment */

	if( ch == '\t' )
	{ fputs("\"}", out_fp);
	  EmitTab(out_fp);
	  fputs("{@C \"", out_fp);
	}
	else if( ch == '\n' )
	{ int nextch = getc(in_fp);
	  if( nextch != EOF )
	  { fputs("\"}\n{@C \"", out_fp);
	  }
	  ungetc(nextch, in_fp);
	  if( ch == '\n' )  line_num--;
	}
	else if( ch == '\\' )
	{ fputs("\\\\", out_fp);
	}
	else if( ch == '"' )
	{ fputs("\\\"", out_fp);
	}
	else if( ch == '*' )
	{ putc(ch, out_fp);
	  state = C_COMMENT_STAR;
	}
	else
	{ putc(ch, out_fp);
	}
	break;


      case CPP_COMMENT:		/* inside a C++ comment */

	if( ch == '\t' )
	{ fputs("\"}", out_fp);
	  EmitTab(out_fp);
	  fputs("{@C \"", out_fp);
	}
	else if( ch == '\n' )
	{ fputs("\"}\n", out_fp);
	  state = C_REGULAR;
	}
	else if( ch == '\\' )
	{ fputs("\\\\", out_fp);
	}
	else if( ch == '"' )
	{ fputs("\\\"", out_fp);
	}
	else
	{ putc(ch, out_fp);
	}
	break;


      case LOUT_INSERT:		/* inside C comment which is a Lout insert */

	if( ch == '*' )
	{ state = LOUT_INSERT_STAR;
	}
	else
	{ putc(ch, out_fp);
	}
	break;

      
      case LOUT_INSERT_STAR:	/* inside Lout insert comment just after * */

	if( ch == '/' )
	{ state = C_REGULAR;
	}
	else if( ch == '*' )
	{ putc('*', out_fp);
	}
	else
	{ putc('*', out_fp);
	  putc(ch, out_fp);
	  state = LOUT_INSERT;
	}
	break;


      case CPP_LOUT_INSERT:	/* inside C++ comment which is a Lout insert */

	if( ch == '\n' )
	{ putc(ch, out_fp);
	  state = C_REGULAR;
	}
	else
	{ putc(ch, out_fp);
	}
	break;

      
      case C_COMMENT_STAR:	/* inside C comment just after * */

	if( ch == '/' )
	{ fputs("/\"}", out_fp);
	  state = C_REGULAR;
	}
	else if( ch == '*' )
	{ putc(ch, out_fp);
	}
	else if( ch == '\t' )
	{ fputs("\"}", out_fp);
	  EmitTab(out_fp);
	  fputs("{@C \"", out_fp);
	  state = C_COMMENT;
	}
	else if( ch == '\n' )
	{ fputs("\"}\n{@C \"", out_fp);
	  state = C_COMMENT;
	}
	else if( ch == '\\' )
	{ fputs("\\\\", out_fp);
	  state = C_COMMENT;
	}
	else if( ch == '"' )
	{ fputs("\\\"", out_fp);
	  state = C_COMMENT;
	}
	else
	{ putc(ch, out_fp);
	  state = C_COMMENT;
	}
	break;


      case C_STRING:		/* inside a C string */

	if( ch == '\t' )
	{ Error0("replaced tab character in string by \\t");
	  fputs("\\\\t", out_fp);
	}
	else if( ch == '\\' )
	{ fputs("\\\\", out_fp);
	  state = C_STRING_BACKSLASH;
	}
	else if( ch == '"' )
	{ /*{*/ fputs("\"}", out_fp);
	  state = C_REGULAR;
	}
	else if( ch == '\n' )
	{ Error0("unterminated C string");
	  /*{*/ fputs("\"}\n", out_fp);
	  state = C_REGULAR;
	}
	else putc(ch, out_fp);
	break;


      case C_STRING_BACKSLASH:	/* inside a C string just after \ */

	if( ch == '\t' )
	{ Error0("replacing literal tab character after \\ by t");
	  putc('t', out_fp);
	  state = C_STRING;
	}
	else if( ch == '\\' )
	{ fputs("\\\\", out_fp);
	  state = C_STRING;
	}
	else if( ch == '"' )
	{ fputs("\\\"", out_fp);
	  state = C_STRING;
	}
	else if( ch == '\n' )
	{ Error0("multi-line string printed as two strings, sorry");
	  fputs("\"}\n{@S \"", out_fp);
	  state = C_STRING;
	}
	else
	{ putc(ch, out_fp);
	  state = C_STRING;
	}
	break;


      case C_CHAR:	/* inside char literal */

	if( ch == '\t' )
	{ Error0("replacing literal tab character by \\t");
	  fputs("\\\\t", out_fp);
	}
	else if( ch == '\\' )
	{ fputs("\\\\", out_fp);
	  state = C_CHAR_BACKSLASH;
	}
	else if( ch == '"' )
	{ fputs("\\\"", out_fp);
	}
	else if( ch == '\n' )
	{ Error0("unterminated C character constant");
	  fputs("\"}\n", out_fp);
	  state = C_REGULAR;
	}
	else if( ch == '\'' )
	{ /*{*/ fputs("\"}", out_fp);
	  state = C_REGULAR;
	}
	else putc(ch, out_fp);
	break;



      case C_CHAR_BACKSLASH:	/* inside quoted char just after \ */

	if( ch == '"' )
	{ fputs("\\\"", out_fp);
	  state = C_CHAR;
	}
	else if( ch == '\\' )
	{ fputs("\\\\", out_fp);
	  state = C_CHAR;
	}
	else if( ch == '\n' )
	{ Error0("unterminated C character constant");
	  /*{*/ fputs("\"}\n", out_fp);
	  state = C_REGULAR;
	}
	else
	{ putc(ch, out_fp);
	  state = C_CHAR;
	}
	break;


      default:

	Error1("unknown case %d", state);
	exit(1);
	break;

    }
  }

  /* check that final state is reasonable */
  switch( state )
  {
    case C_REGULAR:		/* in ordinary C code */

      break;


    case C_SLASH:		/* in ordinary C code just after / */

      fputs("/\n", out_fp);
      break;


    case C_COMMENT:		/* inside a C comment */
    case CPP_COMMENT:		/* inside a C++ comment */
    case C_COMMENT_STAR:	/* inside C comment just after * */

      /*{*/ fputs("\"}\n", out_fp);
      Error0("C text ended inside a comment");
      break;


    case LOUT_INSERT:		/* inside C comment which is a Lout insert */
    case CPP_LOUT_INSERT:	/* inside C++ comment which is a Lout insert */
    case LOUT_INSERT_STAR:	/* inside Lout insert comment just after * */

      Error0("C text ended inside a Lout inclusion");
      exit(1);
      break;


    case C_STRING:		/* inside a C string */
    case C_STRING_BACKSLASH:	/* inside a C string just after \ */

      /*{*/ fputs("\"}\n", out_fp);
      Error0("C text ended inside a string literal");
      break;


    case C_CHAR:		/* inside char literal (has been quoted) */
    case C_CHAR_BACKSLASH:	/* inside quoted char just after \ */

      /*{*/ fputs("\"}\n", out_fp);
      Error0("C text ended inside a character literal");
      break;


    default:

      Error0("unknown final state");
      exit(1);
      break;
  }

  return TRUE;
} /* end Scan */
