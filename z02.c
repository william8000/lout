/*@z02.c:Lexical Analyser:LexInit(), LexGetToken()@***************************/
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
/*  FILE:         z02.c                                                      */
/*  MODULE:       Lexical Analyser                                           */
/*  EXTERNS:      LexLegalName(), LexInit(), LexPush(), LexPop(),            */
/*                LexNextTokenPos(), LexGetToken()                           */
/*                                                                           */
/*  Implementation note:  this fast and cryptic lexical analyser is adapted  */
/*  from Waite, W. M.: The Cost of Lexical Analysis, in Software - Practice  */
/*  and Experience, v16, pp473-488 (May 1986).                               */
/*                                                                           */
/*****************************************************************************/
#include "externs"

#define	BUFFER_SIZE    8192		/* size of buffer for block read     */

#define	WEIRD		0		/* unknown character type            */
#define	LETTER		1		/* letter type                       */
#define	SPECIAL		2		/* special type                      */
#define	QUOTE		3		/* quoted string delimiter type      */
#define	ESCAPE		4		/* escape character inside strings   */
#define	COMMENT		5		/* comment delimiter type            */
#define	CSPACE		6		/* space character type              */
#define	TAB		7		/* tab character type                */
#define	NEWLINE		8		/* newline character type            */
#define	ENDFILE		9		/* end of file character type        */

static	unsigned char	chtbl[256];	/* character type table              */

/* state variables of lexical analyser */
static	unsigned char	*chpt;		/* pointer to current text character */
static	unsigned char	*frst;		/* address of buffer's 1st character */
static	unsigned char	*limit;		/* just past last char in buffer     */
static	unsigned char	*buf;		/* the character buffer start pos    */
static	int		blksize;	/* size of block read; others too    */
static	unsigned char   *startline;	/* position in buff of last newline  */
static	FILE_NUM	this_file;	/* number of currently open file     */
static	FILE		*fp;		/* current input file                */
static	FILE_POS	file_pos;	/* current file position             */
static	short		ftype;		/* the type of the current file      */
static	OBJECT		next_token;	/* next token if already read	     */
static	int		offset;		/* where to start reading in file    */
static	unsigned char	*mem_block;	/* file buffer                       */

static int top_stack;		/* top of lexical analyser stack     */
static struct {
  unsigned char	*chpt;		/* pointer to current text character */
  unsigned char	*frst;		/* address of buffer's 1st character */
  unsigned char	*limit;		/* just past last char in buffer     */
  unsigned char	*buf;		/* the character buffer start pos    */
  int		blksize;	/* size of block read; others too    */
  unsigned char	*startline;	/* position in buff of last newline  */
  FILE_NUM	this_file;	/* number of currently open file     */
  FILE		*fp;		/* current input file                */
  FILE_POS	file_pos;	/* current file position             */
  short		ftype;		/* the type of the current file      */
  OBJECT	next_token;	/* next token if already read	     */
  int		offset;		/* where to start reading in file    */
  unsigned char	*mem_block;	/* file buffer                       */
} lex_stack[MAX_LEX_STACK];


/*@@**************************************************************************/
/*                                                                           */
/*  BOOLEAN LexLegalName(str)                                                */
/*                                                                           */
/*  Check whether str is a valid name for a symbol table entry.              */
/*  Valid names have the BNF form                                            */
/*                                                                           */
/*       <name> ::= <letter>  { <letter> }                                   */
/*       <name> ::= <special> { <special> }                                  */
/*       <name> ::= <escape>  { <letter> }                                   */
/*                                                                           */
/*  The third form is inaccessible to users and is for internal use only.    */
/*                                                                           */
/*****************************************************************************/

BOOLEAN LexLegalName(str)
unsigned char *str;
{ int i;  BOOLEAN res;
  debug1(DLA, DDD, "LexLegalName( %s )", str);
  if( chtbl[str[0]] == QUOTE )  FontStripQuotes(str, no_fpos);
  switch( chtbl[str[0]] )
  {
    case ESCAPE:
    case LETTER:
    
      for( i = 1;  chtbl[str[i]] == LETTER;  i++ );
      res = str[i] == '\0';
      break;


    case SPECIAL:
    
      for( i = 1;  chtbl[str[i]] == SPECIAL;  i++ );
      res = str[i] == '\0';
      break;


    default:
    
      res = FALSE;
      break;

  }
  debug1(DLA, DDD, "LexLegalName returning %s", bool(res));
  return res;
} /* end LexLegalName */


/*****************************************************************************/
/*                                                                           */
/*  LexInit()                                                                */
/*                                                                           */
/*  Initialise character types.  Those not touched are 0 (WEIRD).            */
/*  The function initchtbl() assists in initializing the chtbl.              */
/*                                                                           */
/*****************************************************************************/

static initchtbl(val, str)
int val;  unsigned char *str;
{ int i;
  for( i = 0;  str[i] != '\0';  i++ )
	chtbl[ str[i] ] = val;
} /* end initchtbl */

LexInit()
{ initchtbl( LETTER,     "abcdefghijklmnopqrstuvwxyz"   );
  initchtbl( LETTER,     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"   );
  initchtbl( LETTER,     "@"                            );
  initchtbl( SPECIAL,    "!$%^&*()_-+=~`{[}]:;'|<,.>?/" );
  initchtbl( SPECIAL,    "0123456789"                   );
  initchtbl( QUOTE,      "\""                           );
  initchtbl( ESCAPE,     "\\"                           );
  initchtbl( COMMENT,    "#"                            );
  initchtbl( CSPACE,      " "                            );
  initchtbl( TAB,        "\t"                           );
  initchtbl( NEWLINE,    "\n"                           );
  chtbl['\0'] = ENDFILE;
} /* end LexInit */


/*@@**************************************************************************/
/*                                                                           */
/*  setword(res, file_pos, str, len)                                         */
/*                                                                           */
/*  Set variable res to a WORD token containing string str, etc.             */
/*                                                                           */
/*****************************************************************************/

#define setword(res, file_pos, str, len)				\
{ res = NewWord(len, &file_pos);					\
  FposCopy(fpos(res), file_pos);					\
  for( c = 0;  c < len;  c++ ) string(res)[c] = str[c];			\
  string(res)[c] = '\0';						\
}


/*****************************************************************************/
/*                                                                           */
/*  LexPush(x, offs, ftype)                                                  */
/*  LexPop()                                                                 */
/*                                                                           */
/*  Switch lexical analyser to or from (LexPop) reading from the file        */
/*  sequence whose first file is x (the other files are obtained from        */
/*  NextFile).  The first file of the sequence is to be fseeked to offs.     */
/*  When the sequence is exhausted, ftype determines how to continue:        */
/*                                                                           */
/*      ftype          action                                                */
/*                                                                           */
/*      SOURCE_FILE    last input file ends, return @End \Input              */
/*      DATABASE_FILE  database file, return @End \Input                     */
/*      INCLUDE_FILE   include file, must pop lexical analyser and continue  */
/*                                                                           */
/*****************************************************************************/

LexPush(x, offs, ftyp)
FILE_NUM x;  int offs;  int ftyp;
{ char *malloc();
  debug3(DLA, D, "LexPush(%s, %d, %s)", FileName(x), offs,
    ftyp==SOURCE_FILE ? "source" : ftyp==INCLUDE_FILE ? "include" : "database");
  if( top_stack >= MAX_LEX_STACK - 1 )
    Error(FATAL, PosOfFile(x), "%s or %s file %s too deeply nested",
      KW_INCLUDE, KW_DATABASE, FileName(x));
  if( top_stack >= 0 )  /* save current state */
  { lex_stack[top_stack].chpt		= chpt;
    lex_stack[top_stack].frst		= frst;
    lex_stack[top_stack].limit		= limit;
    lex_stack[top_stack].buf		= buf;
    lex_stack[top_stack].blksize	= blksize;
    lex_stack[top_stack].startline	= startline;
    lex_stack[top_stack].this_file	= this_file;
    lex_stack[top_stack].fp		= fp;
    lex_stack[top_stack].ftype		= ftype;
    lex_stack[top_stack].next_token	= next_token;
    lex_stack[top_stack].offset		= offset;
    lex_stack[top_stack].mem_block	= mem_block;
    FposCopy( lex_stack[top_stack].file_pos, file_pos );
  }
  top_stack += 1;
  mem_block = (unsigned char *) malloc(MAX_LINE + BUFFER_SIZE + 2);
  if( mem_block == NULL )  Error(FATAL, PosOfFile(x),
      "run out of memory when opening file %s", FileName(x));
  buf = chpt = &mem_block[MAX_LINE];
  this_file = x;
  offset = offs;
  ftype = ftyp;
  next_token = nil;
  *chpt = '\0';
  fp = null;
} /* end LexPush */

LexPop()
{ debug0(DLA, D, "LexPop()");
  assert( top_stack > 0, "LexPop: top_stack <= 0!" );
  if( fp != null )  fclose(fp);
  top_stack--;
  free(mem_block);
  mem_block    = lex_stack[top_stack].mem_block;
  chpt         = lex_stack[top_stack].chpt;
  frst         = lex_stack[top_stack].frst;
  limit        = lex_stack[top_stack].limit;
  buf          = lex_stack[top_stack].buf;
  blksize      = lex_stack[top_stack].blksize;
  startline    = lex_stack[top_stack].startline;
  this_file    = lex_stack[top_stack].this_file;
  fp           = lex_stack[top_stack].fp;
  ftype        = lex_stack[top_stack].ftype;
  next_token   = lex_stack[top_stack].next_token;
  offset       = lex_stack[top_stack].offset;
  FposCopy( file_pos, lex_stack[top_stack].file_pos );
} /* end LexPop */


/*@@**************************************************************************/
/*                                                                           */
/*  long LexNextTokenPos()                                                   */
/*                                                                           */
/*  Equivalent to ftell() on the current lex file.  Complicated because      */
/*  the file is buffered.                                                    */
/*                                                                           */
/*****************************************************************************/

long LexNextTokenPos()
{ long res;
  if( next_token != nil )
    Error(FATAL, &fpos(next_token), "illegal macro invokation in database");
  res = ftell(fp) - (limit - chpt) - (buf - frst);
  debug1(DLA, D, "LexNextTokenPos() returning %ld", res);
  return res;
}


/*****************************************************************************/
/*                                                                           */
/*  static srcnext()                                                         */
/*                                                                           */
/*  Move to new line of input file.  May need to recharge buffer.            */
/*                                                                           */
/*****************************************************************************/

static srcnext()
{ register unsigned char *col;
  debug4(DLA, DDD, "srcnext();  buf: %d, chpt: %d, frst: %d, limit: %d",
    buf - mem_block, chpt - mem_block, frst - mem_block, limit - mem_block);

  /* if time to transfer last line to area preceding buffer, do so */
  if( blksize != 0 && chpt < limit )
  { debug0(DLA, DDD, "srcnext: transferring.");
    col = buf;
    while( (*--col = *--limit) != '\n' );
    frst = col + 1;
    limit++;
    blksize = 0;
  }

  /* if buffer is empty, read next block */
  /*** changed by JK 9/92 from "if( chpt == limit )" to fix long lines bug */
  if( chpt >= limit )
  { if( chpt > limit )
    { col_num(file_pos) = 1;
      Error(FATAL, &file_pos, "line is too long (or final newline missing)");
    }
    chpt = frst;
    blksize = fread( (char *) buf, sizeof(char), BUFFER_SIZE, fp);
    debug4(DLA, D, "srcnext: %d = fread(0x%x, %d, %d, fp)",
      blksize, buf, sizeof(char), BUFFER_SIZE);
    frst = buf;
    limit = buf + blksize;
    *limit = '\n';
  }

  /* if nothing more to read, make this clear */
  if( chpt >= limit )
  { debug0(DLA, DDD, "srcnext: nothing more to read");
    chpt = limit = buf;
    *limit = '\0';
  }
  debug4(DLA, DDD, "srcnext returning;  buf: %d, chpt: %d, frst: %d, limit: %d",
    buf - mem_block, chpt - mem_block, frst - mem_block, limit - mem_block);
} /* end srcnext */


/*@@**************************************************************************/
/*                                                                           */
/*  OBJECT LexGetToken()                                                     */
/*                                                                           */
/*  Get next token from input.  Look it up in symbol table.                  */
/*                                                                           */
/*****************************************************************************/

OBJECT LexGetToken()
{
	   unsigned char *startpos;	/* where the latest token started    */
  register unsigned char *p;		/* pointer to current input char     */
  register int      c;			/* temporary character (really char) */
  OBJECT   res;				/* result token                      */
  int vcount, hcount;			/* no. of newlines and spaces seen   */

  if( next_token != nil )
  { next_token = Delete(res = next_token, PARENT);
    debug2(DLA, DD, "LexGetToken%s (in macro) returning %s",
      EchoFilePos(&file_pos), EchoToken(res));
    return res;
  }

  res = nil;  p = chpt;
  vcount = hcount = 0;
  do switch( chtbl[*p++] )
  {
      case WEIRD:
      
	debug1(DLA, DDD, "LexGetToken%s: WEIRD", EchoFilePos(&file_pos) );
	col_num(file_pos) = (startpos = p-1) - startline;
	Error(WARN, &file_pos, "unknown character (%o octal)", *startpos);
	break;


      case ESCAPE:
      
	col_num(file_pos) = (startpos = p-1) - startline;
	Error(WARN, &file_pos, "character %c outside quoted string", *startpos);
	break;


      case COMMENT:
      
	debug1(DLA, DDD, "LexGetToken%s: comment", EchoFilePos(&file_pos));
	while( (c = *p++) != '\n' && c != '\0' );
	--p;
	break;


      case CSPACE:

	hcount++;
	break;


      case TAB:

	hcount += 8;
	break;


      case NEWLINE:
      
	chpt = p;  srcnext();
	line_num(file_pos)++;
	col_num(file_pos) = 0;
	vcount++;  hcount = 0;
	startline = (p = chpt) - 1;
	break;


      case ENDFILE:
      
	/* close current file, if any */
	debug0(DLA, DDD, "LexGetToken: endfile");
	if( fp != null )
	{ fclose(fp);  fp = null;
	  this_file = ftype == SOURCE_FILE ? NextFile(this_file) : NO_FILE;
	}

	/* open next file */
	while( this_file != NO_FILE )
	{ file_num(file_pos) = this_file;
	  line_num(file_pos) = 1;
	  col_num(file_pos) = 0;
	  fp = OpenFile(this_file, FALSE);
	  if( fp != null )  break;
	  Error(WARN, &file_pos, "cannot open %s", FileName(this_file));
	  this_file = ftype == SOURCE_FILE ? NextFile(this_file) : NO_FILE;
	}
	if( fp != null )
	{ if( offset != 0 )
	  { fseek(fp, (long) offset, 0);
	    offset = 0L;
	  }
	  frst = limit = chpt = buf;
	  blksize = 0;  srcnext();
	  startline = (p = chpt) - 1;
	  hcount = 0;
	}

	/* no next file, so take continuation */
	else switch( ftype )
	{
	  case SOURCE_FILE:
	  case DATABASE_FILE:
	  
	    /* input ends with "@End \Input" */
	    res = NewToken(END, &file_pos, 0, 0, END_PREC, nil);
	    next_token = NewToken(CLOSURE, &file_pos, 0,0, NO_PREC, StartSym);
	    --p;  startline = p;
	    break;

	  case INCLUDE_FILE:

	    LexPop();
	    (p = chpt) - 1;
	    hcount = 0;
	    break;

	  default:  Error(INTERN, no_fpos, "ftype!");

	} /* end switch */
	break;


      case SPECIAL:
      
	col_num(file_pos) = (startpos = p-1) - startline;
	while( chtbl[*p++] == SPECIAL );
	c = p - startpos - 1;
	do
	{ res = SearchSym(startpos, c);
	  --c; --p;
	} while( c > 0 && res == nil );
	goto MORE;  /* 7 lines down */
	break;


      case LETTER:
      
	col_num(file_pos) = (startpos = p-1) - startline;
	while( chtbl[*p++] == LETTER );  --p;
	res = SearchSym(startpos, p - startpos);

	MORE: if( res == nil )
	{ setword(res, file_pos, startpos, p-startpos);
	}
	else if( type(res) == MACRO )
	{ if( recursive(res) )
	  { Error(WARN, &file_pos, "recursion in macro");
	    setword(res, file_pos, startpos, p-startpos);
	  }
	  else
	  { res = CopyTokenList( sym_body(res), &file_pos );
	    if( res != nil ) next_token = Delete(res, PARENT);
	    else hcount = 0;
	  }
	}
	else if( predefined(res) == 0 )
	{ res = NewToken(CLOSURE, &file_pos, 0, 0, precedence(res), res);
	}
	else if( is_filecom(predefined(res)) )
	{ OBJECT t, fname, symbs = nil;  FILE_NUM fnum;
	  chpt = p;
	  t = LexGetToken();
	  p = chpt;
	  if( predefined(res)==DATABASE || predefined(res) == SYS_DATABASE )
	  { symbs = New(ACAT);
	    while( type(t) == CLOSURE )
	    { Link(symbs, t);
	      chpt = p;  t = LexGetToken();  p = chpt;
	    }
	  }
	  if( type(t) != LBR )
	  { Error(WARN, &fpos(t), "%s expected after %s", KW_LBR, SymName(res));
	    Dispose(t);
	    res = nil;
	    break;
	  }
	  Dispose(t);
	  chpt = p; fname = LexGetToken(); p = chpt;
	  if( type(fname) != WORD )
	  { Error(WARN, &fpos(fname), "name of %s file expected here",
		    SymName(res));
	    Dispose(fname);
	    res = nil;
	    break;
	  }
	  chpt = p; t = LexGetToken(); p = chpt;
	  if( type(t) != RBR )
	  { Error(WARN, &fpos(t), "%s expected here", KW_RBR);
	    Dispose(t);
	    res = nil;
	    break;
	  }
	  Dispose(t);
	  if( string(fname)[0] == '"' )
	    FontStripQuotes(string(fname), &fpos(fname));
	  if( predefined(res)==INCLUDE  || predefined(res) == SYS_INCLUDE )
	  { fnum = DefineFile(fname, INCLUDE_FILE,
		    predefined(res)==INCLUDE ? INCLUDE_PATH : SYSINCLUDE_PATH);
	    chpt = p;
	    LexPush(fnum, 0, INCLUDE_FILE);
	    res = LexGetToken();
	    p = chpt;
	  }
	  else if( predefined(res)==DATABASE || predefined(res)==SYS_DATABASE )
	  { OBJECT db, ifname;
	    if( Down(symbs) == symbs )
	      Error(FATAL, &fpos(fname), "symbols missing after %s",
	        predefined(res) == DATABASE ? KW_DATABASE : KW_SYSDATABASE);
	    if( strlen(string(fname)) + strlen(INDEX_SUFFIX) >= MAX_LINE )
	      Error(FATAL,&file_pos, "file name %s is too long", string(fname));
	    ifname = MakeWordTwo(string(fname), INDEX_SUFFIX, &fpos(fname));
	    Dispose(fname);
	    fnum = DefineFile(ifname, INDEX_FILE,
	      predefined(res)==DATABASE ? DATABASE_PATH : SYSDATABASE_PATH );
	    db = DbLoad(fnum, string(ifname), &fpos(ifname), TRUE, symbs);
	    res = nil;
	  }
	  else if( predefined(res)==PREPEND || predefined(res)==SYS_PREPEND )
	  { fnum = DefineFile(fname, PREPEND_FILE,
	      predefined(res) == PREPEND ? INCLUDE_PATH : SYSINCLUDE_PATH);
	    res = nil;
	  }
	  else Error(INTERN, &file_pos, "filecom!");
	}
	else res = NewToken(predefined(res), &file_pos,0,0,precedence(res),res);
	break;


      case QUOTE:
      
	col_num(file_pos) = (startpos = p-1) - startline;
	do switch( chtbl[*p++] )
	{
	  case WEIRD:	Error(FATAL, &file_pos, "unknown character (%c octal)",
				*(p-1));
			break;

	  case ESCAPE:	if( chtbl[*p] == NEWLINE || chtbl[*p] == ENDFILE )
			{ Error(WARN, &file_pos, "unterminated string");
			  *(p-1) = '"';
			  setword(res, file_pos, startpos, p-startpos);
			}
			else p++;
			break;

	  case NEWLINE:
	  case ENDFILE:	--p;
			Error(WARN, &file_pos, "unterminated string");
			setword(res, file_pos, startpos, p-startpos);
			break;

	  case TAB:	Error(WARN, &file_pos, "tab character in string");
			*(p-1) = ' ';
			break;

	  case CSPACE:
	  case COMMENT:
	  case SPECIAL:
	  case LETTER:	break;

	  case QUOTE:	setword(res, file_pos, startpos, p-startpos);
			break;

	  default:	Error(INTERN, &file_pos, "LexGetToken: quoted string");
			break;

	} while( res == nil );
	break;


      default:
      
	Error(INTERN, &file_pos, "LexGetToken: bad chtbl[]");
	break;

  } while( res == nil );

  if( p - startline >= MAX_LINE )
  { col_num(file_pos) = 1;
    Error(FATAL, &file_pos, "line is too long (or final newline missing)");
  }

  chpt = p;
  vspace(res) = vcount;
  hspace(res) = hcount;
  debug4(DLA, DD, "LexGetToken%s returning %s %s (@%d)",
	EchoFilePos(&file_pos), Image(type(res)), EchoToken(res), (int) res);
  return res;
} /* end LexGetToken */
