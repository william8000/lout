/*@z02.c:Lexical Analyser:Declarations@***************************************/
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
#define	OTHER		0		/* punctuation or other character    */
#define	LETTER		1		/* letter type                       */
#define	QUOTE		2		/* quoted string delimiter type      */
#define	ESCAPE		3		/* escape character inside strings   */
#define	COMMENT		4		/* comment delimiter type            */
#define	CSPACE		5		/* space character type              */
#define	FORMFEED	6		/* formfeed character type           */
#define	TAB		7		/* tab character type                */
#define	NEWLINE		8		/* newline character type            */
#define	ENDFILE		9		/* end of file character type        */

static	unsigned char	chtbl[256];	/* type table indexed by a FULL_CHAR */
static	FULL_CHAR	*chpt;		/* pointer to current text character */
static	FULL_CHAR	*frst;		/* address of first buffer character */
static	FULL_CHAR	*limit;		/* just past last char in buffer     */
static	FULL_CHAR	*buf;		/* the character buffer start pos    */
static	int		blksize;	/* size of block read; others too    */
static	FULL_CHAR	*startline;	/* position in buff of last newline  */
static	FILE_NUM	this_file;	/* number of currently open file     */
static	FILE		*fp;		/* current input file                */
static	FILE_POS	file_pos;	/* current file position             */
static	short		ftype;		/* the type of the current file      */
static	OBJECT		next_token;	/* next token if already read	     */
static	int		offset;		/* where to start reading in file    */
static	FULL_CHAR	*mem_block;	/* file buffer                       */

static int stack_free;		/* first free slot in lexical stack  */
static struct {
  FULL_CHAR	*chpt;		/* pointer to current text character */
  FULL_CHAR	*frst;		/* address of first buffer character */
  FULL_CHAR	*limit;		/* just past last char in buffer     */
  FULL_CHAR	*buf;		/* the character buffer start pos    */
  int		blksize;	/* size of block read; others too    */
  FULL_CHAR	*startline;	/* position in buff of last newline  */
  FILE_NUM	this_file;	/* number of currently open file     */
  FILE		*fp;		/* current input file                */
  FILE_POS	file_pos;	/* current file position             */
  short		ftype;		/* the type of the current file      */
  OBJECT	next_token;	/* next token if already read	     */
  int		offset;		/* where to start reading in file    */
  FULL_CHAR	*mem_block;	/* file buffer                       */
} lex_stack[MAX_LEX_STACK];

/*@::LexLegalName(), LexInit()@***********************************************/
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
FULL_CHAR *str;
{ int i;  BOOLEAN res;
  debug1(DLA, DDD, "LexLegalName( %s )", str);
  switch( chtbl[str[0]] )
  {
    case ESCAPE:
    case LETTER:
    
      for( i = 1;  chtbl[str[i]] == LETTER;  i++ );
      res = str[i] == '\0';
      break;


    case OTHER:
    
      for( i = 1;  chtbl[str[i]] == OTHER;  i++ );
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
/*  Initialise character types.  Those not touched are 0 (OTHER).            */
/*  The function initchtbl() assists in initializing the chtbl.              */
/*                                                                           */
/*****************************************************************************/

static initchtbl(val, str)
int val;  FULL_CHAR *str;
{ int i;
  for( i = 0;  str[i] != '\0';  i++ )
	chtbl[ str[i] ] = val;
} /* end initchtbl */

LexInit()
{ initchtbl(LETTER,  STR_LETTERS_LOWER);
  initchtbl(LETTER,  STR_LETTERS_UPPER);
  initchtbl(LETTER,  STR_LETTERS_SYMSTART);
  initchtbl(LETTER,  STR_LETTERS_EXTRA0);
  initchtbl(LETTER,  STR_LETTERS_EXTRA1);
  initchtbl(LETTER,  STR_LETTERS_EXTRA2);
  initchtbl(LETTER,  STR_LETTERS_EXTRA3);
  initchtbl(LETTER,  STR_LETTERS_EXTRA4);
  initchtbl(LETTER,  STR_LETTERS_EXTRA5);
  initchtbl(LETTER,  STR_LETTERS_EXTRA6);
  initchtbl(LETTER,  STR_LETTERS_EXTRA7);
  initchtbl(QUOTE,   STR_QUOTE);
  initchtbl(ESCAPE,  STR_ESCAPE);
  initchtbl(COMMENT, STR_COMMENT);
  initchtbl(CSPACE,  STR_SPACE);
  initchtbl(FORMFEED,STR_FORMFEED);
  initchtbl(TAB,     STR_TAB);
  initchtbl(NEWLINE, STR_NEWLINE);
  chtbl['\0'] = ENDFILE;
  stack_free = -1;
} /* end LexInit */

/*@::LexPush(), LexPop()@*****************************************************/
/*                                                                           */
/*  LexPush(x, offs, ftype)                                                  */
/*                                                                           */
/*  Start reading from the file sequence whose first file is x (subsequent   */
/*  files are obtained from NextFile).  The first file (x) is to be fseeked  */
/*  to offs.  When the sequence is done, ftype determines how to continue:   */
/*                                                                           */
/*      ftype          action                                                */
/*                                                                           */
/*      SOURCE_FILE    last input file ends, return @End \Input              */
/*      DATABASE_FILE  database file, return @End \Input                     */
/*      INCLUDE_FILE   include file, must pop lexical analyser and continue  */
/*      FILTER_FILE    filter file, return @End @FilterOut                   */
/*                                                                           */
/*****************************************************************************/

LexPush(x, offs, ftyp)
FILE_NUM x;  int offs;  int ftyp;
{ char *malloc();
  debug3(DLA, D, "LexPush(%s, %d, %s)", FileName(x), offs,
    ftyp==SOURCE_FILE ? "source" : ftyp==INCLUDE_FILE ? "include":"database");
  if( stack_free >= MAX_LEX_STACK - 1 )
  { if( ftyp == INCLUDE_FILE )
      Error(2, 1, "include file %s too deeply nested",
        FATAL, PosOfFile(x), FileName(x));
    else
      Error(2, 2, "database file %s too deeply nested",
        FATAL, PosOfFile(x), FileName(x));
  }
  if( stack_free >= 0 )  /* save current state */
  { lex_stack[stack_free].chpt		= chpt;
    lex_stack[stack_free].frst		= frst;
    lex_stack[stack_free].limit		= limit;
    lex_stack[stack_free].buf		= buf;
    lex_stack[stack_free].blksize	= blksize;
    lex_stack[stack_free].startline	= startline;
    lex_stack[stack_free].this_file	= this_file;
    lex_stack[stack_free].fp		= fp;
    lex_stack[stack_free].ftype		= ftype;
    lex_stack[stack_free].next_token	= next_token;
    lex_stack[stack_free].offset	= offset;
    lex_stack[stack_free].mem_block	= mem_block;
    FposCopy( lex_stack[stack_free].file_pos, file_pos );
  }
  stack_free += 1;
  mem_block = (FULL_CHAR *) malloc((MAX_LINE+BUFFER_SIZE+2)*sizeof(FULL_CHAR));
  if( mem_block == NULL )
    Error(2, 3, "run out of memory when opening file %s",
      FATAL, PosOfFile(x), FileName(x));
  buf = chpt = &mem_block[MAX_LINE];
  this_file = x;  offset = offs;
  ftype = ftyp;  next_token = nil;
  *chpt = '\0';  fp = null;
} /* end LexPush */


/*****************************************************************************/
/*                                                                           */
/*  LexPop() - pop lexical analyser.                                         */
/*                                                                           */
/*****************************************************************************/

LexPop()
{ debug0(DLA, D, "LexPop()");
  assert( stack_free > 0, "LexPop: stack_free <= 0!" );
  if( fp != null )  fclose(fp);
  stack_free--;
  free( (char *) mem_block);
  mem_block    = lex_stack[stack_free].mem_block;
  chpt         = lex_stack[stack_free].chpt;
  frst         = lex_stack[stack_free].frst;
  limit        = lex_stack[stack_free].limit;
  buf          = lex_stack[stack_free].buf;
  blksize      = lex_stack[stack_free].blksize;
  startline    = lex_stack[stack_free].startline;
  this_file    = lex_stack[stack_free].this_file;
  fp           = lex_stack[stack_free].fp;
  ftype        = lex_stack[stack_free].ftype;
  next_token   = lex_stack[stack_free].next_token;
  offset       = lex_stack[stack_free].offset;
  FposCopy( file_pos, lex_stack[stack_free].file_pos );
} /* end LexPop */


/*@::setword(), LexNextTokenPos(), srcnext()@*********************************/
/*                                                                           */
/*  setword(typ, res, file_pos, str, len)                                    */
/*                                                                           */
/*  Set variable res to a WORD or QWORD token containing string str, etc.    */
/*                                                                           */
/*****************************************************************************/

#define setword(typ, res, file_pos, str, len)				\
{ res = NewWord(typ, len, &file_pos);					\
  FposCopy(fpos(res), file_pos);					\
  for( c = 0;  c < len;  c++ ) string(res)[c] = str[c];			\
  string(res)[c] = '\0';						\
}


/*****************************************************************************/
/*                                                                           */
/*  long LexNextTokenPos()                                                   */
/*                                                                           */
/*  Equivalent to ftell() on the (buffered) current lex file.                */
/*                                                                           */
/*****************************************************************************/

long LexNextTokenPos()
{ long res;
  if( next_token != nil )
    Error(2, 4, "illegal macro invocation in database",
      FATAL, &fpos(next_token));
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
{ register FULL_CHAR *col;
  debugcond4(DLA, DD, stack_free <= 1,
    "srcnext();  buf: %d, chpt: %d, frst: %d, limit: %d",
    buf - mem_block, chpt - mem_block, frst - mem_block, limit - mem_block);

  /* if time to transfer last line to area preceding buffer, do so */
  if( blksize != 0 && chpt < limit )
  { debugcond0(DLA, DD, stack_free <= 1, "srcnext: transferring.");
    col = buf;
    while( (*--col = *--limit) != CH_NEWLINE );
    frst = col + 1;  limit++;  blksize = 0;
  }

  /* if buffer is empty, read next block */
  /*** changed by JK 9/92 from "if( chpt == limit )" to fix long lines bug */
  if( chpt >= limit )
  { if( chpt > limit )
    { col_num(file_pos) = 1;
      Error(2, 5, "line is too long (or final newline missing)",
	FATAL, &file_pos);
    }
    chpt = frst;
    blksize = fread( (char *) buf, sizeof(char), BUFFER_SIZE, fp);
    debugcond4(DLA, DD, stack_free <= 1,
      "srcnext: %d = fread(0x%x, %d, %d, fp)",
      blksize, buf, sizeof(char), BUFFER_SIZE);
    frst = buf;  limit = buf + blksize;  *limit = CH_NEWLINE;
  }

  /* if nothing more to read, make this clear */
  if( chpt >= limit )
  { debugcond0(DLA, DD, stack_free <= 1, "srcnext: nothing more to read");
    chpt = limit = buf;  *limit = '\0';
  }
  debugcond4(DLA, DD, stack_free <= 1,
    "srcnext returning;  buf: %d, chpt: %d, frst: %d, limit: %d",
    buf - mem_block, chpt - mem_block, frst - mem_block, limit - mem_block);
} /* end srcnext */


/*@::LexGetToken()@***********************************************************/
/*                                                                           */
/*  OBJECT LexGetToken()                                                     */
/*                                                                           */
/*  Get next token from input.  Look it up in symbol table.                  */
/*                                                                           */
/*****************************************************************************/

OBJECT LexGetToken()
{
	   FULL_CHAR *startpos;		/* where the latest token started    */
  register FULL_CHAR *p, *q;		/* pointer to current input char     */
  register int      c;			/* temporary character (really char) */
  OBJECT   res;				/* result token                      */
  int vcount, hcount;			/* no. of newlines and spaces seen   */

  if( next_token != nil )
  { next_token = Delete(res = next_token, PARENT);
    debugcond4(DLA, DD, stack_free <= 1,
      "LexGetToken%s (in macro) returning %d.%d %s",
      EchoFilePos(&file_pos), vspace(res), hspace(res), EchoToken(res));
    return res;
  }

  res = nil;  p = chpt;
  vcount = hcount = 0;
  do switch( chtbl[*p++] )
  {
      case ESCAPE:
      
	col_num(file_pos) = (startpos = p-1) - startline;
	Error(2, 6, "character %c outside quoted string",
	  WARN, &file_pos, *startpos);
	break;


      case COMMENT:
      
	debug1(DLA, DDD, "LexGetToken%s: comment", EchoFilePos(&file_pos));
	while( (c = *p++) != CH_NEWLINE && c != '\0' );
	--p;
	break;


      case CSPACE:
      case FORMFEED:

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
	  fp = OpenFile(this_file, FALSE, TRUE);
	  if( fp != null )  break;
	  Error(2, 7, "cannot open file %s",
	    WARN, &file_pos, FileName(this_file));
	  this_file = ftype == SOURCE_FILE ? NextFile(this_file) : NO_FILE;
	}
	if( fp != null )
	{ if( offset != 0 )
	  { debugcond1(DLA, DD, stack_free <= 1, "fseek(fp, %d, 0)", offset);
	    fseek(fp, (long) offset, 0);
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
	  
	    /* input ends with "@End \Input" then UNEXPECTED_EOF */
	    res = NewToken(END, &file_pos, 0, 0, END_PREC, StartSym);
	    next_token = NewToken(UNEXPECTED_EOF, &file_pos, 0,0,NO_PREC,nil);
	    --p;  startline = p;
	    break;

	  case FILTER_FILE:
	  
	    /* input ends with "@End @FilterOut" */
	    res = NewToken(END, &file_pos, 0, 0, END_PREC, FilterOutSym);
	    /* ***
	    next_token = NewToken(CLOSURE,&file_pos,0,0,NO_PREC,FilterOutSym);
	    *** */
	    --p;  startline = p;
	    break;

	  case INCLUDE_FILE:

	    LexPop();
	    (p = chpt) - 1;
	    hcount = 0;
	    break;

	  default:  Error(2, 8, "unknown file type", INTERN, no_fpos);

	} /* end switch */
	break;


      case OTHER:
      
	col_num(file_pos) = (startpos = p-1) - startline;
	while( chtbl[*p++] == OTHER );
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
	{ setword(WORD, res, file_pos, startpos, p-startpos);
	}
	else if( type(res) == MACRO )
	{ if( recursive(res) )
	  { Error(2, 9, "recursion in macro", WARN, &file_pos);
	    setword(WORD, res, file_pos, startpos, p-startpos);
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
	else if( predefined(res) == INCLUDE || predefined(res) == SYS_INCLUDE )
	{ OBJECT t, fname;  FILE_NUM fnum;  int len;
	  chpt = p;
	  t = LexGetToken();
	  if( type(t) != LBR )
	  { Error(2, 10, "%s expected (after %s)",
	      WARN, &fpos(t), KW_LBR, SymName(res));
	    Dispose(t);
	    res = nil;
	    break;
	  }
	  fname = Parse(&t, nil, FALSE, FALSE);
	  fname = ReplaceWithTidy(fname, FALSE);
	  if( !is_word(type(fname)) )
	  { Error(2, 11, "name of include file expected here",
	      WARN, &fpos(fname));
	    Dispose(fname);
	    res = nil;
	    break;
	  }
	  len = StringLength(string(fname)) - StringLength(SOURCE_SUFFIX);
	  if( len >= 0 && StringEqual(&string(fname)[len], SOURCE_SUFFIX) )
	    StringCopy(&string(fname)[len], STR_EMPTY);
	  fnum = DefineFile(string(fname), STR_EMPTY, &fpos(fname),
	      INCLUDE_FILE,
	      predefined(res)==INCLUDE ? INCLUDE_PATH : SYSINCLUDE_PATH);
	  Dispose(fname);
	  LexPush(fnum, 0, INCLUDE_FILE);
	  res = LexGetToken();
	  vcount++; /** TEST ADDITION! **/
	  p = chpt;
	}
	else if( predefined(res) == END )
	  res = NewToken(predefined(res), &file_pos,0,0,precedence(res),nil);
	else
	  res = NewToken(predefined(res), &file_pos,0,0,precedence(res),res);
	break;


      case QUOTE:
      
	col_num(file_pos) = (startpos = q = p) - 1 - startline;
	do switch( chtbl[*q++ = *p++] )
	{
	  case OTHER:
	  case LETTER:
	  case COMMENT:
	  case CSPACE:
	  case FORMFEED:
	  case TAB:	break;

	  case NEWLINE:
	  case ENDFILE:	--p;
			Error(2, 12, "unterminated string", WARN, &file_pos);
			setword(QWORD, res, file_pos, startpos, q-1-startpos);
			break;

	  case QUOTE:	setword(QWORD, res, file_pos, startpos, q-1-startpos);
			break;

	  case ESCAPE:	q--;
			if( chtbl[*p] == NEWLINE || chtbl[*p] == ENDFILE )
			{ Error(2, 13, "unterminated string", WARN, &file_pos);
			  setword(QWORD, res, file_pos, startpos, q-startpos);
			}
			else if( octaldigit(*p) )
			{ int count, ch;
			  count = ch = 0;
			  do
			  { ch = ch * 8 + digitchartonum(*p++);
			    count++;
			  } while( octaldigit(*p) && count < 3 );
			  if( ch == '\0' )
			    Error(2, 14, "skipping null character in string",
			      WARN, &file_pos);
			  else *q++ = ch;
			}
			else *q++ = *p++;
			break;

	  default:	Error(2, 15, "LexGetToken: error in quoted string",
			  INTERN, &file_pos);
			break;

	} while( res == nil );
	break;


      default:
      
	Error(2, 16, "LexGetToken: bad chtbl[]", INTERN, &file_pos);
	break;

  } while( res == nil );

  if( p - startline >= MAX_LINE )
  { col_num(file_pos) = 1;
    Error(2, 17, "line is too long (or final newline missing)",FATAL,&file_pos);
  }

  chpt = p;
  vspace(res) = vcount;
  hspace(res) = hcount;
  debugcond5(DLA, DD, stack_free <= 1, "LexGetToken%s returning %s %s %d.%d",
    EchoFilePos(&file_pos), Image(type(res)), EchoToken(res),
    vspace(res), hspace(res));
  return res;
} /* end LexGetToken */


/*@::LexScanFilter@***********************************************************/
/*                                                                           */
/*  LexScanFilter(fp, stop_at_end, err_pos)                                  */
/*                                                                           */
/*  Scan input file and transfer to filter file fp.  If stop_at_end,         */
/*  terminate at @End, else terminate at matching right brace.               */
/*                                                                           */
/*****************************************************************************/

#define clear()								\
{ int i;								\
  for( i = 0;  i < hs_top;  i++ )  putc(hs_buff[i], fp);		\
  hs_top = 0;								\
}

#define hold(ch)							\
{ if( hs_top == MAX_BUFF )  clear();					\
  hs_buff[hs_top++] = ch;						\
}

LexScanFilter(fp, stop_at_end, err_pos)
FILE *fp;  BOOLEAN stop_at_end;  FILE_POS *err_pos;
{
  register FULL_CHAR *p;		/* pointer to current input char     */
  int vcount, hcount;			/* no. of newlines and spaces seen   */
  int depth;				/* depth of nesting of { ... }       */
  BOOLEAN finished;			/* TRUE when finished                */
  BOOLEAN skipping;			/* TRUE when skipping initial spaces */
  FULL_CHAR hs_buff[MAX_BUFF];		/* hold spaces here in case last     */
  int hs_top;				/* next free spot in hs_buff         */

  debug2(DFH, D, "LexScanFilter(fp, %s, %s)",
    bool(stop_at_end), EchoFilePos(err_pos));
  if( next_token != nil )
  { Error(2, 18, "filter parameter in macro", FATAL, err_pos);
  }

  p = chpt;  depth = 0;
  finished = FALSE;
  skipping = TRUE;
  hs_top = 0;
  while( !finished ) switch( chtbl[*p++] )
  {
      case ESCAPE:
      case COMMENT:
      case QUOTE:
      
	skipping = FALSE;
	clear();
	putc(*(p-1), fp);
	break;


      case CSPACE:
      case TAB:
      
	if( !skipping )  hold(*(p-1));
	break;


      case NEWLINE:
      
	if( !skipping )  hold(*(p-1));
	chpt = p;  srcnext();
	line_num(file_pos)++;
	col_num(file_pos) = 0;
	startline = (p = chpt) - 1;
	break;


      case ENDFILE:
      
	Error(2, 19, "end of file reached while reading filter parameter",
	  FATAL, err_pos);
	break;


      case OTHER:
      
	skipping = FALSE;
	if( *(p-1) == '{' )
	{ clear();
	  putc(*(p-1), fp);
	  depth++;
	}
	else if( *(p-1) == '}' )
	{ if( !stop_at_end && depth == 0 )
	  { p--;
	    finished = TRUE;
	  }
	  else
	  { clear();
	    putc(*(p-1), fp);
	    depth--;
	  }
	}
	else
	{ clear();
	  putc(*(p-1), fp);
	}
	break;


      case LETTER:
      
	skipping = FALSE;
	if( *(p-1) == '@' )
	{
	  p--;
	  if( stop_at_end && StringBeginsWith(p, KW_END) )
	  { finished = TRUE;
	  }
	  else if( StringBeginsWith(p, KW_INCLUDE) )
	  { OBJECT incl_fname, t;  FILE *incl_fp;  int ch;
	    clear();
	    p += StringLength(KW_INCLUDE);
	    chpt = p;
	    t = LexGetToken();
	    if( type(t) != LBR )  Error(2, 20, "expected %s here (after %s)",
		FATAL, &fpos(t), KW_LBR, KW_INCLUDE);
	    incl_fname = Parse(&t, nil, FALSE, FALSE);
	    p = chpt;
	    incl_fname = ReplaceWithTidy(incl_fname, FALSE);
	    if( !is_word(type(incl_fname)) )
	      Error(2, 21, "expected file name here", FATAL,&fpos(incl_fname));
	    incl_fp = StringFOpen(string(incl_fname), "r");
	    if( incl_fp == NULL )
	      Error(2, 22, "cannot open include file %s",
		FATAL, &fpos(incl_fname), string(incl_fname));
	    Dispose(incl_fname);
	    while( (ch = getc(incl_fp)) != EOF )
	      putc(ch, fp);
	    fclose(incl_fp);
	  }
	  else if( StringBeginsWith(p, KW_SYSINCLUDE) )
	  { Error(2, 23, "%s in filter parameter not implemented",
	      FATAL, &file_pos, KW_SYSINCLUDE);
	  }
	  else
	  { clear();
	    putc(*p, fp);
	    p++;
	  }
	}
	else
	{ clear();
	  putc(*(p-1), fp);
	}
	break;


      default:
      
	Error(2, 24, "LexScanFilter: bad chtbl[]", INTERN, &file_pos);
	break;

  };
  putc('\n', fp);

  if( p - startline >= MAX_LINE )
  { col_num(file_pos) = 1;
    Error(2, 25, "line is too long (or final newline missing)",FATAL,&file_pos);
  }

  chpt = p;
  debug1(DFH, D, "LexScanFilter returning at %s", EchoFilePos(&file_pos));
} /* end LexScanFilter */
