/*@z40.c:Filter Handler:FilterInit()@*****************************************/
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
/*  FILE:         z40.c                                                      */
/*  MODULE:       Filter Handler                                             */
/*  EXTERNS:      FilterInit(), FilterCreate(), FilterSetFileNames(),        */
/*                FilterExecute(), FilterWrite(), FilterScavenge()           */
/*                                                                           */
/*****************************************************************************/
#include "externs"


static int	filter_count;		/* number of filter files            */
static OBJECT	filter_active;		/* the active filter file records    */
static OBJECT	filter_in_filename;	/* initial name of filter input file */
static FILE_NUM	filter_out_file;	/* file number of filter output file */


/*****************************************************************************/
/*                                                                           */
/*  FilterInit()                                                             */
/*                                                                           */
/*  Initialize this module.                                                  */
/*                                                                           */
/*****************************************************************************/

FilterInit()
{ filter_count = 0;
  filter_active = New(ACAT);
  filter_out_file =
    DefineFile(FILTER_OUT, STR_EMPTY, no_fpos, FILTER_FILE, SOURCE_PATH);
  sym_body(FilterInSym)  = MakeWord(WORD, FILTER_IN,  no_fpos);
  sym_body(FilterOutSym) = MakeWord(WORD, FILTER_OUT, no_fpos);
  sym_body(FilterErrSym) = MakeWord(WORD, FILTER_ERR, no_fpos);
  filter_in_filename = sym_body(FilterInSym);
} /* end FilterInit */


/*@::FilterCreate(), FilterSetFileNames()@************************************/
/*                                                                           */
/*  OBJECT FilterCreate(use_begin, factual, xfpos)                           */
/*                                                                           */
/*  Create and return a new FILTERED object.  Open the corresponding file    */
/*  for writing and dump the parameter text to be filtered into it.          */
/*                                                                           */
/*****************************************************************************/

OBJECT FilterCreate(use_begin, factual, xfpos)
BOOLEAN use_begin;  OBJECT factual;  FILE_POS *xfpos;
{ FULL_CHAR buff[MAX_LINE];  FILE *fp;  OBJECT x, res;
  debug3(DFH, D, "FilterCreate(%s, %s, %s)", bool(use_begin),
    SymName(factual), EchoFilePos(xfpos));
  res = New(FILTERED);
  FposCopy(fpos(res), *xfpos);
  sprintf(buff, "%s%d", FILTER_IN, ++filter_count);
  fp = StringFOpen(buff, "w");
  if( fp == NULL )
    Error(40, 1, "cannot open temporary filter file %s", FATAL, xfpos, buff);
  x = MakeWord(WORD, buff, xfpos);
  filter_use_begin(x) = use_begin;
  filter_actual(x) = factual;
  Link(res, x);
  Link(filter_active, x);
  LexScanFilter(fp, use_begin, xfpos);
  fclose(fp);
  debug1(DFH, D, "FilterCreate returning %s", EchoObject(res));
  return res;
} /* end FilterCreate */


/*****************************************************************************/
/*                                                                           */
/*  FilterSetFileNames(x)                                                    */
/*                                                                           */
/*  Set @FilterIn, @FilterOut, and @FilterErr to suitable values for the     */
/*  manifesting of the command which runs filter x.                          */
/*                                                                           */
/*****************************************************************************/

FilterSetFileNames(x)
OBJECT x;
{ OBJECT y;
  assert( type(x) == FILTERED, "FilterSetFileNames: type(x)!" );
  assert( Down(x) != x, "FilterSetFileNames: x has no children!" );
  debug1(DFH, D, "FilterSetFileNames(%s)", EchoObject(x));
  Child(y, Down(x));
  assert( type(y) == WORD, "FilterSetFileNames: type(y)!" );
  sym_body(FilterInSym) = y;
  debug0(DFH, D, "FilterSetFileNames returning.");
} /* end FilterSetFileNames */


/*@::FilterExecute()@*********************************************************/
/*                                                                           */
/*  OBJECT FilterExecute(x, command, env)                                    */
/*                                                                           */
/*  Execute the filter command on FILTERED object x, and return the result.  */
/*                                                                           */
/*****************************************************************************/

OBJECT FilterExecute(x, command, env)
OBJECT x;  FULL_CHAR *command;  OBJECT env;
{ int i, count, status;  OBJECT t, res;  char line[MAX_LINE];
  FILE *err_fp;

  assert( type(x) == FILTERED, "FilterExecute: type(x)!" );
  assert( type(env) == ENV, "FilterExecute: type(env)!" );
  debug3(DFH, D, "FilterExecute(%s, \"%s\", %s)", EchoObject(x),
    command, EchoObject(env));

  /* reset FilterInSym since Manifest of @Filter is now complete */
  sym_body(FilterInSym) = filter_in_filename;

  /* execute the command, print error messages, and exit if status problem */
  status = system( (char *) command);
  err_fp = StringFOpen(FILTER_ERR, "r");
  if( err_fp != NULL )
  { while( fgets(line, MAX_LINE, err_fp) != NULL )
    { if( line[strlen(line)-1] == '\n' )
	line[strlen(line)-1] = '\0';
      Error(40, 2, "%s", WARN, &fpos(x), line);
    }
    fclose(err_fp);
    StringUnlink(FILTER_ERR);
  }
  if( status != 0 )
    Error(40, 3, "failure (non-zero status) of filter: %s",
      FATAL, &fpos(x), command);

  /* read in output of system command as a Lout object */
  SwitchScope(nil);
  count = 0;
  SetScope(env, &count);
  LexPush(filter_out_file, 0, FILTER_FILE);
  t = NewToken(BEGIN, &fpos(x), 0, 0, BEGIN_PREC, FilterOutSym);
  res = Parse(&t, nil, FALSE, FALSE);
  LexPop();
  for( i = 1;  i <= count;  i++ )  PopScope();
  UnSwitchScope(nil);
  StringUnlink(FILTER_OUT);

  debug1(DFH, D, "FilterExecute returning %s", EchoObject(res));
  return res;
} /* end FilterExecute */


/*@::FilterWrite(), FilterScavenge()@*****************************************/
/*                                                                           */
/*  FilterWrite(x, fp)                                                       */
/*                                                                           */
/*  Write out the active FILTERED object x by copying the file.              */
/*                                                                           */
/*****************************************************************************/

FilterWrite(x, fp)
OBJECT x;  FILE *fp;
{ FILE *in_fp;  OBJECT y;  int ch;
  assert( type(x) == FILTERED, "FilterWrite: type(x)!" );
  debug1(DFH, D, "FilterWrite(%s, fp)", EchoObject(x));
  Child(y, Down(x));
  in_fp = StringFOpen(string(y), "r");
  if( in_fp == NULL )
    Error(40, 4, "cannot read filter temporary file %s",
      FATAL, &fpos(x), string(y));
  if( filter_use_begin(y) )
  { StringFPuts(KW_BEGIN, fp);
    StringFPuts("\n", fp);
    while( (ch = getc(in_fp)) != EOF )  putc(ch, fp);
    StringFPuts(KW_END, fp);
    StringFPuts(" ", fp);
    StringFPuts(SymName(filter_actual(y)), fp);
  }
  else
  { StringFPuts(KW_LBR, fp);
    StringFPuts("\n", fp);
    while( (ch = getc(in_fp)) != EOF )  putc(ch, fp);
    StringFPuts(KW_RBR, fp);
  }
  StringFPuts("\n", fp);
  fclose(in_fp);
  debug0(DFH, D, "FilterWrite returning.");
} /* end FilterWrite */


/*****************************************************************************/
/*                                                                           */
/*  FilterScavenge(all)                                                      */
/*                                                                           */
/*  Unlink unneeded filter files, or all reamining filter files if all.      */
/*                                                                           */
/*****************************************************************************/

FilterScavenge(all)
BOOLEAN all;
{ OBJECT y, link, nextlink;
  debug1(DFH, D, "FilterScavenge(%s)", bool(all));
  for( link = Down(filter_active);  link != filter_active;  link = nextlink )
  { Child(y, link);
    nextlink = NextDown(link);
    if( all || Up(y) == LastUp(y) )
    { debug1(DFH, DD, "FilterScavenge scavenging %s", string(y));
      StringUnlink(string(y));
      DisposeChild(link);
    }
  }
  debug0(DFH, D, "FilterScavenge returning.");
} /* end FilterScavenge */
