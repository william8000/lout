/*@z41.c:Object Input-Output:AppendToFile, ReadFromFile@**********************/
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
/*  FILE:         z41.c                                                      */
/*  MODULE:       Object Input-Output                                        */
/*  EXTERNS:      ReadFromFile(), AppendToFile(), CloseFiles()               */
/*                                                                           */
/*****************************************************************************/
#include "externs"

static FILE_NUM	last_write_fnum = NO_FILE;
static FILE	*last_write_fp  = null;


/*****************************************************************************/
/*                                                                           */
/*  OBJECT ReadFromFile(fnum, pos)                                           */
/*                                                                           */
/*  Read an object from file fnum starting at position pos.                  */
/*  The object may include @Env operators defining its environment, or       */
/*  not, but in any case ReadFromFile assumes that the correct scope is set. */
/*                                                                           */
/*****************************************************************************/

OBJECT ReadFromFile(fnum, pos)
FILE_NUM fnum; long pos;
{ OBJECT t, res; int ipos;
  ifdebug(DPP, D, ProfileOn("ReadFromFile"));
  ifdebug(DIO, D, ipos = (int) pos);
  debug2(DIO, D, "ReadFromFile(%s, %d, %s)", FileName(fnum), ipos);
  LexPush(fnum, (int) pos, DATABASE_FILE);
  t = LexGetToken();
  if( type(t) != LBR )
  { debug1(DIO, D, "  following because type(t) = %s", Image(type(t)));
    Error(41, 1, "database index file seems to be out of date",
      FATAL, &fpos(t));
  }
  res = Parse(&t, StartSym, FALSE, FALSE);
  if( t != nil || type(res) != CLOSURE )
  { debug1(DIO, D, "  following because of %s", t != nil ? "t" : "type(res)");
    Error(41, 2, "syntax error in database file", FATAL, &fpos(res));
  }
  LexPop();
  debug1(DIO, D, "ReadFromFile returning %s", EchoObject(res));
  ifdebug(DPP, D, ProfileOff("ReadFromFile"));
  return res;
} /* end ReadFromFile */


/*@::WriteClosure()@**********************************************************/
/*                                                                           */
/*  static WriteClosure(x)                                                   */
/*                                                                           */
/*  Write closure x to file last_write_fp, without enclosing braces and      */
/*  without any environment attached.  If x happens to be a closure that     */
/*  was previously read as a @Use clause, write only @LUse and the name.     */
/*                                                                           */
/*****************************************************************************/
static WriteObject();

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

  sym = actual(x);
  if( use_invocation(sym) == x )
  { StringFPuts(KW_LUSE, last_write_fp);
    StringFPuts(STR_SPACE, last_write_fp);
    StringFPuts(SymName(sym), last_write_fp);
  }
  else
  { npar_seen = FALSE;  name_printed = FALSE;
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
	  if( has_body(sym) && filter(sym) == nil )
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
      
	  Error(41, 3, "WriteClosure: %s",
	    INTERN, &fpos(y), Image(type(actual(y))) );
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
    case HSHIFT:	name = KW_HSHIFT;	goto SETC;
    case VSHIFT:	name = KW_VSHIFT;	goto SETC;
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
    case BACKEND:	name = KW_BACKEND;	goto SETC;
    case XCHAR:		name = KW_XCHAR;	goto SETC;
    case FONT:		name = KW_FONT;		goto SETC;
    case SPACE:		name = KW_SPACE;	goto SETC;
    case BREAK:		name = KW_BREAK;	goto SETC;
    case COLOUR:	name = KW_COLOUR;	goto SETC;
    case LANGUAGE:	name = KW_LANGUAGE;	goto SETC;
    case CURR_LANG:	name = KW_CURR_LANG;	goto SETC;
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


    case FILTERED:

      FilterWrite(x, last_write_fp);
      break;


    default:

      Error(41, 4, "WriteObject: %s", INTERN, &fpos(x), Image(type(x)));
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
{ FULL_CHAR buff[MAX_BUFF], *str;
  OBJECT fname;
  debug2(DIO, D, "AppendToFile( %s, %s )", EchoObject(x), FileName(fnum));

  /* open file fnum for writing */
  if( last_write_fnum != fnum )
  { if( last_write_fnum != NO_FILE )  fclose(last_write_fp);
    str = FileName(fnum);
    if( StringLength(str) + StringLength(NEW_DATA_SUFFIX) >= MAX_BUFF )
      Error(41, 5, "file name %s%s is too long",
	FATAL, PosOfFile(fnum), str, NEW_DATA_SUFFIX);
    StringCopy(buff, str);  StringCat(buff, NEW_DATA_SUFFIX);
    last_write_fp = StringFOpen(buff, "a");
    if( last_write_fp == null )
      Error(41, 6, "cannot append to database file %s", FATAL, no_fpos, buff);
    last_write_fnum = fnum;
  }

  /* write x out and record the fact that fnum has changed */
  *pos = (int) ftell(last_write_fp);
  StringFPuts(KW_LBR, last_write_fp);
  WriteObject(x, NO_PREC);
  StringFPuts(KW_RBR, last_write_fp);
  StringFPuts(STR_NEWLINE, last_write_fp);
  StringFPuts(STR_NEWLINE, last_write_fp);
  FileSetUpdated(fnum);
  debug0(DIO, D, "AppendToFile returning.");
} /* end AppendToFile */


/*****************************************************************************/
/*                                                                           */
/*  CloseFiles()                                                             */
/*                                                                           */
/*  Close all files and move new versions to the names of old versions.      */
/*                                                                           */
/*****************************************************************************/

CloseFiles()
{ FILE_NUM fnum;  FULL_CHAR oldname[MAX_BUFF], newname[MAX_BUFF];
  ifdebug(DPP, D, ProfileOn("CloseFiles"));
  debug0(DIO, D, "CloseFiles()");

  /* close off last file opened by AppendToFile above */
  if( last_write_fnum != NO_FILE )  fclose(last_write_fp);

  /* get rid of old database files */
  for( fnum=FirstFile(SOURCE_FILE);  fnum != NO_FILE;  fnum = NextFile(fnum) )
  { StringCopy(oldname, FileName(fnum));
    StringCat(oldname, DATA_SUFFIX);
    debug1(DIO, D, "unlink(%s)", oldname);
    StringUnlink(oldname);
  }

  /* move any new database files to the old names, if updated */
  for( fnum=FirstFile(DATABASE_FILE); fnum != NO_FILE; fnum = NextFile(fnum) )
  { if( FileTestUpdated(fnum) )
    { StringCopy(oldname, FileName(fnum));
      StringCopy(newname, oldname);
      StringCat(newname, NEW_DATA_SUFFIX);
      debug1(DIO, D, "unlink(%s)", oldname);
      StringUnlink(oldname); /* may fail if no old version */
      debug2(DIO, D, "link(%s, %s)", newname, oldname);
      if( StringLink(newname, oldname) != 0 )
        Error(41, 7, "link(%s, %s) failed", INTERN, no_fpos, newname, oldname);
      debug1(DIO, D, "unlink(%s)", newname);
      if( StringUnlink(newname) != 0 )
	Error(41, 8, "unlink(%s) failed", INTERN, no_fpos, newname);
    }
  }
  debug0(DIO, D, "CloseFiles returning.");
  ifdebug(DPP, D, ProfileOff("CloseFiles"));
} /* end CloseFiles */
