/*@z05.c:Read Definitions:ReadFontDef()@**************************************/
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
/*  FILE:         z05.c                                                      */
/*  MODULE:       Read Definitions                                           */
/*  EXTERNS:      ReadPrependDef(), ReadDatabaseDef(), ReadDefinitions()     */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  is_string(t, str)                                                        */
/*                                                                           */
/*  If t is a token denoting unquoted word str, return TRUE.                 */
/*                                                                           */
/*****************************************************************************/

#define is_string(t, str)    (type(t) == WORD && StringEqual(string(t), str) )


/*****************************************************************************/
/*                                                                           */
/*  static ReadFontDef(encl)                                                 */
/*                                                                           */
/*  Read one font definition and pass it on to the font module.  The         */
/*  syntax is  fontdef <family> <face> { <object> }.                         */
/*                                                                           */
/*****************************************************************************/

static ReadFontDef(encl)
OBJECT encl;
{ OBJECT t, family, face, inside;
  
  SuppressScope();
  family = LexGetToken();
  if( !is_word(type(family)) )
    Error(WARN, &fpos(family), "expected font family name here");
  face = LexGetToken();
  if( !is_word(type(face)) )
    Error(WARN, &fpos(face), "expected font face name here");
  UnSuppressScope();
  t = LexGetToken();
  if( type(t) != LBR )
  { Error(WARN, &fpos(t), "expected opening %s of fontdef here", KW_LBR);
    Dispose(t);
    return;
  }
  inside = Parse(&t, encl, FALSE, FALSE);
  inside = ReplaceWithTidy(inside);
  FontDefine(family, face, inside);
  return;
} /* end ReadFontDef */


/*@::ReadPrependDef(), ReadDatabaseDef()@*************************************/
/*                                                                           */
/*  ReadPrependDef(typ, encl)                                                */
/*                                                                           */
/*  Read @Prepend { <filename> } and record its presence.                    */
/*                                                                           */
/*****************************************************************************/

ReadPrependDef(typ, encl)
unsigned typ;  OBJECT encl;
{ OBJECT t, fname;  FILE_NUM fnum;
  t = LexGetToken();
  if( type(t) != LBR )
  { Error(WARN, &fpos(t), "symbol name or %s expected here in %s declaration",
      KW_LBR, KW_PREPEND);
    Dispose(t);
    return;
  }
  fname = Parse(&t, encl, FALSE, FALSE);
  fname = ReplaceWithTidy(fname);
  if( !is_word(type(fname)) )
  { Error(WARN, &fpos(fname), "name of %s file expected here", KW_PREPEND);
    DisposeObject(fname);
    return;
  }
  fnum = DefineFile(string(fname), STR_EMPTY, &fpos(fname), PREPEND_FILE,
	   typ == PREPEND ? INCLUDE_PATH : SYSINCLUDE_PATH);

} /* end ReadPrependDef */


/*****************************************************************************/
/*                                                                           */
/*  ReadDatabaseDef(typ, encl)                                               */
/*                                                                           */
/*  Read @Database <symname> ... <symname> { <filename> } and record it.     */
/*                                                                           */
/*****************************************************************************/

ReadDatabaseDef(typ, encl)
unsigned typ;  OBJECT encl;
{ OBJECT symbs, t, db, fname;
  symbs = New(ACAT);
  t = LexGetToken();
  while( type(t) == CLOSURE )
  { Link(symbs, t);
    t = LexGetToken();
  }
  if( type(t) != LBR )
  { Error(WARN, &fpos(t), "symbol name or %s expected here in %s declaration",
      KW_LBR, KW_DATABASE);
    Dispose(t);
    return;
  }
  if( Down(symbs) == symbs )
  { Error(WARN, &fpos(t), "symbol names missing in %s declaration",
      KW_DATABASE);
    Dispose(t);
    return;
  }
  fname = Parse(&t, encl, FALSE, FALSE);
  fname = ReplaceWithTidy(fname);
  if( !is_word(type(fname)) )
  { Error(WARN, &fpos(fname), "name of %s file expected here", KW_DATABASE);
    DisposeObject(fname);
    return;
  }
  db = DbLoad(fname, typ == DATABASE ? DATABASE_PATH : SYSDATABASE_PATH,
	 TRUE, symbs);
} /* end ReadDatabaseDef */


/*@::ReadTokenList()@*********************************************************/
/*                                                                           */
/*  static ReadTokenList(res)                                                */
/*                                                                           */
/*  Read a list of tokens from input and append them to sym_body(res).       */
/*  The list is assumed to begin immediately after an LBR, and input is      */
/*  to be read up to and including the matching RBR.                         */
/*                                                                           */
/*****************************************************************************/
#define NextToken(t, res)						\
  t = LexGetToken(); sym_body(res) = Append(sym_body(res), t, PARENT);

static ReadTokenList(res)
OBJECT res;
{ OBJECT t, xsym, new_par;
  NextToken(t, res);
  for(;;) switch(type(t))
  {
    case WORD:

      if( string(t)[0] == CH_SYMSTART )
	Error(WARN, &fpos(t), "symbol %s unknown", string(t));
      NextToken(t, res);
      break;


    case QWORD:

      NextToken(t, res);
      break;


    case VCAT:
    case HCAT:
    case ACAT:
    case CROSS:
    case NULL_CLOS:
    case ONE_COL:
    case ONE_ROW:
    case WIDE:
    case HIGH:
    case HSCALE:
    case VSCALE:
    case SCALE:
    case HCONTRACT:
    case VCONTRACT:
    case HEXPAND:
    case VEXPAND:
    case PADJUST:
    case HADJUST:
    case VADJUST:
    case ROTATE:
    case CASE:
    case YIELD:
    case XCHAR:
    case FONT:
    case SPACE:
    case BREAK:
    case NEXT:
    case TAGGED:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case GRAPHIC:

      NextToken(t, res);
      break;


    case LVIS:
    case ENV:
    case USE:
    case BEGIN:
    case END:
    case DATABASE:
    case SYS_DATABASE:
    case PREPEND:
    case SYS_PREPEND:
    case OPEN:

      Error(WARN, &fpos(t), "symbol %s not allowed in macro",
	SymName(actual(t)));
      NextToken(t, res);
      break;


    case LBR:

      ReadTokenList(res);
      NextToken(t, res);
      break;


    case RBR:

      return;


    case CLOSURE:

      xsym = actual(t);
      PushScope(xsym, TRUE, FALSE);
      NextToken(t, res);
      PopScope();
      if( type(t) == CROSS )
      { NextToken(t, res);
	break;
      }

      /* read named parameters */
      while( type(t) == CLOSURE && enclosing(actual(t)) == xsym &&
	     type(actual(t)) == NPAR )
      {	new_par = t;
	NextToken(t, res);
	if( type(t) != LBR )
	{ Error(WARN, &fpos(new_par), "%s must follow name parameter %s",
	    KW_LBR, SymName(actual(new_par)));
	  break;
	}
	PushScope(actual(new_par), FALSE, FALSE);
	ReadTokenList(res);
	PopScope();

	/* get next token, possibly another named parameter */
	PushScope(xsym, TRUE, FALSE);
	NextToken(t, res);
	PopScope();
      }

      /* read body parameter, if any */
      if( has_body(xsym) )
      {	if( type(t) == LBR )
	{ PushScope(xsym, FALSE, TRUE);
	  PushScope(ChildSym(xsym, RPAR), FALSE, FALSE);
	  ReadTokenList(res);
	  PopScope();
	  PopScope();
	  NextToken(t, res);
	}
	else Error(WARN, &fpos(t), "right parameter of %s must begin with %s",
		SymName(xsym), KW_LBR);
      }
      break;

    default:


      Error(INTERN, &fpos(t), "unknown token type %s", Image(type(t)));
      break;

  }
} /* end ReadTokenList */


/*@::ReadMacro()@*************************************************************/
/*                                                                           */
/*  static OBJECT ReadMacro(token, encl)                                     */
/*                                                                           */
/*  Read a macro from input and insert into symbol table.                    */
/*  Token *token contains the "macro" keyword.  Input is read up to and      */
/*  including the closing right brace, and nil is returned in *token if OK.  */
/*  The proper scope for reading the macro body is open at entry and exit.   */
/*  ReadMacro returns the new symbol table entry if successful, else nil.    */
/*                                                                           */
/*****************************************************************************/

static OBJECT ReadMacro(token, encl)
OBJECT *token, encl;
{ OBJECT t, res;

  /* find macro name and insert into symbol table */
  SuppressScope();
  Dispose(*token);  t = LexGetToken();
  if( !is_word(type(t)) )
  { Error(WARN, &fpos(t), "%s ignored - name is missing", KW_MACRO);
    debug1(ANY, D, "offending type is %s", Image(type(t)));
    UnSuppressScope();
    *token = t;
    return nil;
  }
  res = InsertSym(string(t), MACRO, &fpos(t), 0, FALSE, TRUE, 0, encl, nil);

  /* find opening left brace */
  t = LexGetToken();
  if( !is_string(t, KW_LBR) )
  { Error(WARN, &fpos(t), "%s ignored - opening %s missing", KW_MACRO, KW_LBR);
    UnSuppressScope();
    *token = t;
    return nil;
  }
  Dispose(t);
  
  /* read macro body */
  UnSuppressScope();
  ReadTokenList(res);

  /* clean up (kill final RBR, dispose macro name) and exit */
  sym_body(res) = DeleteAndDispose(pred(sym_body(res), PARENT), PARENT);
  recursive(res) = FALSE;
  *token = nil;
  return res;
} /* end ReadMacro */


/*@::ReadDefinitions()@*******************************************************/
/*                                                                           */
/*  ReadDefinitions(token, encl, res_type)                                   */
/*                                                                           */
/*  Read a sequence of definitions and insert them into the symbol table.    */
/*  Either a sequence of local definitions (res_type == LOCAL) or named      */
/*  parameters (res_type == NPAR) is expected; *token is the first def etc.  */
/*  A scope appropriate for reading the bodies of the definitions is open.   */
/*  The parent definition is encl.                                           */
/*                                                                           */
/*****************************************************************************/

ReadDefinitions(token, encl, res_type)
OBJECT *token, encl;  unsigned char res_type;
{ OBJECT t, res, res_target, export_list, import_list, link, y, z;
  t = *token;

  while( res_type != LOCAL ? is_string(t, KW_NAMED) : TRUE )
  {
    if( is_string(t, KW_FONTDEF) )
    { ReadFontDef(encl);
      t = LexGetToken();
      continue;  /* next definition */
    }
    else if( type(t) == PREPEND || type(t) == SYS_PREPEND )
    { ReadPrependDef(type(t), encl);
      Dispose(t);
      t = LexGetToken();
      continue;  /* next definition */
    }
    else if( type(t) == DATABASE || type(t) == SYS_DATABASE )
    { ReadDatabaseDef(type(t), encl);
      Dispose(t);
      t = LexGetToken();
      continue;  /* next definition */
    }

    if( !is_string(t, KW_DEF)   && !is_string(t, KW_MACRO)  &&
	!is_string(t, KW_NAMED) && !is_string(t, KW_IMPORT) &&
        !is_string(t, KW_EXPORT) )
      break;

    /* get import list and change scope appropriately */
    BodyParNotAllowed();
    import_list = New(ACAT);
    if( is_string(t, KW_IMPORT) )
    { Dispose(t);
      t = LexGetToken();
      while( type(t) == CLOSURE ||
	       (type(t)==WORD && !is_string(t,KW_EXPORT) && !is_string(t,KW_DEF)
	       && !is_string(t, KW_MACRO)) )
      {	if( type(t) == CLOSURE )
	{ if( type(actual(t)) == LOCAL )
	  { PushScope(actual(t), FALSE, TRUE);
	    Link(import_list, t);
	  }
	  else
	  { Error(WARN, &fpos(t), "import name expected here");
	    Dispose(t);
	  }
	}
	else
	{ Error(WARN,&fpos(t),"import %s not in scope", string(t));
	  Dispose(t);
	}
	t = LexGetToken();
      }
    }

    /* get export list and store for setting visible flags below */
    export_list = New(ACAT);
    if( is_string(t, KW_EXPORT) )
    { Dispose(t);
      SuppressScope();
      t = LexGetToken();
      while( is_word(type(t)) && !is_string(t, KW_DEF) )
      { Link(export_list, t);
	t = LexGetToken();
      }
      UnSuppressScope();
    }


    if( res_type == LOCAL && !is_string(t, KW_DEF) && !is_string(t, KW_MACRO) )
    { Error(WARN,&fpos(t),"keyword %s or %s expected here", KW_DEF, KW_MACRO);
      break;
    }
    if( res_type == NPAR && !is_string(t, KW_NAMED) )
    { Error(WARN, &fpos(t), "keyword %s expected here", KW_NAMED);
      break;
    }

    if( is_string(t, KW_MACRO) )
    { if( Down(export_list) != export_list )
	Error(WARN, &fpos(t), "ignoring %s list of %s", KW_EXPORT, KW_MACRO);
      res = ReadMacro(&t, encl);
    }
    else
    {
      SuppressScope();  Dispose(t);  t = LexGetToken();

      /* find name of symbol and insert it */
      if( !is_word(type(t)) )
      { Error(WARN, &fpos(t), "cannot find symbol name");
	debug1(ANY, D, "offending type is %s", Image(type(t)));
	UnSuppressScope();
	*token = t;
	return;
      }
      res = InsertSym(string(t), res_type, &fpos(t), DEFAULT_PREC,
		FALSE, FALSE, 0, encl, nil);
      t = LexGetToken();

      /* find force, if any */
      if( is_string(t, KW_FORCE) )
      {	force_target(res) = TRUE;
	Dispose(t);  t = LexGetToken();
	if( !is_string(t, KW_INTO) )
	   Error(WARN, &fpos(t), "%s expected after %s", KW_INTO, KW_FORCE);
      }
	
      /* find into clause, if any */
      res_target = nil;
      if( is_string(t, KW_INTO) )
      { UnSuppressScope();
	Dispose(t);  t = LexGetToken();
	if( type(t) != LBR )
	{ Error(WARN, &fpos(t), "%s expected after %s", KW_LBR, KW_INTO);
	  debug1(ANY, D, "offending type is %s", Image(type(t)));
	  UnSuppressScope();
	  *token = t;
	  return;
	}
	res_target = Parse(&t, encl, FALSE, FALSE);
	SuppressScope();
	if( t == nil )  t = LexGetToken();
      }

      /* find precedence clause, if any */
      if( is_string(t, KW_PRECEDENCE) )
      {	int prec = 0;
	Dispose(t);
	t = LexGetToken();
	while( type(t) == WORD && decimaldigit(string(t)[0]) )
	{
	  prec = prec * 10 + digitchartonum(string(t)[0]);
	  Dispose(t);  t = LexGetToken();
	}

	if( prec < MIN_PREC )
	{ Error(WARN, &fpos(t), "%s is too low - %d substituted",
	    KW_PRECEDENCE, MIN_PREC);
	  prec = MIN_PREC;
	}
	else if( prec > MAX_PREC )
	{ Error(WARN, &fpos(t), "%s is too high - %d substituted",
	    KW_PRECEDENCE, MAX_PREC);
	  prec = MAX_PREC;
	}
	precedence(res) = prec;
      }

      /* find associativity clause, if any */
      if( is_string(t, KW_ASSOC) )
      {	Dispose(t);  t = LexGetToken();
	if( is_string(t, KW_LEFT) )  right_assoc(res) = FALSE;
	else if( !is_string(t, KW_RIGHT) )
	  Error(WARN, &fpos(t), "%s replaced by %s", KW_ASSOC, KW_RIGHT);
	Dispose(t);  t = LexGetToken();
      }

      /* find left parameter, if any */
      if( is_string(t, KW_LEFT) )
      {	Dispose(t);  t = LexGetToken();
	if( type(t) != WORD )
	{ Error(WARN, &fpos(t), "cannot find %s parameter name", KW_LEFT);
	  debug1(ANY, D, "offending type is %s", Image(type(t)));
	  UnSuppressScope();
	  *token = t;
	  return;
	}
	InsertSym(string(t), LPAR, &fpos(t), DEFAULT_PREC, 
	  FALSE, FALSE, 0, res, nil);
	Dispose(t);  t = LexGetToken();
      }

      /* find named parameters, if any */
      UnSuppressScope();
      ReadDefinitions(&t, res, NPAR);

      /* find right or body parameter, if any */
      if( is_string(t, KW_RIGHT) || is_string(t, KW_BODY) )
      {	has_body(res) = is_string(t, KW_BODY);
	SuppressScope();
	Dispose(t);  t = LexGetToken();
	if( type(t) != WORD )
	{ Error(WARN, &fpos(t), "cannot find %s parameter name", KW_RIGHT);
	  debug1(ANY, D, "offending type is %s", Image(type(t)));
	  UnSuppressScope();
	  *token = t;
	  return;
	}
	InsertSym(string(t), RPAR, &fpos(t), DEFAULT_PREC,
	  FALSE, FALSE, 0, res, nil);
	UnSuppressScope();
	Dispose(t);  t = LexGetToken();
      }

      /* read local definitions and body */
      if( res_target != nil )
	InsertSym(KW_TARGET, LOCAL, &fpos(res_target), DEFAULT_PREC,
			FALSE, FALSE, 0, res, res_target);
      if( type(t) == WORD && StringEqual(string(t), KW_LBR) )
      {	z = NewToken(LBR, &fpos(t), 0, 0, LBR_PREC, StartSym);
	Dispose(t);
	t = z;
      }
      else if( type(t) == WORD && StringEqual(string(t), KW_BEGIN) )
      {	z = NewToken(BEGIN, &fpos(t), 0, 0, BEGIN_PREC, StartSym);
	Dispose(t);
	t = z;
      }
      else if( type(t) != LBR && type(t) != BEGIN )
	Error(FATAL, &fpos(t), "opening %s or %s of %s expected",
	 KW_LBR, KW_BEGIN, SymName(res));
      if( type(t) == BEGIN )  actual(t) = res;
      PushScope(res, FALSE, FALSE);
      BodyParAllowed();
      sym_body(res) = Parse(&t, res, TRUE, FALSE);

      /* set visible flag of the exported symbols */
      for( link=Down(export_list);  link != export_list;  link=NextDown(link) )
      {	Child(y, link);
	z = SearchSym(string(y), StringLength(string(y)));
	if( z == nil || enclosing(z) != res )
	  Error(WARN, &fpos(y), "exported symbol %s not defined in %s",
		string(y), SymName(res));
	else if( has_body(res) && type(z) == RPAR )
	  Error(WARN, &fpos(y), "body parameter %s may not be exported",
		string(y));
	else if( visible(z) )
	  Error(WARN, &fpos(y), "symbol %s exported twice", string(y));
	else visible(z) = TRUE;
      }
      DisposeObject(export_list);

      /* pop scope of res */
      PopScope();
    }

    /* pop import scopes and store imports in sym tab */
    for( link=Down(import_list);  link != import_list;  link=NextDown(link) )
      PopScope();
    if( Down(import_list) == import_list )
    { Dispose(import_list);
      import_list = nil;
    }
    if( res != nil )  imports(res) = import_list;

    BodyParAllowed();
    if( t == nil ) t = LexGetToken();

  } /* end while */

  *token = t;
  return;
} /* end ReadDefinitions */
