/*@z06.c:Parser:InitParser(), Parse()@****************************************/
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
/*  FILE:         z06.c                                                      */
/*  MODULE:       Parser                                                     */
/*  EXTERNS:      InitParser(), Parse()                                      */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define	is_cat_op(x)	((x) >= ACAT && (x) <= TJUXTA)
#define	LEFT_ASSOC	0
#define	RIGHT_ASSOC	1
static	OBJECT		cross_name;		/* name of the cr database   */


/*****************************************************************************/
/*                                                                           */
/*  static Stack Handler ADT:                                                */
/*                                                                           */
/*      DebugStacks()       Print debug output of the stacks state           */
/*      Shift()             Shift one token onto the stacks                  */
/*      ShiftObj()          Optimized shift for object with no parameters    */
/*      Reduce()            Perform one reduction of the stacks              */
/*                                                                           */
/*****************************************************************************/

#define	MAX_STACK	50			/* size of parser stacks     */
static	OBJECT		obj_stack[MAX_STACK];	/* stack of objects          */
static	int		otop = -1;		/* top of obj_stack          */
static	OBJECT		tok_stack[MAX_STACK];	/* stack of tokens           */
static	int		ttop = -1;		/* top of tok_stack          */
static	BOOLEAN		obj_prev;		/* TRUE when object is prev  */


/*****************************************************************************/
/*                                                                           */
/*  PushObj(x)                                                               */
/*  PushToken(t)                                                             */
/*  OBJECT PopObj()                                                          */
/*  OBJECT PopToken()                                                        */
/*  OBJECT TokenTop                                                          */
/*  OBJECT ObjTop                                                            */
/*                                                                           */
/*  Push and pop from the object and token stacks; examine top item.         */
/*                                                                           */
/*****************************************************************************/

#define PushObj(x)							\
{ zz_hold = x;								\
  if( ++otop < MAX_STACK ) obj_stack[otop] = zz_hold;			\
  else Error(FATAL, &fpos(obj_stack[otop-1]),				\
	"object stack overflow: need to simplify expression");		\
}

#define PushToken(t)							\
{ if( ++ttop < MAX_STACK ) tok_stack[ttop] = t;				\
  else Error(FATAL, &fpos(tok_stack[ttop-1]),				\
	"operator stack overflow: need to simplify expression");	\
}

#define PopObj()	obj_stack[otop--]
#define PopToken()	tok_stack[ttop--]
#define	TokenTop	tok_stack[ttop]
#define	ObjTop		obj_stack[otop]


#if DEBUG_ON
static DebugStacks(initial_ttop)
int initial_ttop;
{ int i;
  fprintf(stderr, "obj_prev: %s; otop: %d; ttop: %d\n",
    bool(obj_prev), otop, ttop);
  for( i = 0;  i <= otop; i++ )
    fprintf(stderr, "obj[%d] = %s\n", i, EchoObject(null, obj_stack[i]));
  for( i = 0;  i <= ttop;  i++ )
  { if( i == initial_ttop+1 ) fprintf(stderr, "$\n");
    fprintf(stderr, "tok[%d] = %s.%d\n", i, type(tok_stack[i]) == CLOSURE ?
	SymName(actual(tok_stack[i])) : Image(type(tok_stack[i])),
	precedence(tok_stack[i]));
  }
}
#endif

/*****************************************************************************/
/*                                                                           */
/*  insert_space(t)                                                          */
/*                                                                           */
/*  Add any missing catenation operator in front of token t.                 */
/*                                                                           */
/*****************************************************************************/

#define insert_space(t)							\
if( obj_prev )								\
{ int typ, prec;							\
  if( hspace(t) + vspace(t) > 0 )					\
  { typ = TSPACE;							\
    prec = ACAT_PREC;							\
  }									\
  else									\
  { typ = TJUXTA;							\
    prec = JUXTA_PREC;							\
  }									\
  while( obj_prev && precedence(TokenTop) >= prec )  Reduce();		\
  if( obj_prev )							\
  { tmp = New(typ);							\
    precedence(tmp) = prec;						\
    vspace(tmp) = vspace(t);						\
    hspace(tmp) = hspace(t);						\
    mark(gap(tmp)) = FALSE;						\
    join(gap(tmp)) = TRUE;						\
    FposCopy( fpos(tmp), fpos(t) );					\
    PushToken(tmp);							\
  }									\
} /* end insert_space */

#define Shift(t, prec, rassoc, leftpar, rightpar)			\
{ if( leftpar )								\
  { for(;;)								\
    { if( !obj_prev )							\
      {	PushObj( MakeWord("", &fpos(t)) );				\
	obj_prev = TRUE;						\
      }									\
      else if( precedence(TokenTop) >= prec + rassoc )			\
      {	Reduce();							\
      }									\
      else break;							\
    }									\
  }									\
  else insert_space(t);							\
  PushToken(t);								\
  if( rightpar )  obj_prev = FALSE;					\
  else									\
  { obj_prev = TRUE;							\
    Reduce();								\
  }									\
} /* end Shift */

#define ShiftObj(t)							\
{ insert_space(t);							\
  PushObj(t);								\
  obj_prev = TRUE;							\
}

static Reduce()
{ OBJECT p1, p2, p3, s1, s2, tmp;
  OBJECT op;
  assert( obj_prev, "Reduce: obj_prev!" );

  op = PopToken();
  obj_prev = TRUE;
  switch( type(op) )
  {

    case GSTUB_INT:
    case GSTUB_EXT:
    
	TransferEnd( PopObj() );
	obj_prev = TRUE;
	PushObj(New(NULL_CLOS));
	Dispose(op);
	break;


    case GSTUB_NONE:

	PushObj(New(NULL_CLOS));
	Dispose(op);
	break;


    case NULL_CLOS:
    case CROSS:
    case ONE_ROW:
    case ONE_COL:
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
    case FONT:
    case SPACE:
    case BREAK:
    case NEXT:
    case TAGGED:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case GRAPHIC:
    case OPEN:

	if( has_rpar(actual(op)) )
	{ s2 = PopObj();
	  Link(op, s2);
	}
	if( has_lpar(actual(op)) )
	{ s1 = PopObj();
	  Link(Down(op), s1);
	  if( type(op)==CROSS && type(s1)!=CLOSURE ) Error(WARN, &fpos(s1),
	    "left parameter of %s is not a symbol (or not visible)", KW_CROSS);
	}
	PushObj(op);
	break;


    case CLOSURE:
    
	if( has_rpar(actual(op)) )
	{ s2 = New(PAR);
	  tmp = PopObj();
	  Link(s2, tmp);
	  FposCopy(fpos(s2), fpos(tmp));
	  actual(s2) = ChildSym(actual(op), RPAR);
	  Link(op, s2);
	}
	if( has_lpar(actual(op)) )
	{ s1 = New(PAR);
	  tmp = PopObj();
	  Link(s1, tmp);
	  FposCopy(fpos(s1), fpos(tmp));
	  actual(s1) = ChildSym(actual(op), LPAR);
	  Link(Down(op), s1);
	}
	PushObj(op);
	break;


    case LBR:
    
	Error(WARN, &fpos(op), "unmatched %s - inserted %s", KW_LBR, KW_RBR);
	Dispose(op);
	break;


    case BEGIN:
    
	Error(INTERN, &fpos(op), "Reduce: unmatched BEGIN");
	break;


    case RBR:
    
	if( type(TokenTop) == LBR )
	{ /* *** FposCopy(fpos(ObjTop), fpos(TokenTop)); *** */
	  Dispose( PopToken() );
	}
	else if( type(TokenTop) == BEGIN )
	  Error(WARN, &fpos(op), "unmatched %s; inserted %s at%s (after %s)",
	    KW_RBR, KW_LBR, EchoFilePos(&fpos(TokenTop)), KW_BEGIN);
	else Error(INTERN, &fpos(op), "Reduce: unmatched RBR");
	Dispose(op);
	break;


    case END:
    
	if( type(TokenTop) != BEGIN )
	  Error(INTERN, &fpos(op), "Reduce: unmatched END");
	else
	{ if( actual(op) != actual(TokenTop) )
	    if( actual(op) == StartSym )
	      Error(WARN, &fpos(op),
		"%s %s appended at end of file to match %s at%s",
		KW_END, SymName(actual(TokenTop)),
		KW_BEGIN, EchoFilePos(&fpos(TokenTop)) );
	    else if( actual(op) == nil )
	      Error(WARN, &fpos(op),
		"%s replaced by %s %s to match %s at%s",
		KW_END, KW_END, SymName(actual(TokenTop)),
		KW_BEGIN, EchoFilePos(&fpos(TokenTop)) );
	    else
	      Error(WARN, &fpos(op),
		"%s %s replaced by %s %s to match %s at%s",
		KW_END, SymName(actual(op)),
		KW_END, SymName(actual(TokenTop)),
		KW_BEGIN, EchoFilePos(&fpos(TokenTop)) );
	  Dispose( PopToken() );
	}
	Dispose(op);
	break;


    case GAP_OBJ:

	p1 = PopObj();
	Link(op, p1);
	PushObj(op);
	obj_prev = FALSE;
	break;


    case VCAT:
    case HCAT:
    case ACAT:
    
	p3 = PopObj();  p2 = PopObj();  p1 = PopObj();
	if( type(p1) == type(op) )  Dispose(op);
	else
	{ Link(op, p1);
	  p1 = op;
	}
	Link(p1, p2);
	Link(p1, p3);
	PushObj(p1);
	break;


    case TSPACE:
    case TJUXTA:

	p2 = PopObj();  p1 = PopObj();
	if( type(p1) != ACAT )
	{ tmp = New(ACAT);
	  Link(tmp, p1);
	  FposCopy(fpos(tmp), fpos(p1));
	  p1 = tmp;
	}
	type(op) = GAP_OBJ;
	Link(p1, op);
	Link(p1, p2);
	PushObj(p1);
	break;


    default:
    
	Error(INTERN, &fpos(op), "Reduce: %s", Image(type(op)) );
	break;

  } /* end switch */
  debug0(DOP, DD, "Reduce returning; ");
  ifdebug(DOP, DD, DebugStacks(0) );
} /* end Reduce */


/*@@**************************************************************************/
/*                                                                           */
/*  static SetScope(env, count)                                              */
/*                                                                           */
/*  Push scopes required to parse object whose environment is env.           */
/*  Add to *count the number of scope pushes made.                           */
/*                                                                           */
/*****************************************************************************/

static SetScope(env, count)
OBJECT env;  int *count;
{ OBJECT link, y, yenv;
  debug2(DOP, D, "SetScope( %s, %d )", EchoObject(null, env), *count);
  assert( env != nil && type(env) == ENV, "SetScope: type(env) != ENV!" );
  if( Down(env) != env )
  { Child(y, Down(env));
    assert( LastDown(y) != y, "SetScope: LastDown(y)!" );
    link = LastDown(env) != Down(env) ? LastDown(env) : LastDown(y);
    Child(yenv, link);
    assert( type(yenv) == ENV, "SetScope: type(yenv) != ENV!" );
    SetScope(yenv, count);
    PushScope(actual(y), FALSE, FALSE);  (*count)++;
  }
  debug1(DOP, D, "SetScope returning, count = %d", *count);
} /* end SetScope */


/*****************************************************************************/
/*                                                                           */
/*  InitParser()                                                             */
/*                                                                           */
/*  Initialise the parser to contain just GstubExt.                          */
/*  Remember cross_db, the name of the cross reference database, for Parse.  */
/*                                                                           */
/*****************************************************************************/

InitParser(cross_db)
unsigned char *cross_db;
{ if( strlen(cross_db) + strlen(INDEX_SUFFIX) >= MAX_LINE )
    Error(FATAL, no_fpos, "cross reference database file name %s%s is too long",
	cross_db, INDEX_SUFFIX);
  cross_name = MakeWordTwo(cross_db, INDEX_SUFFIX, no_fpos);
  PushToken( NewToken(GSTUB_EXT, no_fpos, 0, 0, DEFAULT_PREC, StartSym) );
} /* end InitParser */


/*@@**************************************************************************/
/*                                                                           */
/*  static OBJECT ParseEnvClosure(t, encl)                                   */
/*                                                                           */
/*  Parse an object which is a closure with environment.  Consume the        */
/*  concluding @Clos.                                                        */
/*                                                                           */
/*****************************************************************************/

static OBJECT ParseEnvClosure(t, encl)
OBJECT t, encl;
{ OBJECT env, res, y;  int count, i;
  debug0(DOP, DD, "ParseEnvClosure(t, encl)");
  assert( type(t) == ENV, "ParseEnvClosure: type(t) != ENV!" );
  env = t;  t = LexGetToken();
  while( type(t) != CLOS )  switch( type(t) )
  {
    case LBR:	count = 0;
		SetScope(env, &count);
		y = Parse(&t, encl, FALSE, FALSE);
		if( type(y) != CLOSURE )  Error(FATAL, &fpos(y),
			"syntax error in cross reference database");
		for( i = 1;  i <= count;  i++ )  PopScope();
		AttachEnv(env, y);
		env = SetEnv(y, nil);
		t = LexGetToken();
		break;

    case ENV:	y = ParseEnvClosure(t, encl);
		env = SetEnv(y, env);
		t = LexGetToken();
		break;

    default:	Error(FATAL, &fpos(t), "error in cross reference database");
		break;
  }
  Dispose(t);
  if( Down(env) == env || Down(env) != LastDown(env) )
	Error(FATAL, &fpos(env), "error in cross reference database");
  Child(res, Down(env));
  DeleteNode(env);
  debug1(DOP, DD, "ParseEnvClosure returning %s", EchoObject(null, res));
  assert( type(res) == CLOSURE, "ParseEnvClosure: type(res) != CLOSURE!" );
  return res;
} /* end ParseEnvClosure */



/*@@**************************************************************************/
/*                                                                           */
/*  OBJECT Parse(token, encl, defs_allowed, transfer_allowed)                */
/*                                                                           */
/*  Parse input tokens, beginning with *token, looking for an object of the  */
/*  form { ... } or @Begin ... @End <sym>, and return the object.            */
/*  The parent definition is encl, and scope has been set appropriately.     */
/*  Parse reads up to and including the last token of the object             */
/*  (the right brace or <sym>), and returns nil in *token.                   */
/*                                                                           */
/*  If defs_allowed == TRUE, there may be local definitions in the object.   */
/*  In this case, encl is guaranteed to be the enclosing definition.         */
/*                                                                           */
/*  If transfer_allowed == TRUE, the parser may transfer components to the   */
/*  galley handler as they are read.                                         */
/*                                                                           */
/*  Note: the lexical analyser returns "@End \Input" at end of input, so the */
/*  parser does not have to handle end of input separately.                  */
/*                                                                           */
/*****************************************************************************/

OBJECT Parse(token, encl, defs_allowed, transfer_allowed)
OBJECT *token, encl;  BOOLEAN defs_allowed, transfer_allowed;
{ OBJECT t, x, tmp, xsym, env, y, res;
  int i, initial_ttop = ttop;
  unsigned char buff[MAX_LINE];  FILE_NUM index_fnum;

  debug4(DOP, D, "[ Parse(%s, %s, %s, %s)", EchoToken(*token), SymName(encl),
	bool(defs_allowed), bool(transfer_allowed));
  assert( type(*token) == LBR || type(*token) == BEGIN, "Parse: *token!" );

  obj_prev = FALSE;
  Shift(*token, precedence(*token), 0, FALSE, TRUE);
  t = LexGetToken();
  if( defs_allowed )
  { ReadDefinitions(&t, encl, LOCAL);
    if( encl == StartSym ) /* transition point from defs to content */
    {
      /* if error in definitions, stop now */
      if( ErrorSeen() )  Error(FATAL, &fpos(t), "Exiting now");

      /* load cross-references from previous run, open new cross refs */
      if( AllowCrossDb )
      {	index_fnum = DefineFile(cross_name, INDEX_FILE, SOURCE_PATH);
	OldCrossDb = DbLoad(index_fnum,string(cross_name),&fpos(t),FALSE,nil);
	NewCrossDb = DbCreate(string(cross_name), &fpos(t));
      }
      else OldCrossDb = NewCrossDb = nil;

      /* tidy up and possibly print symbol table */
      FlattenUses();
      ifdebug(DST, D, EchoObject(stderr, StartSym) );

      /* read @Use commands and construct env */
      env = New(ENV);
      while( type(t) == USE )
      {
	OBJECT crs, res_env;  STYLE style;
	Dispose(t);  t = LexGetToken();
	if( type(t) != LBR )
	  Error(FATAL, &fpos(t), "%s expected after %s", KW_LBR, KW_USE);
	y = Parse(&t, encl, FALSE, FALSE);
	if( type(y) == CROSS )
	{ y = CrossExpand(y, env, &style, FALSE, &crs, &res_env);
	  AttachEnv(res_env, y);
	  env = SetEnv(y, env);
	}
	else if( type(y) == CLOSURE )
	{ AttachEnv(env, y);
	  env = SetEnv(y, nil);
	}
	else Error(FATAL, &fpos(y), "invalid parameter of %s", KW_USE);

	PushScope(actual(y), FALSE, TRUE);
	t = LexGetToken();
      }
      TransferInit(env);
    }
  }

  for(;;)
  { 
    ifdebug(DOP, DD, DebugStacks(initial_ttop) );
    debug2(DOP, DD, ">> %s.%d", EchoToken(t), precedence(t) );

    switch( type(t) )
    {

      case WORD:
      
	if( string(t)[0] == '@' )
	  Error(WARN, &fpos(t), "symbol %s unknown or misspelt", string(t));
	ShiftObj(t);
	t = LexGetToken();
	break;


      case VCAT:
      case HCAT:
      case ACAT:
      
	/* clean up left context */
	Shift(t, precedence(t), LEFT_ASSOC, TRUE, TRUE);

	/* invoke transfer subroutines if appropriate */
	if( type(t) == VCAT && !has_join(actual(t))
		&& type(tok_stack[ttop-2]) == GSTUB_EXT )
	{ TransferComponent( PopObj() );
	  obj_prev = FALSE;
	  tmp = New(NULL_CLOS);
	  FposCopy( fpos(tmp), fpos(t) );
	  PushObj(tmp);
	}

	/* push GAP_OBJ token, to cope with 3 parameters */
	x = New(GAP_OBJ);
	mark(gap(x)) = has_mark(actual(t));
	join(gap(x)) = has_join(actual(t));
	precedence(x) = GAP_PREC;
	FposCopy( fpos(x), fpos(t) );
	Shift(x, GAP_PREC, LEFT_ASSOC, FALSE, TRUE);

	/* if op is followed by space, insert {} */
	t = LexGetToken();
	if( hspace(t) + vspace(t) > 0 )
	{ ShiftObj(MakeWord("", &fpos(x)));
	}
	break;


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
      case FONT:
      case SPACE:
      case BREAK:
      case NEXT:
      case TAGGED:
      case INCGRAPHIC:
      case SINCGRAPHIC:
      case GRAPHIC:

	/* clean up left context of t (these ops are all right associative) */
	Shift(t, precedence(t), RIGHT_ASSOC,
		has_lpar(actual(t)), has_rpar(actual(t)));
	t = LexGetToken();
	break;


      case BEGIN:
      
	if( actual(t) == nil )
	{ Error(WARN, &fpos(t), "%s replaced by %s", KW_BEGIN, KW_LBR);
	  type(t) = LBR;
	}
	/* NB NO BREAK! */


      case LBR:
      
	Shift(t, LBR_PREC, 0, FALSE, TRUE);
	t = LexGetToken();
	break;


      case END:
      
	x = LexGetToken();
	if( type(x) == CLOSURE )
	{ actual(t) = actual(x);
	  Dispose(x);
	  x = nil;
	}
	else if( type(x) == WORD && string(x)[0] == '@' )
	{ Error(WARN,&fpos(x),"unknown or misspelt symbol %s after %s deleted",
		string(x), KW_END);
	  actual(t) = nil;
	  Dispose(x);
	  x = nil;
	}
	else
	{ Error(WARN, &fpos(x), "symbol expected after %s", KW_END);
	  actual(t) = nil;
	}
	Shift(t, precedence(t), 0, TRUE, FALSE);
	if( ttop == initial_ttop )
	{ ifdebug(DOP, DD, DebugStacks(initial_ttop));
	  *token = x;
	  debug0(DOP, D, "] Parse returning");
	  ifdebug(DOP, D, EchoObject(stderr, ObjTop));
	  obj_prev = FALSE;
	  return PopObj();
	}
	t = (x != nil) ? x : LexGetToken();
	break;


      case RBR:
      
	Shift(t, precedence(t), 0, TRUE, FALSE);
	if( ttop == initial_ttop )
	{ ifdebug(DOP, DD, DebugStacks(initial_ttop));
	  *token = nil;
	  debug0(DOP, D, "] Parse returning");
	  ifdebug(DOP, D, EchoObject(stderr, ObjTop));
	  obj_prev = FALSE;
	  return PopObj();
	}
	t = LexGetToken();
	break;
				

      case USE:
      
	Error(FATAL, &fpos(t), "%s symbol out of place", SymName(actual(t)));
	break;


      case ENV:
      
	/* only occurs in cross reference databases */
	res = ParseEnvClosure(t, encl);
	ShiftObj(res);
	t = LexGetToken();
	break;


      case LVIS:
      
	/* only occurs in cross-reference databases */
	SuppressVisible();
	Dispose(t);  t = LexGetToken();
	UnSuppressVisible();
	if( type(t) != CLOSURE )
	  Error(FATAL, &fpos(t), "symbol expected following %s", KW_LVIS);
	/* NB NO BREAK! */


      case CLOSURE:
      
	x = t;  xsym = actual(x);

	/* look ahead one token, which could be an NPAR */
	PushScope(xsym, TRUE, FALSE);
	t = LexGetToken();
	PopScope();

	/* if x starts a cross-reference, make it a CLOSURE */
	if( type(t) == CROSS )
	{ ShiftObj(x);
	  break;
	}

	/* clean up left context of x */
	Shift(x, precedence(x),right_assoc(xsym),has_lpar(xsym),has_rpar(xsym));

	/* update uses relation if required */
	if( encl != StartSym )
	{ if( !has_target(xsym) )  InsertUses(encl, xsym);
	  else uses_galley(encl) = TRUE;
	}

	/* read named parameters */
	while( type(t) == CLOSURE && enclosing(actual(t)) == xsym
				       && type(actual(t)) == NPAR )
	{	
	  /* check syntax and attach the named parameter to x */
	  OBJECT new_par = t;
	  t = LexGetToken();
	  if( type(t) != LBR )
	  { Error(WARN, &fpos(new_par), "%s must follow named parameter %s",
		    KW_LBR, SymName(actual(new_par)) );
	    Dispose(new_par);
	    break;
	  }

	  /* read the named parameter's body */
	  PushScope(actual(new_par), FALSE, FALSE);
	  tmp = Parse(&t, encl, FALSE, FALSE);
	  type(new_par) = PAR;
	  Link(x, new_par);
	  Link(new_par, tmp);
	  PopScope();

	  /* get next token, possibly another NPAR */
	  PushScope(xsym, TRUE, FALSE);	 /* allow NPARs only */
	  if( t == nil )  t = LexGetToken();
	  PopScope();

	} /* end while */
	obj_prev = !has_rpar(xsym);

	/* record symbol name in BEGIN following, if any */
	if( type(t) == BEGIN )
	{ if( !has_rpar(xsym) )
	    Error(WARN, &fpos(x), "%s takes no right parameter", SymName(xsym));
	  else actual(t) = xsym;
	}

	/* if x can be transferred, do so */
	if( transfer_allowed && has_target(xsym) && !has_key(xsym) )
	{   
	  if( !has_rpar(xsym) || uses_count(ChildSym(xsym, RPAR)) <= 1 )
	  {
	    debug1(DGT, DD, "examining transfer of %s", SymName(xsym));
	    ifdebug(DGT, DD, DebugStacks(initial_ttop));
	    i = has_rpar(xsym) ? ttop -1 : ttop;
	    while( is_cat_op(type(tok_stack[i])) )   i--;
	    if( (type(tok_stack[i])==LBR || type(tok_stack[i])==BEGIN)
		  && type(tok_stack[i-1]) == GSTUB_EXT )
	    {
	      /* at this point it is likely that x is transferable */
	      if( has_rpar(xsym) )
	      { tmp = New(CLOSURE);
		actual(tmp) = InputSym;
		FposCopy( fpos(tmp), fpos(t) );
		PushObj(tmp);  obj_prev = TRUE;
		Reduce();
	      }
	      x = PopObj();
	      x = TransferBegin(x);
	      if( type(x) == CLOSURE )	/* failure: unReduce */
	      {	if( has_rpar(xsym) )
		{ Child(tmp, LastDown(x));
		  assert(type(tmp)==PAR && type(actual(tmp))==RPAR,
				"Parse: can't undo rpar" );
		  DisposeChild(LastDown(x));
		  if( has_lpar(xsym) )
		  { Child(tmp, Down(x));
		    assert(type(tmp)==PAR && type(actual(tmp))==LPAR,
				"Parse: can't undo lpar" );
		    Child(tmp, Down(tmp));
		    PushObj(tmp);
		    DeleteLink(Up(tmp));
		    DisposeChild(Down(x));
		  }
		  PushToken(x);  obj_prev = FALSE;
		}
		else
		{ PushObj(x);
		  obj_prev = TRUE;
		}
	      }
	      else /* success */
	      { obj_prev = FALSE;
	        Shift(x, NO_PREC, 0, FALSE, has_rpar(xsym));
	      }
	    }
	  }
	} /* end if has_target */

	if( has_body(xsym) )
	{ if( type(t) == BEGIN || type(t) == LBR )
	  { PushScope(xsym, FALSE, TRUE);
	    PushScope(ChildSym(xsym, RPAR), FALSE, FALSE);
	    PushObj( Parse(&t, encl, FALSE, TRUE) );
	    obj_prev = TRUE;
	    Reduce();
	    PopScope();
	    PopScope();
	    if( t == nil )  t = LexGetToken();
	  }
	  else
	  { Error(WARN, &fpos(t),
	      "%s parameter of %s must be enclosed in %s .. %s",
	      KW_BODY, SymName(xsym), KW_LBR, KW_RBR);
	  }
	}
	break;


      case OPEN:

	x = t;  xsym = nil;
	Shift(t, precedence(t), RIGHT_ASSOC, TRUE, TRUE);
	if( type(ObjTop) == CLOSURE )  xsym = actual(ObjTop);
	else if( type(ObjTop) == CROSS && Down(ObjTop) != ObjTop )
	{ Child(tmp, Down(ObjTop));
	  if( type(tmp) == CLOSURE )  xsym = actual(tmp);
	}
	t = LexGetToken();

	if( xsym == nil )
	  Error(WARN, &fpos(x), "invalid left parameter of %s", KW_OPEN);
	else if( type(t) != BEGIN && type(t) != LBR )
	  Error(WARN, &fpos(t), "%s parameter of %s not enclosed in %s .. %s",
		   KW_RIGHT, KW_OPEN, KW_LBR, KW_RBR);
	else
	{ PushScope(xsym, FALSE, TRUE);
	  tmp = Parse(&t, encl, FALSE, FALSE);
	  ShiftObj(tmp);
	  PopScope();
	  if( t == nil )  t = LexGetToken();
	  Reduce();
	}
	break;


      default:
      
	Error(INTERN, &fpos(t), "Parse: type %s", Image(type(t)) );
	break;

    } /* end switch */
  } /* end for */

} /* end Parse */
