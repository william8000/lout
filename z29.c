/*@z29.c:Symbol Table:SearchSym(), InsertSym(), PushScope()@******************/
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
/*  FILE:         z29.c                                                      */
/*  MODULE:       Symbol Table                                               */
/*  EXTERNS:      PushScope(), PopScope(), BodyParAllowed(), BodyParNotAll() */
/*                InitSym(), SearchSym(), InsertSym(), DeleteEverySym(),     */
/*                SymName(), FullSymName(), ChildSym()                       */
/*                                                                           */
/*****************************************************************************/
#include "externs"

#define	MAX_STACK	 40		/* size of scope stack               */
#define	MAX_TAB		1024		/* size of hash table                */
#define	TAB_MASK	0x3FF		/* i & TAB_MASK == i % MAX_TAB       */

#define	length(x)	word_font(x)

static	OBJECT		scope[MAX_STACK];		/* the scope stack   */
static	BOOLEAN		npars_only[MAX_STACK];		/* look for NPAR exc */
static	BOOLEAN		vis_only[MAX_STACK];		/* look for visibles */
static	BOOLEAN		body_ok[MAX_STACK];		/* look for body par */
static	BOOLEAN		suppress_scope;			/* suppress scoping  */
static	BOOLEAN		suppress_visible;		/* suppress visible  */
static	int		scope_top;			/* scope stack top   */
static	struct { OBJECT f1, f2; } symtab[MAX_TAB];	/* the hash table    */
#if DEBUG_ON
static	int		sym_spread[MAX_TAB] = { 0 };	/* hash table spread */
static	int		sym_count = 0;			/* symbol count      */
#endif


/*****************************************************************************/
/*                                                                           */
/*  #define hash(str, len, val)                                              */
/*                                                                           */
/*  Set val to the hash value of string str, which has length len.           */
/*  The hash function is just the character sum mod MAX_TAB.                 */
/*  This definition assumes that working variables rlen and x exist.         */
/*                                                                           */
/*****************************************************************************/

#define hash(str, len, val)						\
{ rlen = len;								\
  x    = str;								\
  val  = *x++;								\
  while( --rlen )  val += *x++;						\
  val  &= TAB_MASK;							\
}


/*****************************************************************************/
/*                                                                           */
/*  InitSym()                                                                */
/*                                                                           */
/*  Initialize the symbol table to empty.                                    */
/*                                                                           */
/*****************************************************************************/

InitSym()
{ int i;
  scope_top = 0;
  suppress_scope = FALSE;
  suppress_visible = FALSE;
  for( i = 0;  i < MAX_TAB;  i++ )
    symtab[i].f1 = symtab[i].f2 = (OBJECT) &symtab[i];
} /* end InitSym */


/*@@**************************************************************************/
/*                                                                           */
/*  PushScope(x, npars, vis)                                                 */
/*  PopScope()                                                               */
/*                                                                           */
/*  Add or remove an OBJECT x (which must be in the symbol table) to or from */
/*  the scope stack.  If npars is TRUE, only the named parameters of x are   */
/*  added to scope.  If vis is TRUE, only visible locals and parameters are  */
/*  added.                                                                   */
/*                                                                           */
/*****************************************************************************/

PushScope(x, npars, vis)
OBJECT x;  BOOLEAN npars, vis;
{ debug2(DST, DD, "[ PushScope( %s, %s )", SymName(x), bool(npars));
  assert( suppress_scope == FALSE, "PushScope: suppress_scope!" );
  if( scope_top >= MAX_STACK )
  { int i;
    for( i = 0; i < scope_top; i++ )
      debug2(DST, D, "  scope[%2d] = %s", i, SymName(scope[i]));
    Error(INTERN, &fpos(x), "scope depth limit exceeded");
  }
  scope[scope_top]      = x;
  npars_only[scope_top] = npars;
  vis_only[scope_top]   = vis;
  body_ok[scope_top]    = FALSE;
  scope_top++;
} /* end PushScope */

PopScope()
{ debug0(DST, DD, "] PopScope()");
  assert( scope_top > 0, "tried to pop empty scope stack");
  assert( suppress_scope == FALSE, "PopScope: suppress_scope!" );
  scope_top--;
} /* end PopScope */


/*****************************************************************************/
/*                                                                           */
/*  SuppressVisible()                                                        */
/*  UnSuppressVisible()                                                      */
/*                                                                           */
/*  Suppress all scopes (so that all calls to SearchSym fail); and undo it.  */
/*                                                                           */
/*****************************************************************************/

SuppressVisible()
{ debug0(DST, DD, "[ SuppressVisible()");
  suppress_visible = TRUE;
} /* end SuppressVisible */

UnSuppressVisible()
{ debug0(DST, DD, "] UnSuppressVisible()");
  suppress_visible = FALSE;
} /* end UnSuppressVisible */


/*****************************************************************************/
/*                                                                           */
/*  SuppressScope()                                                          */
/*  UnSuppressScope()                                                        */
/*                                                                           */
/*  Suppress all scopes (so that all calls to SearchSym fail); and undo it.  */
/*                                                                           */
/*****************************************************************************/


SuppressScope()
{ debug0(DST, DD, "[ SuppressScope()");
  suppress_scope = TRUE;
} /* end SuppressScope */

UnSuppressScope()
{ debug0(DST, DD, "] UnSuppressScope()");
  suppress_scope = FALSE;
} /* end UnSuppressScope */


/*****************************************************************************/
/*                                                                           */
/*  SwitchScope(sym)                                                         */
/*  UnSwitchScope(sym)                                                       */
/*                                                                           */
/*  Switch to the scope of sym (if nil, StartSym); and switch back again.    */
/*                                                                           */
/*****************************************************************************/

SwitchScope(sym)
OBJECT sym;
{ int i;
  OBJECT new_scopes[MAX_STACK];
  if( sym == nil )  PushScope(StartSym, FALSE, FALSE);
  else
  { i = 0;
    while( sym != StartSym )
    { new_scopes[i++] = enclosing(sym);
      sym = enclosing(sym);
    }
    while( i > 0 )  PushScope(new_scopes[--i], FALSE, FALSE);
  }
}

UnSwitchScope(sym)
OBJECT sym;
{ if( sym == nil )  PopScope();
  else
  { while( sym != StartSym )
    { PopScope();
      sym = enclosing(sym);
    }
  }
}


/*****************************************************************************/
/*                                                                           */
/*  BodyParAllowed()                                                         */
/*  BodyParNotAllowed()                                                      */
/*                                                                           */
/*  Allow or disallow invokations of the body parameter of the current tos.  */
/*                                                                           */
/*****************************************************************************/

BodyParAllowed()
{ debug0(DST, DD, "BodyParAllowed()");
  body_ok[scope_top-1] = TRUE;
} /* end BodyParAllowed */

BodyParNotAllowed()
{ debug0(DST, DD, "BodyParNotAllowed()");
  body_ok[scope_top-1] = FALSE;
} /* end BodyParNotAllowed */


/*@@**************************************************************************/
/*                                                                           */
/*  OBJECT InsertSym(str, xtype, xfpos, xprecedence, indefinite, xrecursive, */
/*                                         xpredefined, xenclosing, xbody)   */
/*                                                                           */
/*  Insert a new symbol into the table.  Its string value is str.            */
/*  Initialise the symbol as the parameters indicate.                        */
/*  Return a pointer to the new symbol.                                      */
/*  If str is not a valid symbol name, InsertSym prints an error             */
/*  message and does not insert the symbol.                                  */
/*                                                                           */
/*****************************************************************************/

OBJECT InsertSym(str, xtype, xfpos, xprecedence, xindefinite, xrecursive,
					     xpredefined, xenclosing, xbody)
unsigned char *str;  unsigned char xtype;
FILE_POS *xfpos; unsigned char xprecedence;
BOOLEAN xindefinite, xrecursive;  unsigned xpredefined;
OBJECT xenclosing, xbody;
{ register int sum, rlen;
  register unsigned char *x;
  OBJECT p, q, s, link, entry, plink;  int len;

  debug3(DST, DD, "InsertSym( %s, %s, in %s )",
	Image(xtype), str, SymName(xenclosing));
  if( !LexLegalName(str) ) Error(WARN, xfpos, "invalid symbol name %s", str);

  s = New(xtype);
  FposCopy(fpos(s), *xfpos);
  has_body(s)    = FALSE;
  right_assoc(s) = TRUE;
  precedence(s)  = xprecedence;
  indefinite(s)  = xindefinite;
  recursive(s)   = xrecursive;
  predefined(s)  = xpredefined;
  enclosing(s)   = xenclosing;
  sym_body(s)    = xbody;
  base_uses(s)   = nil;
  uses(s)        = nil;
  marker(s)      = nil;
  cross_sym(s)   = nil;
  is_extern_target(s) = FALSE;
  uses_extern_target(s) = FALSE;
  visible(s)     = FALSE;
  uses_galley(s) = FALSE;

  uses_count(s)  = 0;
  dirty(s)       = FALSE;
  if( enclosing(s) != nil && type(enclosing(s)) == NPAR )
    dirty(enclosing(s)) = TRUE;

  has_par(s)     = FALSE;
  has_lpar(s)    = FALSE;
  has_rpar(s)    = FALSE;
  if( is_par(type(s)) )  has_par(enclosing(s))  = TRUE;
  if( type(s) == LPAR )  has_lpar(enclosing(s)) = TRUE;
  if( type(s) == RPAR )  has_rpar(enclosing(s)) = TRUE;

  has_target(s)  = FALSE;
  force_target(s) = FALSE;
  if( strcmp(str, KW_TARGET) != 0 ) is_target(s) = FALSE;
  else
  { is_target(s) = has_target(enclosing(s)) = TRUE;
    if( has_key(enclosing(s)) && xbody != nil && type(xbody) == CROSS )
    { if( LastDown(xbody) != Down(xbody) )
      { OBJECT sym;
	Child(sym, Down(xbody));
	if( type(sym) == CLOSURE )
	{ is_extern_target(actual(sym)) = TRUE;
	  uses_extern_target(actual(sym)) = TRUE;
	}
      }
    }
  }

  has_tag(s)  = FALSE;
  if( strcmp(str, KW_TAG) != 0 ) is_tag(s) = FALSE;
  else is_tag(s) = has_tag(enclosing(s)) = dirty(enclosing(s)) = TRUE;

  has_key(s)  = FALSE;
  if( strcmp(str, KW_KEY) != 0 ) is_key(s) = FALSE;
  else is_key(s) = has_key(enclosing(s)) = TRUE;

  if( type(s) == RPAR && has_body(enclosing(s)) && (is_tag(s) || is_key(s)) )
    Error(WARN, &fpos(s), "a body parameter may not be named %s", str);

  len = strlen(str);
  hash(str, len, sum);

  ifdebug(DST, D, sym_spread[sum]++;  sym_count++);
  entry = (OBJECT) &symtab[sum];
  for( plink = Down(entry);  plink != entry;  plink = NextDown(plink) )
  { Child(p, plink);
    if( length(p) == len && strcmp(str, string(p)) == 0 )
    { for( link = Down(p);  link != p;  link = NextDown(link) )
      {	Child(q, link);
	if( enclosing(s) == enclosing(q) )
	{ Error(WARN, &fpos(s), "symbol %s previously defined at%s",
	    str, EchoFilePos(&fpos(q)) );
	  break;
	}
      }
      goto wrapup;
    }
  }

  /* need a new OBJECT as well as s */
  p = NewWord(len, xfpos);
  length(p) = len;
  strcpy(string(p), str);
  Link(entry, p);

 wrapup:
  Link(p, s);
  if( enclosing(s) != nil ) Link(enclosing(s), s);
  debug2(DST, DD, "InsertSym Link(%s, %s) and returning.",
		SymName(enclosing(s)), SymName(s));
  return s;
} /* end InsertSym */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT SearchSym(str, len)                                               */
/*                                                                           */
/*  Search the symbol table for str, with length len, and return an          */
/*  OBJECT referencing the entry if found.  Otherwise return nil.            */
/*                                                                           */
/*****************************************************************************/

OBJECT SearchSym(str, len)
unsigned char *str;  int len;
{ register int rlen, sum;
  register unsigned char *x, *y;
  OBJECT p, q, link, plink, entry;
  int s;

  debug2(DST, DDD, "SearchSym( %c..., %d )", str[0], len);

  hash(str, len, sum);
  rlen = len;
  entry = (OBJECT) &symtab[sum];
  for( plink = Down(entry);  plink != entry;  plink = NextDown(plink) )
  { Child(p, plink);
    if( rlen == length(p) )
    { x = str;  y = string(p);
      do; while( *x++ == *y++ && --rlen );
      if( rlen == 0 )
      {	s = scope_top;
	do
	{ s--;
	  for( link = Down(p);  link != p;  link = NextDown(link) )
	  { Child(q, link);
	    if( enclosing(q) == scope[s]
	      && (!npars_only[s] || type(q) == NPAR)
	      && (!vis_only[s] || visible(q) || suppress_visible )
	      && (body_ok[s] || type(q)!=RPAR || !has_body(enclosing(q)) )
	      && !suppress_scope )
	    {	debug1(DST, DDD, "SearchSym returning %s", Image(type(q)));
		return q;
	    }
	  }
	} while( scope[s] != StartSym );
      }
    }
    rlen = len;
  }
  debug0(DST, DDD, "SearchSym returning <nil>");
  return nil;
} /* end SearchSym */


/*@@**************************************************************************/
/*                                                                           */
/*  unsigned char *SymName(s)                                                */
/*                                                                           */
/*  Return the string value of the name of symbol s.                         */
/*                                                                           */
/*****************************************************************************/

unsigned char *SymName(s)
OBJECT s;
{ OBJECT p;
  if( s == nil )  return (unsigned char *) "<nil>";
  Parent(p, Up(s));
  assert( type(p) == WORD, "SymName: type(p) != WORD!" );
  return string(p);
} /* end SymName */
	

/*****************************************************************************/
/*                                                                           */
/*  unsigned char *FullSymName(x, str)                                       */
/*                                                                           */
/*  Return the path name of symbol x. with str separating each entry.        */
/*                                                                           */
/*****************************************************************************/

unsigned char *FullSymName(x, str)
OBJECT x;  unsigned char *str;
{ OBJECT stack[20];  int i;
  static unsigned char buff[MAX_LINE], *sname;
  if( x == nil )  return (unsigned char *) "<nil>";
  assert( enclosing(x) != nil, "FullSymName: enclosing(x) == nil!" );
  for( i = 0;  enclosing(x) != nil && i < 20;  i++ )
  { stack[i] = x;
    x = enclosing(x);
  }
  strcpy(buff, "");
  for( i--;  i > 0;  i-- )
  { sname = SymName(stack[i]);
    if( strlen(sname) + strlen(str) + strlen(buff) >= MAX_LINE )
      Error(FATAL, &fpos(x), "full name of symbol is too long");
    strcat(buff, sname);
    strcat(buff, str);
  }
  sname = SymName(stack[0]);
  if( strlen(sname) + strlen(buff) >= MAX_LINE )
    Error(FATAL, &fpos(x), "full name of symbol is too long");
  strcat(buff, sname);
  return buff;
} /* end FullSymName */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT ChildSym(s, typ)                                                  */
/*                                                                           */
/*  Find the child of symbol s of type typ, either LPAR or RPAR.             */
/*                                                                           */
/*****************************************************************************/

OBJECT ChildSym(s, typ)
OBJECT s;  unsigned typ;
{ OBJECT link, y;
  for( link = Down(s);  link != s;  link = NextDown(link) )
  { Child(y, link);
    if( type(y) == typ && enclosing(y) == s )  return y;
  }
  Error(INTERN, &fpos(s), "Symbol %s has missing %s", SymName(s), Image(typ));
  return nil;
} /* end ChildSym */


#if DEBUG_ON
/*****************************************************************************/
/*                                                                           */
/*  CheckSymSpread()                                                         */
/*                                                                           */
/*  Check the spread of symbols through the hash table.                      */
/*                                                                           */
/*****************************************************************************/

CheckSymSpread()
{ int i, j, sum, usum;  OBJECT entry, plink;
  debug2(DST, D, "Symbol table spread (table size = %d, symbols = %d):",
    MAX_TAB, sym_count);
  usum = sum = 0;
  for( i = 0;  i < MAX_TAB;  i++ )
  { fprintf(stderr, "%4d: ", i);
    for( j = 1;  j <= sym_spread[i];  j++ )
    { fprintf(stderr, ".");
      sum += j;
    }
    entry = (OBJECT) &symtab[i];
    for( plink=Down(entry), j=1;  plink != entry;  plink=NextDown(plink), j++ )
    { fprintf(stderr, "+");
      usum += j;
    }
    fprintf(stderr, "\n");
  }
  fprintf(stderr, "average length counting duplicate names = %.1f\n",
	(float) sum / sym_count);
  fprintf(stderr, "average length not counting duplicate names = %.1f\n",
	(float) usum / sym_count);
} /* end CheckSymSpread */


/*****************************************************************************/
/*                                                                           */
/*  static DeleteSymBody(s)                                                  */
/*                                                                           */
/*  Delete the body of symbol s.                                             */
/*                                                                           */
/*****************************************************************************/

static DeleteSymBody(s)
OBJECT s;
{ debug1(DST, DDD, "DeleteSymBody( %s )", SymName(s));
  switch( type(s) )
  {
    case MACRO:	while( sym_body(s) != nil )
		  sym_body(s) = DeleteAndDispose(sym_body(s), PARENT);
		break;
	
    case LPAR:
    case NPAR:
    case RPAR:
    case LOCAL:	if( sym_body(s) != nil ) DisposeObject(sym_body(s));
		break;

    default:	Error(INTERN,no_fpos, "unknown symbol type %s",Image(type(s)));
		break;
  }
  debug0(DST, DDD, "DeleteSymBody returning.");
} /* end DeleteSymBody */


/*****************************************************************************/
/*                                                                           */
/*  DeleteEverySym()                                                         */
/*                                                                           */
/*  Delete every symbol in the symbol table.                                 */
/*  Note that we first delete all bodies, then the symbols themselves.       */
/*  This is so that the closures within the bodies have well-defined         */
/*  actual() pointers, even while the symbol table is being disposed.        */
/*  If this is not done, debug output during the disposal gets confused.     */
/*                                                                           */
/*****************************************************************************/

DeleteEverySym()
{ int i, j, load, cost;  OBJECT p, plink, link, x, entry;
  debug0(DST, D, "DeleteEverySym()");

  /* dispose the bodies of all symbols */
  for( i = 0;  i < MAX_TAB;  i++ )
  { entry = (OBJECT) &symtab[i];
    for( plink = Down(entry);  plink != entry;  plink = NextDown(plink) )
    { Child(p, plink);
      for( link = Down(p);  link != p;  link = NextDown(link) )
      {	Child(x, link);  DeleteSymBody(x);
	/* *** won't work now
	while( base_uses(x) != nil )
	{ tmp = base_uses(x);  base_uses(x) = next(tmp);
	  PutMem(tmp, USES_SIZE);
	}
	while( uses(x) != nil )
	{ tmp = uses(x);  uses(x) = next(tmp);
	  PutMem(tmp, USES_SIZE);
	}
	*** */
      }
    }
  }

  /* dispose the symbol name strings, gather statistics, and print them */
  load = cost = 0;
  for( i = 0;  i < MAX_TAB;  i++ )
  { j = 1; entry = (OBJECT) &symtab[i];
    while( Down(entry) != entry )
    { load += 1;  cost += j;  j += 1;
      DisposeChild(Down(entry));
    }
  }
  if( load > 0 ) debug4(DST, D, "size = %d, items = %d (%d%%), probes = %.1f",
    MAX_TAB, load, (100*load)/MAX_TAB, (float) cost/load);
  else debug1(DST, D, "table size = %d, no entries in table", MAX_TAB);
  debug0(DST, D, "DeleteEverySym returning.");
} /* end DeleteEverySym */
#endif
