/*@z09.c:Closure Expansion:ClosureExpand()@***********************************/
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
/*  FILE:         z09.c                                                      */
/*  MODULE:       Closure Expansion                                          */
/*  EXTERNS:      SetEnv(), AttachEnv(), GetEnv(), SearchEnv(),              */
/*                ClosureExpand()                                            */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  OBJECT SearchEnv(env, sym)                                               */
/*                                                                           */
/*  Search environment env for a symbol such that actual() == sym.           */
/*                                                                           */
/*****************************************************************************/

OBJECT SearchEnv(env, sym)
OBJECT env, sym;
{ OBJECT link, y;
  debug2(DCE, DD, "SearchEnv(%s, %s)", EchoObject(null, env), SymName(sym));
  for(;;)
  {
    debug1(DCE, DDD, "  searching env %s", EchoObject(null, env));
    assert( env != nil && type(env) == ENV, "SearchEnv: env!" );
    if( Down(env) == env )
    { debug0(DCE, DD, "SearchEnv returning <nil>");
      return nil;
    }
    Child(y, Down(env));
    assert( type(y) == CLOSURE, "SearchEnv: type(y) != CLOSURE!" );
    if( actual(y) == sym )
    { debug1(DCE, DD, "SearchEnv returning %s", EchoObject(null, y));
      return y;
    }
    assert( LastDown(y) != y, "SearchEnv: LastDown(y) == y!" );
    link = LastDown(env) != Down(env) ? LastDown(env) : LastDown(y);
    Child(env, link);
  }
} /* end SearchEnv */


/*@@**************************************************************************/
/*                                                                           */
/*  static OBJECT EvalNext(x, env)                                           */
/*                                                                           */
/*  Evaluate x, which is a @Next something object, in environment env.       */
/*                                                                           */
/*****************************************************************************/

static OBJECT EvalNext(x, env)
OBJECT x, env;
{ OBJECT y, prnt, link, par, py;
  BOOLEAN done;
  assert( type(x) == NEXT, "EvalNext: x is not NEXT!" );
  assert( Down(x) != x, "EvalNext: x has no child!" );
  assert( env == nil || type(env) == ENV, "EvalNext: env!" );
  debug2(DCE,DD, "EvalNext(%s, %s)", EchoObject(null,x), EchoObject(null,env));
  Child(y, Down(x));

  /* if argument of @Next can be evaluated, do so */
  if( type(y) == NEXT )  y = EvalNext(y, env);
  else if( type(y) == CLOSURE && is_par(type(actual(y))) )
  { prnt = SearchEnv(env, enclosing(actual(y)));
    if( prnt == nil )
    { Error(WARN,&fpos(y),"environment missing when evaluating %s", KW_NEXT);
    }
    else
    { assert( prnt != nil, "EvalNext: prnt == nil!" );
      for( link = Down(prnt);  link != prnt;  link = NextDown(link) )
      {	Child(par, link);
	if( type(par) == PAR && actual(par) == actual(y) )
	{ assert( Down(par) != par, "AttachEnv: par!" );
	  Child(py, Down(par));
	  if( type(py) == WORD )
	  { y = CopyObject(py, &fpos(py));
	    DisposeChild(Down(x));
	    Link(x, y);
	  }
	  break;
	}
      }
    }
  }

  /* if argument of @Next is a WORD, increment it */
  if( type(y) == WORD )
  { done = FALSE;
    y = Next(y, 1, &done);
    ReplaceNode(y, x);
    DisposeObject(x);
    x = y;
  }

  debug1(DCE, DD, "EvalNext returning %s", EchoObject(null, x));
  return x;
} /* end EvalNext */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT SetEnv(x, y)                                                      */
/*                                                                           */
/*  Create a new environment containing x and possibly y.                    */
/*                                                                           */
/*****************************************************************************/

OBJECT SetEnv(x, y)
OBJECT x, y;
{ OBJECT res;
  debug2(DCE, D, "SetEnv( %s, %s )", EchoObject(null, x), EchoObject(null, y));
  assert( x != nil && type(x) == CLOSURE, "SetEnv: x == nil or not CLOSURE!" );
  assert( y == nil || type(y) == ENV, "SetEnv: y != nil && type(y) != ENV!" );
  res = New(ENV);
  Link(res, x);
  if( y != nil )  Link(res, y);
  debug1(DCE, D, "SetEnv returning %s", EchoObject(null, res));
  return res;
} /* end SetEnv */


/*@@**************************************************************************/
/*                                                                           */
/*  AttachEnv(env, x)                                                        */
/*                                                                           */
/*  Attach environment env to CLOSURE x.                                     */
/*                                                                           */
/*****************************************************************************/

AttachEnv(env, x)
OBJECT env, x;
{ debug2(DCE,D,"AttachEnv( %s, %s )", EchoObject(null,env), EchoObject(null,x));
  assert( env != nil && type(env) == ENV, "AttachEnv: type(env) != ENV!" );
  assert( type(x) == CLOSURE, "AttachEnv: type(x) != CLOSURE!" );
  Link(x, env);
  debug0(DCE, D, "AttachEnv returning.");
} /* end AttachEnv */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT GetEnv(x)                                                         */
/*                                                                           */
/*  Get from CLOSURE x the environment previously attached.                  */
/*                                                                           */
/*****************************************************************************/

OBJECT GetEnv(x)
OBJECT x;
{ OBJECT env;
  debug1(DCE, DD, "GetEnv( %s )", EchoObject(null, x));
  assert( type(x) == CLOSURE, "GetEnv: type(x) != CLOSURE!" );
  assert( LastDown(x) != x, "GetEnv: LastDown(x) == x!" );
  Child(env, LastDown(x));
  assert( type(env) == ENV, "GetEnv: type(env) != ENV!" );
  debug1(DCE, DD, "GetEnv resturning %s", EchoObject(null, env));
  return env;
} /* end GetEnv */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT DetachEnv(x)                                                      */
/*                                                                           */
/*  Detach from CLOSURE x the environment previously attached.               */
/*                                                                           */
/*****************************************************************************/

OBJECT DetachEnv(x)
OBJECT x;
{ OBJECT env;
  debug1(DCE, DD, "DetachEnv( %s )", EchoObject(null, x));
  assert( type(x) == CLOSURE, "DetachEnv: type(x) != CLOSURE!" );
  assert( LastDown(x) != x, "DetachEnv: LastDown(x) == x!" );
  Child(env, LastDown(x));
  DeleteLink(LastDown(x));
  assert( type(env) == ENV, "DetachEnv: type(env) != ENV!" );
  debug1(DCE, DD, "DetachEnv resturning %s", EchoObject(null, env));
  return env;
} /* end DetachEnv */


/*@@**************************************************************************/
/*                                                                           */
/*  OBJECT ClosureExpand(x, env, style, crs_wanted, crs, res_env)            */
/*                                                                           */
/*  Return expansion of closure x in environment env and style style.        */
/*  The body comes from x's environment if x is a parameter, else from the   */
/*  symbol table.  The original x is pushed into the environments.           */
/*                                                                           */
/*  If crs_wanted and x has a tag, a cross-reference is added to crs.        */
/*                                                                           */
/*****************************************************************************/

OBJECT ClosureExpand(x, env, style, crs_wanted, crs, res_env)
OBJECT x, env;  STYLE *style;  BOOLEAN crs_wanted;  OBJECT *crs, *res_env;
{ OBJECT link, y, res, prnt_env, par, prnt, ppar;
  debug3(DCE, D, "ClosureExpand( %s, crs, %s, %s, res_env )",
		EchoObject(null, x), bool(crs_wanted), EchoObject(null, env));
  assert( type(x) == CLOSURE, "ClosureExpand given non-CLOSURE!");
  assert( predefined(actual(x)) == FALSE, "ClosureExpand given predefined!" );

  /* add a tag to x if needed but none provided */
  if( has_tag(actual(x)) )
  { for( link = Down(x);  link != x;  link = NextDown(link) )
    { Child(par, link);
      if( type(par) == PAR && is_tag(actual(par)) )  break;
    }
    if( link == x )
    { ppar = nil;
      for( link=Down(actual(x));  link != actual(x);  link = NextDown(link) )
      {	Child(y, link);
	if( is_par(type(y)) && is_tag(y) )
	{ ppar = y;
	  break;
	}
      }
      if( ppar != nil )
      {
	/* prepare new PAR containing generated tag */
	par = New(PAR);
	actual(par) = ppar;
	y = CrossGenTag(x);
	Link(par, y);

	/* find the right spot, then link it to x */
	switch( type(ppar) )
	{
	  case LPAR:	link = Down(x);
			break;

	  case NPAR:	link = Down(x);
			if( Down(x) != x )
			{ Child(y, Down(x));
			  if( type(y) == PAR && type(actual(par)) == LPAR )
				link = NextDown(link);
			}
			break;

	  case RPAR:	for( link = Down(x); link != x; link = NextDown(link) )
			{ Child(y, link);
			  if( type(y) != PAR )  break;
			}
			break;
	}
	Link(link, par);
      }
    }
  }

  /* add cross-reference to crs if needed */
  if( crs_wanted && has_tag(actual(x)) )
  { OBJECT tmp;
    tmp = CopyObject(x, no_fpos);  AttachEnv(env, tmp);
    y = CrossMake(actual(x), tmp, CROSS_TARG);
    tmp = New(CROSS_TARG);
    actual(tmp) = y;
    Link(tmp, y);
    if( *crs == nil )  *crs = New(CR_LIST);
    Link(*crs, tmp);
  }


  /* case x is a parameter */
  res = *res_env = nil;
  if( is_par(type(actual(x))) )
  {
    prnt = SearchEnv(env, enclosing(actual(x)));
    if( prnt == nil )
      Error(FATAL, &fpos(x), "symbol with import list used illegally");
    assert( prnt != nil, "ClosureExpand: is_par but prnt == nil!" );
    prnt_env = GetEnv(prnt);
    for( link = Down(prnt);  link != prnt;  link = NextDown(link) )
    { Child(par, link);
      if( type(par) == PAR && actual(par) == actual(x) )
      {	assert( Down(par) != par, "ExpandCLosure: Down(par)!");
	Child(res, Down(par));
	if( dirty(enclosing(actual(par))) )
	{ debug2(DSU, DD, "  copying %s %s",
		SymName(actual(par)), EchoObject(null, res));
	  res = CopyObject(res, no_fpos);
	}
	else
	{ debug2(DSU, DD, "  linking %s %s",
		SymName(actual(par)), EchoObject(null, res));
	  DeleteLink(Down(par));
	  y = MakeWord("??", &fpos(res));
	  Link(par, y);
	}
	ReplaceNode(res, x);
	if( type(actual(x)) == RPAR && has_body(enclosing(actual(x))) )
	{ *res_env = SetEnv(prnt, nil);
	  DisposeObject(x);
	}
	else
	{ AttachEnv(env, x);
	  *res_env = SetEnv(x, prnt_env);
	}
	break;
      }
    }
  }

  /* case x is a user-defined symbol or default parameter */
  if( res == nil )
  { if( sym_body(actual(x)) == nil )  res = MakeWord("??", &fpos(x));
    else res = CopyObject(sym_body(actual(x)), &fpos(x));
    ReplaceNode(res, x);
    AttachEnv(env, x);
    *res_env = SetEnv(x, nil);
  }

  assert( *res_env != nil && type(*res_env) == ENV, "ClosureExpand: *res_env!");
  debug0(DCE, D, "ClosureExpand returning, res =");
  ifdebug(DCE, D, EchoObject(stderr, res));
  debug1(DCE, DD, "  environment = %s", EchoObject(null, *res_env));
  return res;
} /* end ClosureExpand */
