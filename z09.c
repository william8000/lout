/*@z09.c:Closure Expansion:SearchEnv()@***************************************/
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
/*  FILE:         z09.c                                                      */
/*  MODULE:       Closure Expansion                                          */
/*  EXTERNS:      SearchEnv(), SetEnv(), AttachEnv(), GetEnv(),              */
/*                DetachEnv(), ClosureExpand()                               */
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
  debug2(DCE, DD, "SearchEnv(%s, %s)", EchoObject(env), SymName(sym));
  for(;;)
  {
    debug1(DCE, DDD, "  searching env %s", EchoObject(env));
    assert( env != nil && type(env) == ENV, "SearchEnv: env!" );
    if( Down(env) == env )
    { debug0(DCE, DD, "SearchEnv returning <nil>");
      return nil;
    }
    Child(y, Down(env));
    assert( type(y) == CLOSURE, "SearchEnv: type(y) != CLOSURE!" );
    if( actual(y) == sym )
    { debug1(DCE, DD, "SearchEnv returning %s", EchoObject(y));
      return y;
    }
    assert( LastDown(y) != y, "SearchEnv: LastDown(y) == y!" );
    link = LastDown(env) != Down(env) ? LastDown(env) : LastDown(y);
    Child(env, link);
  }
} /* end SearchEnv */


/*@::SetEnv(), AttachEnv(), GetEnv(), DetachEnv()@****************************/
/*                                                                           */
/*  OBJECT SetEnv(x, y)                                                      */
/*                                                                           */
/*  Create a new environment containing x and possibly y.                    */
/*                                                                           */
/*****************************************************************************/

OBJECT SetEnv(x, y)
OBJECT x, y;
{ OBJECT res;
  debug2(DCR, DD, "SetEnv( %s, %s )", EchoObject(x), EchoObject(y));
  debug2(DCE, D, "SetEnv( %s, %s )", EchoObject(x), EchoObject(y));
  assert( x != nil && type(x) == CLOSURE, "SetEnv: x == nil or not CLOSURE!" );
  assert( y == nil || type(y) == ENV, "SetEnv: y != nil && type(y) != ENV!" );
  res = New(ENV);  Link(res, x);
  if( y != nil )  Link(res, y);
  debug1(DCE, D, "SetEnv returning %s", EchoObject(res));
  return res;
} /* end SetEnv */


/*****************************************************************************/
/*                                                                           */
/*  AttachEnv(env, x)                                                        */
/*                                                                           */
/*  Attach environment env to CLOSURE x.                                     */
/*                                                                           */
/*****************************************************************************/

AttachEnv(env, x)
OBJECT env, x;
{ debug2(DCE, D, "AttachEnv( %s, %s )", EchoObject(env), EchoObject(x));
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
  assert( type(x) == CLOSURE, "GetEnv: type(x) != CLOSURE!" );
  assert( LastDown(x) != x, "GetEnv: LastDown(x) == x!" );
  Child(env, LastDown(x));
  assert( type(env) == ENV, "GetEnv: type(env) != ENV!" );
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
  debug1(DCE, DD, "DetachEnv( %s )", EchoObject(x));
  assert( type(x) == CLOSURE, "DetachEnv: type(x) != CLOSURE!" );
  assert( LastDown(x) != x, "DetachEnv: LastDown(x) == x!" );
  Child(env, LastDown(x));
  DeleteLink(LastDown(x));
  assert( type(env) == ENV, "DetachEnv: type(env) != ENV!" );
  debug1(DCE, DD, "DetachEnv resturning %s", EchoObject(env));
  return env;
} /* end DetachEnv */


/*@::ClosureExpand()@*********************************************************/
/*                                                                           */
/*  OBJECT ClosureExpand(x, env, crs_wanted, crs, res_env)                   */
/*                                                                           */
/*  Return expansion of closure x in environment env.                        */
/*  The body comes from the environment of x if x is a parameter, else from  */
/*  the symbol table.  The original x is pushed into the environments.       */
/*  If crs_wanted and x has a tag, a cross-reference is added to crs.        */
/*                                                                           */
/*****************************************************************************/

OBJECT ClosureExpand(x, env, crs_wanted, crs, res_env)
OBJECT x, env;  BOOLEAN crs_wanted;  OBJECT *crs, *res_env;
{ OBJECT link, y, res, prnt_env, par, prnt;
  debug3(DCE, D, "ClosureExpand( %s, crs, %s, %s, res_env )",
    EchoObject(x), bool(crs_wanted), EchoObject(env));
  assert( type(x) == CLOSURE, "ClosureExpand given non-CLOSURE!");
  assert( predefined(actual(x)) == FALSE, "ClosureExpand given predefined!" );

  /* add tag to x if needed but not provided;  add cross-reference to crs  */
  CrossAddTag(x);
  if( crs_wanted && has_tag(actual(x)) )
  { OBJECT tmp = CopyObject(x, no_fpos);  AttachEnv(env, tmp);
    y = CrossMake(actual(x), tmp, CROSS_TARG);
    tmp = New(CROSS_TARG);  actual(tmp) = y;  Link(tmp, y);
    if( *crs == nil )  *crs = New(CR_LIST);   Link(*crs, tmp);
  }

  /* case x is a parameter */
  res = *res_env = nil;
  if( is_par(type(actual(x))) )
  { prnt = SearchEnv(env, enclosing(actual(x)));
    if( prnt==nil ) Error(FATAL, &fpos(x), "symbol with import list misused");
    assert( prnt != nil, "ClosureExpand: is_par but prnt == nil!" );
    prnt_env = GetEnv(prnt);
    for( link = Down(prnt);  link != prnt;  link = NextDown(link) )
    { Child(par, link);
      if( type(par) == PAR && actual(par) == actual(x) )
      {	assert( Down(par) != par, "ExpandCLosure: Down(par)!");
	Child(res, Down(par));
	if( dirty(enclosing(actual(par))) )
	{ debug2(DSU, DD, "c %s %s", SymName(actual(par)), EchoObject(res));
	  res = CopyObject(res, no_fpos);
	}
	else
	{ debug2(DSU, DD, "l %s %s", SymName(actual(par)), EchoObject(res));
	  DeleteLink(Down(par));
	  y = MakeWord(WORD, STR_NOCROSS, &fpos(res));
	  Link(par, y);
	}
	ReplaceNode(res, x);
	if( type(actual(x)) == RPAR && has_body(enclosing(actual(x))) )
	{ debug0(DCR, DD, "  calling SetEnv from ClosureExpand (a)");
	  *res_env = SetEnv(prnt, nil);  DisposeObject(x);
	}
	else
	{ AttachEnv(env, x);
	  debug0(DCR, DD, "  calling SetEnv from ClosureExpand (b)");
	  *res_env = SetEnv(x, prnt_env);
	}
	break;
      }
    }
  }

  /* case x is a user-defined symbol or default parameter */
  if( res == nil )
  { if( sym_body(actual(x)) == nil )  res = MakeWord(WORD,STR_NOCROSS,&fpos(x));
    else res = CopyObject(sym_body(actual(x)), &fpos(x));
    ReplaceNode(res, x);  AttachEnv(env, x);
    debug0(DCR, DD, "  calling SetEnv from ClosureExpand (c)");
    *res_env = SetEnv(x, nil);
  }

  assert( *res_env != nil && type(*res_env) == ENV, "ClosureExpand: *res_env!");
  debug0(DCE, D, "ClosureExpand returning, res =");
  ifdebug(DCE, D, DebugObject(res));
  debug1(DCE, DD, "  environment = %s", EchoObject(*res_env));
  return res;
} /* end ClosureExpand */
