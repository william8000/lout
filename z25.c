/*@z25.c:Object Echo:aprint(), cprint(), printnum()@**************************/
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
/*  FILE:         z25.c                                                      */
/*  MODULE:       Object Echo                                                */
/*  EXTERNS:      EchoObject(), PrintObject()                                */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#if DEBUG_ON

static	int	limit;			/* column where newline is needed    */
static	int	indent;			/* current indent                    */
static	int	col;			/* current output column             */
static	FILE	*fp;			/* current output file               */

#define	moveright()	(indent += 3)
#define	moveleft()	(indent -= 3)


/*****************************************************************************/
/*                                                                           */
/*  static aprint(x)                                                         */
/*  static cprint(x)                                                         */
/*                                                                           */
/*  Print the ASCII or FULL_CHAR string x onto the appropriate output.       */
/*                                                                           */
/*****************************************************************************/

static cprint(x)
FULL_CHAR *x;
{ col += StringLength(x);
  if( fp == null ) AppendString(x);
  else StringFPuts(x, fp);
} /* end print */

static aprint(x)
char *x;
{ cprint(AsciiToFull(x));
} /* end aprint */


/*****************************************************************************/
/*                                                                           */
/*  static printnum(x)                                                       */
/*                                                                           */
/*  Print the number x onto the appropriate output.                          */
/*                                                                           */
/*****************************************************************************/

static printnum(x)
int x;
{ cprint(StringInt(x));
} /* end printnum */


/*@::tab(), newline(), space()@***********************************************/
/*                                                                           */
/*  static tab(x)                                                            */
/*                                                                           */
/*  Tab to column x, or anyway insert at least one space.                    */
/*                                                                           */
/*****************************************************************************/

static tab(x)
int x;
{  do
     aprint(" ");
   while( col < x );
} /* end tab */


/*****************************************************************************/
/*                                                                           */
/*  static newline()                                                         */
/*                                                                           */
/*  Echo a newline to the appropriate output (unless output is a string).    */
/*  Correct indenting and right limits are maintained, if possible.          */
/*                                                                           */
/*****************************************************************************/

static newline()
{ if( fp == null )  AppendString(STR_SPACE);
  else
  { fputs("\n", fp);
    fflush(fp);
    for( col = 0;  col < indent;  col++ )  fputs(" ", fp);
  }
} /* end newline */


/*****************************************************************************/
/*                                                                           */
/*  static space(n)                                                          */
/*                                                                           */
/*  Echo n spaces to the appropriate output.                                 */
/*  Correct indenting and right limits are maintained, if possible.          */
/*                                                                           */
/*****************************************************************************/

static space(n)
int n;
{ int i;
  if( fp == null )
    for( i = 0;  i < n;  i++ )  AppendString(STR_SPACE);
  else if( col + n > limit )
  { fputs("\n", fp);
    for( col = 0;  col < n-1;  col++ )  fputs(" ", fp);
  }
  else for( i = 0;  i < n;  col++, i++ )  fputs(" ", fp);
} /* end space */


/*@::echo()@******************************************************************/
/*                                                                           */
/*  static echo(x, outer_prec)                                               */
/*                                                                           */
/*  Echo x.  The result will be enclosed in braces only if its precedence    */
/*  is less than or equal to outer_prec (words and parameterless closures    */
/*  are taken to have infinite precedence, i.e. never enclosed in braces).   */
/*                                                                           */
/*****************************************************************************/

static echo(x, outer_prec)
OBJECT x;  unsigned outer_prec;
{ OBJECT link, y, tmp, sym;
  char *op;  int prec, i;
  BOOLEAN npar_seen, name_printed, lbr_printed, braces_needed;

  switch( type(x) )
  {

    case DEAD:

	aprint("#dead");
	break;

    case UNATTACHED:
    
	aprint( "#unattached " );
	moveright();
	if( Down(x) != x )
	{ Child(y, Down(x));
	  if( y != x ) echo(y, NO_PREC);
	  else aprint("<child is self!>");
	}
	else aprint("<no child!>");
	moveleft();
	break;


    case EXPAND_IND:
    case GALL_PREC:
    case GALL_FOLL:
    case GALL_TARG:
    case CROSS_PREC:
    case CROSS_FOLL:
    case CROSS_TARG:
    case RECURSIVE:
    
	aprint("#"); cprint(Image(type(x))); aprint(" ");
	echo(actual(x), NO_PREC);
	break;

		
    case RECEPTIVE:
    case RECEIVING:
    
	aprint(type(x) == RECEIVING ? "#receiving " : "#receptive ");
	if( external(actual(x)) )  aprint("(external) ");
	if( threaded(actual(x)) )  aprint("(threaded) ");
	if( blocked(x) )           aprint("(blocked) " );
	if( trigger_externs(x) )   aprint("(trigger_externs) " );
	if( non_blocking(x) )      aprint("(non_blocking) " );
	cprint( type(actual(x)) == CLOSURE ?
		SymName(actual(actual(x))) : Image(type(actual(x))) );
	aprint(" ");
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  moveright();
	  echo(y, NO_PREC);
	  moveleft();
	}
	break;


    case PRECEDES:
    
	aprint("#precedes");
	break;


    case FOLLOWS:
    
	aprint("#follows");
	if( blocked(x) )  aprint(" (blocked)");
	Child(y, Down(x));
	if( Up(y) == LastUp(y) )  aprint(" (no precedes!)");
	break;


    case HEAD:
    
	aprint("Galley ");  cprint(SymName(actual(x)));
	aprint(" into ");   cprint(SymName(whereto(x)));
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  newline();
	  echo(y, type(y) == GAP_OBJ ? VCAT : VCAT_PREC);
	}
	break;


    case ROW_THR:

	aprint("{R ");
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  echo(y, VCAT_PREC);
	  newline();
	  if( NextDown(link) != x )  aprint("/R ");
	}
	aprint("R}");
	break;


    case COL_THR:

	aprint("{C ");
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  echo(y, HCAT_PREC);
	  newline();
	  if( NextDown(link) != x )  aprint("|C ");
	}
	aprint("C}");
	break;


    case VCAT: op = "/", prec = VCAT_PREC;  goto ETC;
    case HCAT: op = "|", prec = HCAT_PREC;  goto ETC;
    case ACAT: op = "&", prec = ACAT_PREC;  goto ETC;
    
	ETC:
	if( Down(x) == x )
	{ aprint(op);
	  aprint("<empty>");
	  break;
	}
	if( prec <= outer_prec ) aprint("{ ");
	/* *** if( Down(x) == LastDown(x) )  aprint(op);  must be manifested */
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  if( is_index(type(y)) )
	    newline();
	  else if( (type(y) == GAP_OBJ && type(x) != ACAT) )
	    newline();
	  if( type(y) == GAP_OBJ )  echo(y, type(x));
	  else echo(y, prec);
	}
	if( prec <= outer_prec )  aprint(" }");
	break;


    case GAP_OBJ:

	/* in this case the outer_prec argument is VCAT, HCAT or ACAT */
	if( Down(x) != x )
	{ if( outer_prec == ACAT )  aprint(" ");
	  cprint( EchoCatOp(outer_prec, mark(gap(x)), join(gap(x))) );
	  Child(y, Down(x));
	  echo(y, FORCE_PREC);
	  aprint(" ");
	}
	else if( outer_prec == ACAT )
	{ for( i = 1;  i <= vspace(x);  i++ )  newline();
	  for( i = 1;  i <= hspace(x);  i++ )  aprint(" ");
	}
	else
	{ cprint( EchoCatOp(outer_prec, mark(gap(x)), join(gap(x))) );
	  cprint( EchoGap(&gap(x)) );
	  aprint(" ");
	}
	break;


    case WORD:
    
	if( StringLength(string(x)) == 0 )
	  aprint("{}");
	else cprint( string(x) );
	break;


    case QWORD:
    
	cprint( StringQuotedWord(x) );
	break;


    case ENV:
    
	/* debug only */
	aprint("<");
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  if( type(y) == CLOSURE )
	  { cprint( SymName(actual(y)) );
	    echo(GetEnv(y), NO_PREC);
	  }
	  else if( type(y) == ENV )  echo(y, NO_PREC);
	  else cprint(Image(type(y)));
	  if( NextDown(link) != x )  aprint(" ");
	}
	aprint(">");
	break;


    case CROSS:

	assert( Down(x) != x, "echo: CROSS Down(x)!" );
	Child(y, Down(x));
	if( type(y) == CLOSURE )  cprint(SymName(actual(y)));
	else
	{ cprint(KW_LBR);
	  echo(y, NO_PREC);
	  cprint(KW_RBR);
	}
	cprint(KW_CROSS);
	if( NextDown(Down(x)) != x )
	{ Child(y, NextDown(Down(x)));
	  echo(y, NO_PREC);
	}
	else aprint("??");
	break;


    case CLOSURE:
    
	sym = actual(x);
	braces_needed =
	    precedence(sym) <= outer_prec && (has_lpar(sym) || has_rpar(sym));

	/* print brace if needed */
	if( braces_needed )  aprint("{ ");

	npar_seen = FALSE;  name_printed = FALSE;
	for( link = Down(x); link != x;  link = NextDown(link) )
	{ Child(y, link);
	  if( type(y) == PAR )
	  { assert( Down(y) != y, "EchoObject: Down(PAR)!" );
	    switch( type(actual(y)) )
	    {
	     case LPAR:	Child(tmp, Down(y));
			echo(tmp, (unsigned) precedence(sym));
			aprint(" ");
			break;

	     case NPAR:	if( !name_printed )
			{ cprint(SymName(sym));
			  if( external(x) || threaded(x) )
			  { aprint(" #");
			    if( external(x) )  aprint(" external");
			    if( threaded(x) )  aprint(" threaded");
			    newline();
			  }
			  name_printed = TRUE;
			}
			newline();  aprint("  ");
			cprint( SymName(actual(y)) );
			aprint(" { ");
			Child(tmp, Down(y));
			echo(tmp, NO_PREC);
			aprint(" }");
			npar_seen = TRUE;
			break;

	     case RPAR:	if( !name_printed )
			{ cprint(SymName(sym));
			  if( external(x) || threaded(x) )
			  { aprint(" #");
			    if( external(x) )  aprint(" external");
			    if( threaded(x) )  aprint(" threaded");
			    newline();
			  }
			  name_printed = TRUE;
			}
			if( npar_seen ) newline();
			else aprint(" ");
			Child(tmp, Down(y));
			if( has_body(sym) )
			{ aprint("{ ");
			  echo(tmp, NO_PREC);
			  aprint(" }");
			}
			else echo(tmp, (unsigned) precedence(sym));
			break;
	
	     default:	Error(INTERN, &fpos(y), "echo: %s",
					Image(type(actual(y))) );
			break;

	    }
	  }
	}
	if( !name_printed )
	{ cprint( SymName(sym) );
	  if( external(x) || threaded(x) )
	  { aprint(" #");
	    if( external(x) )  aprint(" external");
	    if( threaded(x) )  aprint(" threaded");
	    newline();
	  }
	}

	/* print closing brace if needed */
	if( braces_needed ) aprint(" }");
	break;


    case SPLIT:
    
	/* this should occur only in debug output case */
	cprint(KW_SPLIT);  moveright();
	Child(y, DownDim(x, COL));
	aprint(" ");
	echo(y, FORCE_PREC);
	moveleft();
	break;


    case PAR:
    
	/* this should occur only in debug output case */
	aprint("par ");  cprint(SymName(actual(x)));
	break;


    case CR_LIST:

	aprint("(");
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  echo(y, NO_PREC);
	  if( NextDown(link) != x )  aprint(", ");
	}
	aprint(")");
	break;


    case MACRO:
    
	newline();  cprint(KW_MACRO);
	aprint(" ");  cprint(SymName(x));
	if( sym_body(x) != nil )
	{ newline();  cprint(KW_LBR);
	  y = sym_body(x);
	  do
	  { for( i = 1;  i <= vspace(y);  i++ )  newline();
	    for( i = 1;  i <= hspace(y);  i++ )  aprint(" ");
	    cprint(EchoToken(y));
	    y = succ(y, PARENT);
	  } while( y != sym_body(x) );
	  newline();  aprint(KW_RBR);
	}
	else aprint(" {}");
	if( visible(x) )  aprint(" # (visible)");
	break;


    case NPAR:
    case LOCAL:
    
	/* print predefined operators in abbreviated form */
	if( sym_body(x) == nil && enclosing(x) != nil )
	{ tab(3); aprint("# sys ");
	  cprint(SymName(x));
	  break;
	}

	/* print def line and miscellaneous debug info */
	if( type(x) == LOCAL ) newline();
	cprint(type(x) == NPAR ? KW_NAMED : KW_DEF);
	aprint(" ");  cprint( SymName(x) );
	if( recursive(x) || indefinite(x) || visible(x) ||
	    is_extern_target(x) || uses_extern_target(x) || uses_galley(x) )
	{ tab(25);  aprint("#");
	  if( visible(x)  )  aprint(" visible");
	  if( recursive(x)  )  aprint(" recursive");
	  if( indefinite(x) )  aprint(" indefinite");
	  if( is_extern_target(x) )  aprint(" is_extern_target");
	  if( uses_extern_target(x) )  aprint(" uses_extern_target");
	  if( uses_galley(x) )  aprint(" uses_galley");
	}

	/* print uses list, if necessary */
	if( uses(x) != nil || dirty(x) )
	{ newline();  aprint("   # ");
	  if( dirty(x) ) aprint("dirty, ");
	  aprint("uses");
	  if( uses(x) != nil )
	  { tmp = next(uses(x));
	    do
	    { aprint(" "), cprint( SymName(item(tmp)) );
	      tmp = next(tmp);
	    } while( tmp != next(uses(x)) );
	  }
	  /* ***
	  for( tmp = uses(x);  tmp != nil;  tmp = next(tmp) )
	  { aprint(" "), cprint( SymName(item(tmp)) );
	  }
	  *** */
	}

	/* print precedence, if necessary */
	if( precedence(x) != DEFAULT_PREC )
	{ newline();  aprint("   ");  cprint(KW_PRECEDENCE);
	  aprint(" ");  printnum(precedence(x));
	}

	/* print associativity, if necessary */
	if( !right_assoc(x) )
	{ newline();  aprint("   ");
	  cprint(KW_ASSOC);  aprint(" ");  cprint(KW_LEFT);
	}

	/* print named parameters and local objects */
	lbr_printed = FALSE;
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  assert( enclosing(y) == x, "echo: enclosing(y) != x!" );
	  switch( type(y) )
	  {
	    case LPAR:
	    case RPAR:	newline();  aprint("   ");
			cprint( type(y) == LPAR ? KW_LEFT :
			    has_body(x) ? KW_BODY : KW_RIGHT);
			aprint(" ");
			cprint( SymName(y) );
			aprint("   # uses_count = ");
			printnum(uses_count(y));
			if( visible(y) )  aprint(" (visible)");
			break;

	    case NPAR:	moveright();  newline();
			echo(y, NO_PREC);
			aprint("   # uses_count = ");
			printnum(uses_count(y));
			moveleft();
			break;

	    case MACRO:
	    case LOCAL:	if( !lbr_printed )
			{ newline();
			  cprint(KW_LBR);
			  lbr_printed = TRUE;
			}
			moveright();
			echo(y, NO_PREC);
			moveleft();  newline();
			break;

	    default:	Error(FATAL, &fpos(y), "echo: type(y) = %s",
					Image(type(y)));
			break;
	  }
	}
	if( type(x) == NPAR && Down(x) == x )  aprint(" ");
	else newline();
	if( !lbr_printed )
	{ cprint(KW_LBR);  aprint("  ");
	  lbr_printed = TRUE;
	}
	else aprint("   ");

	/* print body */
	moveright();
	if( sym_body(x) != nil )  echo(sym_body(x), NO_PREC);
	moveleft();  if( type(x) == LOCAL ) newline();
	cprint(KW_RBR);
	break;


    case ONE_COL:
    case ONE_ROW:
    case HCONTRACT:
    case VCONTRACT:
    case HEXPAND:
    case VEXPAND:
    case PADJUST:
    case HADJUST:
    case VADJUST:
    case HSCALE:
    case VSCALE:
    case NEXT:
    case WIDE:
    case HIGH:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case GRAPHIC:
    case ROTATE:
    case SCALE:
    case CASE:
    case YIELD:
    case XCHAR:
    case FONT:
    case SPACE:
    case BREAK:
    case OPEN:
    case TAGGED:
    
	/* print enclosing left brace if needed */
	braces_needed = (DEFAULT_PREC <= outer_prec);
	if( braces_needed )  cprint(KW_LBR), aprint(" ");

	/* print left parameter */
	if( Down(x) != LastDown(x) )
	{ Child(y, Down(x));
	  echo(y, max(outer_prec, DEFAULT_PREC));
	  aprint(" ");
	}

	cprint(Image(type(x)));

	/* print right parameter */
	assert( LastDown(x) != x, "echo: right parameter of predefined!" );
	aprint(" ");
	Child(y, LastDown(x));
	echo(y, type(x)==OPEN ? FORCE_PREC : max(outer_prec,DEFAULT_PREC));
	if( braces_needed )  aprint(" "), cprint(KW_RBR);
	break;


    case NULL_CLOS:
    
	cprint(Image(type(x)));
	break;


    case CR_ROOT:

	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  echo(y, NO_PREC);  newline();
	}
	break;


    case CROSS_SYM:

	aprint("Cross-references for ");
	cprint(SymName(symb(x)));  newline();
	switch( target_state(x) )
	{
	  case 0:	aprint("NO_TARGET");
			break;

	  case 1:	aprint("SEEN_TARGET ");
			printnum(target_seq(x));
			aprint(": ");
			echo(target_val(x), NO_PREC);
			break;

	  case 2:	aprint("WRITTEN_TARGET ");
			printnum(target_seq(x));
			aprint(": to file ");
			cprint(FileName(target_file(x)));
			aprint(" at ");
			printnum(target_pos(x));
			break;
	
	  default:	aprint("ILLEGAL!");
			break;
	}
	newline();
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  aprint("   ");
	  if( gall_rec(y) )  aprint("gall_rec!");
	  else cprint(string(y));
	  newline();
	}
	break;


    default:
    
	Error(INTERN, no_fpos, "echo found %s", Image(type(x)));
	break;

  } /* end switch */
} /* end echo */


/*@::EchoObject(), DebugObject()@*********************************************/
/*                                                                           */
/*  FULL_CHAR *EchoObject(x)                                                 */
/*                                                                           */
/*  Return an image of unsized object x in result.                           */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *EchoObject(x)
OBJECT x;
{ debug0(DOE, D, "EchoObject()");
  fp = null;;
  col = 0;
  indent = 0;
  limit  = 60;
  if( fp == null )
  BeginString();
  if( x == nil )  AppendString(AsciiToFull("<nil>"));
  else echo(x, type(x) == GAP_OBJ ? VCAT : 0);
  debug0(DOE, D, "EchoObject returning");
  return EndString();
} /* end EchoObject */


/*****************************************************************************/
/*                                                                           */
/*  DebugObject(x)                                                           */
/*                                                                           */
/*  Send an image of unsized object x to result.                             */
/*                                                                           */
/*****************************************************************************/

DebugObject(x)
OBJECT x;
{ debug0(DOE, D, "DebugObject()");
  fp = stderr;
  col = 0;
  indent = 0;
  limit  = 60;
  if( x == nil )  fprintf(stderr, "<nil>");
  else echo(x, type(x) == GAP_OBJ ? VCAT : 0);
  fprintf(stderr, "\n");
  debug0(DOE, D, "DebugObject returning");
} /* end DebugObject */
#endif
