/*@z25.c:Object Echo:EchoObject()@********************************************/
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
/*  FILE:         z25.c                                                      */
/*  MODULE:       Object Echo                                                */
/*  EXTERNS:      EchoObject()                                               */
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
/*  static print(x)                                                          */
/*                                                                           */
/*  Print the string x onto the appropriate output.                          */
/*                                                                           */
/*****************************************************************************/

static print(x)
unsigned char *x;
{ col += strlen(x);
  if( fp == null ) AppendString(x);
  else fputs(x, fp);
} /* end print */


/*****************************************************************************/
/*                                                                           */
/*  static printnum(x)                                                       */
/*                                                                           */
/*  Print the number x onto the appropriate output.                          */
/*                                                                           */
/*****************************************************************************/

static printnum(x)
int x;
{ static unsigned char buff[20];
  sprintf(buff, "%d", x);
  print(buff);
} /* end printnum */


/*****************************************************************************/
/*                                                                           */
/*  static tab(x)                                                            */
/*                                                                           */
/*  Tab to column x, or anyway insert at least one space.                    */
/*                                                                           */
/*****************************************************************************/

static tab(x)
int x;
{  do
     print(" ");
   while( col < x );
} /* end tab */


/*@@**************************************************************************/
/*                                                                           */
/*  static newline()                                                         */
/*                                                                           */
/*  Echo a newline to the appropriate output (unless output is a string).    */
/*  Correct indenting and right limits are maintained, if possible.          */
/*                                                                           */
/*****************************************************************************/

static newline()
{ if( fp == null )  AppendString(" ");
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
    for( i = 0;  i < n;  i++ )  AppendString(" ");
  else if( col + n > limit )
  { fputs("\n", fp);
    for( col = 0;  col < n-1;  col++ )  fputs(" ", fp);
  }
  else for( i = 0;  i < n;  col++, i++ )  fputs(" ", fp);
} /* end space */


/*@@**************************************************************************/
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
  unsigned char str[10], *op;  int prec, i;
  BOOLEAN npar_seen, name_printed, lbr_printed, braces_needed;

  switch( type(x) )
  {

    case DEAD:

	print("#dead");
	break;

    case UNATTACHED:
    
	print( "#unattached " );
	moveright();
	if( Down(x) != x )
	{ Child(y, Down(x));
	  if( y != x ) echo(y, NO_PREC);
	  else print("<child is self!>");
	}
	else print("<no child!>");
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
    
	print("#"); print(Image(type(x))); print(" ");
	echo(actual(x), NO_PREC);
	break;

		
    case RECEPTIVE:
    case RECEIVING:
    
	print(type(x) == RECEIVING ? "#receiving " : "#receptive ");
	if( external(actual(x)) )  print("(external) ");
	if( threaded(actual(x)) )  print("(threaded) ");
	if( blocked(x) )           print("(blocked) " );
	if( trigger_externs(x) )   print("(trigger_externs) " );
	if( non_blocking(x) )      print("(non_blocking) " );
	print( type(actual(x)) == CLOSURE ?
		SymName(actual(actual(x))) : Image(type(actual(x))) );
	print(" ");
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  moveright();
	  echo(y, NO_PREC);
	  moveleft();
	}
	break;


    case PRECEDES:
    
	print("#precedes");
	break;


    case FOLLOWS:
    
	print("#follows");
	if( blocked(x) )  print(" (blocked)");
	Child(y, Down(x));
	if( Up(y) == LastUp(y) )  print(" (no precedes!)");
	break;


    case HEAD:
    
	print("Galley ");  print(SymName(actual(x)));
	print(" into ");   print(SymName(whereto(x)));
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  newline();
	  echo(y, type(y) == GAP_OBJ ? VCAT : VCAT_PREC);
	}
	break;


    case ROW_THR:

	print("{R ");
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  echo(y, VCAT_PREC);
	  newline();
	  if( NextDown(link) != x )  print("/R ");
	}
	print("R}");
	break;


    case COL_THR:

	print("{C ");
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  echo(y, HCAT_PREC);
	  newline();
	  if( NextDown(link) != x )  print("|C ");
	}
	print("C}");
	break;


    case VCAT: op = (unsigned char *) "/", prec = VCAT_PREC;  goto ETC;
    case HCAT: op = (unsigned char *) "|", prec = HCAT_PREC;  goto ETC;
    case ACAT: op = (unsigned char *) "&", prec = ACAT_PREC;  goto ETC;
    
	ETC:
	if( Down(x) == x )
	{ print(op);
	  print("<empty>");
	  break;
	}
	if( prec <= outer_prec ) print("{ ");
	/* *** if( Down(x) == LastDown(x) )  print(op);  must be manifested */
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  if( is_index(type(y)) )
	    newline();
	  else if( (type(y) == GAP_OBJ && type(x) != ACAT) )
	    newline();
	  if( type(y) == GAP_OBJ )  echo(y, type(x));
	  else echo(y, prec);
	}
	if( prec <= outer_prec )  print(" }");
	break;


    case GAP_OBJ:

	/* in this case the outer_prec argument is VCAT, HCAT or ACAT */
	if( Down(x) != x )
	{ if( outer_prec == ACAT )  print(" ");
	  print( EchoCatOp(outer_prec, mark(gap(x)), join(gap(x))) );
	  Child(y, Down(x));
	  echo(y, FORCE_PREC);
	  print(" ");
	}
	else if( outer_prec == ACAT )
	{ for( i = 1;  i <= vspace(x);  i++ )  newline();
	  for( i = 1;  i <= hspace(x);  i++ )  print(" ");
	}
	else
	{ print( EchoCatOp(outer_prec, mark(gap(x)), join(gap(x))) );
	  print( EchoGap(&gap(x)) );
	  print(" ");
	}
	break;


    case WORD:
    
	if( strlen(string(x)) == 0 )
	{ print(KW_LBR);
	  print(KW_RBR);
	}
	else print( string(x) );
	break;


    case ENV:
    
	/* debug only */
	print("<");
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  if( type(y) == CLOSURE )
	  { print( SymName(actual(y)) );
	    echo(GetEnv(y), NO_PREC);
	  }
	  else if( type(y) == ENV )  echo(y, NO_PREC);
	  else print(Image(type(y)));
	  if( NextDown(link) != x )  print(" ");
	}
	print(">");
	break;


    case CROSS:

	assert( Down(x) != x, "echo: CROSS Down(x)!" );
	Child(y, Down(x));
	if( type(y) == CLOSURE )  print(SymName(actual(y)));
	else
	{ print(KW_LBR);
	  echo(y, NO_PREC);
	  print(KW_RBR);
	}
	print(KW_CROSS);
	if( NextDown(Down(x)) != x )
	{ Child(y, NextDown(Down(x)));
	  echo(y, NO_PREC);
	}
	else print("??");
	break;


    case CLOSURE:
    
	sym = actual(x);
	braces_needed =
	    precedence(sym) <= outer_prec && (has_lpar(sym) || has_rpar(sym));

	/* print brace if needed */
	if( braces_needed )  print(KW_LBR), print(" ");

	npar_seen = FALSE;  name_printed = FALSE;
	for( link = Down(x); link != x;  link = NextDown(link) )
	{ Child(y, link);
	  if( type(y) == PAR )
	  { assert( Down(y) != y, "EchoObject: Down(PAR)!" );
	    switch( type(actual(y)) )
	    {
	     case LPAR:	Child(tmp, Down(y));
			echo(tmp, (unsigned) precedence(sym));
			print(" ");
			break;

	     case NPAR:	if( !name_printed )
			{ print(SymName(sym));
			  if( external(x) || threaded(x) )
			  { print(" #");
			    if( external(x) )  print(" external");
			    if( threaded(x) )  print(" threaded");
			    newline();
			  }
			  name_printed = TRUE;
			}
			newline();  print("  ");
			print( SymName(actual(y)) );
			print(" ");  print(KW_LBR);  print(" ");
			Child(tmp, Down(y));
			echo(tmp, NO_PREC);
			print(" ");  print(KW_RBR);
			npar_seen = TRUE;
			break;

	     case RPAR:	if( !name_printed )
			{ print(SymName(sym));
			  if( external(x) || threaded(x) )
			  { print(" #");
			    if( external(x) )  print(" external");
			    if( threaded(x) )  print(" threaded");
			    newline();
			  }
			  name_printed = TRUE;
			}
			if( npar_seen ) newline();
			else print(" ");
			Child(tmp, Down(y));
			if( has_body(sym) )
			{ print(KW_LBR);  print(" ");
			  echo(tmp, NO_PREC);
			  print(" ");  print(KW_RBR);
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
	{ print( SymName(sym) );
	  if( external(x) || threaded(x) )
	  { print(" #");
	    if( external(x) )  print(" external");
	    if( threaded(x) )  print(" threaded");
	    newline();
	  }
	}

	/* print closing brace if needed */
	if( braces_needed ) print(" "), print(KW_RBR);
	break;


    case SPLIT:
    
	/* this should occur only in debug output case */
	print(KW_SPLIT);  moveright();
	Child(y, DownDim(x, COL));
	print(" ");
	echo(y, FORCE_PREC);
	moveleft();
	break;


    case PAR:
    
	/* this should occur only in debug output case */
	print("par ");  print(SymName(actual(x)));
	break;


    case CR_LIST:

	print("(");
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  echo(y, NO_PREC);
	  if( NextDown(link) != x )  print(", ");
	}
	print(")");
	break;


    case MACRO:
    
	newline();  print(KW_MACRO);
	print(" ");  print(SymName(x));
	if( sym_body(x) != nil )
	{ newline();  print(KW_LBR);
	  y = sym_body(x);
	  do
	  { for( i = 1;  i <= vspace(y);  i++ )  newline();
	    for( i = 1;  i <= hspace(y);  i++ )  print(" ");
	    print(EchoToken(y));
	    y = succ(y, PARENT);
	  } while( y != sym_body(x) );
	  newline();  print(KW_RBR);
	}
	else print(" "), print(KW_LBR), print(KW_RBR);
	if( visible(x) )  print(" # (visible)");
	break;


    case NPAR:
    case LOCAL:
    
	/* print predefined operators in abbreviated form */
	if( sym_body(x) == nil && enclosing(x) != nil )
	{ tab(3); print("# sys ");
	  print(SymName(x));
	  break;
	}

	/* print def line and miscellaneous debug info */
	if( type(x) == LOCAL ) newline();
	print(type(x) == NPAR ? KW_NAMED : KW_DEF);
	print(" ");  print( SymName(x) );
	if( recursive(x) || indefinite(x) || visible(x) ||
	    is_extern_target(x) || uses_extern_target(x) || uses_galley(x) )
	{ tab(25);  print("#");
	  if( visible(x)  )  print(" visible");
	  if( recursive(x)  )  print(" recursive");
	  if( indefinite(x) )  print(" indefinite");
	  if( is_extern_target(x) )  print(" is_extern_target");
	  if( uses_extern_target(x) )  print(" uses_extern_target");
	  if( uses_galley(x) )  print(" uses_galley");
	}

	/* print uses list, if necessary */
	if( uses(x) != nil || dirty(x) )
	{ newline();  print("   # ");
	  if( dirty(x) ) print("dirty, ");
	  print("uses");
	  if( uses(x) != nil )
	  { tmp = next(uses(x));
	    do
	    { print(" "), print( SymName(item(tmp)) );
	      tmp = next(tmp);
	    } while( tmp != next(uses(x)) );
	  }
	  /* ***
	  for( tmp = uses(x);  tmp != nil;  tmp = next(tmp) )
	  { print(" "), print( SymName(item(tmp)) );
	  }
	  *** */
	}

	/* print precedence, if necessary */
	if( precedence(x) != DEFAULT_PREC )
	{ newline();  print("   ");  print(KW_PRECEDENCE);
	  sprintf(str, " %d", precedence(x));
	  print(str);
	}

	/* print associativity, if necessary */
	if( !right_assoc(x) )
	{ newline();  print("   ");
	  print(KW_ASSOC);  print(" ");  print(KW_LEFT);
	}

	/* print named parameters and local objects */
	lbr_printed = FALSE;
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  assert( enclosing(y) == x, "echo: enclosing(y) != x!" );
	  switch( type(y) )
	  {
	    case LPAR:
	    case RPAR:	newline();  print("   ");
			print( type(y) == LPAR ? KW_LEFT :
			    has_body(x) ? KW_BODY : KW_RIGHT);
			print(" ");
			print( SymName(y) );
			print("   # uses_count = ");
			printnum(uses_count(y));
			if( visible(y) )  print(" (visible)");
			break;

	    case NPAR:	moveright();  newline();
			echo(y, NO_PREC);
			print("   # uses_count = ");
			printnum(uses_count(y));
			moveleft();
			break;

	    case MACRO:
	    case LOCAL:	if( !lbr_printed )
			{ newline();
			  print(KW_LBR);
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
	if( type(x) == NPAR && Down(x) == x )  print(" ");
	else newline();
	if( !lbr_printed )
	{ print(KW_LBR);  print("  ");
	  lbr_printed = TRUE;
	}
	else print("   ");

	/* print body */
	moveright();
	if( sym_body(x) != nil )  echo(sym_body(x), NO_PREC);
	moveleft();  if( type(x) == LOCAL ) newline();
	print(KW_RBR);
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
    case FONT:
    case SPACE:
    case BREAK:
    case OPEN:
    case TAGGED:
    
	/* print enclosing left brace if needed */
	braces_needed = (DEFAULT_PREC <= outer_prec);
	if( braces_needed )  print(KW_LBR), print(" ");

	/* print left parameter */
	if( Down(x) != LastDown(x) )
	{ Child(y, Down(x));
	  echo(y, max(outer_prec, DEFAULT_PREC));
	  print(" ");
	}

	print(Image(type(x)));

	/* print right parameter */
	assert( LastDown(x) != x, "echo: right parameter of predefined!" );
	print(" ");
	Child(y, LastDown(x));
	echo(y, type(x)==OPEN ? FORCE_PREC : max(outer_prec,DEFAULT_PREC));
	if( braces_needed )  print(" "), print(KW_RBR);
	break;


    case NULL_CLOS:
    
	print(Image(type(x)));
	break;


    case CR_ROOT:

	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  echo(y, NO_PREC);  newline();
	}
	break;


    case CROSS_SYM:

	print("Cross-references for ");
	print(SymName(symb(x)));  newline();
	switch( target_state(x) )
	{
	  case 0:	print("NO_TARGET");
			break;

	  case 1:	print("SEEN_TARGET ");
			printnum(target_seq(x));
			print(": ");
			echo(target_val(x), NO_PREC);
			break;

	  case 2:	print("WRITTEN_TARGET ");
			printnum(target_seq(x));
			print(": to file ");
			print(FileName(target_file(x)));
			print(" at ");
			printnum(target_pos(x));
			break;
	
	  default:	print("ILLEGAL!");
			break;
	}
	newline();
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  print("   ");
	  if( gall_rec(y) )  print("gall_rec!");
	  else print(string(y));
	  newline();
	}
	break;


    default:
    
	Error(INTERN, no_fpos, "echo found %s", Image(type(x)));
	break;

  } /* end switch */
} /* end echo */


/*@@**************************************************************************/
/*                                                                           */
/*  unsigned char *EchoObject(file_ptr, x)                                   */
/*                                                                           */
/*  Send an image of unsized object x to result (if file_ptr is null) or to  */
/*  file file_ptr if file_ptr is not null.                                   */
/*                                                                           */
/*****************************************************************************/

unsigned char *EchoObject(file_ptr, x)
FILE *file_ptr;  OBJECT x;
{ debug0(DOE, D, "EchoObject()");
  fp = file_ptr;
  col = 0;
  indent = 0;
  limit  = 60;
  if( fp == null )
  { BeginString();
    if( x == nil )  AppendString("<nil>");
    else echo(x, type(x) == GAP_OBJ ? VCAT : 0);
    return EndString();
  }
  else
  { if( x != nil )
    { echo(x, type(x) == GAP_OBJ ? VCAT : 0);
      fprintf(file_ptr, "\n");
    }
    else fprintf(file_ptr, "<nil>\n");
    debug0(DOE, D, "EchoObject returning");
    return (unsigned char *) "";
  }
} /* end EchoObject */
#endif
