/*@z25.c:Object Echo:aprint(), cprint(), printnum()@**************************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.08)                       */
/*  COPYRIGHT (C) 1991, 1996 Jeffrey H. Kingston                             */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@cs.usyd.edu.au)                                */
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
/*  EXTERNS:      EchoObject(), DebugObject()                                */
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

static void cprint(FULL_CHAR *x)
{ col += StringLength(x);
  if( fp == null ) AppendString(x);
  else StringFPuts(x, fp);
} /* end print */

static void aprint(char *x)
{ cprint(AsciiToFull(x));
} /* end aprint */


/*****************************************************************************/
/*                                                                           */
/*  static printnum(x)                                                       */
/*                                                                           */
/*  Print the number x onto the appropriate output.                          */
/*                                                                           */
/*****************************************************************************/

static void printnum(int x)
{ cprint(StringInt(x));
} /* end printnum */


/*@::tab(), newline(), space()@***********************************************/
/*                                                                           */
/*  static tab(x)                                                            */
/*                                                                           */
/*  Tab to column x, or anyway insert at least one space.                    */
/*                                                                           */
/*****************************************************************************/

static void tab(int x)
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

static void newline(void)
{ if( fp == null )  AppendString(STR_SPACE);
  else
  { fputs("\n", fp);
    fflush(fp);
    for( col = 0;  col < indent;  col++ )  fputs(" ", fp);
  }
} /* end newline */


/*@::echo()@******************************************************************/
/*                                                                           */
/*  static echo(x, outer_prec)                                               */
/*                                                                           */
/*  Echo x.  The result will be enclosed in braces only if its precedence    */
/*  is less than or equal to outer_prec (words and parameterless closures    */
/*  are taken to have infinite precedence, i.e. never enclosed in braces).   */
/*                                                                           */
/*****************************************************************************/

static void echo(OBJECT x, unsigned outer_prec)
{ OBJECT link, y, tmp, sym;
  char *op, buff[20];  int prec, i, count;
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


    case SCALE_IND:
    case COVER_IND:
    case EXPAND_IND:
    case GALL_PREC:
    case GALL_FOLL:
    case GALL_FOLL_OR_PREC:
    case GALL_TARG:
    case CROSS_PREC:
    case CROSS_FOLL:
    case CROSS_TARG:
    case RECURSIVE:
    case PAGE_LABEL_IND:
    
	/* aprint("#"); cprint(Image(type(x))); aprint(" "); */
	echo(actual(x), NO_PREC);
	break;

		
    case RECEPTIVE:
    case RECEIVING:
    
	aprint(type(x) == RECEIVING ? "#receiving " : "#receptive ");
	if( external_ver(actual(x)) )  aprint("(external_ver) ");
	if( external_hor(actual(x)) )  aprint("(external_hor) ");
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


    case ACAT: op = "&", prec = ACAT_PREC; /*   goto ETC;  */

	count = 0;
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  if( type(y) == GAP_OBJ ) continue;
	  count++;
	  if( link == Down(x) || link == LastDown(x) )
	    echo(y, prec);
	  else if( NextDown(NextDown(link)) == LastDown(x) )
	  { sprintf(buff, " ++%d++ ", count+1);
	    aprint(buff);
	  }
	}
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
	else
	{ aprint("\"");
	  cprint( string(x) );
	  aprint("\"");
	}
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
	    if( LastDown(y) != y )  echo(GetEnv(y), NO_PREC);
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
	/* ***
	cprint(KW_CROSS);
	aprint("<");
	cprint(Image(cross_type(x)));
	aprint(">");
	*** */
	aprint(" ");
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
			  /* ***
			  aprint("%");
			  cprint(SymName(enclosing(sym)));
			  *** */
			  if( external_ver(x) || external_hor(x) || threaded(x) )
			  { aprint(" #");
			    if( external_ver(x) )  aprint(" external_ver");
			    if( external_hor(x) )  aprint(" external_hor");
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
			  /* ***
			  aprint("%");
			  cprint(SymName(enclosing(sym)));
			  *** */
			  if( external_ver(x) || external_hor(x) || threaded(x) )
			  { aprint(" #");
			    if( external_ver(x) )  aprint(" external_ver");
			    if( external_hor(x) )  aprint(" external_hor");
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
	
	     default:	assert1(FALSE, "echo:", Image(type(actual(y))));
			break;

	    }
	  }
	}
	if( !name_printed )
	{ cprint( SymName(sym) );
	  /* ***
	  aprint("%");
	  cprint(SymName(enclosing(sym)));
	  *** */
	  if( external_ver(x) || external_hor(x) || threaded(x) )
	  { aprint(" #");
	    if( external_ver(x) )  aprint(" external_ver");
	    if( external_hor(x) )  aprint(" external_hor");
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
	Child(y, DownDim(x, COLM));
	aprint(" COLM:");
	echo(y, FORCE_PREC);
	newline();
	/* ***
	Child(y, DownDim(x, ROWM));
	aprint(" ROWM:");
	echo(y, FORCE_PREC);
	*** */
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
	if( sym_body(x) != nilobj )
	{ newline();  cprint(KW_LBR);
	  y = sym_body(x);
	  do
	  { for( i = 1;  i <= vspace(y);  i++ )  newline();
	    for( i = 1;  i <= hspace(y);  i++ )  aprint(" ");
	    cprint(EchoToken(y));
	    y = succ(y, PARENT);
	  } while( y != sym_body(x) );
	  newline();  cprint(KW_RBR);
	}
	else aprint(" {}");
	if( visible(x) )  aprint(" # (visible)");
	break;


    case NPAR:
    case LOCAL:
    
	/* print predefined operators in abbreviated form */
	if( sym_body(x) == nilobj && enclosing(x) != nilobj )
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
	if( uses(x) != nilobj || dirty(x) )
	{ newline();  aprint("   # ");
	  if( dirty(x) ) aprint("dirty, ");
	  aprint("uses");
	  if( uses(x) != nilobj )
	  { tmp = next(uses(x));
	    do
	    { aprint(" "), cprint( SymName(item(tmp)) );
	      tmp = next(tmp);
	    } while( tmp != next(uses(x)) );
	  }
	  /* ***
	  for( tmp = uses(x);  tmp != nilobj;  tmp = next(tmp) )
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

	    default:	assert1(FALSE, "echo:", Image(type(y)));
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
	if( sym_body(x) != nilobj )  echo(sym_body(x), NO_PREC);
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
    case HCOVER:
    case VCOVER:
    case COMMON:
    case RUMP:
    case INSERT:
    case NEXT:
    case WIDE:
    case HIGH:
    case HSHIFT:
    case VSHIFT:
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
    case YUNIT:
    case ZUNIT:
    case BREAK:
    case UNDERLINE:
    case COLOUR:
    case LANGUAGE:
    case OPEN:
    case TAGGED:
    case ENV_OBJ:

    
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


    case CURR_LANG:
    case BACKEND:
    case PAGE_LABEL:

	/* predefined symbols that have (or may have) no parameters */
	cprint(Image(type(x)));
	break;


    case FILTERED:

	aprint("[filtered ");
	if( Down(x) != x )
	{ Child(y, Down(x));
	  if( type(y) != WORD ) cprint(Image(type(y)));
	  else cprint(string(y));
	}
	else aprint("?");
	aprint("]");
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
    
	assert1(FALSE, "echo:", Image(type(x)));
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

FULL_CHAR *EchoObject(OBJECT x)
{ debug0(DOE, D, "EchoObject()");
  fp = null;
  col = 0;
  indent = 0;
  limit  = 60;
  if( fp == null )
  BeginString();
  if( x == nilobj )  AppendString(AsciiToFull("<nilobj>"));
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

void DebugObject(OBJECT x)
{ debug0(DOE, D, "DebugObject()");
  fp = stderr;
  col = 0;
  indent = 0;
  limit  = 60;
  if( x == nilobj )  fprintf(stderr, "<nilobj>");
  else echo(x, type(x) == GAP_OBJ ? VCAT : 0);
  fprintf(stderr, "\n");
  debug0(DOE, D, "DebugObject returning");
} /* end DebugObject */


/*@::EchoIndex()@*************************************************************/
/*                                                                           */
/*  FULL_CHAR *EchoIndex()                                                   */
/*                                                                           */
/*  Echo a component of a galley, briefly.                                   */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *EchoIndex(OBJECT index)
{ static char buff[MAX_BUFF];  OBJECT z;
  if( index == nilobj )
  { sprintf(buff, "<nilobj>");
  }
  else switch( type(index) )
  {
    case RECEIVING:

      sprintf(buff, "receiving %s%s", type(actual(index)) == CLOSURE ?
	SymName(actual(actual(index))) : Image(type(actual(index))),
	non_blocking(index) ? " (non_blocking)" : "");
      break;


    case RECEPTIVE:

      sprintf(buff, "receptive %s%s", type(actual(index)) == CLOSURE ?
	SymName(actual(actual(index))) : Image(type(actual(index))),
	non_blocking(index) ? " (non_blocking)" : "");
      break;


    case UNATTACHED:

      if( Down(index) != index )
      { Child(z, Down(index));
      }
      else z = nilobj;
      sprintf(buff, "unattached %s",
	z == nilobj ? AsciiToFull("<nilobj>") : SymName(actual(z)));
      break;


    case WORD:
    case QWORD:

      sprintf(buff, "\"%s\"", string(index));
      break;


    default:

      sprintf(buff, "%s", Image(type(index)));
      break;
  }
  return AsciiToFull(buff);
} /* end EchoIndex */


/*@::DebugGalley()@***********************************************************/
/*                                                                           */
/*  DebugGalley(hd, pinpt, indent)                                           */
/*                                                                           */
/*  Print overview of galley hd on stderr; mark pinpoint if found            */
/*                                                                           */
/*****************************************************************************/

void DebugGalley(OBJECT hd, OBJECT pinpt, int indent)
{ OBJECT link, y, z;  char istr[30];  int i;
  for( i = 0;  i < indent;  i++ )  istr[i] = ' ';
  istr[i] = '\0';
  if( type(hd) != HEAD )
  { fprintf(stderr, "%shd is %s\n", istr, Image(type(hd)));
    return;
  }
  fprintf(stderr, "%sgalley %s into %s\n", istr,
    SymName(actual(hd)), SymName(whereto(hd)));
  for( link = Down(hd);  link != hd;  link = NextDown(link) )
  { Child(y, link);
    if( y == pinpt )
    { fprintf(stderr, "++ %s ", Image(type(y)));
      DebugObject(y);
    }
    else switch( type(y) )
    {
      case GAP_OBJ:

	fprintf(stderr, "%s%s %s\n", istr, "gap", EchoGap(&gap(y)));
	break;


      case CROSS_PREC:

	fprintf(stderr, "%scross_prec %s\n", istr, EchoObject(y));
	break;


      case PAGE_LABEL_IND:

	fprintf(stderr, "%spage_label_ind %s\n", istr, EchoObject(y));
	break;


      case CROSS_TARG:

	fprintf(stderr, "%scross_targ %s\n", istr, EchoObject(y));
	break;


      case EXPAND_IND:

	fprintf(stderr, "%s%ld expand_ind %s\n", istr, (long) y,
	  Image(type(actual(y))));
	break;


      case RECEIVING:

	fprintf(stderr, "%sreceiving %s\n", istr, type(actual(y)) == CLOSURE ?
	  SymName(actual(actual(y))) : Image(type(actual(y))));
	if( Down(y) != y )
	{ Child(z, Down(y));
	  DebugGalley(z, nilobj, indent+4);
	}
	break;


      case RECEPTIVE:

	fprintf(stderr, "%sreceptive %s\n", istr, type(actual(y)) == CLOSURE ?
	  SymName(actual(actual(y))) : Image(type(actual(y))));
	if( Down(y) != y )
	{ Child(z, Down(y));
	  DebugGalley(z, nilobj, indent+4);
	}
	break;


      case UNATTACHED:

	fprintf(stderr, "%sunattached\n", istr);
	if( Down(y) != y )
	{ Child(z, Down(y));
	  DebugGalley(z, nilobj, indent+4);
	}
	break;


      case ONE_COL:
      case ONE_ROW:
      case WIDE:
      case HIGH:
      case HSHIFT:
      case VSHIFT:
      case HSCALE:
      case VSCALE:
      case HCOVER:
      case VCOVER:
      case HCONTRACT:
      case VCONTRACT:
      case HEXPAND:
      case VEXPAND:
      case PADJUST:
      case HADJUST:
      case VADJUST:
      case ROTATE:
      case SCALE:
      case INCGRAPHIC:
      case SINCGRAPHIC:
      case GRAPHIC:
      case ACAT:
      case HCAT:
      case VCAT:
      case ROW_THR:
      case CROSS:

	fprintf(stderr, "%s%s\n", istr, Image(type(y)));
	break;


      case CLOSURE:

	fprintf(stderr, "%s%s  external_hor = %s, external_ver = %s\n", istr,
	  SymName(actual(y)), bool(external_hor(y)), bool(external_ver(y)));
	break;


      case WORD:
      case QWORD:

	fprintf(stderr, "%s\"%s\"\n", istr, string(y));
	break;


      default:

	break;
    }
  }
} /* end DebugGalley */
#endif
