/*****************************************************************************/
/*                                                                           */
/*  PROG2LOUT: A PROGRAM TO CONVERT PROGRAM SOURCES INTO LOUT (VERSION 1.0)  */
/*  COPYRIGHT (C) 2000 Jeffrey H. Kingston                                   */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@cs.su.oz.au)                                   */
/*  Basser Department of Computer Science                                    */
/*  The University of Sydney 2006                                            */
/*  AUSTRALIA                                                                */
/*                                                                           */
/*  This program is free software; you can redistribute it and/or modify     */
/*  it under the terms of the GNU General Public License as published by     */
/*  the Free Software Foundation; either version 2, or (at your option)      */
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
/*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>

/*****************************************************************************/
/*                                                                           */
/*  How to add another language to prg2lout's list                           */
/*                                                                           */
/*  Step 1.  Have a browse through the token declarations below, and work    */
/*  out which of them you need for your language.  If you need a token that  */
/*  isn't there already, you'll have to define it; there are many examples   */
/*  there to help you.                                                       */
/*                                                                           */
/*  Step 2.  Browse through the language declarations, and declare your      */
/*  language following those examples:  first you give a set of one or more  */
/*  alternative names for your language, then the default printing style for */
/*  it (either "fixed", "varying", or "symbol"), then you list the tokens    */
/*  of the language, then you list the keywords.                             */
/*                                                                           */
/*  Step 3.  Add your language variable to the list in the initializer of    */
/*  variable languages, as you can see the others have been done.            */
/*                                                                           */
/*  Step 4.  If any lists of initializers now contain more than              */
/*  MAX_ARRAY_LENGTH-1 elements, increase MAX_ARRAY_LENGTH until they don't. */
/*                                                                           */
/*  Step 5.  Recompile and reinstall prg2lout, test "prg2lout -u" then       */
/*  "prg2lout -l <mylanguage> <myfile> | lout -s > out.ps".                  */
/*                                                                           */
/*  Step 6.  Send your tested and verified changes to jeff@cs.usyd.edu.au    */
/*  for incorporation in the next Lout release.  If you do this, please      */
/*  try hard to ensure that your changes conform to the formal definition    */
/*  of your language.                                                        */
/*                                                                           */
/*****************************************************************************/


/*****************************************************************************/
/*                                                                           */
/*  Character sets                                                           */
/*                                                                           */
/*  Here are prg2lout's definitions of various commonly needed sets of      */
/*  characters.  May need enhancement for Latin1 etc.                        */
/*                                                                           */
/*****************************************************************************/

char AllPrintable[] = 
  " !\"#$%&'()*+,-./0123456789:;<=>?@[\\]^_`\\{|}~\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ;

char AllPrintablePlusNL[] =
  " !\"#$%&'()*+,-./0123456789:;<=>?@[\\]^_`\\{|}~\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\n" ;

char AllPrintableTabNL[] =
  " !\"#$%&'()*+,-./0123456789:;<=>?@[\\]^_`\\{|}~\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\n\t" ;

char NotDoubleQuote[] =
  " !#$%&'()*+,-./0123456789:;<=>?@[\\]^_`\\{|}~\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ;

char NotQuote[] =
  " !#$%&\"()*+,-./0123456789:;<=>?@[\\]^_`\\{|}~\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ;

char Letters[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ;

char Letter_Digit[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789" ;

#define SepLetters 							\
"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",	\
"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",	\
"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",	\
"n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"

#define SepDigits "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"


/*****************************************************************************/
/*                                                                           */
/*  TOKEN - put your token declarations in this section                      */
/*                                                                           */
/*****************************************************************************/
#define MAX_CHAR 256
#define MAX_STRING_LENGTH 101
#define MAX_ARRAY_LENGTH 101


typedef struct token_rec {
  char *name;			  /* the name of this token, e.g. "string"   */
  char *command;		  /* the Lout command for it, e.g. "@D"      */
  char *alternate_command;	  /* the alternate command for it, e.g. "@K" */
  char *starts[MAX_ARRAY_LENGTH]; /* strings that start this token           */
  char *legal;			  /* legal characters in this token          */
  char *escape;			  /* the escape character e.g. "\\", or ""   */
  char *escape_legal;		  /* legal characters after escape character */
  char *inner_escape;		  /* inner escape, e.g. "`", or else ""      */
  char *end_inner_escape;	  /* end inner escape, e.g. "'"              */
  char *end_delimiter;		  /* the end delimiter, else ""              */

  /* The following options are initialized by the program, so don't you      */
  char chtype[MAX_CHAR];	  /* character types within token            */
  char escape_chtype[MAX_CHAR];	  /* character types after escape            */
} TOKEN;


TOKEN CStringToken = {
  "string",		/* used by error messages involving this token      */
  "@S",			/* Lout command for formatting strings              */
  "",			/* No alternate command                             */
  { "\"" },		/* strings begin with a " character                 */
  NotDoubleQuote,	/* inside, any printable except " is OK             */
  "\\",			/* within strings, \\ is the escape character       */
  AllPrintablePlusNL,	/* after escape char, any printable char or nl OK   */
  "",			/* strings do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "\""			/* strings end with a " character                   */
};

TOKEN CCharacterToken = {
  "character",		/* used by error messages involving this token      */
  "@C",			/* Lout command for formatting characters           */
  "",			/* No alternate command                             */
  { "'" },		/* characters begin with a ' character              */
  NotQuote,		/* inside, any printable except ' is OK             */
  "\\",			/* within characters, \\ is the escape character    */
  AllPrintable,		/* after escape char, any printable char is OK      */
  "",			/* characters do not permit "inner escapes"         */
  "",			/* and so there is no end innner escape either      */
  "'"			/* characters end with a ' character                */
};

TOKEN EiffelStringToken = {
  "string",		/* used by error messages involving this token      */
  "@S",			/* Lout command for formatting strings              */
  "",			/* No alternate command                             */
  { "\"" },		/* strings begin with a " character                 */
  NotDoubleQuote,	/* inside, any printable except " is OK             */
  "%",			/* within strings, % is the escape character        */
  AllPrintable,		/* after escape char, any printable char is OK      */
  "",			/* strings do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "\""			/* strings end with a " character                   */
};

TOKEN EiffelCharacterToken = {
  "character",		/* used by error messages involving this token      */
  "@C",			/* Lout command for formatting characters           */
  "",			/* No alternate command                             */
  { "'" },		/* characters begin with a ' character              */
  NotQuote,		/* inside, any printable except ' is OK             */
  "%",			/* within characters, % is the escape character     */
  AllPrintable,		/* after escape char, any printable char is OK      */
  "",			/* characters do not permit "inner escapes"         */
  "",			/* and so there is no end innner escape either      */
  "'"			/* characters end with a ' character                */
};

TOKEN IdentifierToken = {
  "identifier",		/* used by error messages involving this token      */
  "@D",			/* Lout command for formatting identifiers          */
  "@K",			/* Alternate command (for keywords)                 */
  { SepLetters, "_" },	/* identifiers begin with any letter or _           */
  Letter_Digit,		/* inside, letters, underscores, digits are OK      */
  "",			/* no escape character within identifiers           */
  "",			/* so nothing legal after escape char either        */
  "",			/* identifiers do not permit "inner escapes"        */
  "",			/* and so there is no end innner escape either      */
  ""			/* identifiers do not end with a delimiter          */
};

TOKEN NumberToken = {
  "number",		/* used by error messages involving this token      */
  "@N",			/* Lout command for formatting numbers              */
  "",			/* No alternate command                             */
  { SepDigits },	/* numbers must begin with a digit                  */
  "0123456789.eE",	/* inside, digits, decimal point, exponent          */
  "",			/* no escape character within numbers               */
  "",			/* so nothing legal after escape char either        */
  "",			/* numbers do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  ""			/* numbers do not end with a delimiter              */
};

TOKEN CCommentToken = {
  "comment",		/* used by error messages involving this token      */
  "@C",			/* Lout command for formatting comments             */
  "",			/* No alternate command                             */
  { "/*" },		/* comments begin with this character pair          */
  AllPrintableTabNL,	/* inside, any printable char, tab, or nl is OK     */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* C comments do not permit "inner escapes"         */
  "",			/* and so there is no end innner escape either      */
  "*/"			/* comments end with this character pair            */
};

TOKEN CCommentEscapeToken = {
  "Lout escape",	/* used by error messages involving this token      */
  NULL,			/* no Lout command since passed through unformatted */
  "",			/* No alternate command                             */
  { "/*@" },		/* comments begin with this character pair          */
  AllPrintableTabNL,	/* inside, any printable char, tab, or nl is OK     */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* no "inner escape" in escape comments             */
  "",			/* so no end of "inner escape" either               */
  "*/"			/* comments end with this character pair            */
};

TOKEN CPPCommentToken = {
  "comment",		/* used by error messages involving this token      */
  "@C",			/* Lout command for formatting comments             */
  "",			/* No alternate command                             */
  { "//" },		/* comments begin with this character pair          */
  AllPrintable,		/* inside, any printable char is OK (not NL)        */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* C comments do not permit "inner escapes"         */
  "",			/* and so there is no end innner escape either      */
  ""			/* no end delimiter (end of line will end it)       */
};

TOKEN CPPCommentEscapeToken = {
  "Lout escape",	/* used by error messages involving this token      */
  NULL,			/* no Lout command since passed through unformatted */
  "",			/* No alternate command                             */
  { "//@" },		/* comments begin with this character pair          */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* no "inner escape" in escape comments             */
  "",			/* so no end of "inner escape" either               */
  ""			/* no end delimiter (end of line will end it)       */
};

TOKEN EiffelCommentToken = {
  "comment",		/* used by error messages involving this token      */
  "@C",			/* Lout command for formatting comments             */
  "",			/* No alternate command                             */
  { "--" },		/* comments begin with this character pair          */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "`",			/* start of "inner escape" in Eiffel comment        */
  "'",			/* end of "inner escape" in Eiffel comment          */
  ""			/* no ending delimiter; end of line will end it     */
};

TOKEN EiffelCommentEscapeToken = {
  "Lout escape",	/* used by error messages involving this token      */
  NULL,			/* no Lout command since passed through unformatted */
  "",			/* No alternate command                             */
  { "--@" },		/* comments begin with this character pair          */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* no "inner escape" in escape comments             */
  "",			/* so no end of "inner escape" either               */
  ""			/* no ending delimiter; end of line will end it     */
};

TOKEN BlueCommentToken = {
  "comment",		/* used by error messages involving this token      */
  "@C",			/* Lout command for formatting comments             */
  "",			/* No alternate command                             */
  { "==", "--" },	/* comments begin with this character pair          */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "`",			/* start of "inner escape" in Blue comment          */
  "'",			/* end of "inner escape" in Blue comment            */
  ""			/* no ending delimiter; end of line will end it     */
};

TOKEN BlueCommentEscapeToken = {
  "Lout escape",	/* used by error messages involving this token      */
  NULL,			/* no Lout command since passed through unformatted */
  "",			/* No alternate command                             */
  { "==@", "--@" },	/* comments begin with this character pair          */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* no "inner escape" in escape comments             */
  "",			/* so no end of "inner escape" either               */
  ""			/* no ending delimiter; end of line will end it     */
};


#define FixedToken(str, command) /* define fixed-string token */	\
{									\
  str,			/* name used for debugging only       */	\
  command,		/* Lout command for formatting this   */	\
  "",			/* No alternate command               */	\
  { str },		/* token begins (and ends!) with this */	\
  "",			/* nothing inside, since no inside    */	\
  "",			/* no escape character                */	\
  "",			/* so nothing after escape character  */	\
  "",			/* no inner escape either             */	\
  "",			/* so no end inner escape             */	\
  ""			/* no ending delimiter                */	\
}

TOKEN HashToken			= FixedToken("#",  "@O");
TOKEN ExclamationToken		= FixedToken("!",  "@O");
TOKEN PercentToken		= FixedToken("%",  "@O");
TOKEN HatToken			= FixedToken("^",  "@O");
TOKEN AmpersandToken		= FixedToken("&",  "@O");
TOKEN StarToken			= FixedToken("*",  "@ST");
TOKEN SlashToken		= FixedToken("/",  "@O");
TOKEN ArrowToken		= FixedToken("->", "arrowright @A @O");
TOKEN BackSlashToken		= FixedToken("\\", "@O");
TOKEN LeftParenToken		= FixedToken("(",  "@O");
TOKEN RightParenToken		= FixedToken(")",  "@O");
TOKEN MinusToken		= FixedToken("-",  "@M");
TOKEN PlusToken			= FixedToken("+",  "plus @A @O");
TOKEN EqualToken		= FixedToken("=",  "equal @A @O");
TOKEN LeftBraceToken		= FixedToken("{",  "@O");
TOKEN RightBraceToken		= FixedToken("}",  "@O");
TOKEN BarToken			= FixedToken("|",  "@O");
TOKEN CircumToken		= FixedToken("~",  "@O");
TOKEN LeftBracketToken		= FixedToken("[",  "@O");
TOKEN RightBracketToken		= FixedToken("]",  "@O");
TOKEN SemicolonToken		= FixedToken(";",  "@O");
TOKEN ColonToken		= FixedToken(":",  "@O");
TOKEN LessToken			= FixedToken("<",  "less @A @O");
TOKEN GreaterToken		= FixedToken(">",  "greater @A @O");
TOKEN QuestionToken		= FixedToken("?",  "@O");
TOKEN CommaToken		= FixedToken(",",  "@O");
TOKEN DotToken			= FixedToken(".",  "@O");
TOKEN EiffelDotToken		= FixedToken(".",  "@ED");
TOKEN LessEqualToken		= FixedToken("<=", "lessequal @A @O");
TOKEN GreaterEqualToken		= FixedToken(">=", "greaterequal @A @O");
TOKEN CNotEqualToken		= FixedToken("!=", "notequal @A @O");
TOKEN EiffelNotEqualToken	= FixedToken("/=", "notequal @A @O");
TOKEN BlueNotEqualToken		= FixedToken("<>", "notequal @A @O");
TOKEN AssignToken		= FixedToken(":=", "@O");
TOKEN QuestionAssignToken	= FixedToken("?=", "@O");
TOKEN DollarToken		= FixedToken("$",  "@O");
TOKEN ImpliesToken		= FixedToken("=>", "implies @A @O");


/*****************************************************************************/
/*                                                                           */
/*  LANGUAGE - put your language declarations in this section.               */
/*                                                                           */
/*****************************************************************************/

typedef struct lang_rec {
  char *names[MAX_ARRAY_LENGTH];
  char *default_style;
  TOKEN *tokens[MAX_ARRAY_LENGTH];
  char *keywords[MAX_ARRAY_LENGTH];
} LANGUAGE;

LANGUAGE CLanguage = {
  { "C", "c", "C++", "c++"  },
  "fixed",
  {
    &CStringToken, &CCharacterToken, &IdentifierToken, &NumberToken,
    &CCommentToken, &CCommentEscapeToken,
    &CPPCommentToken, &CPPCommentEscapeToken,
    &HashToken, &ExclamationToken, &PercentToken, &HatToken,
    &AmpersandToken, &StarToken, &LeftParenToken, &RightParenToken,
    &MinusToken, &PlusToken, &EqualToken, &LeftBraceToken, &RightBraceToken,
    &BarToken, &CircumToken, &LeftBracketToken, &RightBracketToken,
    &SemicolonToken, &ColonToken, &LessToken, &GreaterToken,
    &QuestionToken, &CommaToken, &DotToken, &SlashToken, &BackSlashToken,
    &ArrowToken, &LessEqualToken, &GreaterEqualToken, &CNotEqualToken
  },

  { "asm", "auto", "break", "case", "catch", "char", "class", "const", 
    "continue", "default", "delete", "do", "double", "else", "enum", 
    "extern", "float", "for", "friend", "goto", "if", "inline", "int", 
    "long", "new", "operator", "private", "protected", "public", "register", 
    "return", "short", "signed", "sizeof", "static", "struct", "switch", 
    "template", "this", "throw", "try", "typedef", "union", "unsigned", 
    "virtual", "void", "volatile", "while", 
  }
};


LANGUAGE EiffelLanguage = {
  { "Eiffel", "eiffel" },
  "varying",
  {
    &EiffelStringToken, &EiffelCharacterToken, &IdentifierToken, &NumberToken,
    &EiffelCommentToken, &EiffelCommentEscapeToken,
    &SemicolonToken, &CommaToken, &ColonToken, &EiffelDotToken,
    &ExclamationToken, &EqualToken, &EiffelNotEqualToken, &LeftParenToken,
    &RightParenToken, &LeftBracketToken, &RightBracketToken, &LeftBraceToken,
    &RightBraceToken, &AssignToken, &QuestionAssignToken, &PlusToken,
    &MinusToken, &DollarToken, &HatToken, &SlashToken, &BackSlashToken,
    &LessToken, &GreaterToken, &LessEqualToken, &GreaterEqualToken
  },

  { "alias", "all", "and", "as", "check", "class", "creation", "debug",
    "deferred", "do", "else", "elseif", "end", "ensure", "expanded",
    "export", "external", "false", "feature", "from", "frozen", "if",
    "implies", "indexing", "infix", "inherit", "inspect", "invariant",
    "is", "like", "local", "loop", "obsolete", "old", "once", "or",
    "prefix", "redefine", "rename", "require", "rescue", "retry", "select",
    "separate", "strip", "then", "true", "undefine", "unique", "until",
    "variant", "when", "xor", "not", "interface"
  }
};


LANGUAGE BlueLanguage = {
  { "Blue", "blue" },
  "varying",
  {
    &CStringToken, &IdentifierToken, &NumberToken,
    &BlueCommentToken, &BlueCommentEscapeToken,
    &CommaToken, &LessToken, &GreaterToken, &ColonToken, &AssignToken,
    &LeftParenToken, &RightParenToken, &LeftBracketToken, &RightBracketToken,
    &QuestionAssignToken, &ExclamationToken, &EiffelDotToken, &ImpliesToken,
    &EqualToken, &BlueNotEqualToken, &LeftBraceToken, &RightBraceToken,
    &PlusToken, &MinusToken, &StarToken, &SlashToken, &HatToken,
    &LessEqualToken, &GreaterEqualToken

  },

  { "and", "assert", "builtin", "case", "class", "const", "create",
    "creation", "deferred", "div", "do", "else", "elseif", "end",
    "Enumeration", "enumeration", "exit", "if", "in", "interface",
    "internal", "invariant", "is", "loop", "manifest", "mod", "not",
    "of", "old", "on", "or", "post", "pre", "redefined", "return",
    "routines", "super", "then", "uses", "var"
  }
};


LANGUAGE *languages[] = {
  & CLanguage,
  & EiffelLanguage,
  & BlueLanguage,
};

/*****************************************************************************/
/*                                                                           */
/*  If you are adding a new language, you don't need to change anything      */
/*  below this point.  Just repeating: don't change anything below here.     */
/*                                                                           */
/*****************************************************************************/


/*****************************************************************************/
/*                                                                           */
/*  Global constants and variables                                           */
/*                                                                           */
/*****************************************************************************/
#define DEBUG_SETUP	0
#define DEBUG_PROCESS	0
#define DEBUG_TRIE	0
#define DEBUG_NEXTCHAR	0
#define DEBUG_PREFIXEQ	0
#define DEBUG_EMIT	0
#define DEBUG_MAIN	0

#define PROG2LOUT_VERSION "prg2lout Version 1.0 (February 2000)"
#define	BOOLEAN		unsigned
#define	FALSE		0
#define	TRUE		1
#define	MAX_LINE	1024

/* print styles */
#define	NO_STYLE	0
#define	FIXED_STYLE	1
#define	VARYING_STYLE	2
#define	SYMBOL_STYLE	3

static char	file_name[MAX_LINE];	/* current input file name           */
static char	curr_line[MAX_LINE];	/* current input line                */
static int	line_num;		/* current input line number         */
static int	line_pos;		/* current input column number       */
static BOOLEAN	raw_seen;		/* TRUE if -r (raw mode)             */

static BOOLEAN	headers_option;		/* TRUE if no -n option (headers)    */
static int	style_option;		/* value of -p option, or NO_STYLE   */
static char	*font_option;		/* value of -f option, else null     */
static char	*size_option;		/* value of -s option, else null     */
static char	*line_option;		/* value of -v option, else null     */
static char	*tabin_option;		/* value of -t option, else null     */
static char	*tabout_option;		/* value of -T option, else null     */
static char	*language_option;	/* value of -l option, else null     */

static BOOLEAN	tab_by_spacing;		/* TRUE if using space chars to tab  */
static int	tab_in;			/* tab interval, value of -t option  */
static float	tab_out;		/* tab interval width (-T option)    */
static char 	tab_unit;		/* unit of measurement for tab       */

char *ErrorHeader()
{ static char buff[MAX_LINE];
  if( line_num == 0 )
    sprintf(buff, "prg2lout");
  else if( raw_seen )
    sprintf(buff, "prg2lout %d,%d", line_num, line_pos);
  else
    sprintf(buff, "prg2lout %s %d,%d", file_name, line_num, line_pos);
  return buff;
}

#define GetArg(arg, message, null_ok)					\
{ if( strcmp(argv[arg_pos]+2, "") != 0 )				\
    arg = argv[arg_pos]+2;						\
  else if( !null_ok && arg_pos < argc-1 && *argv[arg_pos+1] != '-' )	\
    arg = argv[++arg_pos];						\
  else if( null_ok )							\
    arg = (char *) NULL;						\
  else									\
  { fprintf(err_fp, "%s: %s\n", ErrorHeader(), message);		\
    exit(1);								\
  }									\
} /* end GetArg */


/*****************************************************************************/
/*                                                                           */
/*  char *EchoToken(TOKEN *t)                                                */
/*                                                                           */
/*  Print a brief resume of token t                                          */
/*                                                                           */
/*****************************************************************************/

char *EchoToken(TOKEN *t)
{ static char buff[MAX_LINE];
  if( t == (TOKEN *) NULL )
    sprintf(buff, "(NULL)");
  else
    sprintf(buff, "%s", t->name);
  return buff;
}



/*****************************************************************************/
/*                                                                           */
/*  TRIE                                                                     */
/*                                                                           */
/*  We use a trie to match the input against the opening pattern of each     */
/*  token, since some tokens (e.g. <=, // etc.) have multi-character         */
/*  opening patterns.                                                        */
/*                                                                           */
/*****************************************************************************/

typedef struct trie_node {
  struct trie_node *sub[MAX_CHAR];
  TOKEN		   *value[MAX_CHAR];
} *TRIE;

/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN TrieInsert(&T, str, val)                                         */
/*                                                                           */
/*  Insert str into trie T.  May need a new root so pass T by reference.     */
/*  Return FALSE if the insertion failed, either because the string was      */
/*  empty, or because it was the same as a previously inserted string.       */
/*                                                                           */
/*****************************************************************************/

BOOLEAN TrieInsert(TRIE *T, char *str, TOKEN *val)
{ BOOLEAN res;
  if( DEBUG_TRIE )
    fprintf(stderr, "[ TrieInsert(T, %s, %s)\n", str, EchoToken(val));
  if( *str == '\0' )
    res = FALSE;
  else
  {
    if( *T == (TRIE) NULL )
      *T = (TRIE) calloc(1, sizeof(struct trie_node));  /* will set all to 0 */
    if( *(str + 1) != '\0' )
      res = TrieInsert(&((*T)->sub[(int) *str]), str + 1, val);
    else if( (*T)->value[(int) *str] != (TOKEN *) NULL )
      res = FALSE;
    else
    {
      (*T)->value[(int) *str] = val;
      res = TRUE;
    }
  }
  if( DEBUG_TRIE )
    fprintf(stderr, "] TrieInsert(T, %s, %s) returning %s\n", str,
      EchoToken(val), res ? "TRUE" : "FALSE");
  return res;
}

/*****************************************************************************/
/*                                                                           */
/*  TOKEN *TrieRetrieve(T, str, &len)                                        */
/*                                                                           */
/*  Find the longest prefix of string str in T.  If this is empty, return    */
/*  NULL.  If non-empty, return the corresponding value as the result, and   */
/*  the length of the prefix in *len.                                        */
/*                                                                           */
/*****************************************************************************/

TOKEN *TrieRetrieve(TRIE T, char *str, int *len)
{ TOKEN *res;  int i;
  if( DEBUG_TRIE )
    fprintf(stderr, "[ TrieRetrieve(T, %s, len)\n", str);
  res = (TOKEN *) NULL;
  *len = 0;
  for( i = 0;  T != (TRIE) NULL;  T = T->sub[(int) str[i]], i++ )
  {
    if( DEBUG_TRIE )
      fprintf(stderr, "  i = %d, res = %s\n", i, EchoToken(res));
    if( T->value[(int) str[i]] != (TOKEN *) NULL )
    {
      res = T->value[(int) str[i]];
      *len = i+1;
    }
  }
  if( DEBUG_TRIE )
    fprintf(stderr, "] TrieRetrieve returning (*len = %d) %s\n",
      *len, EchoToken(res));
  return res;
}


/*****************************************************************************/
/*                                                                           */
/*  HASH_TABLE                                                               */
/*                                                                           */
/*  We use a hash table to hold the keywords.  There is no associated        */
/*  value, we just want to know whether they are there or not.               */
/*                                                                           */
/*  NB MAX_SYM must be somewhat larger than the number of keywords.          */
/*                                                                           */
/*****************************************************************************/
#define MAX_SYM 309

static char *HashTable[MAX_SYM];		/* will initialze to NULL    */
static int HashTableCount = 0;			/* number of entries         */

static int hash(char *key)
{ int i, res;
  res = 0;
  for( i = 0;  key[i] != '\0';  i++ )
  { res += key[i];
  }
  return res % MAX_SYM;
} /* end hash */

void HashTableInsert(char *str, FILE *err_fp)
{ int i;
  if( DEBUG_SETUP )
    fprintf(stderr, "[ HashTableInsert(%s)\n", str);
  if( HashTableCount >= MAX_SYM - 20 )
  {
    fprintf(err_fp, "%s internal error: full hash table (increase MAX_SYM)\n",
      ErrorHeader());
    abort();
  }
  for( i = hash(str);  HashTable[i] != (char *) NULL;  i = (i+1) % MAX_SYM );
  HashTable[i] = str;
  HashTableCount++;
  if( DEBUG_SETUP )
    fprintf(stderr, "] HashTableInsert(%s)\n", str);
}

BOOLEAN HashTableRetrieve(char *str)
{ int i;
  for( i = hash(str);  HashTable[i] != (char *) NULL;  i = (i+1) % MAX_SYM )
    if( strcmp(HashTable[i], str) == 0 )
      return TRUE;
  return FALSE;
}


/*****************************************************************************/
/*                                                                           */
/*  BACK END                                                                 */
/*                                                                           */
/*  This is the code that actually prints the output file.  It accumulates   */
/*  each token and only prints it at the end of the token, so that it can    */
/*  check whether an alternative command ought to be printed (keywords).     */
/*                                                                           */
/*****************************************************************************/

static char	save_token[MAX_LINE];		/* the token text            */
static int	save_len;                       /* index of \0 in save_token */
static BOOLEAN	save_on = FALSE;		/* TRUE when saving          */

/*****************************************************************************/
/*                                                                           */
/*  EmitRaw(ch, out_fp, FILE *err_fp)                                        */
/*                                                                           */
/*  Emit this character immediately.  This is only legal when not saving.    */
/*  All characters printed on the output file should pass through here,      */
/*  since EmitRaw keeps track of where we are on the output line, in order   */
/*  to handle tab characters correctly.                                      */
/*                                                                           */
/*  NB out_linepos is the column where the *next* character will go, and     */
/*  it counts the first column on the line as column zero.  It understands   */
/*  that a tab character always produces at least one space, and that the    */
/*  character after a tab goes in a column whose number mod tab_in is zero.  */
/*                                                                           */
/*****************************************************************************/

void EmitRaw(char ch, FILE *out_fp, FILE *err_fp)
{
  static int	 out_linepos = 0;		/* output line position      */
  static BOOLEAN out_linestart = TRUE;		/* TRUE if out line start    */

  if( DEBUG_EMIT )
    fprintf(stderr, "EmitRaw(%c); out_linepos %d, out_linestart %s\n",
      ch, out_linepos, out_linestart ? "TRUE" : "FALSE");
  if( save_on )
  {
    fprintf(err_fp, "%s internal error (EmitRaw save_on)\n", ErrorHeader());
    abort();
  }
  if( ch == '\t' )
  {
    if( tab_by_spacing )
    { putc(' ', out_fp);
      out_linepos++;
      while( out_linepos % tab_in != 0 )
      { putc(' ', out_fp);
        out_linepos++;
      }
    }
    else
    {
      out_linepos++;
      while( out_linepos % tab_in != 0 )
      { out_linepos++;
      }
      if( out_linestart )
      { fprintf(out_fp, "$>%.1f%c {}", tab_out, tab_unit);
	/* NB {} is required in case nothing follows on this line */
      }
      else
      { fprintf(out_fp, "$>%.1f%ct {}", (out_linepos/tab_in)*tab_out, tab_unit);
      }
    }
  }
  else if( ch == '\n' )
  {
    fputc(ch, out_fp);
    out_linepos = 0;
    out_linestart = TRUE;
  }
  else
  {
    fputc(ch, out_fp);
    out_linepos++;
    if( ch != ' ' )
      out_linestart = FALSE;
  }
  if( DEBUG_EMIT )
    fprintf(stderr, "EmitRaw(%c) returning; out_linepos %d, out_linestart %s\n",
      ch, out_linepos, out_linestart ? "TRUE" : "FALSE");
} /* end EmitRaw */


/*****************************************************************************/
/*                                                                           */
/*  StartEmit(FILE *err_fp)                                                  */
/*                                                                           */
/*  Start the emission of a token.                                           */
/*                                                                           */
/*****************************************************************************/

void StartEmit(FILE *err_fp)
{
  if( save_on )
  {
    fprintf(err_fp, "%s internal error (StartEmit)\n", ErrorHeader());
    abort();
  }
  save_on = TRUE;
  save_len = 0;
}


/*****************************************************************************/
/*                                                                           */
/*  EndEmit(LANGUAGE *lang, TOKEN *current_token, FILE *out_fp, *err_fp)     */
/*                                                                           */
/*  End emitting a token of this type on file out_fp.                        */
/*                                                                           */
/*****************************************************************************/

void EndEmit(LANGUAGE *lang, TOKEN *current_token, FILE *out_fp, FILE *err_fp)
{ BOOLEAN altern;  int i;
  if( !save_on )
  {
    fprintf(err_fp, "%s internal error (EndEmit)\n", ErrorHeader());
    abort();
  }
  save_on = FALSE;

  if( save_len == 0 )
  {
    /* nothing in token, must be resumed after a newline or tab */
    /* so emit nothing in this case */
  }
  else if( current_token->command == (char *) NULL )
  {
    /* no command, which means to emit this token without its delimiters */
    for( i = 0;  i < save_len;  i++ )
      EmitRaw(save_token[i], out_fp, err_fp);
  }
  else
  {
    /* if there is an alternate command in this language, check whether the  */
    /* completed token is a keyword, and if so apply the alternate command.  */
    altern = (lang != (LANGUAGE *) NULL
		&& current_token->alternate_command[0] != '\0'
		&& HashTableRetrieve(save_token));

    putc('{', out_fp);
    fputs(altern ? current_token->alternate_command : current_token->command,
      out_fp);
    putc('"', out_fp);
    for( i = 0;  i < save_len;  i++ )
    {
      if( save_token[i] == '"' || save_token[i] == '\\' )
	putc('\\', out_fp);
      EmitRaw(save_token[i], out_fp, err_fp);
    }
    putc('"', out_fp);
    putc('}', out_fp);
  }
} /* end EndEmit */


/*****************************************************************************/
/*                                                                           */
/*  Emit(char ch, TOKEN *current_token, FILE *out_fp, FILE *err_fp)          */
/*                                                                           */
/*  Emit one character.                                                      */
/*                                                                           */
/*****************************************************************************/

void Emit(char ch, TOKEN *current_token, FILE *out_fp, FILE *err_fp)
{
  if( !save_on )
  {
    fprintf(err_fp, "%s internal error (EmitChar)\n", ErrorHeader());
    abort();
  }
  if( ch == '\n' || ch == '\t' )
  {
    EndEmit((LANGUAGE *) NULL, current_token, out_fp, err_fp);
    EmitRaw(ch, out_fp, err_fp);
    StartEmit(err_fp);
  }
  else
  {
    save_token[save_len++] = ch;
    save_token[save_len] = '\0';
  }
} /* end Emit */


/*****************************************************************************/
/*                                                                           */
/*  SetupTokens(LANGUAGE *lang)                                              */
/*                                                                           */
/*  Set up the runtime token structures.  This involves initializing the     */
/*  chtype and escape_chtype fields for each token type in the chosen        */
/*  language, and loading the trie with all the opening delimiters of all    */
/*  the tokens.                                                              */
/*                                                                           */
/*****************************************************************************/
#define LEGAL		1
#define ESCAPE		2
#define INNER_ESCAPE	3

TRIE Trie = (TRIE) NULL;

void SetupTokens(LANGUAGE *lang, FILE *err_fp)
{ TOKEN *t; int i, j;
  if( DEBUG_SETUP )
    fprintf(stderr, "SetupTokens(%s)\n", lang->names[0]);

  /* set up each token in the language */
  for( i = 0;  lang->tokens[i] != (TOKEN *) NULL; i++ )
  {
    t = lang->tokens[i];
    if( DEBUG_SETUP )
      fprintf(stderr, "SetupTokens token starting %s\n", t->starts[0]);

    /* set up the chtype table for this token */
    for( j = 0;  t->legal[j] != '\0'; j++ )
      t->chtype[(int) t->legal[j]] = LEGAL;
    if( t->escape[0] != '\0' )
      t->chtype[(int) t->escape[0]] = ESCAPE;
    if( t->inner_escape[0] != '\0' )
      t->chtype[(int) t->inner_escape[0]] = INNER_ESCAPE;

    /* set up the escape_chtype table for this token */
    for( j = 0;  t->escape_legal[j] != '\0';  j++ )
      t->escape_chtype[(int) t->escape_legal[j]] = LEGAL;

    /* load the opening delimiters of this token into the trie */
    for( j = 0;  t->starts[j] != (char *) NULL;  j++ )
    {
      if( !TrieInsert(&Trie, t->starts[j], t) )
      {
	if( *(t->starts[j]) == '\0' )
	  fprintf(err_fp, "%s: empty starting delimiter\n", ErrorHeader());
	else
	  fprintf(err_fp, "%s: starting delimiter %s appears twice\n",
	    ErrorHeader(), t->starts[j]);
      }
    }

    if( DEBUG_SETUP )
      fprintf(stderr, "SetupTokens token ending %s\n", t->starts[0]);
  }

  /* load the keyword hash table */
  for( j = 0;  lang->keywords[j] != (char *) NULL;  j++ )
    HashTableInsert(lang->keywords[j], err_fp);

  if( DEBUG_SETUP )
    fprintf(stderr, "SetupTokens(%s) returning.\n", lang->names[0]);
} /* end SetupTokens */


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN PrefixEq(char *str, char *prefix)                                */
/*                                                                           */
/*  Returns TRUE if str (unterminated) begins with prefix (terminated).      */
/*                                                                           */
/*****************************************************************************/

BOOLEAN PrefixEq(char *str, char *prefix)
{ char *p, *q;
  for( p = str, q = prefix; *q != '\0' && *p == *q; p++, q++ );
  if( DEBUG_PREFIXEQ )
    fprintf(stderr, "PrefixEq(%s, %s) returning %s\n",
      str, prefix, *q == '\0' ? "TRUE" : "FALSE");
  return (*q == '\0');
} /* end PrefixEq */


/*****************************************************************************/
/*                                                                           */
/*  NextChar(FILE *in_fp)                                                    */
/*                                                                           */
/*  Move to next character in the input file.  This may involve changing     */
/*  global variables curr_line, line_num, and line_pos; the new character    */
/*  may be found in curr_line[line_pos].                                     */
/*                                                                           */
/*  NextChar does not skip any characters at all.  When end of file is       */
/*  reached, curr_line[line_pos] contains '\0'.                              */
/*                                                                           */
/*****************************************************************************/

void NextChar(FILE *in_fp)
{
  if( curr_line[line_pos] == '\n' )
  {
    /* need a new line */
    line_num++;
    line_pos = 1;
    if( fgets(&curr_line[1], MAX_LINE+2, in_fp) == (char *) NULL )
      curr_line[1] = '\0';
  }
  else line_pos++;  /* will yield '\0' as desired if EOF before end of line */
  if( DEBUG_NEXTCHAR )
    fprintf(stderr, "after NextChar, line_num %d, line_pos %d, curr_line %s",
      line_num, line_pos, &curr_line[1]);
} /* end NextChar */


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN Printable(char ch)                                               */
/*                                                                           */
/*  TRUE if ch is a printable character.  Used only by error messages so     */
/*  can be slow.                                                             */
/*                                                                           */
/*****************************************************************************/

BOOLEAN Printable(char ch)
{ char *p;
  for( p = AllPrintable;  *p != '\0' && *p != ch;  p++ );
  return (*p == ch);
} /* end Printable */


/*****************************************************************************/
/*                                                                           */
/*  Process(LANGUAGE*lang, FILE*in_fp, *out_fp, *err_fp, TOKEN *outer_token) */
/*                                                                           */
/*  Process a sequence of input tokens.  If we are currently recursing       */
/*  inside some other token, outer_token is non-null and is that token,      */
/*  and we stop when we reach the ending delimiter of the inner escape       */
/*  of that token.  Otherwise, we stop at end of file.                       */
/*                                                                           */
/*****************************************************************************/
#define	START				1
#define	IN_TOKEN			2
#define	IN_TOKEN_AFTER_ESCAPE		3
#define	IN_TOKEN_AFTER_INNER_ESCAPE	4
#define	STOP				5

void Process(LANGUAGE *lang, FILE *in_fp, FILE *out_fp, FILE *err_fp,
TOKEN *outer_token)
{ TOKEN *current_token; int len, i, state;
  if( DEBUG_PROCESS )
    fprintf(stderr, "[ Process(%s, -, -, -, -)\n", lang->names[0]);

  state = START;
  while( curr_line[line_pos] != '\0' && state != STOP )
  { 
  if( DEBUG_PROCESS )
    fprintf(stderr, "  state %d, ch %c\n", state, curr_line[line_pos]);
  switch( state )
  {
    case START:  /* between tokens */

      /* check whether outer_token's delimiter is current */
      if( outer_token != (TOKEN *) NULL &&
	  outer_token->end_inner_escape[0] != '\0' &&
	  PrefixEq(&curr_line[line_pos], outer_token->end_inner_escape) )
      {
	len = strlen(outer_token->end_inner_escape);
	for( i = 0;  i < len;  i++ )
	  NextChar(in_fp);
	state = STOP;
      }
      else switch( curr_line[line_pos] )
      {

	case ' ':
	
	  EmitRaw(' ', out_fp, err_fp);
	  NextChar(in_fp);
	  break;


	case '\t':
	
	  if( outer_token != (TOKEN *) NULL )
	  {
	    fprintf(err_fp, "%s: replacing tab character in escape by space\n",
	      ErrorHeader());
	    EmitRaw(' ', out_fp, err_fp);
	    NextChar(in_fp);
	  }
	  else
	  {
	    EmitRaw('\t', out_fp, err_fp);
	    NextChar(in_fp);
	  }
	  break;


	case '\n':
	
	  if( outer_token != (TOKEN *) NULL )
	  {
	    fprintf(err_fp, "%s: inserting %s to fix unterminated escape\n",
	      ErrorHeader(), outer_token->end_inner_escape);
	    state = STOP;
	  }
	  else
	  {
	    EmitRaw('\n', out_fp, err_fp);
	    NextChar(in_fp);
	  }
	  break;


	case '\f':
	
	  if( outer_token != (TOKEN *) NULL )
	  {
	    fprintf(err_fp, "%s: replacing formfeed character in escape by space\n",
	      ErrorHeader());
	    EmitRaw(' ', out_fp, err_fp);
	    NextChar(in_fp);
	  }
	  else
	  {
	    fprintf(out_fp, "\n@NP");
	    EmitRaw('\n', out_fp, err_fp);
	    NextChar(in_fp);
	  }
	  break;


	default:

	  /* look up the trie to see which token we are starting */
	  current_token = TrieRetrieve(Trie, &curr_line[line_pos], &len);
	  if( current_token == (TOKEN *) NULL )
	  {
	    /* error; no token starts here, so print error message and skip */
	    if( Printable(curr_line[line_pos]) )
	      fprintf(err_fp, "%s: skipping unexpected %c character\n",
		ErrorHeader(), curr_line[line_pos]);
	    else
	      fprintf(err_fp,
		"%s: skipping unexpected unprintable character (octal %o)\n",
		ErrorHeader(), (int) curr_line[line_pos]);
	    NextChar(in_fp);
	  }
	  else
	  {
	    /* have a token, so start emitting it */
	    if( DEBUG_PROCESS )
	    {
	      fprintf(stderr, "current_token (len = %d): %s\n",
		len, EchoToken(current_token));
	    }
	    StartEmit(err_fp);
	    for( i = 0;  i < len;  i++ )
	    {
	      /* ***
	      if( DEBUG_PROCESS )
		fprintf(stderr, "  emitting delim (i %d, len %d)\n", i, len);
	      *** */
	      if( current_token->command != (char *) NULL )
	        Emit(curr_line[line_pos], current_token, out_fp, err_fp);
	      NextChar(in_fp);
	    }
	    state = IN_TOKEN;
	  }
      }
      break;


    case IN_TOKEN:  /* within a token; current_token says which kind */

      /* check for ending delimiter if there is one */
      if( current_token->end_delimiter[0] != '\0' &&
	  PrefixEq(&curr_line[line_pos], current_token->end_delimiter) )
      {
	if( DEBUG_PROCESS )
	  fprintf(stderr, "  PrefixEq(-, %s) so finishing token\n",
	    current_token->end_delimiter);
	len = strlen(current_token->end_delimiter);
	for( i = 0;  i < len;  i++ )
	{ if( current_token->command != (char *) NULL )
	    Emit(curr_line[line_pos], current_token, out_fp, err_fp);
	  NextChar(in_fp);
	}
	EndEmit(lang, current_token, out_fp, err_fp);
	state = START;
      }
      else switch( current_token->chtype[(int) curr_line[line_pos]] )
      {

	case LEGAL:

	  Emit(curr_line[line_pos], current_token, out_fp, err_fp);
	  NextChar(in_fp);
	  break;


	case ESCAPE:

	  NextChar(in_fp);
	  state = IN_TOKEN_AFTER_ESCAPE;
	  break;


	case INNER_ESCAPE:

	  EndEmit(lang, current_token, out_fp, err_fp);
	  NextChar(in_fp);
	  Process(lang, in_fp, out_fp, err_fp, current_token);
	  state = IN_TOKEN_AFTER_INNER_ESCAPE;
	  break;


	default:

	  if( current_token->end_delimiter[0] != '\0' )
	  {
	    /* error: token ends at delimiter, not at unexpected character */
	    if( Printable(curr_line[line_pos]) )
	      fprintf(err_fp, "%s: skipping unexpected %c character in %s\n",
		ErrorHeader(), curr_line[line_pos], current_token->name);
	    else
	      fprintf(err_fp,
		"%s: skipping unexpected unprintable character (octal %o) in %s\n",
		ErrorHeader(), (int) curr_line[line_pos], current_token->name);
	    NextChar(in_fp);
	  }
	  else
	  {
	    /* normal termination after last legal character */
	    EndEmit(lang, current_token, out_fp, err_fp);
	    state = START;
	  }
	  break;


      }
      break;
      

    case IN_TOKEN_AFTER_ESCAPE:

      if( current_token->escape_chtype[(int) curr_line[line_pos]] == LEGAL )
      {
	Emit(current_token->escape[0], current_token, out_fp, err_fp);
	Emit(curr_line[line_pos], current_token, out_fp, err_fp);
      }
      else
      {
	if( Printable(curr_line[line_pos]) )
	  fprintf(err_fp, "%s: skipping %c%c in %s, since %c not legal here\n",
	    ErrorHeader(), current_token->escape[0], curr_line[line_pos],
	    current_token->name, curr_line[line_pos]);
	else
	  fprintf(err_fp,
	    "%s: skipping %c and unprintable unexpected character (octal %o)\n",
	     ErrorHeader(), current_token->escape[0], (int) curr_line[line_pos]);
      }
      NextChar(in_fp);
      state = IN_TOKEN;
      break;


    case IN_TOKEN_AFTER_INNER_ESCAPE:

      /* ending delimiter of inner escape has been read over */
      StartEmit(err_fp);
      state = IN_TOKEN;
      break;


    default:

      fprintf(err_fp, "%s internal error (state = %d)\n",
	ErrorHeader(), state);
      abort();
      break;
  }
  }

  /* at end, need to tidy up any residual messiness */
  switch( state )
  {

    case START:
    case STOP:

      /* we stopped outside any token, or after an escape */
      break;


    case IN_TOKEN:

      /* we stopped in a token (only a problem if it ends with a delimiter) */
      if( current_token->end_delimiter[0] != '\0' )
      {
	if( outer_token == (TOKEN *) NULL )
	  fprintf(err_fp, "%s: program text ended within %s\n", 
	    ErrorHeader(), current_token->name);
	else
	  fprintf(err_fp, "%s: %s escape ended within %s\n",
	    ErrorHeader(), outer_token->name, current_token->name);
	EndEmit(lang, current_token, out_fp, err_fp);
      }
      break;


    case IN_TOKEN_AFTER_ESCAPE:

      /* we stopped after the escape character */
      fprintf(err_fp, "%s: skipping %c at end of program text\n",
	ErrorHeader(), current_token->escape[0]);
      EndEmit(lang, current_token, out_fp, err_fp);
      break;


    case IN_TOKEN_AFTER_INNER_ESCAPE:

      /* we stopped after an inner escape (NB no EndEmit in this case) */
      if( current_token->end_delimiter[0] != '\0' )
      {
	if( outer_token == (TOKEN *) NULL )
	  fprintf(err_fp, "%s: program text ended within %s after escape\n", 
	    ErrorHeader(), current_token->name);
	else
	  fprintf(err_fp, "%s: %s escape ended within %s after escape\n",
	    ErrorHeader(), outer_token->name, current_token->name);
      }
      break;


    default:

      fprintf(err_fp, "%s: internal error (state %d)\n",
	ErrorHeader(), state);
      abort();
      break;

  }
} /* end Process */


/*****************************************************************************/
/*                                                                           */
/*  PrintUsage(fp)                                                           */
/*                                                                           */
/*  Print usage message on file fp.                                          */
/*                                                                           */
/*****************************************************************************/

void PrintUsage(FILE *fp)
{ int i;
  fprintf(fp, "\n");
  fprintf(fp, "usage: prg2lout <options> <files>\n\n");
  fprintf(fp, "    where <options> can be\n");
  fprintf(fp, "\n");
  fprintf(fp, "    -r           raw mode (used within Lout only)\n");
  fprintf(fp, "    -i<file>     take input from <file>\n");
  fprintf(fp, "    -o<file>     send output to <file>\n");
  fprintf(fp, "    -e<file>     send error messages to <file>\n");
  fprintf(fp, "    -l<language> input is in this programming language\n");
  fprintf(fp, "    -p<style>    print style: fixed, varying, symbol\n");
  fprintf(fp, "    -f<family>   font family (e.g. Times)\n");
  fprintf(fp, "    -s<size>     font size (e.g. 10p or 12p)\n");
  fprintf(fp, "    -v<space>    line spacing (e.g. 1.1fx)\n");
  fprintf(fp, "    -t<num>      tab interval (e.g. 8 is default)\n");
  fprintf(fp, "    -T<dist>     output tab interval (e.g. 0.5i)\n");
  fprintf(fp, "    -n           no file names as page headers\n");
  fprintf(fp, "    -V           print version information and exit\n");
  fprintf(fp, "    -u           print this usage message and exit\n");
  fprintf(fp, "\n");
  fprintf(fp, "    and <language> (which is compulsory) can be any one of:\n");
  for( i = 0;  languages[i] != (LANGUAGE *) NULL;  i++ )
    fprintf(fp, "        %s\n", languages[i]->names[0]);
  fprintf(fp, "\n");
} /* end PrintUsage */


/*****************************************************************************/
/*                                                                           */
/*  main(argc, argv)                                                         */
/*                                                                           */
/*  Read command line and either process each file in turn, or, in the       */
/*  raw case, do the actual conversion of one file.                          */
/*                                                                           */
/*****************************************************************************/

int main(int argc, char *argv[])
{ FILE *in_fp, *out_fp, *err_fp;
  BOOLEAN stdin_seen;  int i, j, arg_pos;
  char *infilename, *outfilename, *errfilename, *str;
  LANGUAGE *language = (LANGUAGE *) NULL;
  char *file_names[1024];  int file_count = 0;

  /* echo command line */
  if( DEBUG_MAIN )
  {
    for( i = 0;  i < argc;  i++ )
      fprintf(stderr, i == 0 ? "%s" : " %s", argv[i]);
    fprintf(stderr, "\n\n");
  }

  /* read command line */
  in_fp = out_fp = (FILE *) NULL;
  err_fp = stderr;
  line_num = 0;
  stdin_seen = raw_seen = FALSE;
  tab_by_spacing = TRUE;
  style_option = NO_STYLE;
  tab_in = 8;
  tab_out = 3;
  tab_unit = 'f';
  headers_option = TRUE;
  font_option = size_option = line_option = tabin_option =
    tabout_option = language_option = (char *) NULL;
  if( argc == 1 )
  { PrintUsage(err_fp);
    exit(1);
  }
  for( arg_pos = 1;  arg_pos < argc;  arg_pos++ )
  {
    if( DEBUG_SETUP )
      fprintf(stderr, "examining argument %d = \"%s\"\n",
	arg_pos, argv[arg_pos]);
    if( *argv[arg_pos] == '-' ) switch( *(argv[arg_pos]+1) )
    {
      case 'r':

	if( arg_pos > 1 )
	{ fprintf(err_fp, "%s: -r must be first if it occurs at all\n",
	    ErrorHeader());
	  exit(1);
	}
	raw_seen = TRUE;
	break;


      case 'i':
     
	/* read name of input file */
	if( !raw_seen )
	{ fprintf(err_fp, "%s: -i illegal with -r\n", ErrorHeader());
	  exit(1);
	}
	if( in_fp != NULL )
	{ fprintf(err_fp, "%s: -i seen twice\n", ErrorHeader());
	  exit(1);
	}
	GetArg(infilename, "usage: -i<filename>", FALSE);

	/* open the file */
	in_fp = fopen(infilename, "r");
	if( in_fp == NULL )
	{ fprintf(err_fp, "%s: cannot open input file %s\n",
	    ErrorHeader(), infilename);
	  exit(1);
	}

	/* initialize file position */
	strcpy(file_name, infilename);
	line_num = 1;
	line_pos = 0;
	break;


      case 'o':
     
	/* read name of output file */
	if( out_fp != NULL )
	{ fprintf(err_fp, "%s: -o seen twice\n", ErrorHeader());
	  exit(1);
	}
	GetArg(outfilename, "usage: -o<filename>", FALSE);
	out_fp = fopen(outfilename, "w");
	if( out_fp == NULL )
	{ fprintf(err_fp, "%s: cannot open output file %s\n",
	    ErrorHeader(), outfilename);
	  exit(1);
	}
	break;


      case 'e':
     
	/* read name of error file */
	GetArg(errfilename, "usage: -e<filename>", FALSE);
	err_fp = fopen(errfilename, "w");
	if( err_fp == NULL )
	{ fprintf(stderr, "%s: cannot open error file %s",
	    ErrorHeader(), errfilename);
	  exit(1);
	}
	break;


      case 'p':
     
	/* read print style */
	if( raw_seen )
	{ fprintf(err_fp, "%s: -p illegal with -r option\n", ErrorHeader());
	  exit(1);
	}
	GetArg(str, "usage: -p<printstyle>", FALSE);
	/* *** -l can set style, so don't do this now 
	if( style_option != NO_STYLE )
	{ fprintf(err_fp, "%s: -p option appears twice!\n", ErrorHeader());
	  exit(1);
	}
	else
	*** */
	if( strcmp(str, "fixed") == 0 )
	{ style_option = FIXED_STYLE;
	}
	else if( strcmp(str, "varying") == 0 )
	{ style_option = VARYING_STYLE;
	  tab_by_spacing = FALSE;
	}
	else if( strcmp(str, "symbol") == 0 )
	{ style_option = SYMBOL_STYLE;
	  tab_by_spacing = FALSE;
	}
	else
	{ fprintf(err_fp, "%s: unknown -p option %s\n", ErrorHeader(), str);
	  exit(1);
	}
	break;


      case 'f':
     
	/* read font family */
	if( raw_seen )
	{ fprintf(err_fp, "%s: -f illegal with -r option\n", ErrorHeader());
	  exit(1);
	}
	GetArg(font_option, "usage: -f<font_family>", FALSE);
	break;


      case 's':
     
	/* read font size */
	if( raw_seen )
	{ fprintf(err_fp, "%s: -s illegal with -r option\n", ErrorHeader());
	  exit(1);
	}
	GetArg(size_option, "usage: -s<size>", FALSE);
	break;


      case 'v':
     
	/* read line spacing */
	if( raw_seen )
	{ fprintf(err_fp, "%s: -v illegal with -r option\n", ErrorHeader());
	  exit(1);
	}
	GetArg(line_option, "usage: -v<line_spacing>", FALSE);
	break;


      case 't':
     
	/* read tab interval */
	GetArg(tabin_option, "usage: -t<number>", TRUE);
	if( tabin_option != NULL && sscanf(tabin_option,"%d",&tab_in) != 1 )
	{ fprintf(err_fp, "%s usage: -t<number>\n", ErrorHeader());
	  exit(1);
	}
	if( tab_in <= 0 )
	{ fprintf(err_fp, "%s -t: tab interval must be greater than 0\n",
	    ErrorHeader());
	  exit(1);
	}
	break;


      case 'T':
     
	/* read tab_out and tab_unit */
	GetArg(tabout_option, "usage: -T<number><unit>", TRUE);
	if( tabout_option != NULL )
	{ if( sscanf(tabout_option, "%f%c",&tab_out,&tab_unit) != 2 )
	  { fprintf(err_fp, "%s usage: -T<number><unit>\n", ErrorHeader());
	    exit(1);
	  }
	  if( tab_out <= 0 || tab_out >= 50 )
	  { fprintf(err_fp, "%s -T: unreasonably large or small tab interval\n",
	      ErrorHeader());
	    exit(1);
	  }
	  if( tab_unit != 'c' && tab_unit != 'i' && tab_unit != 'p' &&
	      tab_unit != 'm' && tab_unit != 'f' && tab_unit != 's' &&
	      tab_unit != 'v' )
	  { fprintf(err_fp, "%s -T: tab unit must be one of cipmfsv\n",
	      ErrorHeader());
	    exit(1);
	  }
	  tab_by_spacing = FALSE;
	}
	break;


      case 'n':
     
	if( raw_seen )
	{ fprintf(err_fp, "%s: -n illegal with -r option\n", ErrorHeader());
	  exit(1);
	}
	headers_option = FALSE;
	break;


      case 'V':
     
	if( raw_seen )
	{ fprintf(err_fp, "%s: -V illegal with -r option\n", ErrorHeader());
	  exit(1);
	}
	fprintf(err_fp, "%s\n", PROG2LOUT_VERSION);
	exit(0);
	break;


      case 'u':
     
	if( raw_seen )
	{ fprintf(err_fp, "%s: -u illegal with -r option\n", ErrorHeader());
	  exit(1);
	}
	PrintUsage(err_fp);
	exit(0);
	break;


      case 'l':
     
	if( language_option != (char *) NULL )
	{ fprintf(err_fp, "%s: -l seen twice\n", ErrorHeader());
	  exit(1);
	}
	GetArg(language_option, "usage: -l<language>", FALSE);
	for( i = 0;  languages[i] != (LANGUAGE *) NULL;  i++ )
	{
	  for( j = 0;  languages[i]->names[j] != (char *) NULL;  j++ )
	  {
	    if( strcmp(languages[i]->names[j], language_option) == 0 )
	      break;
	  }
	  if( languages[i]->names[j] != (char *) NULL )
	    break;
	}
	if( languages[i] != (LANGUAGE *) NULL )
	{
	  language = languages[i];
	}
	else
	{
	  fprintf(err_fp, "%s: unknown language %s\n", ErrorHeader(),
	    language_option);
	  exit(1);
	}

	/* set style unless already done by a -p flag */
	if( style_option == NO_STYLE )
	{
	  if( strcmp(language->default_style, "fixed") == 0 )
	  { style_option = FIXED_STYLE;
	  }
	  else if( strcmp(language->default_style, "varying") == 0 )
	  { style_option = VARYING_STYLE;
	    tab_by_spacing = FALSE;
	  }
	  else if( strcmp(language->default_style, "symbol") == 0 )
	  { style_option = SYMBOL_STYLE;
	    tab_by_spacing = FALSE;
	  }
	  else
	  { fprintf(err_fp,
	      "%s internal error: bad default print style for language %s\n",
	      ErrorHeader(), language_option);
	    abort();
	  }
	}
	break;


      default:
     
	fprintf(err_fp, "%s: unknown command line flag %s\n", ErrorHeader(),
	  argv[i]);
	exit(1);
	break;

    }
    else
    {
      if( raw_seen )
      { fprintf(err_fp, "%s: file parameter illegal with -r flag\n",
	    ErrorHeader());
	  exit(1);
      }
      if( DEBUG_SETUP )
	fprintf(stderr, "file_names[%d++] = argv[%d] = %s\n",
	  file_count, arg_pos, argv[arg_pos]);
      file_names[file_count++] = argv[arg_pos];
    }
  } /* for */

  /* make sure we have a language */
  if( language == (LANGUAGE *) NULL )
  {
    fprintf(err_fp, "%s: missing -l option\n", ErrorHeader());
    exit(0);
  }

  /* do the actual work */
  if( raw_seen )
  {
    /* check that input and output files are open */
    if( in_fp == NULL )
      in_fp = stdin;
    if( out_fp == NULL )
    { fprintf(err_fp, "%s -r: missing -o option\n", ErrorHeader());
      exit(1);
    }

    /* process the file */
    SetupTokens(language, err_fp);
    line_pos = 1;
    curr_line[line_pos] = '\n';  /* forces line read */
    line_num = 0;
    NextChar(in_fp);
    Process(language, in_fp, out_fp, err_fp, (TOKEN *) NULL);
  }
  else if( file_count > 0 )
  {
    int ch;
    char *style_str, *font_str, *size_str, *line_str, *face_str;
    char *tabin_str, *tabout_str;

    /* sort out the options' values */
    switch( style_option )
    {

      case FIXED_STYLE:

	style_str  = "fixed";
	face_str   = "Base";
	font_str   = font_option   != NULL ? font_option   : "Courier";
	size_str   = size_option   != NULL ? size_option   : "9p";
	line_str   = line_option   != NULL ? line_option   : "1.1fx";
	tabin_str  = tabin_option  != NULL ? tabin_option  : "8";
	tabout_str = tabout_option != NULL ? tabout_option : "8s";
	break;


      case NO_STYLE:
      case VARYING_STYLE:

	style_str  = "varying";
	face_str   = "Slope";
	font_str   = font_option   != NULL ? font_option   : "Times";
	size_str   = size_option   != NULL ? size_option   : "10p";
	line_str   = line_option   != NULL ? line_option   : "1.1fx";
	tabin_str  = tabin_option  != NULL ? tabin_option  : "8";
	tabout_str = tabout_option != NULL ? tabout_option : "3f";
	break;


      case SYMBOL_STYLE:

	style_str  = "symbol";
	face_str   = "Slope";
	font_str   = font_option   != NULL ? font_option   : "Times";
	size_str   = size_option   != NULL ? size_option   : "10p";
	line_str   = line_option   != NULL ? line_option   : "1.1fx";
	tabin_str  = tabin_option  != NULL ? tabin_option  : "8";
	tabout_str = tabout_option != NULL ? tabout_option : "3f";
	break;


      default:

	fprintf(err_fp, "%s internal error in -p option\n", ErrorHeader());
	abort();
	break;
    }
		 
    /* make sure we have an output file */
    if( out_fp == (FILE *) NULL )
      out_fp = stdout;

    /* print the initial @Use clauses etc.*/
    fprintf(out_fp, "%s%s\n", "@Sy", "sInclude { progf }");
    fprintf(out_fp, "%s%s\n", "@Sy", "sInclude { doc }");
    fprintf(out_fp, "@Use { @ProgSetup\n");
    fprintf(out_fp, "    language  { %s }\n", language_option);
    fprintf(out_fp, "    style  { %s }\n", style_str);
    fprintf(out_fp, "    %sfont   { %s }\n", style_str, font_str);
    fprintf(out_fp, "    %ssize   { %s }\n", style_str, size_str);
    fprintf(out_fp, "    %sline   { %s }\n", style_str, line_str);
    fprintf(out_fp, "    %stabin  { %s }\n", style_str, tabin_str);
    fprintf(out_fp, "    %stabout { %s }\n", style_str, tabout_str);
    fprintf(out_fp, "}\n");
    fprintf(out_fp, "@Document\n");
    fprintf(out_fp, "    @InitialFont { \"%s\" \"%s\" \"%s\" }\n",
      font_str, face_str, size_str);
    fprintf(out_fp, "    @InitialBreak { lines \"%s\" nohyphen }\n", line_str);
    fprintf(out_fp, "//\n");
    fprintf(out_fp, "%s%s\n", "@Text @Be", "gin");
 
    /* print each file, possibly with a header */
    for( i = 0;  i < file_count;  i++ )
    {
      /* open file and initialize file position */
      in_fp = fopen(file_names[i], "r");
      if( in_fp == NULL )
      { fprintf(err_fp, "%s: skipping input file %s (cannot open)\n",
	  ErrorHeader(), file_names[i]);
        continue;
      }
      strcpy(file_name, file_names[i]);

      /* print @NP if not first, and header if required */
      if( i > 0 )
	fprintf(out_fp, "\n\n@NP\n\n");
      if( headers_option )
	fprintf(out_fp, "{ Times Bold \"+3p\" } @Font \"%s\"\n@DP\n",
	  file_names[i]);

      /* print file name and contents (don't format, let Lout call back) */
      /* this string has been disguised so as not to fool progwlout!     */
      fprintf(out_fp, "%s%s%s\n", "@P", "rog @Be", "gin");
      while( (ch = getc(in_fp)) != EOF )
        putc(ch, out_fp);
      fprintf(out_fp, "%s%s%s\n", "@E", "nd @P", "rog");
    }

    /* finish off whole input */
    fprintf(out_fp, "%s%s%s\n", "@E", "nd @T", "ext");

  }
  exit(0);
} /* end main */
