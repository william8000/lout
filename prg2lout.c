
/*****************************************************************************/
/*                                                                           */
/*  PRG2LOUT: A PROGRAM TO CONVERT PROGRAM SOURCES INTO LOUT (VERSION 2.0)   */
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


/*****************************************************************************/
/*                                                                           */
/*  GENERAL INTRODUCTION TO PRG2LOUT                                         */
/*                                                                           */
/*  The best way to see what the aim of prg2lout as currently conceived is,  */
/*  is to look in file cprint at the setup file options.  You will see that  */
/*  the aim is to provide three basic styles: fixed (essentially mono font), */
/*  varying (essentially varying-width font with various faces for different */
/*  elements at the user's choice), and symbol (similar to varying).         */
/*                                                                           */
/*  The elements currently aimed for are strings, identifiers, comments,     */
/*  keywords, numbers, and operators, and the end user is able to choose,    */
/*  for each of these kinds of elements, which font to set them in.          */
/*                                                                           */
/*  This is achieved by a simple division of labour:  prg2lout does the      */
/*  classifying of the input into a sequence of these elements, and the Lout */
/*  end (cprint and cprintf, or their clones for other languages) does the   */
/*  formatting.  For example, the C text                                     */
/*                                                                           */
/*      inc = inc + 1                                                        */
/*                                                                           */
/*  would be classified by prg2lout into identifier, operator, identifier,   */
/*  operator, number; and consequently prg2lout would emit                   */
/*                                                                           */
/*      @PI{"inc"} @PO{"="} @PI{"inc"} @PO{"+"} @PN{"1"}                     */
/*                                                                           */
/*  which is readable by Lout, thanks to having quotes around everything     */
/*  potentially dangerous, and clearly tells Lout, by means of the commands  */
/*  @PC, @PI, etc., how each part of the input has been classified.          */
/*                                                                           */
/*  The actual classification is carried out by prg2lout as follows.  Each   */
/*  programming language is described to prg2lout as a collection of tokens; */
/*  you say what the token begins with, what's a legal character inside the  */
/*  token, and how it ends.  You also say which command (@PC, @PI etc.) to   */
/*  emit when a token of that kind is found.  Prg2lout does the rest.        */
/*                                                                           */
/*  Prg2lout knows all about tricky problems such as multi-line tokens (it   */
/*  breaks them up into single-line pieces) and backslash in Lout strings    */
/*  (it replaces any \ within an output string by \\, " by \", etc.).  It    */
/*  also handles tab characters and formfeed characters properly, and it     */
/*  produces intelligible error messages when unexpected things happen,      */
/*  such as input terminating in the middle of a string.  This attention to  */
/*  detail is a strong reason for using prg2lout rather than something more  */
/*  ad-hoc, such as @Verbatim or a quick script.                             */
/*                                                                           */
/*****************************************************************************/


/*****************************************************************************/
/*                                                                           */
/*  HOW TO ADD ANOTHER LANGUAGE TO PRG2LOUT                                  */
/*                                                                           */
/*  Step 1.  Construct clones of (say) eiffel and eiffelf (these are in      */
/*  $LOUTLIB/include) with every occurrence of eiffel or Eiffel in them      */
/*  changed to your language as appropriate.  Install your files in the      */
/*  Lout system include directory alongside eiffel and eiffelf.              */
/*                                                                           */
/*  It is good to clone the files exactly because that way all program       */
/*  formatting works the same way, and one chapter of the User's Guide       */
/*  covers the lot.  However if your language has some unique element, not   */
/*  readily classifiable as a string, identifier, comment, keyword,          */
/*  number, or operator, it is possible to emit a different command of       */
/*  your choice for the new element; but then your clones of eiffel and      */
/*  eiffelf have to be extended to handle that command.                      */
/*                                                                           */
/*  Step 2.  Have a browse through the token declarations below, and work    */
/*  out which of them you need for your language.  If you need a token that  */
/*  isn't there already, you'll have to define it; there are many examples   */
/*  and documentation there to help you.  The tokens for Perl are rather     */
/*  complicated and don't make a good model for most languages, so look      */
/*  more at the C and Eiffel ones.                                           */
/*                                                                           */
/*  Step 3.  Browse through the language declarations, and declare your      */
/*  language following those examples:  first you give a set of one or more  */
/*  alternative names for your language, then some other things, including   */
/*  the list of tokens of the language, and its keywords.                    */
/*                                                                           */
/*  Step 4.  Add your language variable to the list in the initializer of    */
/*  variable languages, as you can see the others have been done.            */
/*                                                                           */
/*  Step 5.  If any lists of initializers now contain more than              */
/*  MAX_ARRAY_LENGTH-1 elements, increase MAX_ARRAY_LENGTH until they don't. */
/*  The gcc compiler will warn you if you forget to do this.                 */
/*                                                                           */
/*  Step 6.  Recompile and reinstall prg2lout, test "prg2lout -u" then       */
/*  "prg2lout -l <mylanguage> <myfile> | lout -s > out.ps".                  */
/*                                                                           */
/*  Step 7.  Send your tested and tidied files to me for incorporation       */
/*  in the next Lout release.  If you do this, please try hard to ensure     */
/*  that your new code conforms to the formal definition of your language.   */
/*  Feel free to email me for advice as you go along.                        */
/*                                                                           */
/*  Jeff Kingston                                                            */
/*  jeff@cs.usyd.edu.au                                                      */
/*                                                                           */
/*****************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#define	FALSE		0
#define	TRUE		1
#define	BOOLEAN		unsigned

/*****************************************************************************/
/*                                                                           */
/*  Character sets                                                           */
/*                                                                           */
/*  These are prg2lout's definitions of various commonly needed sets of      */
/*  characters.  May need enhancement for Latin1 etc.                        */
/*                                                                           */
/*****************************************************************************/

#define AllCharacters	NULL	/* code will recognize NULL and do this */

char AllPrintable[] = 
  " !\"#$%&'()*+,-./0123456789:;<=>?@[\\]^_`\\{|}~\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ;

char AllPrintablePlusNL[] =
  " !\"#$%&'()*+,-./0123456789:;<=>?@[\\]^_`\\{|}~\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\n" ;

char AllPrintablePlusTab[] =
  " !\"#$%&'()*+,-./0123456789:;<=>?@[\\]^_`\\{|}~\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\t" ;

char AllPrintableTabNL[] =
  " !\"#$%&'()*+,-./0123456789:;<=>?@[\\]^_`\\{|}~\
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\n\t" ;

char Letters[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ;

char Letter_Digit[] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789" ;

#define SepLetters 							\
"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",	\
"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",	\
"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",	\
"n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"

#define SepDigits "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"

#define HexDigits "A", "a", "B", "b", "C", "c", "D", "d", "E", "e", "F", "f"


/*****************************************************************************/
/*                                                                           */
/*  TOKEN - put your token declarations in this section                      */
/*                                                                           */
/*  The fields of token_rec have the following meanings:                     */
/*                                                                           */
/*  name                                                                     */
/*      The name of this token, e.g. "string" or "identifier".  This field   */
/*      is used only by error messages generated by prg2lout; for example,   */
/*      prg2lout might print the message "input ended within string".        */
/*                                                                           */
/*  print_style                                                              */
/*                                                                           */
/*          print_style               What gets printed                      */
/*          -------------------------------------------------------          */
/*          PRINT_WHOLE_QUOTED        command{"token"}                       */
/*          PRINT_NODELIMS_QUOTED     command{"token-minus-delims"}          */
/*          PRINT_WHOLE_UNQUOTED      command{token}                         */
/*          PRINT_NODELIMS_UNQUOTED   command{token-minus-delims}            */
/*          PRINT_NODELIMS_INNER      command{inner}                         */
/*          PRINT_COMMAND_ONLY        command                                */
/*                                                                           */
/*      If command (see next) is empty then the braces {} are not printed.   */
/*                                                                           */
/*      PRINT_WHOLE_QUOTED.  This command is the most frequently used one;   */
/*      it prints the token, enclosed in braces and quotes, preceded by the  */
/*      command.  The quotes ensure that the result is legal Lout; any " or  */
/*      \ in the token is printed with a preceding \ as required in Lout.    */
/*      The usual arrangement for handling white space is that none of the   */
/*      tokens contain it; when it is encountered prg2lout generates the     */
/*      appropriate Lout without being told (a space for a space, a newline  */
/*      for a newline, @NP for a formfeed, and something clever for tab      */
/*      which does the required thing).  However, you can define a token     */
/*      that contains white space if you wish.  If you do this, the          */
/*      quotation marks will be temporarily closed off, the white space      */
/*      handled as just described, then the quotes opened again.             */
/*                                                                           */
/*      PRINT_NODELIMS_QUOTED.  This is like PRINT_WHOLE_QUOTED except that  */
/*      the opening and closing delimiters of the token are omitted from     */
/*      the print.  This is useful occasionally when these delimiters are    */
/*      formatting markers, not intended to be printed.                      */
/*                                                                           */
/*      PRINT_WHOLE_UNQUOTED.  This style prints the command and braces      */
/*      as usual, but omits the quotes and prints the token absolutely       */
/*      verbatim.  In general this is not going to produce legal Lout,       */
/*      but it is useful in two cases:  when the token is a Lout escape,     */
/*      so that it is the user's responsibility to ensure that its content   */
/*      is legal Lout; and when the command is another filter command, so    */
/*      that the token content will not go directly into Lout anyway, it     */
/*      will go through the other filter first.                              */
/*                                                                           */
/*      PRINT_NODELIMS_UNQUOTED.  This is like PRINT_WHOLE_UNQUOTED except   */
/*      that the opening and closing delimiters of the token are omitted.    */
/*                                                                           */
/*      PRINT_NODELIMS_INNER.  Like PRINT_NODELIMS_UNQUOTED except that the  */
/*      inner part (i.e. not delimiters) is formatted in the same language.  */
/*                                                                           */
/*      PRINT_COMMAND_ONLY.  This ignores the token and prints just the      */
/*      command, presumably because the command says it all for that token.  */
/*      When using PRINT_COMMAND_ONLY you will probably need to finish your  */
/*      command with a space: since there are no following braces in this    */
/*      print style, your command will run into the next one otherwise.      */
/*                                                                           */
/*****************************************************************************/


/*****************************************************************************/
/*                                                                           */
/*  Meaning of TOKEN fields (ctd.)                                           */
/*                                                                           */
/*  command                                                                  */
/*      The Lout command to print.  This command could be an legal Lout;     */
/*      programming language setup files offer the following Lout symbols    */
/*      that make the most common commands:                                  */
/*                                                                           */
/*          @PI  for formatting identifiers                                  */
/*          @PK  for formatting keywords                                     */
/*          @PO  for formatting operators                                    */
/*          @PN  for formatting numbers                                      */
/*          @PS  for formatting strings                                      */
/*          @PC  for formatting comments                                     */
/*          @PA  for printing an asterisk (lower on the line than usual)     */
/*          @PM  for printing a minus sign (longer than a hyphen)            */
/*          @PD  for printing a dot (.), only larger than usual              */
/*                                                                           */
/*      The last three require PRINT_COMMAND_ONLY (they take no parameter).  */
/*      If command is NULL or "", then no command will be printed and        */
/*      furthermore the token will not be enclosed in the usual braces.      */
/*                                                                           */
/*  alternate_command                                                        */
/*      Every language has a list of keywords.  Just before printing each    */
/*      token, it is compared against the keywords.  If it is one of them,   */
/*      then alternate_command is used instead of command.  For example,     */
/*      identifiers usually have command @PI and alternate_command @PK.      */
/*                                                                           */
/*  following_command                                                        */
/*      Print this Lout command (or commands) after the token.               */
/*                                                                           */
/*  start_line_only                                                          */
/*      A Boolean field.  If TRUE, this token is to be recognized only       */
/*      if it occurs at the very start of a line.                            */
/*                                                                           */
/*  starts[]                                                                 */
/*      This field holds an array of strings.  If prg2lout discovers any     */
/*      one of these strings while it is not reading some other token,       */
/*      then it deems that this token has begun.  The recognized string      */
/*      is the token's "starting delimiter".                                 */
/*                                                                           */
/*  starts2[], ends2[]                                                       */
/*      These fields each hold an array of strings, and the two arrays       */
/*      must have equal length.  If they have length zero, these fields      */
/*      do not apply.  Otherwise, they modify the meaning of starts[] and    */
/*      end_delimiter below.  Their effect is best seen by looking at some   */
/*      examples from Perl, their only user at the time of writing:          */
/*                                                                           */
/*         q/hello/  qq/hello/  qq?hello?  qq{hello}                         */
/*                                                                           */
/*      These strings may begin with q, qq, qx, and several other things;    */
/*      this is then followed by a single character which determines the     */
/*      string terminator; e.g. / means "terminate with /", { means          */
/*      "terminate with }", etc.  This is implemented by putting these       */
/*      pairs of strings in corresponding places in starts[] and ends[].     */
/*                                                                           */
/*         starts:  { "q", "qq", "qx" }                                      */
/*         starts2: { "/", "?",  "{"  }                                      */
/*         ends2:   { "/", "?",  "}"  }                                      */
/*                                                                           */
/*      PerlQTypeToken is a larger example of this.                          */
/*                                                                           */
/*****************************************************************************/


/*****************************************************************************/
/*                                                                           */
/*  Meaning of TOKEN fields (ctd.)                                           */
/*                                                                           */
/*  legal                                                                    */
/*      This string defines the set of legal characters inside this token.   */
/*      For example, numbers might have "0123456789." for this field, since  */
/*      these are the characters that are legal within numbers, usually.     */
/*                                                                           */
/*  escape                                                                   */
/*      This string defines a single character which is the escape           */
/*      character for this token.  That is, if we are reading this token     */
/*      and come upon this character, the character following it is          */
/*      treated differently.  An empty string "" means no escape character.  */
/*                                                                           */
/*  escape_legal                                                             */
/*      This string defines the set of characters which are legal after      */
/*      the escape character just mentioned.  If any one of these appears    */
/*      immediately after the escape character, it is deemed to be part      */
/*      of the token even if without the preceding escape it would not be.   */
/*                                                                           */
/*  inner_escape                                                             */
/*  end_inner_escape                                                         */
/*      The inner_escape string should be either empty (in which case it     */
/*      does not apply), or else it should contain a single character, the   */
/*      "inner escape" character.  An inner escape is a temporary suspension */
/*      of a token, reverting to the original language.  It is used to set   */
/*      program text within comments.  For example, in Eiffel and Blue,      */
/*      inner_escape is "`" and end_inner_escape is "'" and so we can write  */
/*                                                                           */
/*          -- increment `balance' by `amount'                               */
/*                                                                           */
/*      to treat balance and amount as identifiers within a comment token.   */
/*      The inner escape is not limited to one token, it may have any        */
/*      number of tokens, and they may have inner escapes too; prg2lout      */
/*      imposes no limit on the depth of nesting of inner escapes.           */
/*                                                                           */
/*  end_delimiter                                                            */
/*      This string shows how the token ends; for example, a string would    */
/*      have end_delimiter ".  If empty, it means that the token ends        */
/*      just before the first character encountered that is not legal (see   */
/*      "legal" above).  For example, identifiers and numbers would have     */
/*      empty end_delimiter.  If ends2[] is not empty then end_delimiter     */
/*      is ignored, since ends2[] explains how the token ends.               */
/*                                                                           */
/*  end_start_line_only                                                      */
/*      A BOOLEAN field.  If true, the end delimiter is to be recognized     */
/*      only if it occurs at the very start of a line.                       */
/*                                                                           */
/*  want_two_ends                                                            */
/*      A Boolean feature used only by Perl; TRUE means that end_delimiter   */
/*      (or ends2[]) has to be encountered twice before the token ends,      */
/*      rather than the usual once.  Used by PerSTypeToken to recognise      */
/*                                                                           */
/*           s/abc/ABC/                                                      */
/*                                                                           */
/*      and similar things as single tokens.  Note that this works without   */
/*      modification on s{abc}{ABC} provided { is legal within strings: as   */
/*      long as the token terminates at the second }, we are safe.           */
/*                                                                           */
/*****************************************************************************/


#define MAX_CHAR		256
#define MAX_ARRAY_LENGTH	401
#define	PRINT_WHOLE_QUOTED	1
#define	PRINT_NODELIMS_QUOTED	2
#define	PRINT_WHOLE_UNQUOTED	3
#define	PRINT_NODELIMS_UNQUOTED	4
#define	PRINT_NODELIMS_INNER	5
#define	PRINT_COMMAND_ONLY	6

typedef struct token_rec {
  char *name;
  int print_style;
  char *command;
  char *alternate_command;
  char *following_command;
  BOOLEAN start_line_only;
  char *starts[MAX_ARRAY_LENGTH];
  char *starts2[MAX_ARRAY_LENGTH];
  char *ends2[MAX_ARRAY_LENGTH];
  char *legal;
  char *escape;
  char *escape_legal;
  char *inner_escape;
  char *end_inner_escape;
  char *end_delimiter;
  BOOLEAN end_start_line_only;
  BOOLEAN want_two_ends;

  /* The following options are initialized by the program, so don't you      */
  char chtype[MAX_CHAR];  	  /* character types within token            */
  char escape_chtype[MAX_CHAR];	  /* character types after escape            */
} TOKEN;


/*****************************************************************************/
/*                                                                           */
/*  Tokens defining strings and literal characters in non-Perl languages.    */
/*                                                                           */
/*****************************************************************************/

TOKEN CStringToken = {
  "string",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PS",			/* Lout command for formatting strings              */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "\"" },		/* strings begin with a " character                 */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable is OK		            */
  "\\",			/* within strings, \\ is the escape character       */
  AllPrintablePlusNL,	/* after escape char, any printable char or nl OK   */
  "",			/* strings do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "\"",			/* strings end with a " character                   */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN CCharacterToken = {
  "character",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PC",		/* Lout command for formatting characters           */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "'" },		/* characters begin with a ' character              */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable character is OK            */
  "\\",			/* within characters, \\ is the escape character    */
  AllPrintable,		/* after escape char, any printable char is OK      */
  "",			/* characters do not permit "inner escapes"         */
  "",			/* and so there is no end innner escape either      */
  "'",			/* characters end with a ' character                */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


TOKEN EiffelStringToken = {
  "string",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PS",			/* Lout command for formatting strings              */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "\"" },		/* strings begin with a " character                 */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable except " is OK             */
  "%",			/* within strings, % is the escape character        */
  AllPrintable,		/* after escape char, any printable char is OK      */
  "",			/* strings do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "\"",			/* strings end with a " character                   */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN EiffelCharacterToken = {
  "character",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PC",		/* Lout command for formatting characters           */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "'" },		/* characters begin with a ' character              */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable except ' is OK             */
  "%",			/* within characters, % is the escape character     */
  AllPrintable,		/* after escape char, any printable char is OK      */
  "",			/* characters do not permit "inner escapes"         */
  "",			/* and so there is no end innner escape either      */
  "'",			/* characters end with a ' character                */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Identifiers, in the form common to most programming languages.           */
/*                                                                           */
/*****************************************************************************/

TOKEN IdentifierToken = {
  "identifier",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PI",			/* Lout command for formatting identifiers          */
  "@PK",			/* Alternate command (for keywords)                 */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { SepLetters, "_" },	/* identifiers begin with any letter or _           */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  Letter_Digit,		/* inside, letters, underscores, digits are OK      */
  "",			/* no escape character within identifiers           */
  "",			/* so nothing legal after escape char either        */
  "",			/* identifiers do not permit "inner escapes"        */
  "",			/* and so there is no end innner escape either      */
  "",			/* identifiers do not end with a delimiter          */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Numbers, in the form common to most programming languages.               */
/*                                                                           */
/*****************************************************************************/

TOKEN NumberToken = {
  "number",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PN",			/* Lout command for formatting numbers              */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { SepDigits },	/* numbers must begin with a digit                  */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  "0123456789.eE",	/* inside, digits, decimal point, exponent          */
  "",			/* no escape character within numbers               */
  "",			/* so nothing legal after escape char either        */
  "",			/* numbers do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "",			/* numbers do not end with a delimiter              */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Tokens defining comments in various languages.                           */
/*                                                                           */
/*****************************************************************************/

TOKEN CCommentToken = {
  "comment",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PC",		/* Lout command for formatting comments             */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "/*" },		/* comments begin with this character pair          */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintableTabNL,	/* inside, any printable char, tab, or nl is OK     */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* C comments do not permit "inner escapes"         */
  "",			/* and so there is no end innner escape either      */
  "*/",			/* comments end with this character pair            */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN CPPCommentToken = {
  "comment",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PC",		/* Lout command for formatting comments             */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "//" },		/* comments begin with this character pair          */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable char is OK (not NL)        */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* C comments do not permit "inner escapes"         */
  "",			/* and so there is no end innner escape either      */
  "",			/* no end delimiter (end of line will end it)       */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


TOKEN EiffelCommentToken = {
  "comment",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PC",		/* Lout command for formatting comments             */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "--" },		/* comments begin with this character pair          */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "`",			/* start of "inner escape" in Eiffel comment        */
  "'",			/* end of "inner escape" in Eiffel comment          */
  "",			/* no ending delimiter; end of line will end it     */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN BlueCommentToken = {
  "comment",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PC",		/* Lout command for formatting comments             */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "==", "--" },	/* comments begin with this character pair          */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "`",			/* start of "inner escape" in Blue comment          */
  "'",			/* end of "inner escape" in Blue comment            */
  "",			/* no ending delimiter; end of line will end it     */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Tokens defining escape comments in various languages.                    */
/*                                                                           */
/*  See discussion of "inner escapes" above for more information.            */
/*                                                                           */
/*****************************************************************************/

TOKEN CCommentEscapeToken = {
  "Lout escape",	/* used by error messages involving this token      */
  PRINT_NODELIMS_UNQUOTED,  /* print this token unformatted                 */
  "",			/* no Lout command since we are printing raw        */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "/*@" },		/* escape comments begin with this delimiter        */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintableTabNL,	/* inside, any printable char, tab, or nl is OK     */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* no "inner escape" in escape comments             */
  "",			/* so no end of "inner escape" either               */
  "*/",			/* comments end with this character pair            */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN CPPCommentEscapeToken = {
  "Lout escape",	/* used by error messages involving this token      */
  PRINT_NODELIMS_UNQUOTED, /* print this token unformatted                  */
  "",			/* no Lout command since we are printing raw        */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "//@" },		/* escape comments begin with this delimiter        */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* no "inner escape" in escape comments             */
  "",			/* so no end of "inner escape" either               */
  "",			/* no end delimiter (end of line will end it)       */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


TOKEN EiffelCommentEscapeToken = {
  "Lout escape",	/* used by error messages involving this token      */
  PRINT_NODELIMS_UNQUOTED,  /* print this token unformatted                 */
  "",			/* no Lout command since we are printing raw        */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "--@" },		/* escape comments begin with this delimiter        */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* no "inner escape" in escape comments             */
  "",			/* so no end of "inner escape" either               */
  "",			/* no ending delimiter; end of line will end it     */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN BlueCommentEscapeToken = {
  "Lout escape",	/* used by error messages involving this token      */
  PRINT_NODELIMS_UNQUOTED,  /* print this token unformatted                 */
  "",			/* no Lout command since we are printing raw        */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "==@", "--@" },	/* escape comments begin with these delimiters      */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* no "inner escape" in escape comments             */
  "",			/* so no end of "inner escape" either               */
  "",			/* no ending delimiter; end of line will end it     */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Tokens which are fixed strings, hence simpler than the above.            */
/*                                                                           */
/*****************************************************************************/

#define FixedToken(str, command) /* define fixed-string token */	\
{									\
  str,			/* name used for debugging only       */	\
  PRINT_WHOLE_QUOTED,	/* print this token as usual          */	\
  command,		/* Lout command for formatting this   */	\
  "",			/* no alternate command               */	\
  "",			/* no following command               */	\
  FALSE,		/* token not just start of line       */	\
  { str },		/* token begins (and ends!) with this */	\
  { NULL },		/* no start2 needed		      */	\
  { NULL },		/* so no end2 either		      */	\
  "",			/* nothing inside, since no inside    */	\
  "", "",		/* no escape character                */	\
  "", "",		/* no inner escape; no end inner esc  */	\
  "",			/* no ending delimiter                */	\
  FALSE,		/* end not have to be at line start   */	\
  FALSE,		/* don't end delimiter twice to stop  */	\
}

TOKEN HashToken			= FixedToken("#",  "@PO");
TOKEN ExclamationToken		= FixedToken("!",  "@PO");
TOKEN PercentToken		= FixedToken("%",  "@PO");
TOKEN HatToken			= FixedToken("^",  "@PO");
TOKEN AmpersandToken		= FixedToken("&",  "@PO");
TOKEN SlashToken		= FixedToken("/",  "@PO");
TOKEN ArrowToken		= FixedToken("->", "arrowright @A @PO");
TOKEN BackSlashToken		= FixedToken("\\", "@PO");
TOKEN LeftParenToken		= FixedToken("(",  "@PO");
TOKEN RightParenToken		= FixedToken(")",  "@PO");
TOKEN PlusToken			= FixedToken("+",  "plus @A @PO");
TOKEN EqualToken		= FixedToken("=",  "equal @A @PO");
TOKEN LeftBraceToken		= FixedToken("{",  "@PO");
TOKEN RightBraceToken		= FixedToken("}",  "@PO");
TOKEN BarToken			= FixedToken("|",  "@PO");
TOKEN CircumToken		= FixedToken("~",  "@PO");
TOKEN LeftBracketToken		= FixedToken("[",  "@PO");
TOKEN RightBracketToken		= FixedToken("]",  "@PO");
TOKEN SemicolonToken		= FixedToken(";",  "@PO");
TOKEN ColonToken		= FixedToken(":",  "@PO");
TOKEN LessToken			= FixedToken("<",  "less @A @PO");
TOKEN GreaterToken		= FixedToken(">",  "greater @A @PO");
TOKEN QuestionToken		= FixedToken("?",  "@PO");
TOKEN CommaToken		= FixedToken(",",  "@PO");
TOKEN DotToken			= FixedToken(".",  "@PO");
TOKEN LessEqualToken		= FixedToken("<=", "lessequal @A @PO");
TOKEN GreaterEqualToken		= FixedToken(">=", "greaterequal @A @PO");
TOKEN CNotEqualToken		= FixedToken("!=", "notequal @A @PO");
TOKEN EiffelNotEqualToken	= FixedToken("/=", "notequal @A @PO");
TOKEN BlueNotEqualToken		= FixedToken("<>", "notequal @A @PO");
TOKEN AssignToken		= FixedToken(":=", "@PO");
TOKEN QuestionAssignToken	= FixedToken("?=", "@PO");
TOKEN DollarToken		= FixedToken("$",  "@PO");
TOKEN ImpliesToken		= FixedToken("=>", "implies @A @PO");


/*****************************************************************************/
/*                                                                           */
/*  Fixed-string tokens that are to be printed COMMAND_ONLY (no parameter).  */
/*                                                                           */
/*****************************************************************************/

#define NoParameterToken(str, command) /* define fixed-string token */	\
{									\
  str,			/* name used for debugging only       */	\
  PRINT_COMMAND_ONLY,	/* print only the command             */	\
  command,		/* Lout command for formatting this   */	\
  "",			/* no alternate command               */	\
  "",			/* no following command               */	\
  FALSE,		/* token not just start of line       */	\
  { str },		/* token begins (and ends!) with this */	\
  { NULL },		/* no start2 needed		      */	\
  { NULL },		/* so no end2 either		      */	\
  "",			/* nothing inside, since no inside    */	\
  "", "",		/* no escape character                */	\
  "", "",		/* no inner escape; no end inner esc  */	\
  "",			/* no ending delimiter                */	\
  FALSE,		/* end not have to be at line start   */	\
  FALSE,		/* don't end delimiter twice to stop  */	\
}

TOKEN StarToken			= NoParameterToken("*",  "{@PA}");
TOKEN MinusToken		= NoParameterToken("-",  "{@PM}");
TOKEN EiffelDotToken		= NoParameterToken(".",  "{@PD}");


/*****************************************************************************/
/*                                                                           */
/*  Perl (quarantined from other languages since it's very different).       */
/*                                                                           */
/*  Perl code co-authored by Jeffrey H. Kingston and Mark Summerfield        */
/*  March 2000                                                               */
/*                                                                           */
/*  In the comments below, WCS refers to "Programming Perl", Second          */
/*  Edition (1996), by Wall, Christiansen, and Schwartz.  However Perl       */
/*  has changed since then and this code also reflects those changes.        */
/*                                                                           */
/*****************************************************************************/

/*****************************************************************************/
/*                                                                           */
/*  Perl's strings and regular expressions                                   */
/*                                                                           */
/*  The table in WCS pp. 41 is a good summary of the possibilities:          */
/*                                                                           */
/*      ''    q//                                                            */
/*      ""    qq//                                                           */
/*      ``    qx//                                                           */
/*      ()    qw//                                                           */
/*      //    m//                                                            */
/*      s///  s///                                                           */
/*      y///  tr///                                                          */
/*                                                                           */
/*  To this must be added the following quotation, which begins just         */
/*  below the table:                                                         */
/*                                                                           */
/*      Any non-alphabetic, non-whitespace delimiter can be used in place    */
/*      of /.  If the opening delimiter is a parenthesis, bracket, brace,    */
/*      or angle bracket, the closing delimiter will be the matching         */
/*      construct.  (Embedded occurrences of the delimiters must match in    */
/*      pairs.) ... Finally, for two-string constructs like s/// and tr///,  */
/*      if the first pair of quotes is a bracketing pair, then the second    */
/*      part gets its own starting quote character, which needn't be the     */
/*      same as the first pair.  So you can write things like s{foo}(bar)    */
/*      or tr[a-z][A-Z].  Whitespace is allowed between the two inner quote  */
/*      characters, so you could even write that last one as                 */
/*                                                                           */
/*          tr [a-z]                                                         */
/*             [A-Z]                                                         */
/*                                                                           */
/*  At present the tokens below have three problems implementing this:       */
/*                                                                           */
/*  (1)  When / appears without anything in front, it will be recognized     */
/*       as a division operator, hence the following regular expression      */
/*       will be treated as ordinary Perl, not as a regular expression;      */
/*                                                                           */
/*  (2)  When the delimiters are a matching pair, embedded occurrences       */
/*       are not recognised as such: the first closing delimiter will be     */
/*       taken to close the pair;                                            */
/*                                                                           */
/*  (3)  The case s{foo}(bar) is not implemented; the second bracketing      */
/*       pair must be the same as the first; in fact prg2lout will keep      */
/*       going, looking for a second closing brace, in this example.         */
/*                                                                           */
/*****************************************************************************/


TOKEN PerlSingleQuoteStringToken = {
  "''-string",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PS",			/* Lout command for formatting strings              */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "'" },		/* strings begin with a ' character                 */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllCharacters,	/* inside, any character at all is OK               */
  "\\",			/* within strings, \\ is the escape character       */
  "\\'",		/* after escape, only \ and ' are OK (WCS p. 39)    */
  "",			/* strings do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "\'",			/* strings end with a ' character                   */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PerlDoubleQuoteStringToken = {
  "\"\"-string",	/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PS",			/* Lout command for formatting strings              */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "\"" },		/* strings begin with a " character                 */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllCharacters,	/* inside, any character at all is OK               */
  "\\",			/* within strings, \\ is the escape character       */
  AllCharacters,	/* after escape char, any character at all is OK    */
  "",			/* strings do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "\"",			/* strings end with a " character                   */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PerlBackQuoteStringToken = {
  "``-string",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PS",			/* Lout command for formatting strings              */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "`" },		/* strings begin with a ` character                 */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllCharacters,	/* inside, any character at all is OK               */
  "\\",			/* within strings, \\ is the escape character       */
  AllCharacters,	/* after escape char, any character at all is OK    */
  "",			/* strings do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "`",			/* strings end with a ` character                   */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


TOKEN PerlQTypeStringToken = {
  "q-type-string",	/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PS",			/* Lout command for formatting strings              */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "q", "qq", "qx", "qw", "qr", "m" },	/* q-type strings begin with these  */
  { "/", "(", "[", "{", "<", "!", "%", "#", "|", ",", ":", ";" },  /*start2 */
  { "/", ")", "]", "}", ">", "!", "%", "#", "|", ",", ":", ";" },  /* end2  */
  AllCharacters,	/* inside, any character at all is OK               */
  "\\",			/* within strings, \\ is the escape character       */
  AllCharacters,	/* after escape char, any character at all is OK    */
  "",			/* strings do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "",			/* will be using end2 for the end delimiter here    */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PerlSTypeStringToken = {
  "s-type-string",	/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PS",			/* Lout command for formatting strings              */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "s", "y", "tr" },	/* s-type strings begin with these		    */
  { "/", "(", "[", "{", "<", "!", "%", "#", "|", ",", ":", ";" },  /*start2 */
  { "/", ")", "]", "}", ">", "!", "%", "#", "|", ",", ":", ";" },  /* end2  */
  AllCharacters,	/* inside, any character at all is OK               */
  "\\",			/* within strings, \\ is the escape character       */
  AllCharacters,	/* after escape char, any character at all is OK    */
  "",			/* strings do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "",			/* will be using end2 for the end delimiter here    */
  FALSE,		/* end delimiter does not have to be at line start  */
  TRUE,			/* need to see end delimiter twice to stop	    */
};


/*****************************************************************************/
/*                                                                           */
/*  Perl "bare" regular expressions                                          */
/*                                                                           */
/*  By a bare regular expression, we mean one that is not preceded           */
/*  by q, qq, m etc.  These are distinguished from division by being         */
/*  preceded by one of =, =~, !~, split, if, not, unless, for, foreach,      */
/*  or while, with up to two white space characters intervening.             */
/*                                                                           */
/*****************************************************************************/

#define PerlREToken(start, com)						\
{									\
  "regexp",		/* used by error messages                 */	\
  PRINT_NODELIMS_QUOTED,/* no delims since we supply them         */	\
  com,			/* the command                            */	\
  "",			/* no alternate command                   */	\
  "@PS{\"/\"}",		/* following command (final /)            */	\
  FALSE,		/* token allowed not just start of line   */	\
  { start },		/* preceding token in this case           */	\
  { "/", " /", "\t/", "  /", " \t/", "\t /", "\t\t/" }, /* start2 */	\
  { "/",  "/",   "/",   "/",    "/",    "/",     "/" }, /* end2   */	\
  AllCharacters,	/* any character OK inside                */	\
  "\\",			/* \\ is the escape character             */	\
  AllCharacters,	/* after escape char, any is OK           */	\
  "",			/* no inner escapes                       */	\
  "",			/* no end innner escape either            */	\
  "",			/* will be using end2 here                */	\
  FALSE,		/* no need to end at line start           */	\
  FALSE,		/* don't want end delimiter twice         */	\
}

TOKEN PerlRegExpLPar    = PerlREToken("(",      "@PO{\"(\"} @PS{\"/\"}@PS");
TOKEN PerlRegExpEq      = PerlREToken("=",      "@PO{\"=\"} @PS{\"/\"}@PS");
TOKEN PerlRegExpMatch   = PerlREToken("=~",     "@PO{\"=~\"} @PS{\"/\"}@PS");
TOKEN PerlRegExpNoMatch = PerlREToken("!~",     "@PO{\"!~\"} @PS{\"/\"}@PS");
TOKEN PerlRegExpSplit   = PerlREToken("split",  "@PK{split} @PS{\"/\"}@PS");
TOKEN PerlRegExpIf      = PerlREToken("if",     "@PK{if} @PS{\"/\"}@PS");
TOKEN PerlRegExpNot     = PerlREToken("not",    "@PK{not} @PS{\"/\"}@PS");
TOKEN PerlRegExpUnless  = PerlREToken("unless", "@PK{unless} @PS{\"/\"}@PS");
TOKEN PerlRegExpFor     = PerlREToken("for",    "@PK{for} @PS{\"/\"}@PS");
TOKEN PerlRegExpForEach = PerlREToken("foreach","@PK{foreach} @PS{\"/\"}@PS");
TOKEN PerlRegExpWhile   = PerlREToken("while",  "@PK{while} @PS{\"/\"}@PS");

TOKEN PerlRegExpStartLineToken =
{
  "regexp",		/* used by error messages                 */
  PRINT_WHOLE_QUOTED,	/* we can print the whole thing this time */
  "@PS",		/* the command                            */
  "",			/* no alternate command                   */
  "",			/* no following command                   */
  TRUE,			/* token allowed only at start of line    */
  { "/" },		/* starting delimiter (so easy!)          */
  { NULL },		/* no start2				  */
  { NULL },		/* no end2				  */
  AllCharacters,	/* any character OK inside                */
  "\\",			/* \\ is the escape character             */
  AllCharacters,	/* after escape char, any is OK           */
  "",			/* no inner escapes                       */
  "",			/* no end innner escape either            */
  "/",			/* ending delimiter                       */
  FALSE,		/* no need to end at line start           */
  FALSE,		/* don't want end delimiter twice         */
};


/*****************************************************************************/
/*                                                                           */
/*  Perl's here-documents                                                    */
/*                                                                           */
/*  At present the only terminating strings recognized are EOT, EOF, END,    */
/*  and the empty string.  These may all be quoted in the usual ways.        */
/*                                                                           */
/*****************************************************************************/

#define X(startstr, endstr, startcom, endcom)				\
{									\
  "here-document",	/* used by error messages                 */	\
  PRINT_NODELIMS_QUOTED,/* no delims since we supply them         */	\
  startcom,		/* the command                            */	\
  "",			/* no alternate command                   */	\
  endcom,		/* following command (final /)            */	\
  FALSE,		/* token allowed not just start of line   */	\
  { startstr },		/* starting delimiter                     */	\
  { NULL },		/* no start2				  */	\
  { NULL },		/* no end2				  */	\
  AllCharacters,	/* any character OK inside                */	\
  "", "",		/* no escape character                    */	\
  "", "",		/* no inner escapes                       */	\
  endstr,		/* token ends with this                   */	\
  TRUE,			/* must be found at line start		  */	\
  FALSE,		/* don't want end delimiter twice         */	\
}

#define sEOT	"\n@PS{\"EOT\"}\n"
#define sEOF	"\n@PS{\"EOF\"}\n"
#define sEND	"\n@PS{\"END\"}\n"
#define sBLA	"\n@PS{\"\"}\n"

TOKEN HereEOTuq = X("<<EOT",    "EOT\n", "@PO{<<}@PS{\"EOT\"}@PS",        sEOT);
TOKEN HereEOTdq = X("<<\"EOT\"","EOT\n", "@PO{<<}@PS{\"\\\"EOT\\\"\"}@PS",sEOT);
TOKEN HereEOTfq = X("<<'EOT'",  "EOT\n", "@PO{<<}@PS{\"'EOT'\"}@PS",      sEOT);
TOKEN HereEOTbq = X("<<`EOT`",  "EOT\n", "@PO{<<}@PS{\"`EOT`\"}@PS",      sEOT);

TOKEN HereEOFuq = X("<<EOF",    "EOF\n", "@PO{<<}@PS{\"EOF\"}@PS",        sEOF);
TOKEN HereEOFdq = X("<<\"EOF\"","EOF\n", "@PO{<<}@PS{\"\\\"EOF\\\"\"}@PS",sEOF);
TOKEN HereEOFfq = X("<<'EOF'",  "EOF\n", "@PO{<<}@PS{\"'EOF'\"}@PS",      sEOF);
TOKEN HereEOFbq = X("<<`EOF`",  "EOF\n", "@PO{<<}@PS{\"`EOF`\"}@PS",      sEOF);

TOKEN HereENDuq = X("<<END",    "END\n", "@PO{<<}@PS{\"END\"}@PS",        sEND);
TOKEN HereENDdq = X("<<\"END\"","END\n", "@PO{<<}@PS{\"\\\"END\\\"\"}@PS",sEND);
TOKEN HereENDfq = X("<<'END'",  "END\n", "@PO{<<}@PS{\"'END'\"}@PS",      sEND);
TOKEN HereENDbq = X("<<`END`",  "END\n", "@PO{<<}@PS{\"`END`\"}@PS",      sEND);

TOKEN HereBLAuq = X("<< ",      "\n",    "@PO{<<} @PS",                  sBLA);
TOKEN HereBLAdq = X("<<\"\"",   "\n",    "@PO{<<}@PS{\"\\\"\\\"\"}@PS",   sBLA);
TOKEN HereBLAfq = X("<<''",     "\n",    "@PO{<<}@PS{\"''\"}@PS",         sBLA);
TOKEN HereBLAbq = X("<<``",     "\n",    "@PO{<<}@PS{\"``\"}@PS",         sBLA);


/*****************************************************************************/
/*                                                                           */
/*  Perl's identifiers                                                       */
/*                                                                           */
/*      "Names that start with a letter or underscore may be of any          */
/*       length ... and may contain letters, digits, and underscores.        */
/*       Names that start with a digit may only contain more digits.         */
/*       Names that start with anything else are limited to that one         */
/*       character (like $? or $$) and generally have a predefined           */
/*       significance to Perl."  (WCS page 38)                               */
/*                                                                           */
/*  In addition we have to consider that variable names may be preceded      */
/*  by $, @, %, &, or *.  Whether these are part of the variable or not is   */
/*  rather doubtful.  We will treat $ and @ as part of the variable and the  */
/*  others not (since they occur elsewhere in the token list anyway); plus   */
/*  we have a separate token type for identifiers beginning with $ and       */
/*  followed by one character, which we will list explicitly.                */
/*                                                                           */
/*****************************************************************************/

TOKEN PerlIdentifierToken = {
  "identifier",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PI",			/* Lout command for formatting identifiers          */
  "@PK",		/* Alternate command (for keywords)                 */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  {SepLetters,"_","$","@"},	/* identifiers begin with these		    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  Letter_Digit,		/* inside, letters, underscores, digits are OK      */
  "",			/* no escape character within identifiers           */
  "",			/* so nothing legal after escape char either        */
  "",			/* identifiers do not permit "inner escapes"        */
  "",			/* and so there is no end innner escape either      */
  "",			/* identifiers do not end with a delimiter          */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PerlSpecialIdentifierToken = {
  "special identifier",	/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PI",			/* Lout command for formatting identifiers          */
  "@PK",		/* Alternate command (for keywords)                 */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "$_", "$.", "$/", "$,", "$\\", "$\"", "$;", "$^L", "$:",
    "$^A", "$#", "$?", "$!", "$@", "$$", "$<", "$>", "$(",
    "$)", "$0", "$[", "$]", "$^D", "$^E", "$^F", "$^H", "$^I",
    "$^M", "$^O", "$^P", "$^T", "$^W", "$^X"
  },			/* all possible values of special variables         */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  "",			/* nothing allowed inside, since ends after start   */
  "",			/* no escape character within identifiers           */
  "",			/* so nothing legal after escape char either        */
  "",			/* identifiers do not permit "inner escapes"        */
  "",			/* and so there is no end innner escape either      */
  "",			/* identifiers do not end with a delimiter          */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Perl's numeric literals                                                  */
/*                                                                           */
/*  These are defined in WCS page 39 basically by giving these examples:     */
/*                                                                           */
/*      12345           # integer                                            */
/*      12345.67        # floating point                                     */
/*      6.02E23         # scientific notation                                */
/*      0xffff          # hexadecimal                                        */
/*      0377            # octal                                              */
/*      4_294_967_296   # underline for legibility                           */
/*                                                                           */
/*  Implementation is straightforward; hexadecimal is a separate token.      */
/*                                                                           */
/*****************************************************************************/

TOKEN PerlLiteralNumberToken = {
  "number",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PN",			/* Lout command for formatting numbers              */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { SepDigits },	/* numbers must begin with a digit                  */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  "0123456789.eE_",	/* inside, digits, point, exponent, underscore      */
  "",			/* no escape character within numbers               */
  "",			/* so nothing legal after escape char either        */
  "",			/* numbers do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "",			/* numbers do not end with a delimiter              */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PerlHexNumberToken = {
  "number",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PN",			/* Lout command for formatting numbers              */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "0x" },		/* hex numbers must begin with 0x		    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  "0123456789AaBbCcDdEeFf",	/* inside, hexadecimal digits		    */
  "",			/* no escape character within numbers               */
  "",			/* so nothing legal after escape char either        */
  "",			/* numbers do not permit "inner escapes"            */
  "",			/* and so there is no end innner escape either      */
  "",			/* numbers do not end with a delimiter              */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Perl's comments                                                          */
/*                                                                           */
/*  "Comments are indicated by the # character and extend to the end of      */
/*  the line." (WCS page 35).  To this we have added the usual Lout escape   */
/*  comment beginning with #@.                                               */
/*                                                                           */
/*****************************************************************************/

TOKEN PerlCommentToken = {
  "comment",		/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* print this token in quotes etc. as usual         */
  "@PC",		/* Lout command for formatting comments             */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "#" },		/* comments begin with this character		    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable char is OK (not NL)        */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* C comments do not permit "inner escapes"         */
  "",			/* and so there is no end innner escape either      */
  "",			/* no end delimiter (end of line will end it)       */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PerlCommentEscapeToken = {
  "Lout escape",	/* used by error messages involving this token      */
  PRINT_NODELIMS_UNQUOTED, /* print this token unformatted                  */
  "",			/* no Lout command since we are printing raw        */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  FALSE,		/* token allowed anywhere, not just start of line   */
  { "#@" },		/* comments begin with this character pair	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintable,		/* inside, any printable char is OK                 */
  "",			/* no escape character within comments              */
  "",			/* so nothing legal after escape char either        */
  "",			/* no "inner escape" in escape comments             */
  "",			/* so no end of "inner escape" either               */
  "",			/* no end delimiter (end of line will end it)       */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Perl's POD sub-language                                                  */
/*                                                                           */
/*  Pod is handled as a completely different language.  However we need      */
/*  one Perl token which recognizes an entire Pod interpolation and prints   */
/*  it enclosed in @Pod { ... } so that Lout knows to call back later on it. */
/*                                                                           */
/*  "A line beginning with = is assumed to introduce some documentation,     */
/*  which continues until another line is reached beginning with =cut"       */
/*  (WCS page36).  Strictly speaking this is only valid at points where      */
/*  a statement would be legal, but that is beyond prg2lout to implement.    */
/*                                                                           */
/*****************************************************************************/

TOKEN PerlPodToken = {
  "perl-pod",		/* used by error messages involving this token      */
  PRINT_NODELIMS_UNQUOTED,  /* unquoted but with a command enclosing it     */
  "@DP @Pod",		/* Lout command for formatting Pod                  */
  "",			/* no alternate command                             */
  "@DP\n",		/* following command                                */
  TRUE,			/* token allowed at start of line only              */
  { "=", "=pod" },	/* pod insert begins with either of these	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllCharacters,	/* inside, any character at all is OK               */
  "",			/* no escape character within pod comments          */
  "",			/* so nothing legal after escape char either        */
  "",			/* pod comments do not permit "inner escapes"       */
  "",			/* and so there is no end innner escape either      */
  "=cut",		/* pod comments end with this string                */
  TRUE,			/* end delimiter must be at line start		    */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Perl's operators                                                         */
/*                                                                           */
/*  Only those not already in the C/C++ list are given here.                 */
/*                                                                           */
/*****************************************************************************/

TOKEN PerlIncrementToken        = FixedToken( "++",  "@PO" ) ;
TOKEN PerlDecrementToken        = FixedToken( "--",  "@PO" ) ;
TOKEN PerlExponentiateToken     = FixedToken( "**",  "@PO" ) ;
TOKEN PerlMatchToken            = FixedToken( "=~",  "@PO" ) ;
TOKEN PerlNotMatchToken         = FixedToken( "!~",  "@PO" ) ;
TOKEN PerlEqualToken            = FixedToken( "==",  "@PO" ) ;
TOKEN PerlAssignToken           = FixedToken( "=",   "@PO" ) ;
TOKEN PerlBitLeftShiftToken     = FixedToken( "<<",  "@PO" ) ;
TOKEN PerlBitRightShiftToken    = FixedToken( ">>",  "@PO" ) ;
TOKEN PerlSpaceshipToken        = FixedToken( "<=>", "@PO" ) ;
TOKEN PerlAndToken              = FixedToken( "&&",  "@PO" ) ;
TOKEN PerlOrToken               = FixedToken( "||",  "@PO" ) ;
TOKEN PerlRange2Token           = FixedToken( "..",  "@PO" ) ;
TOKEN PerlRange3Token           = FixedToken( "...", "@PO" ) ;
TOKEN PerlFileTestrToken        = FixedToken( "-r",  "@PO" ) ;
TOKEN PerlFileTestwToken        = FixedToken( "-w",  "@PO" ) ;
TOKEN PerlFileTestxToken        = FixedToken( "-x",  "@PO" ) ;
TOKEN PerlFileTestoToken        = FixedToken( "-o",  "@PO" ) ;
TOKEN PerlFileTestRToken        = FixedToken( "-R",  "@PO" ) ;
TOKEN PerlFileTestWToken        = FixedToken( "-W",  "@PO" ) ;
TOKEN PerlFileTestXToken        = FixedToken( "-X",  "@PO" ) ;
TOKEN PerlFileTestOToken        = FixedToken( "-O",  "@PO" ) ;
TOKEN PerlFileTesteToken        = FixedToken( "-e",  "@PO" ) ;
TOKEN PerlFileTestzToken        = FixedToken( "-z",  "@PO" ) ;
TOKEN PerlFileTestsToken        = FixedToken( "-s",  "@PO" ) ;
TOKEN PerlFileTestfToken        = FixedToken( "-f",  "@PO" ) ;
TOKEN PerlFileTestdToken        = FixedToken( "-d",  "@PO" ) ;
TOKEN PerlFileTestlToken        = FixedToken( "-l",  "@PO" ) ;
TOKEN PerlFileTestpToken        = FixedToken( "-p",  "@PO" ) ;
TOKEN PerlFileTestSToken        = FixedToken( "-S",  "@PO" ) ;
TOKEN PerlFileTestbToken        = FixedToken( "-b",  "@PO" ) ;
TOKEN PerlFileTestcToken        = FixedToken( "-c",  "@PO" ) ;
TOKEN PerlFileTesttToken        = FixedToken( "-t",  "@PO" ) ;
TOKEN PerlFileTestuToken        = FixedToken( "-u",  "@PO" ) ;
TOKEN PerlFileTestgToken        = FixedToken( "-g",  "@PO" ) ;
TOKEN PerlFileTestkToken        = FixedToken( "-k",  "@PO" ) ;
TOKEN PerlFileTestTToken        = FixedToken( "-T",  "@PO" ) ;
TOKEN PerlFileTestBToken        = FixedToken( "-B",  "@PO" ) ;
TOKEN PerlFileTestMToken        = FixedToken( "-M",  "@PO" ) ;
TOKEN PerlFileTestAToken        = FixedToken( "-A",  "@PO" ) ;
TOKEN PerlFileTestCToken        = FixedToken( "-C",  "@PO" ) ;


/*****************************************************************************/
/*                                                                           */
/*  Pod (Plain Old Documentation, used with Perl) tokens                     */
/*                                                                           */
/*  Pod is treated as a completely different language to Perl.  It is        */
/*  quite possible to use Pod alone without Perl; or, thanks to the          */
/*  PerlPodToken, to embed Pod in Perl in the usual way.  Quotations below   */
/*  are from Larry Wall's documentation, communicated by Mark Summerfield.   */
/*                                                                           */
/*****************************************************************************/

/*****************************************************************************/
/*                                                                           */
/*  Pod Verbatim paragraphs                                                  */
/*                                                                           */
/*  "A verbatim paragraph [is] distinguished by being indented (that is, it  */
/*   starts with a space or tab).  It should be reproduced exactly, with     */
/*   tabs assumed to be on 8-column boundaries.  There are no special        */
/*   formatting escapes."                                                    */
/*                                                                           */
/*  By a "paragraph" is meant a sequence of lines down to the next empty     */
/*  line; but we will handle verbatim paragraphs one line at a time.         */
/*  It also seems that an empty line in the input has to become an empty     */
/*  line in the output; so we include a token for that.                      */
/*                                                                           */
/*****************************************************************************/

TOKEN PodVerbatimLineToken = {
  "verbatim-para",	/* used by error messages involving this token      */
  PRINT_WHOLE_QUOTED,	/* printing the whole paragraph quoted		    */
  "@PV ",		/* Lout command for formatting verbatim line        */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  TRUE,			/* token allowed at start of line only              */
  { "\t", " " },	/* command begins with this                   	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllPrintablePlusTab,	/* inside, any printable char except newline is OK  */
  "", "",		/* no escape character within verbatim lines        */
  "", "",		/* no "inner escapes" within verbatim lines         */
  "",			/* ends at end of line                              */
  FALSE,		/* don't need to be at start of line to end it      */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PodEmptyLineToken = {
  "pod-empty-line",	/* used by error messages involving this token      */
  PRINT_COMMAND_ONLY,	/* printing just the command                        */
  "@PPG\n",		/* Lout command for formatting Pod empty line       */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  TRUE,			/* token allowed at start of line only              */
  { "\n" },		/* command begins with this                   	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  "",			/* nothing inside                                   */
  "", "",		/* no escape character                              */
  "", "",		/* no inner escape                                  */
  "",			/* token will end with the end of the line          */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Pod Command paragraphs                                                   */
/*                                                                           */
/*  "All command paragraphs start with =, followed by an identifier,         */
/*   followed by arbitrary text that the command can use."                   */
/*                                                                           */
/*  "[A] command lasts up until the end of the paragraph, not the line.      */
/*   Hence, ... you can see the empty lines after each command to end        */
/*   its paragraph."                                                         */
/*                                                                           */
/*****************************************************************************/

/*****************************************************************************/
/*                                                                           */
/*  Pod command paragraphs: =pod                                             */
/*                                                                           */
/*  "The =pod directive does nothing beyond telling the compiler to lay off  */
/*   parsing code through the next =cut."                                    */
/*                                                                           */
/*****************************************************************************/

TOKEN PodIgnoreToken = {
  "pod-cut",		/* used by error messages involving this token      */
  PRINT_COMMAND_ONLY,	/* printing just the command                        */
  "",			/* Lout command for formatting Pod cut (nothing)    */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  TRUE,			/* token allowed at start of line only              */
  { "=pod", "=cut" },	/* command begins with this                   	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllCharacters,	/* anything at all can be inside                    */
  "",			/* no escape character                              */
  "",			/* so nothing legal after escape char either        */
  "",			/* cut tokens do not permit "inner escapes"         */
  "",			/* and so there is no end innner escape either      */
  "\n",			/* token will end with the end of the line          */
  TRUE,			/* end delimiter (\n) has to be at a line start     */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Pod command paragraphs: =head1, =head2 (and =head3, folklore extension)  */
/*                                                                           */
/*  "... produce first and second level heading"                             */
/*                                                                           */
/*****************************************************************************/

TOKEN PodHeading1Token = {
  "=head1",		/* used by error messages involving this token      */
  PRINT_NODELIMS_QUOTED,  /* print without delimiters                       */
  "@PHA",		/* Lout command for formatting Pod heading          */
  "",			/* no alternate command                             */
  "",			/* following command                                */
  TRUE,			/* token allowed at start of line only              */
  {"=head1","head1"},	/* command begins with this                   	    */
  { " ", "\t" },	/* helps to skip following white space		    */
  { "\n", "\n" },	/* token ends at end of line			    */
  AllCharacters,	/* anything in the heading			    */
  "", "",		/* no escape character; nothing legal after escape  */
  "", "",		/* no inner escapes; no end inner escape            */
  "\n\n",		/* token will end with the first blank line         */
  FALSE,		/* end delimiter (\n) has to be at a line start     */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PodHeading2Token = {
  "=head2",		/* used by error messages involving this token      */
  PRINT_NODELIMS_QUOTED,  /* print without delimiters                       */
  "@PHB",		/* Lout command for formatting Pod heading          */
  "",			/* no alternate command                             */
  "",			/* following command                                */
  TRUE,			/* token allowed at start of line only              */
  { "=head2" },		/* command begins with this                   	    */
  { " ", "\t" },	/* helps to skip following white space		    */
  { "\n", "\n" },	/* token ends at end of line			    */
  AllCharacters,	/* anything in the heading			    */
  "", "",		/* no escape character; nothing legal after escape  */
  "", "",		/* no inner escapes; no end inner escape            */
  "\n\n",		/* token will end with the first blank line         */
  FALSE,		/* end delimiter (\n) has to be at a line start     */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PodHeading3Token = {
  "=head3",		/* used by error messages involving this token      */
  PRINT_NODELIMS_QUOTED,  /* print without delimiters                       */
  "@PHC",		/* Lout command for formatting Pod heading          */
  "",			/* no alternate command                             */
  "",			/* following command                                */
  TRUE,			/* token allowed at start of line only              */
  { "=head3" },		/* command begins with this                   	    */
  { " ", "\t" },	/* helps to skip following white space		    */
  { "\n", "\n" },	/* token ends at end of line			    */
  AllCharacters,	/* anything in the heading			    */
  "", "",		/* no escape character; nothing legal after escape  */
  "", "",		/* no inner escapes; no end inner escape            */
  "\n\n",		/* token will end with the first blank line         */
  FALSE,		/* end delimiter (\n) has to be at a line start     */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Pod command paragraphs: =over, =item, and =back (for lists)              */
/*                                                                           */
/*****************************************************************************/

TOKEN PodOverToken = {
  "=over",		/* used by error messages involving this token      */
  PRINT_NODELIMS_UNQUOTED, /* just a number after =over, so this is safe    */
  "@RawTaggedList gap{@PLG}indent{@PLI}rightindent{@PLRI}labelwidth{@PLLW ",
  "",			/* no alternate command                             */
  "} // {",		/* open brace to match } at first item              */
  TRUE,			/* token allowed at start of line only              */
  { "=over" },		/* command begins with this                   	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllCharacters,	/* inside, any printable char is OK		    */
  "", "",		/* no escape character; nothing legal after escape  */
  "", "",		/* no inner escapes; no end inner escape            */
  "\n",			/* token will end with the end of the line          */
  TRUE,			/* end delimiter (\n) has to be at a line start     */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PodItemToken = {
  "=item",		/* used by error messages involving this token      */
  PRINT_NODELIMS_QUOTED,/* printing just what follows =item on the line     */
  "@Null //}\n@TI {@PLL", /* Lout command for formatting Pod item           */
  "",			/* no alternate command                             */
  "} {",		/* open brace to enclose the item content           */
  TRUE,			/* token allowed at start of line only              */
  { "=item" },		/* command begins with this                   	    */
  { " ", "\t" },	/* helps to skip following white space		    */
  { "\n\n", "\n\n"  },	/* token will end at blank line			    */
  AllPrintableTabNL,	/* any printable inside				    */
  "", "",		/* no escape character; nothing legal after escape  */
  "", "",		/* no inner escapes; no end inner escape            */
  "",			/* see ends2[]                                      */
  FALSE,		/* end delimiter (\n) must already be at start      */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PodBackToken = {
  "=back",		/* used by error messages involving this token      */
  PRINT_COMMAND_ONLY,	/* printing just the command                        */
  "@Null // }\n@EndList\n", /* Lout command for formatting Pod back         */
  "",			/* no alternate command                             */
  "",			/* no following command                             */
  TRUE,			/* token allowed at start of line only              */
  { "=back" },		/* command begins with this                   	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllCharacters,	/* anything inside (in principle)   		    */
  "", "",		/* no escape character; nothing legal after escape  */
  "", "",		/* no inner escapes; no end inner escape            */
  "\n",			/* token will end with the next blank line          */
  TRUE,			/* end delimiter (\n) has to be at a line start     */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Pod command paragraphs: =for, =begin, =end                               */
/*                                                                           */
/*  "passed directly to particular formatters.  A formatter that can utilize */
/*   that format will use the section, otherwise it will be ignored."        */
/*                                                                           */
/*  So I've put in a "=begin lout" token, also recognized as "=begin Lout".  */
/*                                                                           */
/*****************************************************************************/

TOKEN PodForToken = {
  "=for",		/* used by error messages involving this token      */
  PRINT_COMMAND_ONLY,	/* printing just the command                        */
  "",			/* Lout command for formatting Pod for (nothing)    */
  "", "",		/* no alternate command; no following command       */
  TRUE,			/* token allowed at start of line only              */
  { "=for" },		/* command begins with this                   	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllCharacters,	/* anything inside 				    */
  "", "",		/* no escape character; nothing legal after escape  */
  "", "",		/* no inner escapes; no end inner escape            */
  "\n",			/* token will end with the end of the line          */
  TRUE,			/* end delimiter (\n) has to be at a line start     */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PodBeginToken = {
  "=begin",		/* used by error messages involving this token      */
  PRINT_COMMAND_ONLY,	/* printing just the command                        */
  "",			/* Lout command for formatting Pod for (nothing)    */
  "", "",		/* no alternate command; no following command       */
  TRUE,			/* token allowed at start of line only              */
  { "=begin" },		/* command begins with this                   	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllCharacters,	/* anything inside 				    */
  "", "",		/* no escape character; nothing legal after escape  */
  "", "",		/* no inner escapes; no end inner escape            */
  "=end",		/* token will end with =end character               */
  TRUE,			/* end delimiter has to be at a line start          */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

TOKEN PodBeginLoutToken = {
  "=begin lout",	/* used by error messages involving this token      */
  PRINT_NODELIMS_UNQUOTED,/* this is a Lout escape, no delims or quotes     */
  "",			/* Lout command for formatting Pod for (nothing)    */
  "", "",		/* no alternate command; no following command       */
  TRUE,			/* token allowed at start of line only              */
  { "=begin lout", "=begin Lout" },	/* command begins with this   	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  AllCharacters,	/* anything inside 				    */
  "", "",		/* no escape character; nothing legal after escape  */
  "", "",		/* no inner escapes; no end inner escape            */
  "=end",		/* token will end with =end character               */
  TRUE,			/* end delimiter has to be at a line start          */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};


/*****************************************************************************/
/*                                                                           */
/*  Pod "Ordinary Block of Text" paragraphs                                  */
/*                                                                           */
/*  "It will be filled, and maybe even justified" - I'm setting the whole    */
/*  Pod in adjust @Break, and making sure that verbatim and command          */
/*  paragraphs don't get adjusted.  So no special requirements here, it      */
/*  should all happen without any explicit tokens, given that I've set       */
/*  the Pod language up to simply echo any characters (suitably quoted if    */
/*  necessary in Lout) that don't match anything else.                       */
/*                                                                           */
/*****************************************************************************/

/*****************************************************************************/
/*                                                                           */
/*  Pod interior sequences (recursive)                                       */
/*                                                                           */
/*  I<text>    Italicize text                                                */
/*  B<text>    Embolden text                                                 */
/*  S<text>    Text containing non-break spaces                              */
/*  C<code>    Code "render in typewriter font, or ..."                      */
/*                                                                           */
/*  Alternatively, instead of "<" .. ">" we may use "<< " .. " >>", or       */
/*  "<<< " .. " >>>", etc. (Note the whitespace.)                            */
/*                                                                           */
/*****************************************************************************/

#define RecursiveToken(str, command) /* Pod recursive token */		\
{									\
  str,			/* name used for debugging only       */	\
  PRINT_NODELIMS_INNER,	/* recursively format the inside      */	\
  command,		/* Lout command for formatting this   */	\
  "", "",		/* no alternate command; no following */	\
  FALSE,		/* token not just start of line       */	\
  { str },		/* token begins with this	      */	\
  { "<", "<< ", "<<< ", "<<<< " }, /* start2		      */	\
  { ">", " >>", " >>>", " >>>>" }, /* end2		      */	\
  AllCharacters,	/* anything inside (in fact, not used)*/	\
  "", "",		/* no escape character                */	\
  "", "",		/* no inner escape; no end inner esc  */	\
  "",			/* will use end2 here                 */	\
  FALSE,		/* end not have to be at line start   */	\
  FALSE,		/* don't end delimiter twice to stop  */	\
}

TOKEN PodItalicToken	= RecursiveToken("I", "@PFI");
TOKEN PodBoldToken	= RecursiveToken("B", "@PFB");
TOKEN PodNoBreakToken	= RecursiveToken("S", "@OneCol");
TOKEN PodCodeToken	= RecursiveToken("C", "@PFC");


/*****************************************************************************/
/*                                                                           */
/*  Pod interior sequences (non-recursive)                                   */
/*                                                                           */
/*  L<name>    A link; these have an internal format I've not looked at yet. */
/*  F<file>    File name                                                     */
/*  X<index>   Index                                                         */
/*  Z<>        A zero-width space                                            */
/*                                                                           */
/*  Alternatively, instead of "<" .. ">" we may use "<< " .. " >>", or       */
/*  "<<< " .. " >>>", etc. (Note the whitespace.)                            */
/*                                                                           */
/*****************************************************************************/

#define InteriorToken(str, command, style) /* Pod delimited token */	\
{									\
  str,			/* name used for debugging only       */	\
  style,		/* print this token unquoted          */	\
  command,		/* Lout command for formatting this   */	\
  "", "",		/* no alternate command; no following */	\
  FALSE,		/* token not just start of line       */	\
  { str },		/* token begins with this	      */	\
  { "<", "<< ", "<<< ", "<<<< " }, /* start2		      */	\
  { ">", " >>", " >>>", " >>>>" }, /* end2		      */	\
  AllCharacters,	/* anything inside                    */	\
  "", "",		/* no escape character                */	\
  "", "",		/* no inner escape; no end inner esc  */	\
  "",			/* will use end2 here                 */	\
  FALSE,		/* end not have to be at line start   */	\
  FALSE,		/* don't end delimiter twice to stop  */	\
}

TOKEN PodFileToken	= InteriorToken("F", "@PFF",    PRINT_NODELIMS_QUOTED);
TOKEN PodLinkToken	= InteriorToken("L", "@PFL",    PRINT_NODELIMS_QUOTED);
TOKEN PodIndexToken	= InteriorToken("X", "@PFX",    PRINT_NODELIMS_QUOTED);
TOKEN PodZeroToken	= InteriorToken("Z", "",        PRINT_COMMAND_ONLY);


/*****************************************************************************/
/*                                                                           */
/*  Pod interior sequences (escape sequences)                                */
/*                                                                           */
/*  E<escape>  A named character ("optional except in other interior         */
/*             sequences and when preceded by a capital letter")             */
/*                                                                           */
/*                E<lt>     A literal <                                      */
/*                E<gt>     A literal >                                      */
/*                E<sol>    A literal /                                      */
/*                E<verbar> A literal |                                      */
/*                E<n>      Character number n (probably in ASCII)           */
/*                E<html>   Some non-numeric HTML entity, such as E<Agrave>  */
/*                                                                           */
/*  PodNumCharToken not tested.                                              */
/*                                                                           */
/*****************************************************************************/

TOKEN PodNumCharToken = {
  "E<>",		/* used by error messages involving this token      */
  PRINT_NODELIMS_UNQUOTED,/* we're doing these manually, since they're funny*/
  "\"\\",		/* precede character number with \"                 */
  "",			/* no alternate command                             */
  "\"",			/* follow character number with "                   */
  FALSE,		/* token allowed at start of line only              */
  { "E<" },		/* command begins with this              	    */
  { NULL },		/* no start2 needed				    */
  { NULL },		/* so no end2 either				    */
  "0123456789",		/* digits inside 				    */
  "", "",		/* no escape character                              */
  "", "",		/* no "inner escapes"                               */
  ">",			/* token will end with > character                  */
  FALSE,		/* end delimiter does not have to be at line start  */
  FALSE,		/* don't need to see end delimiter twice to stop    */
};

#define PodEscapeToken(str, command) /* Pod delimited token */		\
{									\
  str,			/* name used for debugging only       */	\
  PRINT_COMMAND_ONLY,	/* print this token unquoted          */	\
  command,		/* Lout command for formatting this   */	\
  "",			/* no alternate command               */	\
  "",			/* no following command               */	\
  FALSE,		/* token not just start of line       */	\
  { str },		/* token begins with this	      */	\
  { NULL },		/* start2			      */	\
  { NULL },		/* end2				      */	\
  "",			/* nothing inside                     */	\
  "", "",		/* no escape character                */	\
  "", "",		/* no inner escape either             */	\
  "",			/* no ending delimiter except end2    */	\
  FALSE,		/* end not have to be at line start   */	\
  FALSE,		/* don't end delimiter twice to stop  */	\
}

TOKEN PodLessThanToken = PodEscapeToken("E<lt>", "<");
TOKEN PodGreaterThanToken = PodEscapeToken("E<gt>", ">");
TOKEN PodSlashToken = PodEscapeToken("E<sol>", "/");
TOKEN PodVerbarToken = PodEscapeToken("E<verbar>", "|");


/*****************************************************************************/
/*                                                                           */
/*  Mark Summerfield writes:                                                 */
/*                                                                           */
/*  The following table (and most of its comments) is copied from Gisle Aas  */
/*  HTML::Entities.pm module with the plain text characters being replaced   */
/*  by their Lout equivalents and the HTML entities with their pod           */
/*  equivalents.                                                             */
/*                                                                           */
/*****************************************************************************/

/* Some normal chars that have special meaning in SGML context */
TOKEN PE00 = PodEscapeToken("E<amp>",		"&");
/* already done above TOKEN PE01 = PodEscapeToken("E<gt>",	">"); */
/* already done above TOKEN PE02 = PodEscapeToken("E<lt>",	"<"); */
TOKEN PE03 = PodEscapeToken("E<quot>",	"\"\\\"\"");

/* PUBLIC ISO 8879-1986//ENTITIES Added Latin 1//EN//HTML */
TOKEN PE04 = PodEscapeToken("E<AElig>",		"{@Char AE}");
TOKEN PE05 = PodEscapeToken("E<Aacute>",	"{@Char Aacute}");
TOKEN PE06 = PodEscapeToken("E<Acirc>",		"{@Char Acircumflex}");
TOKEN PE07 = PodEscapeToken("E<Agrave>",	"{@Char Agrave}");
TOKEN PE08 = PodEscapeToken("E<Aring>",		"{@Char Aring}");
TOKEN PE09 = PodEscapeToken("E<Atilde>",	"{@Char Atilde}");
TOKEN PE10 = PodEscapeToken("E<Auml>",		"{@Char Adieresis}");
TOKEN PE11 = PodEscapeToken("E<Ccedil>",	"{@Char Ccedilla}");
TOKEN PE12 = PodEscapeToken("E<ETH>",		"{@Char Eth}");
TOKEN PE13 = PodEscapeToken("E<Eacute>",	"{@Char Eacute}");
TOKEN PE14 = PodEscapeToken("E<Ecirc>",		"{@Char Ecircumflex}");
TOKEN PE15 = PodEscapeToken("E<Egrave>",	"{@Char Egrave}");
TOKEN PE16 = PodEscapeToken("E<Euml>",		"{@Char Edieresis}");
TOKEN PE17 = PodEscapeToken("E<Iacute>",	"{@Char Iacute}");
TOKEN PE18 = PodEscapeToken("E<Icirc>",		"{@Char Icircumflex}");
TOKEN PE19 = PodEscapeToken("E<Igrave>",	"{@Char Igrave}");
TOKEN PE20 = PodEscapeToken("E<Iuml>",		"{@Char Idieresis}");
TOKEN PE21 = PodEscapeToken("E<Ntilde>",	"{@Char Ntilde}");
TOKEN PE22 = PodEscapeToken("E<Oacute>",	"{@Char Oacute}");
TOKEN PE23 = PodEscapeToken("E<Ocirc>",		"{@Char Ocircumflex}");
TOKEN PE24 = PodEscapeToken("E<Ograve>",	"{@Char Ograve}");
TOKEN PE25 = PodEscapeToken("E<Oslash>",	"{@Char Oslash}");
TOKEN PE26 = PodEscapeToken("E<Otilde>",	"{@Char Otilde}");
TOKEN PE27 = PodEscapeToken("E<Ouml>",		"{@Char Odieresis}");
TOKEN PE28 = PodEscapeToken("E<THORN>",		"{@Char Thorn}");
TOKEN PE29 = PodEscapeToken("E<Uacute>",	"{@Char Uacute}");
TOKEN PE30 = PodEscapeToken("E<Ucirc>",		"{@Char Ucircumflex}");
TOKEN PE31 = PodEscapeToken("E<Ugrave>",	"{@Char Ugrave}");
TOKEN PE32 = PodEscapeToken("E<Uuml>",		"{@Char Udieresis}");
TOKEN PE33 = PodEscapeToken("E<Yacute>",	"{@Char Yacute}");
TOKEN PE34 = PodEscapeToken("E<aacute>",	"{@Char aacute}");
TOKEN PE35 = PodEscapeToken("E<acirc>",		"{@Char acircumflex}");
TOKEN PE36 = PodEscapeToken("E<aelig>",		"{@Char ae}");
TOKEN PE37 = PodEscapeToken("E<agrave>",	"{@Char agrave}");
TOKEN PE38 = PodEscapeToken("E<aring>",		"{@Char aring}");
TOKEN PE39 = PodEscapeToken("E<atilde>",	"{@Char atilde}");
TOKEN PE40 = PodEscapeToken("E<auml>",		"{@Char adieresis}");
TOKEN PE41 = PodEscapeToken("E<ccedil>",	"{@Char ccedilla}");
TOKEN PE42 = PodEscapeToken("E<eacute>",	"{@Char eacute}");
TOKEN PE43 = PodEscapeToken("E<ecirc>",		"{@Char ecircumflex}");
TOKEN PE44 = PodEscapeToken("E<egrave>",	"{@Char egrave}");
TOKEN PE45 = PodEscapeToken("E<eth>",		"{@Char eth}");
TOKEN PE46 = PodEscapeToken("E<euml>",		"{@Char edieresis}");
TOKEN PE47 = PodEscapeToken("E<iacute>",	"{@Char iacute}");
TOKEN PE48 = PodEscapeToken("E<icirc>",		"{@Char icircumflex}");
TOKEN PE49 = PodEscapeToken("E<igrave>",	"{@Char igrave}");
TOKEN PE50 = PodEscapeToken("E<iuml>",		"{@Char idieresis}");
TOKEN PE51 = PodEscapeToken("E<ntilde>",	"{@Char ntilde}");
TOKEN PE52 = PodEscapeToken("E<oacute>",	"{@Char oacute}");
TOKEN PE53 = PodEscapeToken("E<ocirc>",		"{@Char ocircumflex}");
TOKEN PE54 = PodEscapeToken("E<ograve>",	"{@Char ograve}");
TOKEN PE55 = PodEscapeToken("E<oslash>",	"{@Char oslash}");
TOKEN PE56 = PodEscapeToken("E<otilde>",	"{@Char otilde}");
TOKEN PE57 = PodEscapeToken("E<ouml>",		"{@Char odieresis}");
TOKEN PE58 = PodEscapeToken("E<szlig>",		"{@Char germandbls}");
TOKEN PE59 = PodEscapeToken("E<thorn>",		"{@Char thorn}");
TOKEN PE60 = PodEscapeToken("E<uacute>",	"{@Char uacute}");
TOKEN PE61 = PodEscapeToken("E<ucirc>",		"{@Char ucircumflex}");
TOKEN PE62 = PodEscapeToken("E<ugrave>",	"{@Char ugrave}");
TOKEN PE63 = PodEscapeToken("E<uuml>",		"{@Char udieresis}");
TOKEN PE64 = PodEscapeToken("E<yacute>",	"{@Char yacute}");
TOKEN PE65 = PodEscapeToken("E<yuml>",		"{@Char ydieresis}");

/* Some extra Latin 1 chars that are listed in the HTML3.2 draft 1996/05/21 */
TOKEN PE66 = PodEscapeToken("E<copy>",		"{@CopyRight}");
TOKEN PE67 = PodEscapeToken("E<reg>",		"{@Register}");
TOKEN PE68 = PodEscapeToken("E<nbsp>",		"~");

/* Additional ISO-8859/1 entities listed in rfc1866 (section 14) */
TOKEN PE69 = PodEscapeToken("E<iexcl>",		"{@Char exclamdown}");
TOKEN PE70 = PodEscapeToken("E<cent>",		"{@Char cent}");
TOKEN PE71 = PodEscapeToken("E<pound>",		"{@Sterling}");
TOKEN PE72 = PodEscapeToken("E<curren>",	"{@Char currency}");
TOKEN PE73 = PodEscapeToken("E<yen>",		"{@Yen}");
TOKEN PE74 = PodEscapeToken("E<brvbar>",	"{@Char bar}");
TOKEN PE75 = PodEscapeToken("E<sect>",		"{@SectSym}");
TOKEN PE76 = PodEscapeToken("E<uml>",		"{@Char dieresis}");
TOKEN PE77 = PodEscapeToken("E<ordf>",		"{@Char ordfeminine}");
TOKEN PE78 = PodEscapeToken("E<laquo>",		"{@Char guillemotleft}");
TOKEN PE79 = PodEscapeToken("E<not>",		"{@Char logicalnot}");
TOKEN PE80 = PodEscapeToken("E<shy>",		"{@Char hyphen}");
TOKEN PE81 = PodEscapeToken("E<macr>",		"{@Char macron}");
TOKEN PE82 = PodEscapeToken("E<deg>",		"{@Char degree}");
TOKEN PE83 = PodEscapeToken("E<plusmn>",	"{@Char plusminus}");
TOKEN PE84 = PodEscapeToken("E<sup1>",		"{@Char onesuperior}");
TOKEN PE85 = PodEscapeToken("E<sup2>",		"{@Char twosuperior}");
TOKEN PE86 = PodEscapeToken("E<sup3>",		"{@Char threesuperior}");
TOKEN PE87 = PodEscapeToken("E<acute>",		"{@Char acute}");
TOKEN PE88 = PodEscapeToken("E<micro>",		"{@Char mu}");
TOKEN PE89 = PodEscapeToken("E<para>",		"{@ParSym}");
TOKEN PE90 = PodEscapeToken("E<middot>",	"{@Char periodcentered}");
TOKEN PE91 = PodEscapeToken("E<cedil>",		"{@Char cedilla}");
TOKEN PE92 = PodEscapeToken("E<ordm>",		"{@Char ordmasculine}");
TOKEN PE93 = PodEscapeToken("E<raquo>",		"{@Char guillemotright}");
TOKEN PE94 = PodEscapeToken("E<frac14>",	"{@Char onequarter}");
TOKEN PE95 = PodEscapeToken("E<frac12>",	"{@Char onehalf}");
TOKEN PE96 = PodEscapeToken("E<frac34>",	"{@Char threequarters}");
TOKEN PE97 = PodEscapeToken("E<iquest>",	"{@Char questiondown}");
TOKEN PE98 = PodEscapeToken("E<times>",		"{@Multiply}");
TOKEN PE99 = PodEscapeToken("E<divide>",	"{@Divide}");


/*****************************************************************************/
/*                                                                           */
/*  LANGUAGE - put your language declarations in this section.               */
/*                                                                           */
/*  The field names and their meanings are:                                  */
/*                                                                           */
/*     names           Set of alternative names for this languages           */
/*     setup_file      The default Lout setup file (e.g. "cprint", "eiffel") */
/*     lang_sym        The symbol for the language (e.g. "@CP", "@Eiffel")   */
/*     no_match        What to do if something fails to match (see below)    */
/*     tokens          Set of all tokens of this language                    */
/*     keywords        Set of all keywords for this language                 */
/*                                                                           */
/*  Acceptable values for no_match are:                                      */
/*                                                                           */
/*     NO_MATCH_ERROR  Generate an error message and skip the character.     */
/*                                                                           */
/*     NO_MATCH_PRINT  Print the character in a way that is Lout-safe; that  */
/*                     is, mostly raw but in quotes for "/", "@" etc., and   */
/*                     handling tabs and newlines appropriately.             */
/*                                                                           */
/*****************************************************************************/
#define	NO_MATCH_ERROR	1
#define	NO_MATCH_PRINT	2
#define NO_LANGUAGE	((LANGUAGE *) NULL)

typedef struct lang_rec {
  char *names[MAX_ARRAY_LENGTH];
  char *setup_file;
  char *lang_sym;
  int no_match;
  TOKEN *tokens[MAX_ARRAY_LENGTH];
  char *keywords[MAX_ARRAY_LENGTH];
} LANGUAGE;


LANGUAGE CLanguage = {
  { "C", "c", "C++", "c++"  },
  "cprint", "@CP",
  NO_MATCH_ERROR,
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
    "continue", "default", "delete", "do", "double", "else", "enum", "extern",
    "float", "for", "friend", "goto", "if", "inline", "int", "long", "new",
    "operator", "private", "protected", "public", "register", "return",
    "short", "signed", "sizeof", "static", "struct", "switch", "template",
    "this", "throw", "try", "typedef", "union", "unsigned", "virtual",
    "void", "volatile", "while", 
  }
};


/*****************************************************************************/
/*                                                                           */
/*  Eiffel and Blue                                                          */
/*                                                                           */
/*****************************************************************************/

LANGUAGE EiffelLanguage = {
  { "Eiffel", "eiffel" },
  "eiffel", "@Eiffel",
  NO_MATCH_ERROR,
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
    "deferred", "do", "else", "elseif", "end", "ensure", "expanded", "export",
    "external", "false", "feature", "from", "frozen", "if", "implies",
    "indexing", "infix", "inherit", "inspect", "invariant", "is", "like",
    "local", "loop", "obsolete", "old", "once", "or", "prefix", "redefine",
    "rename", "require", "rescue", "retry", "select", "separate", "strip",
    "then", "true", "undefine", "unique", "until", "variant", "when", "xor",
    "not", "interface"
  }
};

LANGUAGE BlueLanguage = {
  { "Blue", "blue" },
  "blue", "@Blue",
  NO_MATCH_ERROR,
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
  { "and", "assert", "builtin", "case", "class", "const", "create", "creation",
    "deferred", "div", "do", "else", "elseif", "end", "Enumeration",
    "enumeration", "exit", "if", "in", "interface", "internal", "invariant",
    "is", "loop", "manifest", "mod", "not", "of", "old", "on", "or", "post",
    "pre", "redefined", "return", "routines", "super", "then", "uses", "var"
  }
};


/*****************************************************************************/
/*                                                                           */
/*  Perl and Pod                                                             */
/*                                                                           */
/*****************************************************************************/

LANGUAGE PerlLanguage = {
  { "Perl", "perl", },
  "perl", "@Perl",
  NO_MATCH_ERROR,
  {
    &PerlSingleQuoteStringToken, &PerlDoubleQuoteStringToken,
    &PerlBackQuoteStringToken, &PerlQTypeStringToken, &PerlSTypeStringToken,
    &PerlRegExpLPar, &PerlRegExpEq, &PerlRegExpMatch, &PerlRegExpNoMatch,
    &PerlRegExpSplit, &PerlRegExpIf, &PerlRegExpNot, &PerlRegExpUnless,
    &PerlRegExpFor, &PerlRegExpForEach, &PerlRegExpWhile,
    &PerlRegExpStartLineToken,
    &HereEOTuq, &HereEOTdq, &HereEOTfq, &HereEOTbq,
    &HereEOFuq, &HereEOFdq, &HereEOFfq, &HereEOFbq,
    &HereENDuq, &HereENDdq, &HereENDfq, &HereENDbq,
    &HereBLAuq, &HereBLAdq, &HereBLAfq, &HereBLAbq,
    &PerlIdentifierToken, &PerlSpecialIdentifierToken,
    &PerlLiteralNumberToken, &PerlHexNumberToken,
    &PerlCommentToken, &PerlCommentEscapeToken, &PerlPodToken,
    &ExclamationToken, &PercentToken, &HatToken, &AmpersandToken, 
    &StarToken, &SlashToken, &ArrowToken, &BackSlashToken, 
    &LeftParenToken, &RightParenToken, &MinusToken, &PlusToken, 
    &LeftBraceToken, &RightBraceToken, &BarToken, &CircumToken, 
    &LeftBracketToken, &RightBracketToken, &SemicolonToken, &ColonToken, 
    &LessToken, &GreaterToken, &QuestionToken, &CommaToken, &DotToken, 
    &LessEqualToken, &GreaterEqualToken, &CNotEqualToken, 
    &PerlIncrementToken, &PerlDecrementToken, &PerlExponentiateToken, 
    &PerlMatchToken, &PerlNotMatchToken,
    &PerlEqualToken, &PerlAssignToken, &PerlBitLeftShiftToken, 
    &PerlBitRightShiftToken, &PerlSpaceshipToken, 
    &PerlAndToken, &PerlOrToken, &PerlRange2Token, &PerlRange3Token, 
    &PerlFileTestrToken, &PerlFileTestwToken, &PerlFileTestxToken, 
    &PerlFileTestoToken, &PerlFileTestRToken, &PerlFileTestWToken, 
    &PerlFileTestXToken, &PerlFileTestOToken, &PerlFileTesteToken, 
    &PerlFileTestzToken, &PerlFileTestsToken, &PerlFileTestfToken, 
    &PerlFileTestdToken, &PerlFileTestlToken, &PerlFileTestpToken, 
    &PerlFileTestSToken, &PerlFileTestbToken, &PerlFileTestcToken, 
    &PerlFileTesttToken, &PerlFileTestuToken, &PerlFileTestgToken, 
    &PerlFileTestkToken, &PerlFileTestTToken, &PerlFileTestBToken, 
    &PerlFileTestMToken, &PerlFileTestAToken, &PerlFileTestCToken, 
  },

  { 
    /* Built-ins taken from Programming Perl 2nd Ed. */
    "abs", "accept", "alarm", "atan2", 
    "bind", "binmode", "bless", 
    "caller", "chdir", "chmod", "chomp", "chop", "chown", "chr", "chroot", 
    "close", "closedir", "connect", "continue", "cos", "crypt", 
    "dbmclose", "dbmopen", "defined", "delete", "die", "do", "dump", 
    "each", "endgrent", "endhostent", "endnetent", "endprotoent", 
    "endpwent", "endservent", "eof", "eval", "exec", "exists", "exit", 
    "exp", 
    "fcntl", "fileno", "flock", "fork", "format", "formline", 
    "getc", "getgrent", "getgrgid", "getgrnam", "gethostbyaddr", 
    "gethostbyname", "gethostent", "getlogin", "getnetbyaddr", 
    "getnetbyname", "getnetent", "getpeername", "getpgrp", "getppid", 
    "getpriority", "getprotobyname", "getprotobynumber", "getprotoent", 
    "getpwent", "getpwnam", "getpwuid", "getservbyname", "getservbyport", 
    "getservent", "getsockname", "getsockopt", "glob", "gmtime", "goto", 
    "grep", 
    "hex", 
    "import", "index", "int", "ioctl", 
    "join", 
    "keys", "kill", 
    "last", "lc", "lcfirst", "length", "link", "listen", "local", 
    "localtime", "log", "lstat", 
    "map", "mkdir", "msgctl", "msgget", "msgrcv", "msgsnd", "my", 
    "next", "no", 
    "oct", "open", "opendir", "ord", 
    "pack", "package", "pipe", "pop", "pos", "print", "printf", "push", 
    "quotemeta", 
    "rand", "read", "readdir", "readlink", "recv", "redo", "ref", 
    "rename", "require", "reset", "return", "reverse", "rewinddir", 
    "rindex", "rmdir", 
    "scalar", "seek", "seekdir", "select", "semctl", "semget", "semop", 
    "send", "setgrent", "sethostent", "setnetent", "setpgrp", 
    "setpriority", "setprotoent", "setpwent", "setservent", 
    "setsockopt", "shift", "shmctl", "shmget", "shmread", "shmwrite", 
    "shutdown", "sin", "sleep", "socket", "socketpair", "sort", 
    "splice", "split", "sprintf", "sqrt", "srand", "stat", "study", 
    "sub", "substr", "symlink", "syscall", "sysopen", "sysread", "sysseek",
    "system", "syswrite", 
    "tell", "telldir", "tie", "tied", "time", "times", "truncate", 
    "uc", "ucfirst", "umask", "undef", "unlink", "unpack", "unshift", 
    "untie", "use", "utime", 
    "values", "vec", 
    "wait", "waitpid", "wantarray", "warn", "write", 

    /* from later versions of Perl (excluding experimental thread stuff) */ 
    "our", "prototype", "readline", "readpipe",

    /* operators */
    "lt", "gt", "eq", "ne", "cmp", "le", "ge",

    /* Perl special variables                                              */
    /* NB special variables that begin $^, e.g. $^D can also be written as */
    /* dollar control D (yes literal control D) -- but suggest we ignore   */
    "$_", "$ARG", 
    "$.", "$INPUT_LINE_SEPARATOR", "$NR", 
    "$/", "$INPUT_RECORD_SEPARATOR", "$RS", 
    "$,", "$OUTPUT_FIELD_SEPARATOR", "$OFS", 
    "$\\", "$OUTPUT_RECORD_SEPARATOR", "$ORS", 
    "$\"", "$LIST_SEPARATOR", 
    "$;", "$SUBSCRIPT_SEPARATOR", "$SUBSEP", 
    "$^L", "$FORMAT_FORMFEED", 
    "$:", "$FORMAT_LINE_BREAK_CHARACTERS", 
    "$^A", "$ACCUMULATOR", 
    "$#", "$OFMT", 
    "$?", "$CHILD_ERROR", 
    "$!", "$OS_ERROR", "$ERRNO", 
    "$@", "$EVAL_ERROR", 
    "$$", "$PROCESS_ID", "$PID", 
    "$<", "$REAL_USER_ID", "$UID", 
    "$>", "$EFFECTIVE_USER_ID", "$EUID", 
    "$(", "$REAL_GROUP_ID", "$GID", 
    "$)", "$EFFECTIVE_GROUP_ID", "$EGID", 
    "$0", /* dollar zero */ "$PROGRAM_NAME", 
    "$[", /* deprecated */ 
    "$]", "$PERL_VERSION", 
    "$^D", "$DEBUGGING", 
    "$^E", "$EXTENDED_OS_ERROR", 
    "$^F", "$SYSTEM_FD_MAX", 
    "$^H", 
    "$^I", "$INPLACE_EDIT", 
    "$^M", 
    "$^O", "$OSNAME", 
    "$^P", "$PERLDB", 
    "$^T", "$BASETIME", 
    "$^W", "$WARNING", 
    "$^X", "$EXECUTABLE_NAME", 
    "$ARGV", 
    "@ARGV", "@INC", "@F", "%INC", "%ENV", "%SIG",
    "ARGV", "STDERR", "STDIN", "STDOUT", "DATA"
    "$&", "$MATCH",
    "$`", "$PREMATCH",
    "$'", "$POSTMATCH",
    "$+", "$LAST_PAREN_MATCH",
    "$|", "$OUTPUT_AUTOFLUSH",
    "$%", "$FORMAT_PAGE_NUMBER",
    "$=", "$FORMAT_LINES_PER_PAGE",
    "$-", "$FORMAT_LINES_LEFT",
    "$~", "$FORMAT_NAME",
    "$^", "$FORMAT_TOP_NAME",

    /* Perl special constants */
    "__END__", "__FILE__", "__LINE__", "__PACKAGE__",

    /* Perl regex variables - first 9 only */
    "$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8", "$9",

    /* Perl pragmas */
    "attrs", "autouse", "base", "blib", "constant", "diagnostics", "fields",
    "integer", "less", "lib", "locale", "ops", "overload", "re", "sigtrap",
    "strict", "subs", "vars",

    /* Perl textual operators */
    "and", "or", "xor", "not", 

    /* the x keyword */
    "x",

    /* Perl control structures */
    "if", "elsif", /* yes one e */ "else", "unless", 
    "while", "for", "foreach", "continue", "until",
  }
};


LANGUAGE PodLanguage = {
  { "Pod", "pod", "POD" },
  "pod", "@Pod",
  NO_MATCH_PRINT,
  {
    &PodVerbatimLineToken, &PodEmptyLineToken, &PodIgnoreToken,
    &PodHeading1Token, &PodHeading2Token,
    &PodOverToken, &PodItemToken, &PodBackToken,
    &PodForToken, &PodBeginToken, &PodBeginLoutToken,
    &PodItalicToken, &PodBoldToken, &PodCodeToken, &PodFileToken,
    &PodNoBreakToken, &PodLinkToken, &PodIndexToken, &PodZeroToken,
    &PodLessThanToken, &PodGreaterThanToken, &PodSlashToken,
    &PodVerbarToken,
    &PE00, /* &PE01, &PE02, */ &PE03, &PE04, &PE05, &PE06, &PE07, &PE08, &PE09,
    &PE10, &PE11, &PE12, &PE13, &PE14, &PE15, &PE16, &PE17, &PE18, &PE19,
    &PE20, &PE21, &PE22, &PE23, &PE24, &PE25, &PE26, &PE27, &PE28, &PE29,
    &PE30, &PE31, &PE32, &PE33, &PE34, &PE35, &PE36, &PE37, &PE38, &PE39,
    &PE40, &PE41, &PE42, &PE43, &PE44, &PE45, &PE46, &PE47, &PE48, &PE49,
    &PE50, &PE51, &PE52, &PE53, &PE54, &PE55, &PE56, &PE57, &PE58, &PE59,
    &PE60, &PE61, &PE62, &PE63, &PE64, &PE65, &PE66, &PE67, &PE68, &PE69,
    &PE70, &PE71, &PE72, &PE73, &PE74, &PE75, &PE76, &PE77, &PE78, &PE79,
    &PE80, &PE81, &PE82, &PE83, &PE84, &PE85, &PE86, &PE87, &PE88, &PE89,
    &PE90, &PE91, &PE92, &PE93, &PE94, &PE95, &PE96, &PE97, &PE98, &PE99,
    &PodNumCharToken,
  },

  { NULL },
};


/*****************************************************************************/
/*                                                                           */
/*  The "languages" variable - add your language to this list                */
/*                                                                           */
/*****************************************************************************/

LANGUAGE *languages[] = {
  & CLanguage,
  & EiffelLanguage,
  & BlueLanguage,
  & PerlLanguage,
  & PodLanguage,
};





/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
/***                                                                       ***/
/***  If you are adding a new language, you don't need to change anything  ***/
/***  below this point.  Just repeating: don't change anything below here. ***/
/***                                                                       ***/
/*****************************************************************************/
/*****************************************************************************/
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

#define PRG2LOUT_VERSION "prg2lout Version 2.0 (April 2000)"
#define	MAX_LINE	1024

static char	file_name[MAX_LINE];	/* current input file name           */
static char	curr_line[MAX_LINE];	/* current input line                */
static int	line_num;		/* current input line number         */
static int	line_pos;		/* current input column number       */
static BOOLEAN	raw_seen;		/* TRUE if -r (raw mode)             */

static BOOLEAN	headers_option;		/* TRUE if no -n option (headers)    */
static char	*style_option;		/* value of -p option, else null     */
static char	*font_option;		/* value of -f option, else null     */
static char	*size_option;		/* value of -s option, else null     */
static char	*line_option;		/* value of -v option, else null     */
static char	*tabin_option;		/* value of -t option, else null     */
static char	*tabout_option;		/* value of -T option, else null     */
static char	*setup_option;		/* value of -S option, else null     */
static char	*language_option;	/* value of -l option, else null     */

static BOOLEAN	tab_by_spacing;		/* TRUE if using space chars to tab  */
static int	tab_in;			/* tab interval, value of -t option  */
static float	tab_out;		/* tab interval width (-T option)    */
static char 	tab_unit;		/* unit of measurement for tab       */

static FILE	*in_fp;			/* where input comes from	     */
static FILE	*out_fp;		/* where output goes		     */
static FILE	*err_fp;		/* where error messages go 	     */


/*****************************************************************************/
/*                                                                           */
/*  char *ErrorHeader()                                                      */
/*                                                                           */
/*  Returns a string showing the current file, line, and column.             */
/*                                                                           */
/*****************************************************************************/

char *ErrorHeader()
{ static char buff[MAX_LINE];
  if( line_num == 0 || line_pos == 0 )
    sprintf(buff, "prg2lout");
  else if( raw_seen )
    sprintf(buff, "prg2lout %d,%d", line_num, line_pos);
  else
    sprintf(buff, "prg2lout %s %d,%d", file_name, line_num, line_pos);
  return buff;
}


/*****************************************************************************/
/*                                                                           */
/*  GetArg(arg, message, null_ok)                                            */
/*                                                                           */
/*  Get the next command line argument's value into arg.  If there isn't     */
/*  one, print an error message and quit unless null_ok is true.             */
/*                                                                           */
/*****************************************************************************/

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
/*  NextChar()                                                               */
/*                                                                           */
/*  Move to next character in the input file.  This may involve changing     */
/*  global variables curr_line, line_num, and line_pos; the new character    */
/*  may be found in curr_line[line_pos].                                     */
/*                                                                           */
/*  NextChar does not skip any characters at all.  When end of file is       */
/*  reached, curr_line[line_pos] contains '\0'.                              */
/*                                                                           */
/*  It is possible for code to read ahead of curr_line[line_pos] up to and   */
/*  including the newline character at the end of the line after the line    */
/*  we are currently on (thus it is possible to recognize an empty line as   */
/*  \n\n), but not beyond, using curr_line[line_pos + i] for i > 0.          */
/*                                                                           */
/*****************************************************************************/

void NextChar()
{
  if( curr_line[line_pos] != '\n' )
  {
    /* we can carry on with the current line.  This will yield '\0' as */
    /* desired if EOF arrives before the end of the line               */
    line_pos++;
  }
  else if( curr_line[line_pos+1] != '\0' )
  {
    /* we've already read in the next line; it's at &curr_line[line_pos+1] */
    strcpy(&curr_line[1], &curr_line[line_pos+1]);
    line_num++;
    line_pos = 1;
  }
  else
  {
    /* we need to read in the new line */
    line_num++;
    line_pos = 1;
    if( fgets(&curr_line[1], MAX_LINE+2, in_fp) == (char *) NULL )
      curr_line[1] = '\0';
  }
  if( DEBUG_NEXTCHAR )
    fprintf(stderr, "after NextChar, line_num %d, line_pos %d, curr_line %s",
      line_num, line_pos, &curr_line[1]);
} /* end NextChar */


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN InputMatches(char *pattern)                                      */
/*                                                                           */
/*  Returns TRUE if input starting at curr_line[line_pos] matches pattern.   */
/*  To check this we may have to read an extra line or more of input.        */
/*                                                                           */
/*****************************************************************************/

BOOLEAN InputMatches(char *pattern)
{ char *p, *q;
  for(p = &curr_line[line_pos], q = pattern;  *q != '\0';  p++, q++ )
  {
    if( *p == '\0' )
    {
      /* attempt to read another line of input, since we are off the end */
      if( fgets(p, MAX_LINE+2-(p - curr_line), in_fp) == (char *) NULL )
      *p = '\0';
    }
    if( *p != *q )
      break;
  }
  if( DEBUG_PREFIXEQ )
    fprintf(stderr, "InputMatches(%s, %s) returning %s\n",
      &curr_line[line_pos], pattern, *q == '\0' ? "TRUE" : "FALSE");
  return (*q == '\0');
} /* end InputMatches */


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
  { if( *T == (TRIE) NULL )
      *T = (TRIE) calloc(1, sizeof(struct trie_node));  /* will set all to 0 */
    if( *(str + 1) != '\0' )
      res = TrieInsert(&((*T)->sub[(int) *str]), str + 1, val);
    else if( (*T)->value[(int) *str] != (TOKEN *) NULL )
      res = FALSE;
    else
    { (*T)->value[(int) *str] = val;
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
  { if( DEBUG_TRIE )
      fprintf(stderr, "  i = %d, res = %s\n", i, EchoToken(res));
    if( T->value[(int) str[i]] != (TOKEN *) NULL )
    { res = T->value[(int) str[i]];
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
#define MAX_SYM 609

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

void HashInsert(char *str)
{ int i;
  if( DEBUG_SETUP )
    fprintf(stderr, "[ HashInsert(%s)\n", str);
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
    fprintf(stderr, "] HashInsert(%s)\n", str);
}

BOOLEAN HashRetrieve(char *str)
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
/*  This is the code that actually prints the output file.                   */
/*  To emit one token, the call sequence should be as follows:               */
/*                                                                           */
/*     StartEmit(LANGUAGE *lang, TOKEN *current_token, char *start_delim, l) */
/*     Emit(TOKEN *current_token, char ch)                                   */
/*     ...                                                                   */
/*     Emit(TOKEN *current_token, char ch)                                   */
/*     EndEmit(TOKEN *current_token, char *end_delim)                        */
/*                                                                           */
/*  The back end will then take care of all print styles automatically,      */
/*  including checking for keywords.  When emitting white space each space   */
/*  can be sent directly:                                                    */
/*                                                                           */
/*     EmitRaw(ch)                                                           */
/*                                                                           */
/*****************************************************************************/

static char	save_value[MAX_LINE];		/* the token text            */
static int	save_len;                       /* index of \0 in save_value */
static BOOLEAN	save_on = FALSE;		/* TRUE when saving          */
static LANGUAGE	*save_language;			/* the current language      */


/*****************************************************************************/
/*                                                                           */
/*  EmitRaw(ch)                                                              */
/*                                                                           */
/*  Emit this character immediately.  This is only legal when not saving.    */
/*  All characters printed on the output file that represent actual text     */
/*  of the program (i.e. not commands, {}, "", \ in strings etc.) should     */
/*  pass through here, since EmitRaw keeps track of where we are on          */
/*  the output line, in order to handle tab characters correctly.            */
/*                                                                           */
/*  NB out_linepos is the column where the *next* character will go, and     */
/*  it counts the first column on the line as column zero.  It understands   */
/*  that a tab character always produces at least one space, and that the    */
/*  character after a tab goes in a column whose number mod tab_in is zero.  */
/*                                                                           */
/*****************************************************************************/

void EmitRaw(char ch)
{ static int	 out_linepos = 0;		/* output line position      */
  static BOOLEAN out_linestart = TRUE;		/* TRUE if out line start    */

  if( DEBUG_EMIT )
    fprintf(stderr, "EmitRaw(%c); out_linepos %d, out_linestart %s\n",
      ch, out_linepos, out_linestart ? "TRUE" : "FALSE");
  if( save_on )
  { fprintf(err_fp, "%s internal error (EmitRaw save_on)\n", ErrorHeader());
    abort();
  }
  if( ch == '\t' )
  { if( tab_by_spacing )
    { putc(' ', out_fp);
      out_linepos++;
      while( out_linepos % tab_in != 0 )
      { putc(' ', out_fp);
	out_linepos++;
      }
    }
    else
    { out_linepos++;
      while( out_linepos % tab_in != 0 )  out_linepos++;
      if( out_linestart )
      { fprintf(out_fp, "$>\"%.1f%c\" {}", tab_out, tab_unit);
	/* NB {} is required in case nothing follows on this line */
      }
      else
        fprintf(out_fp, "$>\"%.1f%ct\" {}", (out_linepos/tab_in)*tab_out, tab_unit);
    }
  }
  else if( ch == '\n' )
  { fputc(ch, out_fp);
    out_linepos = 0;
    out_linestart = TRUE;
  }
  else
  { fputc(ch, out_fp);
    out_linepos++;
    if( ch != ' ' )  out_linestart = FALSE;
  }
  if( DEBUG_EMIT )
    fprintf(stderr, "EmitRaw(%c) returning; out_linepos %d, out_linestart %s\n",
      ch, out_linepos, out_linestart ? "TRUE" : "FALSE");
} /* end EmitRaw */


/*****************************************************************************/
/*                                                                           */
/*  StartEmit(LANGUAGE *lang, TOKEN *current_token, char *start_delim, len)  */
/*                                                                           */
/*  Start the emission of a token.  If it is a PRINT_WHOLE_QUOTED, it has    */
/*  to be saved since it might be a keyword.                                 */
/*                                                                           */
/*  The token began with the starting delimiter start_delim[0..len-1].       */
/*                                                                           */
/*****************************************************************************/

void StartEmit(LANGUAGE *lang, TOKEN *current_token, char *start_delim, int len)
{ int i;
  if( save_on )
  {
    fprintf(err_fp, "%s internal error (StartEmit)\n", ErrorHeader());
    abort();
  }
  save_language = lang;
  switch( current_token->print_style )
  {
    case PRINT_WHOLE_QUOTED:

      /* start_delim is to be printed, but must be saved up */
      save_on = TRUE;
      for( i = 0;  i < len;  i++ )
	save_value[i] = start_delim[i];
      save_len = len;
      save_value[save_len] = '\0';
      break;


    case PRINT_NODELIMS_QUOTED:

      /* easiest to save this, treat like PRINT_WHOLE_QUOTED, but no delims */
      save_on = TRUE;
      save_len = 0;
      save_value[save_len] = '\0';
      break;


    case PRINT_WHOLE_UNQUOTED:

      /* print command */
      if( current_token->command[0] != '\0' )
	fprintf(out_fp, "%s{", current_token->command); /*}*/

      /* print opening delimiter, verbatim */
      for( i = 0;  i < len;  i++ )
	putc(start_delim[i], out_fp);
      break;


    case PRINT_NODELIMS_UNQUOTED:

      /* command is printed but not delimiter */
      if( current_token->command[0] != '\0' )
	fprintf(out_fp, "%s{", current_token->command); /*}*/
      break;


    case PRINT_NODELIMS_INNER:

      /* command is printed but not delimiter; always print opening brace */
      fprintf(out_fp, "%s{", current_token->command); /*}*/
      break;


    case PRINT_COMMAND_ONLY:

      /* command is printed but nothing else */
      fprintf(out_fp, "%s", current_token->command);
      break;


    default:

      fprintf(err_fp, "%s internal error (print_style)\n", ErrorHeader());
      abort();
      break;
  }
} /* end StartEmit */


/*****************************************************************************/
/*                                                                           */
/*  EndEmit(TOKEN *current_token, char *end_delim)                           */
/*                                                                           */
/*  End emitting the current token.  Its ending delimiter was end_delim.     */
/*                                                                           */
/*****************************************************************************/
#define is_whitespace(ch) ((ch)==' ' || (ch)=='\t' || (ch)=='\n' || (ch)=='\f')

void EndEmit(TOKEN *current_token, char *end_delim)
{ char *com;  int i;  BOOLEAN quoted_now = FALSE;
  switch( current_token->print_style )
  {

    case PRINT_WHOLE_QUOTED:

      strcpy(&save_value[save_len], end_delim);
      save_len += strlen(end_delim);
      /* NB NO BREAK */


    case PRINT_NODELIMS_QUOTED:

      /* work out whether we are printing the command or its alternative */
      com = (current_token->alternate_command[0]!='\0' && HashRetrieve(save_value)?
	current_token->alternate_command : current_token->command);

      /* print command, opening brace */
      if( com[0] != '\0' )  fprintf(out_fp, "%s{", com);  /* } */

      /* omit trailing white space (will not be significant to Lout anyway) */
      /* *** not doing this now 
      while( save_len>0 && is_whitespace(save_value[save_len-1]) )  save_len--;
      *** */

      /* print the token with appropriate escapes */
      save_on = FALSE;
      for( i = 0;  i < save_len;  i++ ) switch( save_value[i] )
      {
	case '/':
	case '|':
	case '&':
	case '#':
	case '{':
	case '}':
	case '^':
	case '~':
	case '-':

	  if( !quoted_now ) { putc('"', out_fp); quoted_now = TRUE; }
	  EmitRaw(save_value[i]);
	  break;


	case '"':
	case '\\':

	  if( !quoted_now ) { putc('"', out_fp); quoted_now = TRUE; }
	  putc('\\', out_fp);
	  EmitRaw(save_value[i]);
	  break;


	case ' ':
	case '\t':
	case '\n':
	case '\f':

	  if( i == 0 )
	  { /* make initial white space significant using "" */
	    putc('"', out_fp);
	    quoted_now = TRUE;
	  }
	  if( quoted_now ) { putc('"', out_fp); quoted_now = FALSE; }
	  EmitRaw(save_value[i]);
	  break;


	default:

	  /* anything else can be quoted or unquoted ad. lib. */
	  EmitRaw(save_value[i]);
	  break;
      }
      /* print closing quote and closing brace if needed */
      if( quoted_now )  putc('"', out_fp);
      else if( save_len > 0 && is_whitespace(save_value[save_len-1]) )
	fputs("\"\"", out_fp);  /* makes trailing white space significant */
      if( com[0] != '\0' )  /*{*/ putc('}', out_fp);
      break;


    case PRINT_WHOLE_UNQUOTED:

      /* print end delimiter, verbatim */
      fputs(end_delim, out_fp);
      /* NB NO BREAK */


    case PRINT_NODELIMS_UNQUOTED:

      /* print closing brace if required*/
      if( current_token->command[0] != '\0' ) /*{*/ putc('}', out_fp);
      break;


    case PRINT_NODELIMS_INNER:

      /* always print closing brace */
      /*{*/ putc('}', out_fp);
      break;


    case PRINT_COMMAND_ONLY:

      break;


    default:

      fprintf(err_fp, "%s internal error (print_style)\n", ErrorHeader());
      abort();
      break;
  }

  /* print following command if any */
  if( current_token->following_command != NULL )
    fprintf(out_fp, "%s", current_token->following_command);
} /* end EndEmit */


/*****************************************************************************/
/*                                                                           */
/*  Emit(TOKEN *current_token, char ch)                                      */
/*                                                                           */
/*  Emit one character of the current token.                                 */
/*                                                                           */
/*****************************************************************************/

void Emit(TOKEN *current_token, char ch)
{
  switch( current_token->print_style )
  {
    case PRINT_WHOLE_QUOTED:
    case PRINT_NODELIMS_QUOTED:

      if( !save_on )
      { fprintf(err_fp, "%s internal error (EmitChar)\n", ErrorHeader());
	abort();
      }
      save_value[save_len++] = ch;
      save_value[save_len] = '\0';
      break;


    case PRINT_WHOLE_UNQUOTED:
    case PRINT_NODELIMS_UNQUOTED:

      /* verbatim output */
      putc(ch, out_fp);
      break;


    case PRINT_NODELIMS_INNER:

      fprintf(err_fp, "%s internal error (emitting INNER)\n", ErrorHeader());
      abort();
      break;


    case PRINT_COMMAND_ONLY:

      /* emit nothing since printing the command only */
      break;


    default:

      fprintf(err_fp, "%s internal error (print_style)\n", ErrorHeader());
      abort();
      break;

  }
} /* end Emit */


/*****************************************************************************/
/*                                                                           */
/*  EmitProtected(char ch)                                                   */
/*                                                                           */
/*  Emit one character of the current token.  If the character is a special  */
/*  one in Lout, protect it with quotes.                                     */
/*                                                                           */
/*****************************************************************************/

void EmitProtected(char ch)
{
  switch( ch )
  {
    case '/':
    case '|':
    case '&':
    case '#':
    case '{':
    case '}':
    case '^':
    case '~':
    case '-':

      putc('"', out_fp);
      EmitRaw(ch);
      putc('"', out_fp);
      break;


    case '"':
    case '\\':

      putc('"', out_fp);
      putc('\\', out_fp);
      EmitRaw(ch);
      putc('"', out_fp);
      break;


    default:

      EmitRaw(ch);
      break;
  }
} /* end EmitProtected */


/*****************************************************************************/
/*                                                                           */
/*  TOKEN *ExpandToken(TOKEN *t, int starts_pos)                             */
/*                                                                           */
/*  Create a new token corresponding to t but using starts2[starts_pos] and  */
/*  ends2[starts_pos] only.                                                  */
/*                                                                           */
/*****************************************************************************/

char *clone2strings(char *s1, char *s2)
{ char *res;
  res = (char *) malloc( (strlen(s1) + strlen(s2) + 1) * sizeof(char));
  sprintf(res, "%s%s", s1, s2);
  if( DEBUG_SETUP )
    fprintf(stderr, "clone2strings(%s, %s) = %s\n", s1, s2, res);
  return res;
} /* end clone2strings */

TOKEN *ExpandToken(TOKEN *t, int starts_pos)
{ TOKEN *res; int i;
  if( DEBUG_SETUP )
    fprintf(stderr, "ExpandToken(%s, starts[0] = %s)\n", t->name, t->starts[0]);
  res = (TOKEN *) calloc(1, sizeof(struct token_rec));
  res->name = t->name;
  res->print_style = t->print_style;
  res->command = t->command;
  res->alternate_command = t->alternate_command;
  res->following_command = t->following_command;
  res->start_line_only = t->start_line_only;
  for( i = 0;  t->starts[i] != NULL;  i++ )
  {
    /* the starts of res are the start of t with starts2[starts_pos] added */
    res->starts[i] = clone2strings(t->starts[i], t->starts2[starts_pos]);
  }
  res->legal = t->legal;
  res->escape = t->escape;
  res->escape_legal = t->escape_legal;
  res->inner_escape = t->inner_escape;
  res->end_inner_escape = t->end_inner_escape;
  res->end_delimiter = t->ends2[starts_pos];
  res->end_start_line_only = t->end_start_line_only;
  res->want_two_ends = t->want_two_ends;
  if( DEBUG_SETUP )
    fprintf(stderr, "ExpandToken returning res = %s, starts[0] = %s)\n",
      res->name, res->starts[0]);
  return res;
} /* end ExpandToken */


/*****************************************************************************/
/*                                                                           */
/*  void SetupOneToken(TOKEN *t)                                             */
/*                                                                           */
/*  Set up one token.  This involves initializing the chtype and             */
/*  escape_chtype fields for the token, and loading the trie with all        */
/*  the opening delimiters of the token.                                     */
/*                                                                           */
/*****************************************************************************/
#define LEGAL		1
#define ESCAPE		2
#define INNER_ESCAPE	3
TRIE Trie = (TRIE) NULL;		/* these tokens allowed anywhere     */
TRIE StartLineTrie = (TRIE) NULL;	/* these allowed at line start only  */

void SetupOneToken(TOKEN *t)
{ int j;
  if( DEBUG_SETUP )
    fprintf(stderr, "SetupOneToken starting %s\n", t->starts[0]);

  /* check that any PRINT_NODELIMS_INNER styles have an end delimiter */
  if( t->print_style == PRINT_NODELIMS_INNER )
  {
    if( t->end_delimiter == NULL || t->end_delimiter[0] == '\0' )
    {
      fprintf(err_fp, "%s: token %s is INNER but has no end delimiter\n",
	t->name, ErrorHeader());
    }
  }

  /* set up the chtype table for this token */
  if( t->legal == NULL )
  { /* all characters are legal */
    for( j = 0;  j < MAX_CHAR;  j++ )
      t->chtype[j] = LEGAL;
  }
  else
  { /* the characters in t->legal are legal */
    for( j = 0;  t->legal[j] != '\0'; j++ )
      t->chtype[(int) t->legal[j]] = LEGAL;
  }
  if( t->escape[0] != '\0' )
    t->chtype[(int) t->escape[0]] = ESCAPE;
  if( t->inner_escape[0] != '\0' )
    t->chtype[(int) t->inner_escape[0]] = INNER_ESCAPE;

  /* set up the escape_chtype table for this token */
  if( t->escape_legal == NULL )
  { /* all characters are legal after an escape character */
    for( j = 0;  j < MAX_CHAR;  j++ )
      t->escape_chtype[j] = LEGAL;
  }
  else
  { /* the characters in t->escape_legal are legal after an escape character */
    for( j = 0;  t->escape_legal[j] != '\0';  j++ )
      t->escape_chtype[(int) t->escape_legal[j]] = LEGAL;
  }

  /* load the opening delimiters of this token into the trie */
  for( j = 0;  t->starts[j] != (char *) NULL;  j++ )
  {
    if( !TrieInsert(t->start_line_only ? &StartLineTrie:&Trie,t->starts[j],t) )
    { if( *(t->starts[j]) == '\0' )
	fprintf(err_fp, "%s: empty starting delimiter\n", ErrorHeader());
      else
	fprintf(err_fp, "%s: starting delimiter %s appears twice\n",
	  ErrorHeader(), t->starts[j]);
    }
  }

  if( DEBUG_SETUP )
    fprintf(stderr, "SetupOneToken ending %s\n", t->starts[0]);
} /* end SetupOneToken */


/*****************************************************************************/
/*                                                                           */
/*  SetupLanguage(LANGUAGE *lang)                                            */
/*                                                                           */
/*  Set up the runtime token structures.  This involves setting up each      */
/*  token (see above), and also loading the hash table with the keywords.    */
/*                                                                           */
/*  If a token has non-empty start2 and end2 pairs, it is expanded into      */
/*  a set of tokens, one for each pair, with the start delimiter set to      */
/*  the concatenation of the start delimiters and starts2, and end           */
/*  delimiter set to the corresponding end2.                                 */
/*                                                                           */
/*****************************************************************************/

void SetupLanguage(LANGUAGE *lang)
{ int i, j; TOKEN *t;
  if( DEBUG_SETUP )
    fprintf(stderr, "SetupLanguage(%s)\n", lang->names[0]);

  /* set up each token in the language */
  for( i = 0;  lang->tokens[i] != (TOKEN *) NULL; i++ )
  {
    if( DEBUG_SETUP )
      fprintf(stderr, "  (1) setting up token %s (starts[0] = %s)\n",
	lang->tokens[i]->name, lang->tokens[i]->starts[0]);
    if( lang->tokens[i]->starts2[0] != NULL )
    {
      /* starts2, so set up one token for each entry in starts[2] */
      for( j = 0;  lang->tokens[i]->starts2[j] != NULL;  j++ )
      {
	t = ExpandToken(lang->tokens[i], j);
	if( DEBUG_SETUP )
	  fprintf(stderr, "  (2) setting up token %s (starts[0] = %s)\n",
	    t->name, t->starts[0]);
	SetupOneToken(t);
      }
    }
    else
    {
      /* no starts2, so set up just one token */
      SetupOneToken(lang->tokens[i]);
    }
  }

  /* load the keyword hash table */
  for( j = 0;  lang->keywords[j] != (char *) NULL;  j++ )
    HashInsert(lang->keywords[j]);

  if( DEBUG_SETUP )
    fprintf(stderr, "SetupLanguage(%s) returning.\n", lang->names[0]);
} /* end SetupLanguage */


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
/*  TOKEN *TokenStartingHere(int *len)                                       */
/*                                                                           */
/*  Returns the token starting here if there is one, else NULL.              */
/*  If found, the length of its starting delimiter is returned in *len.      */
/*                                                                           */
/*****************************************************************************/

TOKEN *TokenStartingHere(int *len)
{ TOKEN *res;

  if( line_pos == 1 )
  {
    res = TrieRetrieve(StartLineTrie, &curr_line[line_pos], len);
    if( res == (TOKEN *) NULL )
      res = TrieRetrieve(Trie, &curr_line[line_pos], len);
  }
  else
  {
    res = TrieRetrieve(Trie, &curr_line[line_pos], len);
  }
  return res;
}


/*****************************************************************************/
/*                                                                           */
/*  Process(LANGUAGE *lang, TOKEN *outer_token, char *outer_end_delimiter)   */
/*                                                                           */
/*  Process a sequence of input tokens.  If we are currently recursing       */
/*  inside some other token, outer_token is non-null and is that token,      */
/*  and we stop when we reach outer_end_delimiter outside any token.         */
/*  Otherwise we stop at end of file.                                        */
/*                                                                           */
/*****************************************************************************/
#define	START				1
#define	IN_TOKEN			2
#define	IN_TOKEN_AFTER_ESCAPE		3
#define	IN_TOKEN_AFTER_INNER_ESCAPE	4
#define	STOP				5

void Process(LANGUAGE *lang, TOKEN *outer_token, char *outer_end_delimiter)
{ TOKEN *current_token; int len, i, state;
  BOOLEAN end_delimiter_seen;
  if( DEBUG_PROCESS )
    fprintf(stderr, "[ Process(%s, -, -, -, -)\n", lang->names[0]);

  state = START;
  end_delimiter_seen = FALSE;
  while( curr_line[line_pos] != '\0' && state != STOP )
  { 
    if( DEBUG_PROCESS )
      fprintf(stderr, "  state %d, ch %c\n", state, curr_line[line_pos]);
    switch( state )
    {

      case START:  /* between tokens; try each of the following */

	/* check whether outer_token is ending here, in which case stop */
	if( outer_token != (TOKEN *) NULL &&
	    curr_line[line_pos] == outer_end_delimiter[0] &&
	    InputMatches(outer_end_delimiter) )
	{
	  len = strlen(outer_end_delimiter);
	  for( i = 0;  i < len;  i++ )
	    NextChar();
	  state = STOP;
	}

	/* check whether a token is starting here, in which case start it */
	else if( (current_token = TokenStartingHere(&len)) != (TOKEN *) NULL )
	{
	  if( DEBUG_PROCESS )
	  { fprintf(stderr, "current_token (len = %d): %s\n",
	      len, EchoToken(current_token));
	  }
	  StartEmit(lang, current_token, &curr_line[line_pos], len);

	  /* skip the starting delimiter */
	  for( i = 0;  i < len;  i++ )
	    NextChar();

	  /* we are now either in a token, or else we have to start an inner */
	  if( current_token->print_style == PRINT_NODELIMS_INNER )
	  {
	    Process(lang, current_token, current_token->end_delimiter);
	    EndEmit(current_token, "");
	  }
	  else state = IN_TOKEN;
	}

	/* check whether we have a space */
	else if( curr_line[line_pos] == ' ' )
	{
	  EmitRaw(' ');
	  NextChar();
	}

	/* check whether we have a tab character */
	else if( curr_line[line_pos] == '\t' )
	{
	    EmitRaw('\t');
	    NextChar();
	}

	/* check whether we have a newline character */
	else if( curr_line[line_pos] == '\n' )
	{
	    EmitRaw('\n');
	    NextChar();
	}

	/* check whether we have a formfeed character */
	else if( curr_line[line_pos] == '\f' )
	{
	    fprintf(out_fp, "\n@NP");
	    EmitRaw('\n');
	    NextChar();
	}

	/* check whether we are supposed to echo things that don't match */
	else if( lang->no_match == NO_MATCH_PRINT )
	{
	  EmitProtected(curr_line[line_pos]);
	  NextChar();
	}

	/* finally, we have an error and must skip the character */
	else if( lang->no_match == NO_MATCH_ERROR )
	{
	  if( Printable(curr_line[line_pos]) )
	    fprintf(err_fp, "%s: skipping unexpected %c character\n",
	      ErrorHeader(), curr_line[line_pos]);
	  else
	    fprintf(err_fp, "%s: %s (octal %o)\n",
	      "skipping unexpected unprintable character",
	      ErrorHeader(), (int) curr_line[line_pos]);
	  NextChar();
	}
	else
	{
	  fprintf(err_fp, "%s internal error: lang->no_match\n", ErrorHeader());
	}
	break;


      case IN_TOKEN:  /* within a token; current_token says which kind */

	/* check for ending delimiter if there is one */
	if( current_token->end_delimiter[0] != '\0' &&
	    (!current_token->end_start_line_only || line_pos == 1) &&
	    InputMatches(current_token->end_delimiter) )
	{
	  if( current_token->want_two_ends && !end_delimiter_seen )
	  {
	    /* if we have to see the end delimiter twice before stopping,  */
	    /* and we haven't seen it yet, then emit the char and carry on */
	    Emit(current_token, curr_line[line_pos]);
	    NextChar();
	    end_delimiter_seen = TRUE;
	  }
	  else
	  {
	    if( DEBUG_PROCESS )
	      fprintf(stderr, "  InputMatches(%s) so finishing token\n",
	        current_token->end_delimiter);
	    len = strlen(current_token->end_delimiter);
	    for( i = 0;  i < len;  i++ )
	      NextChar();
	    EndEmit(current_token, current_token->end_delimiter);
	    state = START;
	  }
	}
	else switch( current_token->chtype[(int) curr_line[line_pos]] )
	{

	  case LEGAL:

	    Emit(current_token, curr_line[line_pos]);
	    NextChar();
	    break;


	  case ESCAPE:

	    NextChar();
	    state = IN_TOKEN_AFTER_ESCAPE;
	    break;


	  case INNER_ESCAPE:

	    EndEmit(current_token, "");
	    NextChar();
	    Process(lang, current_token, current_token->end_inner_escape);
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
	        fprintf(err_fp, "%s: %s (octal %o) in %s\n",
		  ErrorHeader(), "skipping unexpected unprintable character",
		  (int) curr_line[line_pos], current_token->name);
	      NextChar();
	    }
	    else
	    {
	      /* normal termination after last legal character */
	      EndEmit(current_token, "");
	      state = START;
	    }
	    break;


	}
	break;
      

      case IN_TOKEN_AFTER_ESCAPE:

	if( current_token->escape_chtype[(int) curr_line[line_pos]] == LEGAL )
	{
	  Emit(current_token, current_token->escape[0]);
	  Emit(current_token, curr_line[line_pos]);
	}
	else
	{
	  if( Printable(curr_line[line_pos]) )
	    fprintf(err_fp,"%s: skipping %c%c in %s, since %c not legal here\n",
	      ErrorHeader(), current_token->escape[0], curr_line[line_pos],
	      current_token->name, curr_line[line_pos]);
	  else
	    fprintf(err_fp,
	      "%s: skipping %c and %s (octal %o)\n", ErrorHeader(),
	      current_token->escape[0], "unprintable unexpected character",
	       (int) curr_line[line_pos]);
	}
	NextChar();
	state = IN_TOKEN;
	break;


      case IN_TOKEN_AFTER_INNER_ESCAPE:

	/* ending delimiter of inner escape has been read over */
	StartEmit(lang, current_token, "", 0);
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
	  fprintf(err_fp, "%s: %s token ended within %s\n",
	    ErrorHeader(), outer_token->name, current_token->name);
	EndEmit(current_token, "");
      }
      break;


    case IN_TOKEN_AFTER_ESCAPE:

      /* we stopped after the escape character */
      fprintf(err_fp, "%s: skipping %c at end of program text\n",
	ErrorHeader(), current_token->escape[0]);
      EndEmit(current_token, "");
      break;


    case IN_TOKEN_AFTER_INNER_ESCAPE:

      /* we stopped after an inner escape (NB no EndEmit in this case) */
      if( current_token->end_delimiter[0] != '\0' )
      {
	if( outer_token == (TOKEN *) NULL )
	  fprintf(err_fp, "%s: program text ended within %s after escape\n", 
	    ErrorHeader(), current_token->name);
	else
	  fprintf(err_fp, "%s: %s token ended within %s after escape\n",
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
/*  PrintUsage()                                                             */
/*                                                                           */
/*  Print usage message on file err_fp.                                      */
/*                                                                           */
/*****************************************************************************/

void PrintUsage()
{ int i;
  fprintf(err_fp, "\n");
  fprintf(err_fp, "usage: prg2lout <options> <files>\n\n");
  fprintf(err_fp, "    where <options> can be\n");
  fprintf(err_fp, "\n");
  fprintf(err_fp, "    -r           raw mode (used within Lout only)\n");
  fprintf(err_fp, "    -i<file>     take input from <file>\n");
  fprintf(err_fp, "    -o<file>     send output to <file>\n");
  fprintf(err_fp, "    -e<file>     send error messages to <file>\n");
  fprintf(err_fp, "    -l<language> input is in this programming language\n");
  fprintf(err_fp, "    -p<style>    print style: fixed, varying, symbol\n");
  fprintf(err_fp, "    -f<family>   font family (e.g. Times)\n");
  fprintf(err_fp, "    -s<size>     font size (e.g. 10p or 12p)\n");
  fprintf(err_fp, "    -v<space>    line spacing (e.g. 1.1fx)\n");
  fprintf(err_fp, "    -t<num>      tab interval (e.g. 8 is default)\n");
  fprintf(err_fp, "    -T<dist>     output tab interval (e.g. 0.5i)\n");
  fprintf(err_fp, "    -S<file>     use this as the setup file\n");
  fprintf(err_fp, "    -n           no file names as page headers\n");
  fprintf(err_fp, "    -V           print version information and exit\n");
  fprintf(err_fp, "    -u           print this usage message and exit\n");
  fprintf(err_fp, "\n");
  fprintf(err_fp, "    <language> (which is compulsory) can be any one of:\n");
  for( i = 0;  languages[i] != (LANGUAGE *) NULL;  i++ )
    fprintf(err_fp, "        %s\n", languages[i]->names[0]);
  fprintf(err_fp, "\n");
  fprintf(err_fp, "The values of all formatting options not given are\n");
  fprintf(err_fp, "taken from the setup file: either the file given after\n");
  fprintf(err_fp, "-S, or the system default setup file for this language\n");
  fprintf(err_fp, "if there is no -S option.\n");
  fprintf(err_fp, "\n");
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
{ BOOLEAN stdin_seen;  int i, j, arg_pos;
  char *infilename, *outfilename, *errfilename;
  LANGUAGE *lang = NO_LANGUAGE;
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
  tab_in = 8;
  tab_out = 3;
  tab_unit = 'f';
  headers_option = TRUE;
  font_option = size_option = line_option = tabin_option =
    tabout_option = setup_option = language_option = (char *) NULL;
  if( argc == 1 )
  { PrintUsage();
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
	GetArg(style_option, "usage: -p<printstyle>", FALSE);
	if( strcmp(style_option, "fixed") != 0 &&
	    strcmp(style_option, "varying") != 0 &&
	    strcmp(style_option, "symbol") != 0 )
	{ fprintf(err_fp, "%s: unknown -p option %s\n", ErrorHeader(),
	    style_option);
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


      case 'S':
     
	/* read alternative setup file */
	if( raw_seen )
	{ fprintf(err_fp, "%s: -s illegal with -r option\n", ErrorHeader());
	  exit(1);
	}
	GetArg(setup_option, "usage: -S<filename>", FALSE);
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
	fprintf(err_fp, "%s\n", PRG2LOUT_VERSION);
	exit(0);
	break;


      case 'u':
     
	if( raw_seen )
	{ fprintf(err_fp, "%s: -u illegal with -r option\n", ErrorHeader());
	  exit(1);
	}
	PrintUsage();
	exit(0);
	break;


      case 'l':
     
	if( language_option != (char *) NULL )
	{ fprintf(err_fp, "%s: -l seen twice\n", ErrorHeader());
	  exit(1);
	}
	GetArg(language_option, "usage: -l<language>", FALSE);
	i = 0;  j = 0;
	while( lang == NO_LANGUAGE && languages[i] != NO_LANGUAGE )
	{
	  if( languages[i]->names[j] == (char *) NULL )
	    i++, j = 0;
	  else if( strcmp(languages[i]->names[j], language_option) == 0 )
	    lang = languages[i];
	  else
	    j++;
	}
	if( lang == NO_LANGUAGE )
	{
	  fprintf(err_fp, "%s: unknown language %s\n", ErrorHeader(),
	    language_option);
	  exit(1);
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
  if( lang == NO_LANGUAGE )
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
    SetupLanguage(lang);
    line_pos = 1;
    curr_line[line_pos] = '\n';  /* forces line read */
    line_num = 0;
    NextChar();
    Process(lang, (TOKEN *) NULL, "");
  }
  else if( file_count > 0 )
  { int ch;

    /* make sure we have an output file */
    if( out_fp == (FILE *) NULL )
      out_fp = stdout;

    /* print the initial @Use clauses etc.*/
    fprintf(out_fp, "%s%s\n", "@Sy", "sInclude { doc }");
    if( setup_option != NULL )
      fprintf(out_fp, "%s%s { %s }\n", "@In", "clude", setup_option);
    else
      fprintf(out_fp, "%s%s { %s }\n", "@Sy", "sInclude", lang->setup_file);
    fprintf(out_fp, "@Document\n");
    fprintf(out_fp, "    @InitialBreak { lines 1.2fx nohyphen }\n");
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
      /* this string has been disguised to avoid recognition by prg2lout */
      fprintf(out_fp, "%s\n", lang->lang_sym);
      if( style_option != NULL )
	fprintf(out_fp, "    style { %s }\n", style_option );
      if( font_option != NULL )
	fprintf(out_fp, "    font { %s }\n", font_option );
      if( size_option != NULL )
	fprintf(out_fp, "    size { %s }\n", size_option );
      if( line_option != NULL )
	fprintf(out_fp, "    line { %s }\n", line_option );
      if( tabin_option != NULL )
	fprintf(out_fp, "    tabin { %s }\n", tabin_option );
      if( tabout_option != NULL )
	fprintf(out_fp, "    tabout { %s }\n", tabout_option );
      fprintf(out_fp, "%s%s\n", "@Be", "gin");
      while( (ch = getc(in_fp)) != EOF )
	putc(ch, out_fp);
      fprintf(out_fp, "%s%s %s\n", "@E", "nd", lang->lang_sym);
    }

    /* finish off whole input */
    fprintf(out_fp, "%s%s%s\n", "@E", "nd @T", "ext");

  }
  exit(0);
} /* end main */
