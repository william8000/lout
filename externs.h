/*@externs.h:External Declarations:Directories and file conventions@**********/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.12)                       */
/*  COPYRIGHT (C) 1991, 1996 Jeffrey H. Kingston                             */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@cs.usyd.edu.au)                                */
/*  Basser Department of Computer Science                                    */
/*  The University of Sydney 2006                                            */
/*  AUSTRALIA                                                                */
/*                                                                           */
/*  This program is free software; you can redistribute it and/or modify     */
/*  it under the terms of the GNU General Public License as published by     */
/*  the Free Software Foundation; either Version 2, or (at your option)      */
/*  any later version.                                                       */
/*                                                                           */
/*  This program is distributed in the hope that it will be useful,          */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of           */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            */
/*  GNU General Public License for more details.                             */
/*                                                                           */
/*  You should have received a copy of the GNU General Public License        */
/*  along with this program; if not, write to the Free Software              */
/*  Foundation, Inc., 59 Temple Place, Suite 330, Boston MA 02111-1307 USA   */
/*                                                                           */
/*  FILE:         externs.h                                                  */
/*  MODULE:       External Declarations                                      */
/*                                                                           */
/*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#if LOCALE_ON || COLLATE
#include <locale.h>
#endif

#if LOCALE_ON
#include <nl_types.h>
extern nl_catd MsgCat;
#define condcatgets(cat, set, msg, s) catgets(cat, set, msg, s) 
#else
#define condcatgets(cat, set, msg, s) s
#endif


/*****************************************************************************/
/*                                                                           */
/*  Include, font and database directories, and the DEBUG_ON and ASSERT_ON   */
/*  flags (defined by -D options on the cc command line in the makefile).    */
/*                                                                           */
/*  LATIN         Non-zero means compile for ISO-LATIN-1 char set.           */
/*  LIB_DIR       The system directory where library files are kept          */
/*  INCL_DIR      The subdirectory of LIB_DIR where @Include files are kept  */
/*  FONT_DIR      The subdirectory of LIB_DIR where .AFM font files are kept */
/*  MAPS_DIR      The subdirectory of LIB_DIR where .LCM files are kept      */
/*  DATA_DIR      The subdirectory of LIB_DIR where database files are kept  */
/*  HYPH_DIR      The subdirectory of LIB_DIR where hyphenation files kept   */
/*  LOCALE_DIR    The subdirectory of LIB_DIR where locale files are kept    */
/*  CHAR_IN       Determines assignment of input chars to lex classes        */
/*  CHAR_OUT      Determines appearance of literal chars in output           */
/*  DEBUG_ON      Non-zero means compile debug code (lout -d)                */
/*  ASSERT_ON     Non-zero means test assertions                             */
/*  LOCALE_ON     Non-zero means compile setlocale() etc. code               */
/*                                                                           */
/*  #define  LIB_DIR    "/usr/local/lib/lout"                                */
/*  #define  INCL_DIR   "include"                                            */
/*  #define  FONT_DIR   "font"                                               */
/*  #define  MAPS_DIR   "maps"                                               */
/*  #define  DATA_DIR   "data"                                               */
/*  #define  HYPH_DIR   "hyph"                                               */
/*  #define  LOCALE_DIR "locale"  (only used if LOCALE_ON)                   */
/*  #define  CHAR_IN    0                                                    */
/*  #define  CHAR_OUT   0                                                    */
/*  #define  DEBUG_ON   0                                                    */
/*  #define  ASSERT_ON  1                                                    */
/*  #define  LOCALE_ON  1                                                    */
/*                                                                           */
/*****************************************************************************/


/*@::File naming conventions and version@*************************************/
/*                                                                           */
/*  File naming conventions and version                                      */
/*                                                                           */
/*  LOUT_VERSION        Version information                                  */
/*  CROSS_DB            The default name of the cross reference database     */
/*  SOURCE_SUFFIX       Optional suffix of source files and include files    */
/*  INDEX_SUFFIX        The suffix of database index files                   */
/*  NEW_INDEX_SUFFIX    The suffix of new database index files               */
/*  DATA_SUFFIX         The suffix of database data files                    */
/*  NEW_DATA_SUFFIX     The additional suffix of new database data files     */
/*  HYPH_SUFFIX         The suffix of unpacked hyphenation files             */
/*  HYPH_PACKED_SUFFIX  The suffix of packed hyphenation files               */
/*  FILTER_IN           The prefix of the name of the input file to filters  */
/*  FILTER_OUT          The prefix of the name of the output file to filters */
/*  FILTER_ERR          The name of the error file to filters                */
/*                                                                           */
/*****************************************************************************/

#define	LOUT_VERSION    AsciiToFull("Basser Lout Version 3.12 (April 1998)")
#define	CROSS_DB	   AsciiToFull("lout")
#define	SOURCE_SUFFIX	   AsciiToFull(".lt")
#define	INDEX_SUFFIX	   AsciiToFull(".li")
#define	NEW_INDEX_SUFFIX   AsciiToFull(".lix")
#define	DATA_SUFFIX	   AsciiToFull(".ld")
#define	NEW_DATA_SUFFIX	   AsciiToFull("x")
#define	HYPH_SUFFIX	   AsciiToFull(".lh")
#define	HYPH_PACKED_SUFFIX AsciiToFull(".lp")
#define	FILTER_IN	   AsciiToFull("louti")
#define	FILTER_OUT	   AsciiToFull("lout")
#define	FILTER_ERR	   AsciiToFull("lout.err")


/*****************************************************************************/
/*                                                                           */
/*  Operating system dependent things                                        */
/*                                                                           */
/*  (1) File read and write modes for binary files, and directory character  */
/*                                                                           */
/*  MS-DOS text file line endings differ from Unix line endings.  This is    */
/*  usually ignorable but causes problems with binary files and files where  */
/*  you do arithmetic on fseek() values.  In Lout the problematic files are  */
/*  compressed trie and hyphenation files, and database index files.  So     */
/*  we must read and write these files in "binary mode" in MS-DOS.  For      */
/*  completeness we have included synonyms for all file modes used by Lout:  */
/*                                                                           */
/*  READ_BINARY       Mode passed to fopen() when reading a "binary file"    */
/*  WRITE_BINARY      Mode passed to fopen() when writing a "binary file"    */
/*  READ_TEXT         Mode passed to fopen() when reading a "text file"      */
/*  APPEND_TEXT       Mode passed to fopen() when appending to "text file"   */
/*  WRITE_TEXT        Mode passed to fopen() when writing a "text file"      */
/*  STR_DIR           Directory character used in file path names            */
/*                                                                           */
/*  (2) System command and file name for uncompressing EPS files             */
/*                                                                           */
/*  UNCOMPRESS_COM    System command for uncompressing compressed EPS file   */
/*  LOUT_EPS          Name of temporary uncompressed EPS file                */
/*                                                                           */
/*  There is one further call to system() in the Lout source code:  the one  */
/*  that implements filtered parameters such as c2lout.  The strings passed  */
/*  to this call to system() are the values of @Filter symbols within Lout   */
/*  definitions.                                                             */
/*                                                                           */
/*****************************************************************************/

#if OS_UNIX
#define	READ_BINARY	"r"
#define	WRITE_BINARY	"w"
#define	READ_TEXT	"r"
#define	APPEND_TEXT	"a"
#define	WRITE_TEXT	"w"
#define	STR_DIR		AsciiToFull("/")
#define	UNCOMPRESS_COM	"gunzip -c %s > %s"
#define	LOUT_EPS	"lout.eps"
#else
#if OS_DOS
#define	READ_BINARY	"rb"
#define	WRITE_BINARY	"wb"
#define	READ_TEXT	"rt"
#define	APPEND_TEXT	"at"
#define	WRITE_TEXT	"wt"
#define	STR_DIR		AsciiToFull("/")
#define	UNCOMPRESS_COM	"gunzip -c %s > %s"
#define	LOUT_EPS	"lout.eps"
#else
#if OS_MAC
#define	READ_BINARY	"r"
#define	WRITE_BINARY	"w"
#define	READ_TEXT	"r"
#define	APPEND_TEXT	"a"
#define	WRITE_TEXT	"w"
#define	STR_DIR		AsciiToFull(":")
#define	UNCOMPRESS_COM	"gunzip -c %s > %s"
#define	LOUT_EPS	"lout.eps"
#else
If you're compiling this, you've got the wrong settings in the makefile!
#endif
#endif
#endif

/*@::Significant limits@******************************************************/
/*                                                                           */
/*  Significant Limits (other insignificant ones appear in other files)      */
/*                                                                           */
/*  MAX_FULL_LENGTH     The maximum value storable in type FULL_LENGTH.      */
/*                      NB this cannot be 2**31 - 1 because there are        */
/*                      intermediate results that exceed MAX_FULL_LENGTH     */
/*                      and are subsequently reduced to MAX_FULL_LENGTH.     */
/*                      For example, some intermediate results may exceed    */
/*                      MAX_FULL_LENGTH by a factor of SF, which is defined  */
/*                      below to be 128 (2**7).  The value given is 2**23-1, */
/*                      which is about 148 metres in Lout's precision.       */
/*                                                                           */
/*  MAX_SHORT_LENGTH    The maximum value storable in type SHORT_LENGTH.     */
/*                                                                           */
/*  MAX_FILES           The maximum number of files.  This could only be     */
/*                      increased if the file_num() field of type FILE_POS   */
/*                      is enlarged beyond its present 16 bits.              */
/*                                                                           */
/*  MAX_LINE            1 + the maximum length of an input line in source    */
/*                      and database files.  This is used for the lexical    */
/*                      analyser's input line buffer only, and could be      */
/*                      increased immediately to 4096, and even further if   */
/*                      more than the current 12 bits was assigned to the    */
/*                      col_num() field of type FILE_POS.                    */
/*                                                                           */
/*  MAX_WORD            1 + the maximum length of a word storable in an      */
/*                      object record, which includes all file path names    */
/*                      too.  It is reasonable to make this MAX_LINE, since  */
/*                      a word longer than MAX_LINE cannot be read in.       */
/*                                                                           */
/*  MAX_OBJECT_REC      1 + the maximum size of an object record, measured   */
/*                      in ALIGNs.  The value chosen should exceed           */
/*                      ceiling( (wr + MAX_WORD - 4) / sizeof(ALIGN) )       */
/*                      where wr = sizeof(struct word_rec), so that words of */
/*                      length MAX_WORD-1 can be stored in an object record. */
/*                                                                           */
/*  MAX_BUFF            1 + the maximum length of a "standard buffer"; these */
/*                      buffers are used in a variety of places throughout   */
/*                      the program for holding one line of a font file,     */
/*                      one file path name, one symbol full name, etc.  This */
/*                      may be increased immediately without limit.          */
/*                                                                           */
/*  MAX_FONT            The maximum number of sized fonts allowed.  This     */
/*                      can be increased beyond 4096 only by setting aside   */
/*                      a larger word_font() field.                          */
/*                                                                           */
/*  MAX_COLOUR          The maximum number of distinct left parameters of    */
/*                      @SetColour and @SetColor symbols allowed (after      */
/*                      evaluation).  This can be increased beyond 4096      */
/*                      only by setting aside a larger word_colour() field.  */
/*                                                                           */
/*  MAX_LANGUAGE        The maximum number of distinct languages allowed.    */
/*                      This can be increased beyond 256 only by setting     */
/*                      aside a larger word_language() field.                */
/*                                                                           */
/*  MAX_LEX_STACK       The maximum depth of @Includes and @Databases.  This */
/*                      can be increased immediately by any small amount.    */
/*                                                                           */
/*  MAX_CHARS		The maximimum number of characters in a font.  This  */
/*                      cannot be increased easily.                          */
/*                                                                           */
/*****************************************************************************/

#define	MAX_FULL_LENGTH	8388607	/* 2**23 - 1, about 148 metres */
#define	MAX_SHORT_LENGTH 32767
#define	MAX_FILES	65535
#define MAX_LINE        2048
#define MAX_WORD        2048
#define	MAX_OBJECT_REC	ceiling(sizeof(struct word_type)+MAX_WORD,sizeof(ALIGN))
#define MAX_BUFF        512
#define MAX_FONT	4096
#define MAX_COLOUR	4096
#define	MAX_LANGUAGE	64
#define	MAX_LEX_STACK	7
#define	MAX_CHARS	256

/*****************************************************************************/
/*                                                                           */
/*  Miscellaneous Macros                                                     */
/*                                                                           */
/*****************************************************************************/

#define	BOOLEAN		unsigned
#define	FALSE		0
#define	TRUE		1
#define	bool(x)		(x ? AsciiToFull("TRUE") : AsciiToFull("FALSE") )
#define	CHILD		0
#define	PARENT		1
#define	COLM		0
#define	ROWM		1
#define	dimen(x)	(x == COLM ? AsciiToFull("COLM") : AsciiToFull("ROWM") )
#define	nilobj		( (OBJECT) NULL )
#define	null		( (FILE *) NULL )

#define find_max(a, b)	((a) < (b) ? (b) : (a))
#define find_min(a, b)	((a) < (b) ? (a) : (b))
#define	ceiling(a, b)	( ((a) - 1)/(b) + 1 )	/* ceiling(a/b)              */
#define is_odd(x)	( (x) & 1 )		/* TRUE if x is odd number   */

/*@::ALIGN, FULL_LENGTH, FONT_NUM, COLOUR_NUM, LANGUAGE_NUM, FULL_CHAR@*******/
/*                                                                           */
/*  typedef ALIGN - used for forcing record alignment.                       */
/*                                                                           */
/*****************************************************************************/

typedef char *ALIGN;


/*****************************************************************************/
/*                                                                           */
/*  typedef FULL_LENGTH - an integer physical distance.                      */
/*                                                                           */
/*****************************************************************************/

typedef int FULL_LENGTH;


/*****************************************************************************/
/*                                                                           */
/*  typedef SHORT_LENGTH - an short integer physical distance.               */
/*                                                                           */
/*****************************************************************************/

typedef short int SHORT_LENGTH;


/*****************************************************************************/
/*                                                                           */
/*  FONT_NUM - internal name for a font.                                     */
/*                                                                           */
/*****************************************************************************/

typedef unsigned FONT_NUM;


/*****************************************************************************/
/*                                                                           */
/*  COLOUR_NUM - internal name for a colour.                                 */
/*                                                                           */
/*****************************************************************************/

typedef unsigned COLOUR_NUM;


/*****************************************************************************/
/*                                                                           */
/*  LANGUAGE_NUM - internal name for a language.                             */
/*                                                                           */
/*****************************************************************************/

typedef unsigned LANGUAGE_NUM;


/*****************************************************************************/
/*                                                                           */
/*  MAPPING - internal name for a character mapping vector.                  */
/*                                                                           */
/*****************************************************************************/

typedef unsigned MAPPING;


/*****************************************************************************/
/*                                                                           */
/*  typedef FULL_CHAR - one of the characters manipulated by Lout.           */
/*                                                                           */
/*  This program does not deal with 7-bit ASCII characters.  Instead, its    */
/*  characters are defined by the FULL_CHAR typedef, and could be anything   */
/*  from 7-bit ASCII to 8-bit ISO-LATIN-1 to 16-bit UNICODE and beyond.      */
/*                                                                           */
/*  Unfortunately C favours signed 8-bit characters: literal strings are     */
/*  pointers to them, argv[] and the standard libraries assume them.  We get */
/*  around these problems by using our own library, including AsciiToFull()  */
/*  to convert an ASCII string (such as a C string) into a FULL_CHAR string. */
/*                                                                           */
/*  Formally this library appears in module z39.c; however since this        */
/*  implementation uses 8-bit unsigned characters, most of the routines      */
/*  can be implemented by macros containing type-cast calls to C standard    */
/*  library routines.  These appear in the z39.c externs list below.         */
/*                                                                           */
/*****************************************************************************/

typedef unsigned char FULL_CHAR;

/*****************************************************************************/
/*                                                                           */
/*  typedef POINTER- name for type of generic pointer                        */
/*                                                                           */
/*****************************************************************************/

typedef void *POINTER;


/*@::Character literals@******************************************************/
/*                                                                           */
/*  Character Literals                                                       */
/*                                                                           */
/*  The following macros ensure that no Lout source is ever compared to a    */
/*  literal character other than '\0':                                       */
/*                                                                           */
/*****************************************************************************/

#define	CH_FLAG_OUTFILE		'o'	/* the -o command line flag          */
#define	CH_FLAG_SUPPRESS	's'	/* the -s command line flag          */
#define	CH_FLAG_NOKERN		'k'	/* the -k command line flag          */
#define	CH_FLAG_CROSS		'c'	/* the -c command line flag          */
#define	CH_FLAG_ERRFILE		'e'	/* the -e command line flag          */
#define	CH_FLAG_ALTERR		'a'	/* the -a command line flag          */
#define	CH_FLAG_EPSFIRST	'E'	/* first letter of the -EPS flag     */
#define	CH_FLAG_DIRPATH		'D'	/* the -D command line flag          */
#define	CH_FLAG_ENCPATH		'C'	/* the -C command line flag          */
#define	CH_FLAG_FNTPATH		'F'	/* the -F command line flag          */
#define	CH_FLAG_HYPPATH		'H'	/* the -H command line flag          */
#define	CH_FLAG_INCPATH		'I'	/* the -I command line flag          */
#define	CH_FLAG_INCLUDE		'i'	/* the -i command line flag          */
#define	CH_FLAG_HYPHEN		'h'	/* the -h command line flag          */
#define	CH_FLAG_VERSION		'V'	/* the -V command line flag          */
#define	CH_FLAG_USAGE		'u'	/* the -u command line flag          */
#define	CH_FLAG_PLAIN		'p'	/* the -p command line flag          */
#define	CH_FLAG_FFPLAIN		'P'	/* the -P command line flag          */
#define	CH_FLAG_DEBUG		'd'	/* the -d command line flag          */
#define	CH_FLAG_INITALL		'x'	/* the -x command line flag          */
#define	CH_FLAG_OPTION		'-'	/* the -- command line flag          */
#define	CH_FLAG_SAFE		'S'	/* the -S command line flag          */
#define	CH_FLAG_UNSAFE		'U'	/* the -U command line flag          */
#define	CH_FLAG_WORDS		'w'	/* the -w command line flag          */
#define	CH_FLAG_PDF		'Z'	/* the -Z command line flag	     */

#define	CH_SPACE		' '	/* space character                   */
#define	CH_NEWLINE		'\n'	/* the newline character             */
#define	CH_SYMSTART		'@'	/* extra letter symbols may have     */
#define	CH_UNDERSCORE		'_'	/* extra letter symbols may have     */
#define	CH_QUOTE		'"'	/* the quote character		     */
#define	CH_ZERO			'0'	/* the first digit character, zero   */
#define	CH_EIGHT		'8'	/* the last even digit character     */
#define	CH_NINE			'9'	/* the last odd digit character      */
#define	CH_INCGAP		'+'	/* begins an incrementing gap	     */
#define	CH_DECGAP		'-'	/* begins a decrementing gap	     */
#define	CH_MINUS		'-'	/* minus sign                        */
#define	CH_HYPHEN		'-'	/* the hyphen character		     */
#define	CH_NOBREAK		'u'	/* `unbreakable' character for gaps  */

#define	CH_UNIT_CM		'c'	/* unit of measurement: centimetres  */
#define	CH_UNIT_IN		'i'	/* unit of measurement: inches       */
#define	CH_UNIT_PT		'p'	/* unit of measurement: points       */
#define	CH_UNIT_EM		'm'	/* unit of measurement: ems          */
#define	CH_UNIT_FT		'f'	/* unit of measurement: fontsizes    */
#define	CH_UNIT_SP		's'	/* unit of measurement: spacewidths  */
#define	CH_UNIT_VS		'v'	/* unit of measurement: vspaces      */
#define	CH_UNIT_WD		'w'	/* unit of measurement: follwidths   */
#define	CH_UNIT_BD		'b'	/* unit of measurement: boundwidths  */
#define	CH_UNIT_RL		'r'	/* unit of measurement: relwidths    */
#define	CH_UNIT_DG		'd'	/* unit of measurement: degrees      */
#define	CH_UNIT_YU		'y'	/* unit of measurement: y unit       */
#define	CH_UNIT_ZU		'z'	/* unit of measurement: z unit       */

#define	CH_MODE_EDGE		'e'	/* spacing mode: edge-to-edge        */
#define	CH_MODE_HYPH		'h'	/* spacing mode: hyphenation         */
#define	CH_MODE_MARK		'x'	/* spacing mode: mark-to-mark        */
#define	CH_MODE_OVER		'o'	/* spacing mode: overstrike          */
#define	CH_MODE_KERN		'k'	/* spacing mode: kerning             */
#define	CH_MODE_TABL		't'	/* spacing mode: tabulation          */

#define octaldigit(ch)		( (ch) >= '0' && (ch) <= '7' )
#define decimaldigit(ch)	( (ch) >= '0' && (ch) <= '9' )
#define	digitchartonum(ch)	( (ch) - '0' )
#define	numtodigitchar(ch)	( (ch) + '0' )
#define	beginsbreakstyle(ch)	( (ch) >= 'a' && (ch) <= 'z' )
#define	numericchar(ch)		( decimaldigit(ch) || (ch) == '.' )


/*@::String literals, FULL_CHAR type@*****************************************/
/*                                                                           */
/*  String Literals.                                                         */
/*                                                                           */
/*  All significant string literals are defined here.  The program has many  */
/*  others, however: format strings, debug output, etc.                      */
/*                                                                           */
/*****************************************************************************/

#define	STR_EMPTY		AsciiToFull("")
#define	STR_QUOTE		AsciiToFull("\"")
#define	STR_ESCAPE		AsciiToFull("\\")
#define	STR_COMMENT		AsciiToFull("#")
#define	STR_SPACE		AsciiToFull(" ")
#define	STR_FORMFEED		AsciiToFull("\f")
#define	STR_TAB			AsciiToFull("\t")
#define	STR_NEWLINE		AsciiToFull("\n")
#define	STR_LETTERS_LOWER	AsciiToFull("abcdefghijklmnopqrstuvwxyz")
#define	STR_LETTERS_UPPER	AsciiToFull("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
#define	STR_LETTERS_SYMSTART	AsciiToFull("@")
#define	STR_LETTERS_UNDERSCORE	AsciiToFull("_")

#if CHAR_IN==0
#define	STR_LETTERS_EXTRA0	AsciiToFull("")
#define	STR_LETTERS_EXTRA1	AsciiToFull("")
#define	STR_LETTERS_EXTRA2	AsciiToFull("")
#define	STR_LETTERS_EXTRA3	AsciiToFull("")
#define	STR_LETTERS_EXTRA4	AsciiToFull("")
#define	STR_LETTERS_EXTRA5	AsciiToFull("")
#define	STR_LETTERS_EXTRA6	AsciiToFull("")
#define	STR_LETTERS_EXTRA7	AsciiToFull("")
#else
#define	STR_LETTERS_EXTRA0	AsciiToFull("\300\301\302\303\304\305\306\307")
#define	STR_LETTERS_EXTRA1	AsciiToFull("\310\311\312\313\314\315\316\317")
#define	STR_LETTERS_EXTRA2	AsciiToFull("\320\321\322\323\324\325\326")
#define	STR_LETTERS_EXTRA3	AsciiToFull("\330\331\332\333\334\335\336\337")
#define	STR_LETTERS_EXTRA4	AsciiToFull("\340\341\342\343\344\345\346\347")
#define	STR_LETTERS_EXTRA5	AsciiToFull("\350\351\352\353\354\355\356\357")
#define	STR_LETTERS_EXTRA6	AsciiToFull("\360\361\362\363\364\365\366")
#define	STR_LETTERS_EXTRA7	AsciiToFull("\370\371\372\373\374\375\376\377")
#endif

#define	STR_STDIN		AsciiToFull("-")
#define	STR_STDOUT		AsciiToFull("-")
#define	STR_HYPHEN		AsciiToFull("-")
#define	STR_EPS			AsciiToFull("EPS")
#define	STR_POSTSCRIPT		AsciiToFull("PostScript")
#define	STR_PLAINTEXT		AsciiToFull("PlainText")
#define	STR_PDF			AsciiToFull("PDF")
#define	STR_ELSE		AsciiToFull("else")
#define	STR_NOCROSS		AsciiToFull("??")
#define	STR_BADKEY		AsciiToFull("badkey")
#define	STR_NONE		AsciiToFull("none")
#define	STR_NOCHAR		AsciiToFull("-none-")
#define	STR_ZERO		AsciiToFull("0")
#define	STR_PS_SPACENAME	AsciiToFull("space")
#define	STR_FONT_RECODE		AsciiToFull("Recode")
#define	STR_FONT_NORECODE	AsciiToFull("NoRecode")
#define	STR_COLOUR_NOCHANGE	AsciiToFull("nochange")

#define	STR_BREAK_HYPHEN	AsciiToFull("hyphen")
#define	STR_BREAK_NOHYPHEN	AsciiToFull("nohyphen")
#define	STR_BREAK_ADJUST	AsciiToFull("adjust")
#define	STR_BREAK_OUTDENT	AsciiToFull("outdent")
#define	STR_BREAK_RAGGED	AsciiToFull("ragged")
#define	STR_BREAK_CRAGGED	AsciiToFull("cragged")
#define	STR_BREAK_RRAGGED	AsciiToFull("rragged")
#define	STR_BREAK_ORAGGED	AsciiToFull("oragged")
#define	STR_BREAK_LINES		AsciiToFull("lines")
#define	STR_BREAK_CLINES	AsciiToFull("clines")
#define	STR_BREAK_RLINES	AsciiToFull("rlines")

#define STR_SPACE_LOUT		AsciiToFull("lout")
#define STR_SPACE_COMPRESS	AsciiToFull("compress")
#define STR_SPACE_SEPARATE	AsciiToFull("separate")
#define STR_SPACE_TROFF		AsciiToFull("troff")
#define STR_SPACE_TEX		AsciiToFull("tex")

#define	STR_SMALL_CAPS_ON	AsciiToFull("smallcaps")
#define	STR_SMALL_CAPS_OFF	AsciiToFull("nosmallcaps")

#define	STR_GAP_RJUSTIFY	AsciiToFull("1rt")
#define	STR_GAP_ZERO_HYPH	AsciiToFull("0ch")


/*@::GAP, STYLE@**************************************************************/
/*                                                                           */
/*  typedef GAP - what separates one object from another.                    */
/*                                                                           */
/*****************************************************************************/

typedef struct
{ unsigned	ospare	 : 7;		/* left for other things in STYLE    */
  BOOLEAN	onobreak : 1;		/* TRUE if this gap is unbreakable   */
  BOOLEAN	omark	 : 1;		/* TRUE if this gap is marked        */
  BOOLEAN	ojoin	 : 1;		/* TRUE if joins exist across gap    */
  unsigned	ounits	 : 3;		/* units of measurement: fixed, etc  */
  unsigned	omode	 : 3;		/* spacing mode: edge-to-edge, etc   */
  SHORT_LENGTH	owidth;			/* width of the gap                  */
} GAP;

#define	nobreak(x)	(x).onobreak
#define	mark(x)		(x).omark
#define	join(x)		(x).ojoin
#define	units(x)	(x).ounits
#define	mode(x)		(x).omode
#define	width(x)	(x).owidth

#define SetGap(x, xnobreak, xmark, xjoin, xunits, xmode, xwidth)	\
( nobreak(x) = xnobreak, mark(x) = xmark, join(x) = xjoin,		\
  units(x) = xunits, mode(x) = xmode, width(x) = xwidth			\
)

#define GapCopy(x, y)							\
( nobreak(x) = nobreak(y), mark(x) = mark(y), join(x) = join(y),	\
  units(x) = units(y), mode(x) = mode(y), width(x) = width(y)		\
)

#define ClearGap(x)	SetGap(x, FALSE, FALSE, TRUE, FIXED_UNIT, NO_MODE, 0)


/*****************************************************************************/
/*                                                                           */
/*  typedef STYLE - information about how to break text, etc.                */
/*                                                                           */
/*****************************************************************************/

typedef struct
{ union {
    GAP		oline_gap;		/* separation between lines          */
    struct {
      BOOLEAN	ovadjust	: 1;	/* @VAdjust in effect                */
      BOOLEAN	ohadjust	: 1;	/* @HAdjust in effect                */
      BOOLEAN	opadjust	: 1;	/* @PAdjust in effect                */
      unsigned	osmall_caps	: 1;	/* small capitals                    */
      unsigned  ospace_style	: 3;	/* space style: lout, troff, tex, .. */
    } oss1;
  } osu1;
  union {
    GAP		ospace_gap;		/* separation induced by white space */
    struct {
      unsigned	ohyph_style    : 2;	/* hyphenation off or on             */
      unsigned	ofill_style    : 2;	/* fill lines with text off/on       */
      unsigned	odisplay_style : 3;	/* display lines adjusted, ragged... */
    } oss2;
  } osu2;
  SHORT_LENGTH	oyunit;			/* value of y unit of measurement    */
  SHORT_LENGTH	ozunit;			/* value of z unit of measurement    */
  FONT_NUM	ofont      : 12;	/* current font                      */
  COLOUR_NUM	ocolour    : 12;	/* current colour		     */
  LANGUAGE_NUM	olanguage  : 6;		/* current language		     */
} STYLE;

#define	line_gap(x)	(x).osu1.oline_gap
#define	vadjust(x)	(x).osu1.oss1.ovadjust
#define	hadjust(x)	(x).osu1.oss1.ohadjust
#define	padjust(x)	(x).osu1.oss1.opadjust
#define	small_caps(x)	(x).osu1.oss1.osmall_caps
#define	space_style(x)	(x).osu1.oss1.ospace_style
#define	space_gap(x)	(x).osu2.ospace_gap
#define	hyph_style(x)	(x).osu2.oss2.ohyph_style
#define	fill_style(x)	(x).osu2.oss2.ofill_style
#define	display_style(x)(x).osu2.oss2.odisplay_style
#define	font(x)		(x).ofont
#define	colour(x)	(x).ocolour
#define	language(x)	(x).olanguage
#define	yunit(x)	(x).oyunit
#define	zunit(x)	(x).ozunit

#define StyleCopy(x, y)							\
( GapCopy(line_gap(x), line_gap(y)),					\
  hyph_style(x) = hyph_style(y),					\
  fill_style(x) = fill_style(y),					\
  display_style(x) = display_style(y),					\
  small_caps(x) = small_caps(y),					\
  GapCopy(space_gap(x), space_gap(y)),					\
  font(x) = font(y),							\
  colour(x) = colour(y),						\
  language(x) = language(y), 						\
  vadjust(x) = vadjust(y), 						\
  hadjust(x) = hadjust(y), 						\
  padjust(x) = padjust(y), 						\
  space_style(x) = space_style(y),					\
  yunit(x) = yunit(y),							\
  zunit(x) = zunit(y)							\
)


/*@::CONSTRAINT, FILE_NUM, FILE_POS, LIST@************************************/
/*                                                                           */
/*  typedef CONSTRAINT - a size constraint.                                  */
/*                                                                           */
/*****************************************************************************/

typedef struct
{ FULL_LENGTH  obc;
  FULL_LENGTH  obfc;
  FULL_LENGTH  ofc;
  FULL_LENGTH  osparec;
} CONSTRAINT;

#define	bc(x)		(x).obc
#define	bfc(x)		(x).obfc
#define	fc(x)		(x).ofc
#define	sparec(x)	(x).osparec
#define	constrained(x)	(bc(x) != MAX_FULL_LENGTH ||			\
			 bfc(x) != MAX_FULL_LENGTH || fc(x) != MAX_FULL_LENGTH)

#define	SetConstraint(c,x,y,z)	(bc(c) = (x),   bfc(c) = (y),    fc(c) = (z))
#define	CopyConstraint(x, y)	(bc(x) = bc(y), bfc(x) = bfc(y), fc(x) = fc(y))
#define FitsConstraint(b, f, c)	(b <= bc(c)  && b + f <= bfc(c) && f <= fc(c))

#define	ig_fnum(x)	bc(constraint(x))
#define	ig_xtrans(x)	bfc(constraint(x))
#define	ig_ytrans(x)	fc(constraint(x))


/*****************************************************************************/
/*                                                                           */
/*  typedef FILE_NUM - the internal representation of a file.                */
/*                                                                           */
/*****************************************************************************/

typedef unsigned short	FILE_NUM;
#define	NO_FILE		0


/*****************************************************************************/
/*                                                                           */
/*  typedef FILE_POS - a position in the set of input files.                 */
/*                                                                           */
/*****************************************************************************/

typedef	struct
{ unsigned char	 otype;			/* space for object type field	     */
  unsigned char	 orec_size;		/* space for object record size      */
  FILE_NUM	 ofile_num;		/* no. of file this record is from   */
  unsigned       oline_num  : 20;	/* the line number of this record    */
  unsigned       ocol_num   : 12;	/* column number this is related to  */
} FILE_POS;

#define	file_num(x)	(x).ofile_num
#define	col_num(x)	(x).ocol_num
#define	line_num(x)	(x).oline_num

#define FposCopy(x, y)							\
( file_num(x) = file_num(y),						\
  line_num(x) = line_num(y),						\
  col_num(x)  = col_num(y)						\
)


/*****************************************************************************/
/*                                                                           */
/*  typedef LIST - two pointers used to make one doubly linked list          */
/*                                                                           */
/*****************************************************************************/

typedef struct { union rec *opred, *osucc; } LIST;


/*@::FIRST_UNION@*************************************************************/
/*                                                                           */
/*  typedef FIRST_UNION - first eight bytes of object record (after LISTs).  */
/*                                                                           */
/*  The fpos is overwritten in WORDs and QWORDs during FixAndPrintObject by  */
/*  the horizontal coordinate of the word, which has to be remembered.       */
/*                                                                           */
/*****************************************************************************/

typedef union
{
  FILE_POS	ofpos;
  struct
  {	unsigned char	otype, orec_size;
	int		oword_save_mark;
  } os11;

} FIRST_UNION;


/*@::SECOND_UNION, THIRD_UNION, FOURTH_UNION@*********************************/
/*                                                                           */
/*  typedef SECOND_UNION - four bytes holding various flags etc.             */
/*                                                                           */
/*****************************************************************************/

typedef union
{
  struct /* used by all tokens */
  {	unsigned char	oprecedence;
	unsigned char	ohspace, ovspace;
  } os21;

  struct /* used by WORD objects only, except underline used by all */
	 /* objects, including GAP_OBJ                                  */
  {	FONT_NUM	oword_font     : 12;
	COLOUR_NUM	oword_colour   : 12;
	LANGUAGE_NUM	oword_language : 6;
	unsigned	ounderline     : 1;
	unsigned	oword_hyph     : 1;
  } os22;

  struct /* used by non-WORD objects */
  {	unsigned char	ofoll_or_prec;
	unsigned char	ocross_type;	     /* CROSS objects only */
	BOOLEAN		onon_blocking: 1;
	BOOLEAN		osized       : 1;
	BOOLEAN		othreaded    : 1;
	BOOLEAN		oexternal_hor: 1;
	BOOLEAN		oexternal_ver: 1;
	BOOLEAN		oblocked     : 1;
	BOOLEAN		otrigger_ext : 1;
	BOOLEAN	        omust_expand : 1;
	BOOLEAN		ogall_dir    : 1;
	BOOLEAN		oopt_hyph    : 1;
	BOOLEAN		oopt_gazumped: 1;
	BOOLEAN		oadjust_cat  : 1;
	BOOLEAN		oforce_gall  : 1;
	unsigned	ounderline   : 1;
	/* don't forget ounderline from os22 applies in this union! */
  } os23;

  struct /* used by WORD and QWORD when they are database nodes */
  {	unsigned short	oleft_pos;
	unsigned short	oreading;
  } os24;

  struct /* used by WORD and QWORD when they are font records */
  {	FONT_NUM	ofont_num : 12;
	unsigned short	ofont_page;
  } os25;

  struct /* used by symbol table entries */
  {	unsigned char	oprecedence;
	BOOLEAN		ois_tag		     : 1;
	BOOLEAN		ohas_tag             : 1;
	BOOLEAN		ohas_lpar            : 1;
	BOOLEAN		ohas_rpar            : 1;
	BOOLEAN		oright_assoc         : 1;
	BOOLEAN		ois_target           : 1;
	BOOLEAN		ohas_target          : 1;
	BOOLEAN		oforce_target	     : 1;
	BOOLEAN		ohas_body            : 1;
	BOOLEAN		oindefinite          : 1;
	BOOLEAN		orecursive           : 1;
	BOOLEAN		ouses_extern_target  : 1;
	BOOLEAN		ois_extern_target    : 1;
	BOOLEAN		ois_key		     : 1;
	BOOLEAN		ohas_key	     : 1;
	BOOLEAN		odirty               : 1;
	BOOLEAN		ovisible	     : 1;
	BOOLEAN		ohas_mark	     : 1;
	BOOLEAN		ohas_join	     : 1;
	BOOLEAN		ohas_par             : 1;
	BOOLEAN		ouses_galley	     : 1;
	BOOLEAN		ohoriz_galley	     : 1;
  } os26;

} SECOND_UNION;


/*****************************************************************************/
/*                                                                           */
/*  typedef THIRD_UNION - eight bytes usually holding an object size.        */
/*                                                                           */
/*  In database records this space is used for a file pointer; in certain    */
/*  WORD objects used privately in z10.c it is used for a galley-position.   */
/*  In font records it holds the font size, space width, etc.  In filter     */
/*  words it holds a pointer to the symbol being filtered.                   */
/*                                                                           */
/*****************************************************************************/

typedef union
{
  struct
  {	FULL_LENGTH	oback[2];
	FULL_LENGTH	ofwd[2];
  } os31;

  FILE *ofilep;

  struct
  {	FULL_LENGTH	ofont_size;
	FULL_LENGTH	ofont_xheight2;
	FULL_LENGTH	ofont_spacewidth;
	MAPPING		ofont_mapping	: 7;
	BOOLEAN		ofont_recoded	: 1;
	BOOLEAN		ofont_firstpage	: 1;
  } os32;

  struct
  {
	unsigned char	ocs_type;
	FILE_NUM	ocs_fnum;
	int		ocs_pos;
	int		ocs_lnum;

	/*** old values
	BOOLEAN ogall_rec;
	unsigned char ogall_type;
	int	ogall_pos;
	***/
  } os33;

  union rec *ofilter_actual;

} THIRD_UNION;


/*****************************************************************************/
/*                                                                           */
/*  typedef FOURTH_UNION - twelve bytes holding a STYLE or CONSTRAINT.       */
/*                                                                           */
/*****************************************************************************/

typedef union
{
  STYLE		osave_style;
  CONSTRAINT	oconstraint;

} FOURTH_UNION;


/*@::OBJECT@******************************************************************/
/*                                                                           */
/*  typedef OBJECT - the general-purpose record used throughout Lout.        */
/*                                                                           */
/*****************************************************************************/

typedef union rec
{
  struct word_type	/* all fields of WORD and QWORD, token and object */
  {  LIST		olist[2];
     FIRST_UNION	ou1;
     SECOND_UNION	ou2;
     THIRD_UNION	ou3;
     FULL_CHAR		ostring[4];
  } os1;

  struct closure_type	/* all fields of CLOSURE, both as token and object */
  {  LIST		olist[2];
     FIRST_UNION	ou1;
     SECOND_UNION	ou2;
     THIRD_UNION	ou3;
     FOURTH_UNION	ou4;
     union rec		*oactual;
     union
     { union rec *owhereto;
       union rec *opinpoint;
       FULL_LENGTH    osave_mark;
     } oux;
     /* union rec *oready_galls; */
  } os2;
  
  struct head_type	/* all fields of HEAD, both as token and object */
  {  LIST		olist[2];
     FIRST_UNION	ou1;
     SECOND_UNION	ou2;
     THIRD_UNION	ou3;
     FOURTH_UNION	ou4;
     union rec		*oactual;
     union
     { union rec *owhereto;
       union rec *opinpoint;
       FULL_LENGTH    osave_mark;
     } oux;
     union rec *oready_galls;
     union rec *oopt_components;
     union rec *oopt_constraints;
     union rec *oopt_counts;
     union rec *olimiter;
     union rec *oenclose_obj;
     int        oopt_comps_permitted;
  } os2a;
  
  struct object_type	/* the general OBJECT */
  {  LIST		olist[2];
     FIRST_UNION	ou1;
     SECOND_UNION	ou2;
     THIRD_UNION	ou3;
     FOURTH_UNION	ou4;
  } os3;

  struct link_type	/* LINK */
  {  LIST		olist[2];
     unsigned char	otype;
     unsigned char	onumber;
     unsigned char	odb_targ;
  } os4;
  
  struct gapobj_type	/* GAP_OBJ */
  {  LIST		olist[2];
     FIRST_UNION	ou1;
     SECOND_UNION	ou2;
     GAP		ogap;
     int		osave_badness;		/* optimum paragraph breaker */
     SHORT_LENGTH	osave_space;		/* optimum paragraph breaker */
     SHORT_LENGTH	osave_actual_gap;	/* optimum paragraph breaker */
     union rec  	*osave_prev;		/* optimum paragraph breaker */
     union rec  	*osave_cwid;		/* optimum paragraph breaker */
  } os5;

  struct symbol_type
  {  LIST		olist[2];
     FIRST_UNION	ou1;
     SECOND_UNION	ou2;
     union rec		*oenclosing;
     union rec		*osym_body;
     union rec		*obase_uses;
     union rec		*ouses;
     union rec		*omarker;
     union rec		*ocross_sym;
     union rec		*oimports;
     union rec		*ofilter;
     union rec		*ouse_invocation;
     short unsigned 	opredefined;
     short unsigned 	ohas_compulsory;
     unsigned char	ouses_count;
     unsigned char	onpar_code;
     BOOLEAN		ois_optimize	     : 1;
     BOOLEAN		ohas_optimize	     : 1;
     BOOLEAN		ois_merge	     : 1;
     BOOLEAN		ohas_merge	     : 1;
     BOOLEAN		ois_enclose	     : 1;
     BOOLEAN		ohas_enclose	     : 1;
     BOOLEAN		ois_compulsory	     : 1;
  } os6;

  struct cr_type
  {  LIST		olist[2];
     unsigned char	otype;
     unsigned char	otarget_state;
     FILE_NUM		otarget_file;
     FILE_NUM		ocr_file;
     union rec		*otarget_val;
     int		otarget_seq;
     int		otarget_pos;
     int		otarget_lnum;
     int		ocr_seq;
     int		ogall_seq;
     union rec		*osymb;
     union rec		*ogall_tag;
     FILE_NUM		ogall_tfile;
  } os7;

  struct ext_gall_type
  {  LIST		olist[2];
     unsigned char	otype;
     FILE_NUM		oeg_fnum;
     int		oeg_lnum;
     long		oeg_fpos;
     long		oeg_cont;
     union rec		*oeg_symbol;
  } os8;

  struct uses_type
  {  union rec	*oitem;
     union rec	*onext;
  } os9;
#define	USES_SIZE ceiling( sizeof(struct uses_type), sizeof(ALIGN) )

  struct hash_entry_type
  {  LIST	olist[1];
  } os10;

} *OBJECT;


/*@::macros for fields of OBJECT@*********************************************/
/*                                                                           */
/*  Macros for fields of OBJECT.                                             */
/*                                                                           */
/*****************************************************************************/

#define	succ(x, dim)		(x)->os1.olist[dim].osucc
#define	pred(x, dim)		(x)->os1.olist[dim].opred

#define type(x)			(x)->os1.ou1.os11.otype
#define	rec_size(x)		(x)->os1.ou1.os11.orec_size
#define	precedence(x)		(x)->os1.ou2.os21.oprecedence
#define	hspace(x)		(x)->os1.ou2.os21.ohspace
#define	vspace(x)		(x)->os1.ou2.os21.ovspace

#define	word_font(x)		(x)->os1.ou2.os22.oword_font
#define	word_colour(x)		(x)->os1.ou2.os22.oword_colour
#define	word_language(x)	(x)->os1.ou2.os22.oword_language
#define	underline(x)		(x)->os1.ou2.os22.ounderline
#define	word_hyph(x)		(x)->os1.ou2.os22.oword_hyph
#define	filter_use_begin(x)	(x)->os1.ou2.os22.oword_colour

#define	non_blocking(x)		(x)->os1.ou2.os23.onon_blocking
#define	vert_sized(x)		non_blocking(x)
#define	sized(x)		(x)->os1.ou2.os23.osized
#define	threaded(x)		(x)->os1.ou2.os23.othreaded
#define	external_ver(x)		(x)->os1.ou2.os23.oexternal_ver
#define	external_hor(x)		(x)->os1.ou2.os23.oexternal_hor
#define	blocked(x)		(x)->os1.ou2.os23.oblocked
#define	seen_nojoin(x)		blocked(x)
#define	trigger_externs(x)	(x)->os1.ou2.os23.otrigger_ext
#define	must_expand(x)		(x)->os1.ou2.os23.omust_expand
#define	gall_dir(x)		(x)->os1.ou2.os23.ogall_dir
#define	opt_hyph(x)		(x)->os1.ou2.os23.oopt_hyph
#define	opt_gazumped(x)		(x)->os1.ou2.os23.oopt_gazumped
#define	adjust_cat(x)		(x)->os1.ou2.os23.oadjust_cat
#define	force_gall(x)		(x)->os1.ou2.os23.oforce_gall
#define	cross_type(x)		(x)->os1.ou2.os23.ocross_type
#define	foll_or_prec(x)		(x)->os1.ou2.os23.ofoll_or_prec
#define	thr_state(x)		cross_type(x)
#define	incgraphic_ok(x)	cross_type(x)

#define	left_pos(x)		(x)->os1.ou2.os24.oleft_pos
#define	reading(x)		(x)->os1.ou2.os24.oreading

#define	is_tag(x)		(x)->os1.ou2.os26.ois_tag
#define	has_tag(x)		(x)->os1.ou2.os26.ohas_tag
#define	has_lpar(x)		(x)->os1.ou2.os26.ohas_lpar
#define	has_rpar(x)		(x)->os1.ou2.os26.ohas_rpar
#define	right_assoc(x)		(x)->os1.ou2.os26.oright_assoc
#define	is_target(x)		(x)->os1.ou2.os26.ois_target
#define	has_target(x)		(x)->os1.ou2.os26.ohas_target
#define	force_target(x)		(x)->os1.ou2.os26.oforce_target
#define	has_body(x)		(x)->os1.ou2.os26.ohas_body
#define	indefinite(x)		(x)->os1.ou2.os26.oindefinite
#define	recursive(x)		(x)->os1.ou2.os26.orecursive
#define	uses_extern_target(x)	(x)->os1.ou2.os26.ouses_extern_target
#define	is_extern_target(x)	(x)->os1.ou2.os26.ois_extern_target
#define	is_key(x)		(x)->os1.ou2.os26.ois_key
#define	has_key(x)		(x)->os1.ou2.os26.ohas_key
#define	dirty(x)		(x)->os1.ou2.os26.odirty
#define	visible(x)		(x)->os1.ou2.os26.ovisible
#define	has_mark(x)		(x)->os1.ou2.os26.ohas_mark
#define	has_join(x)		(x)->os1.ou2.os26.ohas_join
#define	has_par(x)		(x)->os1.ou2.os26.ohas_par
#define	uses_galley(x)		(x)->os1.ou2.os26.ouses_galley
#define	horiz_galley(x)		(x)->os1.ou2.os26.ohoriz_galley

#define	fpos(x)			(x)->os1.ou1.ofpos
#define word_save_mark(x)	(x)->os1.ou1.os11.oword_save_mark

#define	back(x, dim)		(x)->os1.ou3.os31.oback[dim]
#define	comp_count(x)		back(x, COLM)
#define	fwd(x, dim)		(x)->os1.ou3.os31.ofwd[dim]
#define	size(x, dim)		(back(x, dim) + fwd(x, dim))
#define	filep(x)		(x)->os1.ou3.ofilep
#define	filter_actual(x)	(x)->os1.ou3.ofilter_actual
#define	db_checksym(x)		filter_actual(x)

#define	cs_type(x)		(x)->os1.ou3.os33.ocs_type
#define	cs_fnum(x)		(x)->os1.ou3.os33.ocs_fnum
#define	cs_pos(x)		(x)->os1.ou3.os33.ocs_pos
#define	cs_lnum(x)		(x)->os1.ou3.os33.ocs_lnum

#define	gall_rec(x)		(x)->os1.ou3.os33.ogall_rec
#define	gall_type(x)		(x)->os1.ou3.os33.ogall_type
#define	gall_pos(x)		(x)->os1.ou3.os33.ogall_pos

#define string(x)		(x)->os1.ostring

#define	save_style(x)		(x)->os2.ou4.osave_style
#define	constraint(x)		(x)->os2.ou4.oconstraint
#define	shift_type(x)		width(space_gap(save_style(x)))
#define	shift_gap(x)		line_gap(save_style(x))

#define actual(x)		(x)->os2.oactual
#define whereto(x)		(x)->os2.oux.owhereto
#define pinpoint(x)		(x)->os2.oux.opinpoint
#define save_mark(x)		(x)->os2.oux.osave_mark
#define ready_galls(x)		(x)->os2a.oready_galls
#define opt_components(x)	(x)->os2a.oopt_components
#define opt_constraints(x)	(x)->os2a.oopt_constraints
#define opt_counts(x)		(x)->os2a.oopt_counts
#define limiter(x)		(x)->os2a.olimiter
#define enclose_obj(x)		(x)->os2a.oenclose_obj
#define opt_comps_permitted(x)	(x)->os2a.oopt_comps_permitted

#define	number(x)		(x)->os4.onumber
#define	db_targ(x)		(x)->os4.odb_targ

#define	gap(x)			(x)->os5.ogap
#define	save_badness(x)		(x)->os5.osave_badness
#define	save_space(x)		(x)->os5.osave_space
#define	save_actual_gap(x)	(x)->os5.osave_actual_gap
#define	save_prev(x)		(x)->os5.osave_prev
#define	save_cwid(x)		(x)->os5.osave_cwid

#define	enclosing(x)		(x)->os6.oenclosing
#define	sym_body(x)		(x)->os6.osym_body
#define	base_uses(x)		(x)->os6.obase_uses
#define	uses(x)			(x)->os6.ouses
#define	marker(x)		(x)->os6.omarker
#define	cross_sym(x)		(x)->os6.ocross_sym
#define	imports(x)		(x)->os6.oimports
#define	filter(x)		(x)->os6.ofilter
#define	use_invocation(x)	(x)->os6.ouse_invocation
#define	predefined(x)		(x)->os6.opredefined
#define	has_compulsory(x)	(x)->os6.ohas_compulsory
#define	uses_count(x)		(x)->os6.ouses_count
#define	npar_code(x)		(x)->os6.onpar_code
#define	is_optimize(x)		(x)->os6.ois_optimize
#define	has_optimize(x)		(x)->os6.ohas_optimize
#define	is_merge(x)		(x)->os6.ois_merge
#define	has_merge(x)		(x)->os6.ohas_merge
#define	is_enclose(x)		(x)->os6.ois_enclose
#define	has_enclose(x)		(x)->os6.ohas_enclose
#define	is_compulsory(x)	(x)->os6.ois_compulsory

#define	target_state(x)		(x)->os7.otarget_state
#define	target_file(x)		(x)->os7.otarget_file
#define	cr_file(x)		(x)->os7.ocr_file
#define	target_val(x)		(x)->os7.otarget_val
#define	target_seq(x)		(x)->os7.otarget_seq
#define	target_pos(x)		(x)->os7.otarget_pos
#define	target_lnum(x)		(x)->os7.otarget_lnum
#define	cr_seq(x)		(x)->os7.ocr_seq
#define	gall_seq(x)		(x)->os7.ogall_seq
#define	symb(x)			(x)->os7.osymb
#define	gall_tag(x)		(x)->os7.ogall_tag
#define	gall_tfile(x)		(x)->os7.ogall_tfile

#define	eg_fnum(x)		(x)->os8.oeg_fnum
#define	eg_fpos(x)		(x)->os8.oeg_fpos
#define	eg_lnum(x)		(x)->os8.oeg_lnum
#define	eg_cont(x)		(x)->os8.oeg_cont
#define	eg_symbol(x)		(x)->os8.oeg_symbol

#define	item(x)			(x)->os9.oitem
#define	next(x)			(x)->os9.onext

#define	font_num(x)		(x)->os1.ou2.os25.ofont_num
#define	font_page(x)		(x)->os1.ou2.os25.ofont_page
#define	font_size(x)		(x)->os1.ou3.os32.ofont_size
#define	font_xheight2(x)	(x)->os1.ou3.os32.ofont_xheight2
#define	font_spacewidth(x)	(x)->os1.ou3.os32.ofont_spacewidth
#define	font_mapping(x)		(x)->os1.ou3.os32.ofont_mapping
#define	font_recoded(x)		(x)->os1.ou3.os32.ofont_recoded
#define	font_firstpage(x)	(x)->os1.ou3.os32.ofont_firstpage


/*@::FONT_INFO@***************************************************************/
/*                                                                           */
/*  typedef FONT_INFO - information about font metrics etc.  Really private  */
/*  but shared between z37.c and z24.c                                       */
/*                                                                           */
/*****************************************************************************/

struct metrics {
  SHORT_LENGTH up;
  SHORT_LENGTH down;
  SHORT_LENGTH left;
  SHORT_LENGTH right;
  SHORT_LENGTH last_adjust;
};

typedef struct font_rec {
  struct metrics	*size_table;		/* metrics of sized fonts    */
  FULL_CHAR		*lig_table;		/* ligatures                 */
  OBJECT		font_table;		/* record of sized fonts     */
  OBJECT		original_font;		/* font rec before resizing  */
  SHORT_LENGTH		underline_pos;		/* position of underline     */
  SHORT_LENGTH		underline_thick;	/* thickness of underline    */
  unsigned short	*kern_table;		/* first kerning chars       */
  FULL_CHAR		*kern_chars;		/* second kerning chars      */
  unsigned char		*kern_value;		/* points into kern_lengths  */
  SHORT_LENGTH		*kern_sizes;		/* sizes of kernings         */
} FONT_INFO;


/*@::MAP_VEC@*****************************************************************/
/*                                                                           */
/*  typedef MAP_VEC - information about character mappings.  Really private  */
/*  to z38.c but (for efficiency) shared with z37.c and z24.c                */
/*                                                                           */
/*****************************************************************************/

#define MAX_CHASH       353             /* size of hash table                */
#define MAP_UPPERCASE     0             /* the map to upper case             */
#define MAP_LOWERCASE     1             /* the map to lower case             */
#define MAP_UNACCENTED    2             /* the map to unaccented             */
#define MAP_ACCENT        3             /* the map to the accent character   */
#define MAPS              4             /* the number of maps in each file   */
 
typedef struct mapvec {
  OBJECT        file_name;              /* name of file containing the vec   */
  FILE_NUM      fnum;                   /* the file number of this file      */
  BOOLEAN       must_print;             /* TRUE if this vec must be printed  */
  OBJECT        name;                   /* PostScript name of encoding vec   */
  OBJECT        vector[MAX_CHARS];      /* character names                   */
  FULL_CHAR     hash_table[MAX_CHASH];  /* character hash table for inverse  */
  FULL_CHAR     map[MAPS][MAX_CHARS];   /* the mappings                      */
} *MAP_VEC;


/*@::object types@************************************************************/
/*                                                                           */
/*  OBJECT, TOKEN AND OTHER TYPES inhabiting type(x) and predefined(x)       */
/*                                                                           */
/*  Key letters in the adjacent comment indicate where the tag is legal:     */
/*                                                                           */
/*    t  a token type, pushed on token stack                                 */
/*    o  an object type (returned by reduce(), inserted by Manifest)         */
/*    i  an index type (a child of a galley header other than an object)     */
/*    s  a predefined symbol (some symbol table entry has this predefined()) */
/*    n  an indefinite object i.e. one which is ignored in catenation ops    */
/*                                                                           */
/*****************************************************************************/

#define	LINK		    0		/*        a link between objects     */
#define	GAP_OBJ		    1		/*  o     a gap object               */
#define	CLOSURE		    2		/* to  n  a closure of a symbol      */
#define	UNDER_REC	    3		/*  o  n  record of underlining      */
#define	PAGE_LABEL	    4		/* to sn  @PageLabel                 */
#define	NULL_CLOS	    5		/* to sn  @Null                      */
#define	CROSS		    6		/* to sn  && (a cross reference obj) */
#define	FORCE_CROSS	    7		/* to sn  &&& (a forcing cross ref.) */
#define	HEAD		    8		/*  o  n  a galley header            */
#define	SPLIT		    9		/*  o     @Split                     */
#define	PAR		   10		/*  o     a parameter of a closure   */
#define	WORD		   11		/*  o     a word                     */
#define	QWORD		   12		/*  o     a word (was quoted in i/p) */
#define	ROW_THR		   13		/*  o     a row thread               */
#define	COL_THR		   14		/*  o     a column thread            */
#define	ACAT		   15		/* to s   a sequence of &-ed objs    */
#define	HCAT		   16		/* to s   a sequence of |-ed objs    */
#define	VCAT		   17		/* to s   a sequence of /-ed objs    */
#define	ONE_COL		   18		/* to s   @OneCol                    */
#define	ONE_ROW		   19		/* to s   @OneRow                    */
#define	WIDE		   20		/* to s   @Wide                      */
#define	HIGH		   21		/* to s   @High                      */
#define	HSHIFT		   22		/* to s   @HShift                    */
#define	VSHIFT		   23		/* to s   @VShift                    */
#define	HSCALE		   24		/* to s   @HScale                    */
#define	VSCALE		   25		/* to s   @VScale                    */
#define	HCOVER		   26		/* to s   @HCover                    */
#define	VCOVER		   27		/* to s   @VCover                    */
#define	SCALE		   28		/* to s   @Scale                     */
#define	KERN_SHRINK	   29		/* to s   @KernShrink                */
#define	HCONTRACT	   30		/* to s   @HContract                 */
#define	VCONTRACT	   31		/* to s   @VContract                 */
#define	HEXPAND		   32		/* to s   @HExpand                   */
#define	VEXPAND		   33		/* to s   @VExpand                   */
#define	PADJUST		   34		/* to s   @PAdjust                   */
#define	HADJUST		   35		/* to s   @HAdjust                   */
#define	VADJUST		   36		/* to s   @VAdjust                   */
#define	ROTATE		   37		/* to s   @Rotate                    */
#define	CASE		   38		/* to s   @Case                      */
#define	YIELD		   39		/* to s   @Yield                     */
#define	BACKEND		   40		/* to s   @BackEnd                   */
#define	FILTERED	   41		/* to s   filtered object (no name)  */
#define	XCHAR		   42		/* to s   @Char                      */
#define	FONT		   43		/* to s   @Font                      */
#define	SPACE		   44		/* to s   @Space                     */
#define	YUNIT		   45		/* to s   @YUnit                     */
#define	ZUNIT		   46		/* to s   @ZUnit                     */
#define	BREAK		   47		/* to s   @Break                     */
#define	UNDERLINE	   48		/* to s   @Underline                 */
#define	COLOUR		   49		/* to s   @SetColour and @SetColor   */
#define	LANGUAGE	   50		/* to s   @Language                  */
#define	CURR_LANG	   51		/* to s   @CurrLang                  */
#define	CURR_FAMILY	   52		/* to s   @CurrFamily                */
#define	CURR_FACE	   53		/* to s   @CurrFace                  */
#define	COMMON		   54		/* to s   @Common                    */
#define	RUMP		   55		/* to s   @Rump                      */
#define	INSERT		   56		/* to s   @Insert                    */
#define	NEXT		   57		/* to s   @Next                      */
#define	PLUS		   58		/* to s   @Plus                      */
#define	MINUS		   59		/* to s   @Minus                     */
#define	ENV_OBJ		   60		/* to s   object with envt (no name) */
#define	ENV		   61		/* to s   @LEnv                      */
#define	ENVA		   62		/* to s   @LEnvA                     */
#define	ENVB		   63		/* to s   @LEnvB                     */
#define	ENVC		   64		/* to s   @LEnvC                     */
#define	ENVD		   65		/* to s   @LEnvD                     */
#define	CENV		   66		/* to s   @LCEnv                     */
#define	CLOS		   67		/* to s   @LClos                     */
#define	LVIS		   68		/* to s   @LVis                      */
#define	LUSE		   69		/* to s   @LUse                      */
#define	LEO 		   70		/* to s   @LEO                       */
#define	OPEN		   71		/* to s   @Open                      */
#define	TAGGED		   72		/* to s   @Tagged                    */
#define	INCGRAPHIC	   73		/* to s   @IncludeGraphic            */
#define	SINCGRAPHIC	   74		/* to s   @SysIncludeGraphic         */
#define	GRAPHIC		   75		/* to s   @Graphic                   */

#define	TSPACE		   76		/* t      a space token, parser only */
#define	TJUXTA		   77		/* t      a juxta token, parser only */
#define	LBR		   78		/* t  s   left brace token           */
#define	RBR		   79		/* t  s   right brace token          */
#define	BEGIN		   80		/* t  s   @Begin token               */
#define	END		   81		/* t  s   @End token                 */
#define	USE		   82		/* t  s   @Use                       */
#define	NOT_REVEALED	   83		/* t  s   @NotRevealed               */
#define	GSTUB_NONE	   84		/* t      a galley stub, no rpar     */
#define	GSTUB_INT	   85		/* t      galley stub internal rpar  */
#define	GSTUB_EXT	   86		/* t      galley stub external rpar  */
#define	UNEXPECTED_EOF	   87		/* t      unexpected end of file     */
#define	INCLUDE		   88		/*    s   @Include                   */
#define	SYS_INCLUDE	   89		/*    s   @SysInclude                */
#define	PREPEND		   90		/*    s   @Prepend                   */
#define	SYS_PREPEND	   91		/*    s   @SysPrepend                */
#define	DATABASE	   92		/*    s   @Database                  */
#define	SYS_DATABASE	   93		/*    s   @SysDatabase               */
#define	START		   94		/*    s   \Start                     */

#define	DEAD		   95		/*   i    a dead galley              */
#define	UNATTACHED	   96		/*   i    an inner, unsized galley   */
#define	RECEPTIVE	   97		/*   i    a receptive object index   */
#define	RECEIVING	   98		/*   i    a receiving object index   */
#define	RECURSIVE	   99		/*   i    a recursive definite obj.  */
#define	PRECEDES	  100		/*   i    an ordering constraint     */
#define	FOLLOWS		  101		/*   i    other end of ordering c.   */
#define	CROSS_LIT	  102		/*   i    literal word cross-ref     */
#define	CROSS_FOLL	  103		/*   i    following type cross-ref   */
#define	CROSS_FOLL_OR_PREC 104		/*   i    follorprec type cross-ref  */
#define	GALL_FOLL	  105		/*   i    galley with &&following    */
#define	GALL_FOLL_OR_PREC 106		/*   i    galley with &&following    */
#define	CROSS_TARG	  107		/*   i    value of cross-ref         */
#define	GALL_TARG	  108		/*   i    target of these galleys    */
#define	GALL_PREC	  109		/*   i    galley with &&preceding    */
#define	CROSS_PREC	  110		/*   i    preceding type cross-ref   */
#define	PAGE_LABEL_IND	  111		/*   i    index of PAGE_LABEL        */
#define	SCALE_IND	  112		/*   i    index of auto SCALE        */
#define	COVER_IND	  113		/*   i    index of HCOVER or VCOVER  */
#define	EXPAND_IND	  114		/*   i    index of HEXPAND or VEXPD  */
#define	THREAD		  115		/*        a sequence of threads      */
#define	CROSS_SYM	  116		/*        cross-ref info             */
#define	CR_ROOT		  117		/*        RootCross                  */
#define	MACRO	          118		/*        a macro symbol             */
#define	LOCAL	          119		/*        a local symbol             */
#define	LPAR	          120		/*        a left parameter           */
#define	NPAR	          121		/*        a named parameter          */
#define	RPAR	          122		/*        a right parameter          */
#define	EXT_GALL          123		/*        an external galley         */
#define	CR_LIST	          124		/*        a list of cross references */
#define	DISPOSED          125		/*        a disposed record          */

#define is_indefinite(x)  ((x) >= CLOSURE && (x) <= HEAD)
#define is_definite(x) 	 ((x) >= SPLIT && (x) <= GRAPHIC)
#define	is_par(x)	((x) >= LPAR   && (x) <= RPAR)
#define	is_index(x)	((x) >= DEAD && (x) <= EXPAND_IND)
#define	is_type(x)	((x) >= LINK && (x) < DISPOSED)
#define	is_word(x)	((x) == WORD || (x) == QWORD)
#define	is_cross(x)	((x) == CROSS || (x) == FORCE_CROSS)
#define is_cat_op(x)    (((x)>=ACAT && (x)<=VCAT) || (x)==TSPACE || (x)<=TJUXTA)


/*@::miscellaneous constants@*************************************************/
/*                                                                           */
/*  Miscellaneous globally defined constants                                 */
/*                                                                           */
/*****************************************************************************/

/* gap modes occupying mode(x) */
#define	NO_MODE		0		/* for error detection: no mode      */
#define	EDGE_MODE	1		/* edge-to-edge spacing              */
#define	HYPH_MODE	2		/* edge-to-edge with hyphenation     */
#define	MARK_MODE	3		/* mark-to-mark spacing              */
#define	OVER_MODE	4		/* overstrike spacing                */
#define	KERN_MODE	5		/* kerning spacing                   */
#define	TAB_MODE	6		/* tabulation spacing                */
#define	ADD_HYPH	7		/* temp value used by FillObject     */

/* hyph_style(style) options                                                 */
#define	HYPH_UNDEF	0		/* hyphenation option undefined      */
#define	HYPH_OFF	1		/* hyphenation off                   */
#define	HYPH_ON		2		/* hyphenation on                    */

/* fill_style(style) options                                                 */
#define	FILL_UNDEF	0		/* fill option undefined             */
#define	FILL_OFF	1		/* no filling of lines               */
#define	FILL_ON		2		/* fill lines with text              */

/* spoace_style(style) options                                               */
#define	SPACE_LOUT	0		/* interpret white space Lout's way  */
#define	SPACE_COMPRESS	1		/* compress multiple white spaces    */
#define	SPACE_SEPARATE	2		/* compress an separate              */
#define	SPACE_TROFF	3		/* interpret white space troff's way */
#define	SPACE_TEX	4		/* interpret white space TeX's way   */

/* display_style(style) options                                              */
#define	DISPLAY_UNDEF	0		/* display option undefined          */
#define	DISPLAY_ADJUST	1		/* adjust lines (except last)        */
#define	DISPLAY_OUTDENT	2		/* outdent lines (except first)      */
#define	DISPLAY_ORAGGED	3		/* outdent but don't adjust          */
#define	DISPLAY_LEFT	4		/* left-justify lines, no adjust     */
#define	DISPLAY_CENTRE	5		/* centre lines, no adjust           */
#define	DISPLAY_RIGHT	6		/* right-justify lines, no adjust    */
#define	DO_ADJUST	7		/* placed in ACATs when adjust need  */

/* small_caps(style) options                                                 */
#define	SMALL_CAPS_OFF	 0		/* don't want small capitals         */
#define	SMALL_CAPS_ON	 1		/* small capitals                    */

/* sides of a mark */
#define	BACK	          126		/* means lies to left of mark        */
#define	ON	          127		/* means lies on mark                */
#define	FWD	          128		/* means lies to right of mark       */

/* statuses of thread objects */
#define	NOTSIZED	 0		/* this thread object is not sized   */
#define	SIZED		 1		/* thread is sized but not printed   */
#define	FINALSIZE	 2		/* thread object size is now final   */

/* constraint statuses */
#define	PROMOTE	          129		/* this component may be promoted    */
#define	CLOSE	          130		/* must close dest before promoting  */
#define	BLOCK	          131		/* cannot promote this component     */
#define	CLEAR	          132		/* this constraint is now satisfied  */

/* gap increment types */
#define	GAP_ABS	          133		/* absolute,  e.g.  3p               */
#define	GAP_INC	          134		/* increment, e.g. +3p               */
#define	GAP_DEC	          135		/* decrement, e.g. -3p               */

/* file types */
#define	SOURCE_FILE	 0		/* input file from command line      */
#define	INCLUDE_FILE	 1		/* @Include file                     */
#define	INCGRAPHIC_FILE	 2		/* @IncludeGraphic file              */
#define	DATABASE_FILE	 3		/* database file                     */
#define	INDEX_FILE	 4		/* database index file               */
#define	FONT_FILE	 5		/* font file                         */
#define	PREPEND_FILE	 6		/* PostScript prologue file          */
#define	HYPH_FILE	 7		/* hyphenation file                  */
#define	HYPH_PACKED_FILE 8		/* packed hyphenation file           */
#define	MAPPING_FILE	 9		/* character mapping file            */
#define	FILTER_FILE	10		/* filter output file                */
#define	MAX_TYPES	11		/* number of file types              */

/* path types (i.e. sequences of directories for file searching) */
#define	SOURCE_PATH	 0		/* path to search for source files   */
#define	INCLUDE_PATH	 1		/* path for @Include files           */
#define	SYSINCLUDE_PATH	 2		/* path for @SysInclude files        */
#define	DATABASE_PATH	 3		/* path for @Database files          */
#define	SYSDATABASE_PATH 4		/* path for @SysDatabase files       */
#define	FONT_PATH	 5		/* path for font metrics (AFM) files */
#define	HYPH_PATH	 6		/* path for hyphenation files        */
#define	MAPPING_PATH	 7		/* path for mapping (LCM) files      */
#define	MAX_PATHS	 8		/* number of mapping paths           */

/* units of measurement */
#define	NO_UNIT		 0		/* no unit - for error detection     */
#define	FIXED_UNIT	 1		/* inches, cm, points, ems, y, z     */
#define	FRAME_UNIT	 2		/* b unit (frame widths)             */
#define	AVAIL_UNIT	 3		/* r unit (available spaces)         */
#define	DEG_UNIT	 4		/* d unit (degrees)                  */
#define	NEXT_UNIT	 5		/* w unit (inners)                   */
 
/* units of distance as multiples of the basic unit */
#define	CM	       567		/* 1 centimetre                      */
#define	IN	      1440		/* 1 inch                            */
#define	EM	       120		/* 1 em (= 1/12 inch)                */
#define	PT		20		/* 1 point (= 1/72 inch)             */
#define	FR	      4096		/* virtual unit for frame units      */
#define	DG	       128		/* virtual unit for degrees          */
#define	SF	       128		/* virtual unit for @Scale factors   */

/* default size of characters for the PLAINTEXT back end */
#define	PLAIN_WIDTH    144		/* default char width, 10 per inch   */
#define	PLAIN_HEIGHT   240		/* default char height, 6 per inch   */

/* precedences */
#define	NO_PREC		 0		/* lower than any precedence         */
#define	BEGIN_PREC	 1		/* precedence of @Begin              */
#define	END_PREC	 2		/* precedence of @End                */
#define	LBR_PREC	 3		/* precedence of {                   */
#define	RBR_PREC	 4		/* precedence of }                   */
#define	VCAT_PREC	 5		/* precedence of /                   */
#define	HCAT_PREC	 6		/* precedence of |                   */
#define	ACAT_PREC	 7		/* precedence of & and white space   */
#define	MIN_PREC        10		/* minimum precedence of user ops    */
#define	MAX_PREC       100		/* maximim precedence of user ops    */
#define	DEFAULT_PREC   100		/* default precedence of user ops    */
#define CROSSOP_PREC   101		/* precedence of && and &&& ops      */
#define GAP_PREC       102		/* precedence of gap op after cat op */
#define JUXTA_PREC     103		/* precedence of juxtaposition &     */
#define	FORCE_PREC     104		/* higher than any precedence        */

/* back ends */
#define POSTSCRIPT       0		/* PostScript back end               */
#define	PLAINTEXT	 1		/* plain text back end               */
#define	PDF		 2

/* error types */
#define	INTERN	0			/* internal error (i.e. bug)         */
#define	FATAL	1			/* fatal error, abort now            */
#define	WARN	2			/* warning, non-fatal                */

/* status values returned by AttachGalley() */
#define	ATTACH_KILLED	 0
#define	ATTACH_INPUT	 1
#define	ATTACH_NOTARGET	 2
#define	ATTACH_SUSPEND	 3
#define	ATTACH_NULL	 4
#define	ATTACH_ACCEPT	 5

/* types of memory usage, used to debug memory consumption */
#define	MEM_BINARY	 0		/* the executable binary	     */
#define	MEM_OBJECTS	 1		/* objects currently in free list    */
#define	MEM_FONTS	 2		/* fonts                             */
#define	MEM_LEX		 3		/* lexical analyser file buffers     */
#define	MEM_FILES	 4		/* table of file names               */
#define	MEM_CROSSREF	 5		/* table of file names               */
#define	MEM_PAGES	 6		/* page grids (-p only)              */
#define	MEM_DBCHECK	 7		/* page grids (-p only)              */
#define	MEM_HYPH_PATS	 8		/* hyphenation patterns              */
#define	MEM_CMAPS	 9		/* character maps                    */
#define	MEM_COLOUR_TAB	10		/* colour table                      */
#define	MEM_LANG_TAB	11		/* language table                    */
#define	MEM_USAGE_MAX	12		/* number of memory usage types      */

/*@::Keywords@****************************************************************/
/*                                                                           */
/*  Keywords.                                                                */
/*                                                                           */
/*****************************************************************************/

#define	KW_START		AsciiToFull("\\Start")
#define	KW_PRINT		AsciiToFull("\\Print")
#define	KW_OPTGALL		AsciiToFull("@OptGall")
#define	KW_DEF			AsciiToFull("def")
#define	KW_FONTDEF		AsciiToFull("fontdef")
#define	KW_LANGDEF		AsciiToFull("langdef")
#define	KW_FORCE		AsciiToFull("force")
#define	KW_HORIZ		AsciiToFull("horizontally")
#define	KW_INTO			AsciiToFull("into")
#define	KW_EXTEND		AsciiToFull("extend")
#define	KW_IMPORT		AsciiToFull("import")
#define	KW_EXPORT		AsciiToFull("export")
#define	KW_PRECEDENCE		AsciiToFull("precedence")
#define	KW_ASSOC		AsciiToFull("associativity")
#define	KW_LEFT			AsciiToFull("left")
#define	KW_RIGHT		AsciiToFull("right")
#define	KW_BODY			AsciiToFull("body")
#define	KW_FILTER		AsciiToFull("@Filter")
#define	KW_FILTERIN		AsciiToFull("@FilterIn")
#define	KW_FILTEROUT		AsciiToFull("@FilterOut")
#define	KW_FILTERERR		AsciiToFull("@FilterErr")
#define	KW_MACRO		AsciiToFull("macro")
#define	KW_NAMED		AsciiToFull("named")
#define	KW_COMPULSORY		AsciiToFull("compulsory")
#define	KW_COMMON		AsciiToFull("@Common")
#define	KW_RUMP			AsciiToFull("@Rump")
#define	KW_INSERT		AsciiToFull("@Insert")
#define	KW_NEXT			AsciiToFull("@Next")
#define	KW_PLUS			AsciiToFull("@Plus")
#define	KW_MINUS		AsciiToFull("@Minus")
#define	KW_WIDE			AsciiToFull("@Wide")
#define	KW_HIGH			AsciiToFull("@High")
#define	KW_HSHIFT		AsciiToFull("@HShift")
#define	KW_VSHIFT		AsciiToFull("@VShift")
#define	KW_ONE_COL		AsciiToFull("@OneCol")
#define	KW_ONE_ROW		AsciiToFull("@OneRow")
#define	KW_HSCALE		AsciiToFull("@HScale")
#define	KW_VSCALE		AsciiToFull("@VScale")
#define	KW_HCOVER		AsciiToFull("@HCover")
#define	KW_VCOVER		AsciiToFull("@VCover")
#define	KW_SCALE		AsciiToFull("@Scale")
#define	KW_KERN_SHRINK		AsciiToFull("@KernShrink")
#define	KW_HCONTRACT		AsciiToFull("@HContract")
#define	KW_VCONTRACT		AsciiToFull("@VContract")
#define	KW_HEXPAND		AsciiToFull("@HExpand")
#define	KW_VEXPAND		AsciiToFull("@VExpand")
#define	KW_PADJUST		AsciiToFull("@PAdjust")
#define	KW_HADJUST		AsciiToFull("@HAdjust")
#define	KW_VADJUST		AsciiToFull("@VAdjust")
#define	KW_ROTATE		AsciiToFull("@Rotate")
#define	KW_INCGRAPHIC		AsciiToFull("@IncludeGraphic")
#define	KW_SINCGRAPHIC		AsciiToFull("@SysIncludeGraphic")
#define	KW_GRAPHIC		AsciiToFull("@Graphic")
#define	KW_CASE			AsciiToFull("@Case")
#define	KW_YIELD		AsciiToFull("@Yield")
#define	KW_BACKEND		AsciiToFull("@BackEnd")
#define	KW_XCHAR		AsciiToFull("@Char")
#define	KW_FONT			AsciiToFull("@Font")
#define	KW_SPACE		AsciiToFull("@Space")
#define	KW_YUNIT		AsciiToFull("@YUnit")
#define	KW_ZUNIT		AsciiToFull("@ZUnit")
#define	KW_BREAK		AsciiToFull("@Break")
#define	KW_UNDERLINE		AsciiToFull("@Underline")
#define	KW_COLOUR		AsciiToFull("@SetColour")
#define	KW_COLOR		AsciiToFull("@SetColor")
#define	KW_LANGUAGE		AsciiToFull("@Language")
#define	KW_CURR_LANG		AsciiToFull("@CurrLang")
#define	KW_CURR_FAMILY		AsciiToFull("@CurrFamily")
#define	KW_CURR_FACE		AsciiToFull("@CurrFace")
#define	KW_ENV			AsciiToFull("@LEnv")
#define	KW_ENVA			AsciiToFull("@@A")
#define	KW_ENVB			AsciiToFull("@@B")
#define	KW_ENVC			AsciiToFull("@@C")
#define	KW_ENVD			AsciiToFull("@@D")
#define	KW_CENV			AsciiToFull("@@E")
#define	KW_CLOS			AsciiToFull("@LClos")
#define	KW_LVIS			AsciiToFull("@@V")
#define	KW_LUSE			AsciiToFull("@LUse")
#define	KW_LEO			AsciiToFull("@LEO")
#define	KW_OPEN			AsciiToFull("@Open")
#define	KW_USE			AsciiToFull("@Use")
#define	KW_NOT_REVEALED		AsciiToFull("@NotRevealed")
#define	KW_TAGGED		AsciiToFull("@Tagged")
#define	KW_DATABASE		AsciiToFull("@Database")
#define	KW_SYSDATABASE		AsciiToFull("@SysDatabase")
#define	KW_INCLUDE		AsciiToFull("@Include")
#define	KW_SYSINCLUDE		AsciiToFull("@SysInclude")
#define	KW_PREPEND		AsciiToFull("@PrependGraphic")
#define	KW_SYSPREPEND		AsciiToFull("@SysPrependGraphic")
#define	KW_TARGET		AsciiToFull("@Target")
#define	KW_FOLLOWING		AsciiToFull("following")
#define	KW_PRECEDING		AsciiToFull("preceding")
#define	KW_FOLL_OR_PREC		AsciiToFull("foll_or_prec")
#define	KW_NOW			AsciiToFull("now")
#define	KW_NULL			AsciiToFull("@Null")
#define	KW_PAGE_LABEL		AsciiToFull("@PageLabel")
#define	KW_GALLEY		AsciiToFull("@Galley")
#define	KW_FORCE_GALLEY		AsciiToFull("@ForceGalley")
#define	KW_INPUT		AsciiToFull("@LInput")
#define	KW_SPLIT		AsciiToFull("@Split")
#define	KW_TAG			AsciiToFull("@Tag")
#define	KW_KEY			AsciiToFull("@Key")
#define	KW_OPTIMIZE		AsciiToFull("@Optimize")
#define	KW_MERGE		AsciiToFull("@Merge")
#define	KW_ENCLOSE		AsciiToFull("@Enclose")
#define	KW_CROSS		AsciiToFull("&&")
#define	KW_FORCE_CROSS		AsciiToFull("&&&")
#define	KW_LBR			AsciiToFull("{")
#define	KW_RBR			AsciiToFull("}")
#define	KW_BEGIN		AsciiToFull("@Begin")
#define	KW_END			AsciiToFull("@End")
#define	KW_VCAT_NN		AsciiToFull("//")
#define	KW_VCAT_MN		AsciiToFull("^//")
#define	KW_VCAT_NJ		AsciiToFull("/")
#define	KW_VCAT_MJ		AsciiToFull("^/")
#define	KW_HCAT_NN		AsciiToFull("||")
#define	KW_HCAT_MN		AsciiToFull("^||")
#define	KW_HCAT_NJ		AsciiToFull("|")
#define	KW_HCAT_MJ		AsciiToFull("^|")
#define	KW_ACAT_NJ		AsciiToFull("&")
#define	KW_ACAT_MJ		AsciiToFull("^&")
#define	KW_MOMENT		AsciiToFull("@Moment")
#define	KW_SECOND		AsciiToFull("@Second")
#define	KW_MINUTE		AsciiToFull("@Minute")
#define	KW_HOUR			AsciiToFull("@Hour")
#define	KW_DAY			AsciiToFull("@Day")
#define	KW_MONTH		AsciiToFull("@Month")
#define	KW_YEAR			AsciiToFull("@Year")
#define	KW_CENTURY		AsciiToFull("@Century")
#define	KW_WEEKDAY		AsciiToFull("@WeekDay")
#define	KW_YEARDAY		AsciiToFull("@YearDay")
#define	KW_DAYLIGHTSAVING	AsciiToFull("@DaylightSaving")

/*@::GetMem(), New(), NewWord()@**********************************************/
/*                                                                           */
/*  GetMem(x, siz, pos)                                                      */
/*  New(x, typ)                                                              */
/*  NewWord(x, typ, len, pos)                                                */
/*                                                                           */
/*  Set x to point to a new record, of appropriate length (in ALIGNs).       */
/*  The New and NewWord versions initialise LIST, type and rec_size fields.  */
/*  NewWord must be used for WORD and QWORD objects.                         */
/*                                                                           */
/*****************************************************************************/

#if DEBUG_ON
#define newcount zz_newcount++
#define freecount zz_listcount--

#define checknew(typ)							\
{ assert1( is_type(typ), "New: type", Image(typ) );			\
  assert(  zz_lengths[typ] > 0, "New: zero length!" );			\
}

#define checkmem(z, typ)						\
{ if( (MemCheck != 0) && ( (POINTER) z == MemCheck) )			\
    fprintf(stderr, "%ld = New(%s)\n", (long) z, Image(type(z)));	\
}

#else
#define newcount
#define freecount
#define checknew(typ)
#define checkmem(z, typ)
#endif

#define	GetMem(x, siz, pos)						\
{ newcount;								\
  if( (zz_size=(siz)) >= MAX_OBJECT_REC )				\
    Error(1, 1, "word is too long", FATAL, pos);			\
  else if( zz_free[zz_size] == nilobj )					\
    x = GetMemory(zz_size, pos);					\
  else									\
  { x = zz_hold = zz_free[zz_size];					\
    freecount;								\
    zz_free[zz_size] = pred(zz_hold, CHILD);				\
  }									\
}

#define	New(x, typ)							\
{ checknew(typ);							\
  GetMem(zz_hold, zz_lengths[typ], no_fpos);				\
  type(zz_hold) = typ;							\
  checkmem(zz_hold, typ);						\
  x = pred(zz_hold, CHILD) = succ(zz_hold, CHILD) =			\
  pred(zz_hold, PARENT) = succ(zz_hold, PARENT) = zz_hold;		\
}

#define NewWord(x, typ, len, pos)					\
{ zz_size = sizeof(struct word_type) - 4 + ((len)+1)*sizeof(FULL_CHAR);	\
  /* NB the following line RESETS zz_size */				\
  GetMem(zz_hold, ceiling(zz_size, sizeof(ALIGN)), pos);		\
  checkmem(zz_hold, typ);						\
  rec_size(zz_hold) = zz_size;						\
  type(zz_hold) = typ;							\
  x = pred(zz_hold, CHILD) = succ(zz_hold, CHILD) =			\
  pred(zz_hold, PARENT) = succ(zz_hold, PARENT) = zz_hold;		\
}

/*@::PutMem(), Dispose()@*****************************************************/
/*                                                                           */
/*  PutMem(x, siz)                                                           */
/*  Dispose(x)                                                               */
/*                                                                           */
/*  Dispose x, which is of size siz.  Dispose works out the size itself.     */
/*                                                                           */
/*****************************************************************************/
#if DEBUG_ON
#define disposecount zz_disposecount++; zz_listcount++;

#define disposecheck							\
{ assert( zz_size >= 0 && zz_size < MAX_OBJECT_REC, "Dispose: size" );	\
}
 
#define	setdisposed							\
{ if( (MemCheck != 0) && ((POINTER) zz_hold == MemCheck) )		\
    fprintf(stderr, "Dispose(%ld, %s)\n", (long) zz_hold,		\
      Image(type(zz_hold)));						\
  type(zz_hold) = DISPOSED;						\
}

#else
#define disposecount
#define disposecheck
#define	setdisposed
#endif

#define PutMem(x, siz)							\
{ disposecount;								\
  zz_hold = (x);							\
  zz_size = (siz);							\
  disposecheck;								\
  pred(zz_hold, CHILD) = zz_free[zz_size];				\
  zz_free[zz_size] = zz_hold;						\
}

#define Dispose(x)							\
{ zz_hold = (x);							\
  PutMem(zz_hold, is_word(type(zz_hold)) ?				\
    rec_size(zz_hold) : zz_lengths[type(zz_hold)]);			\
  setdisposed;								\
}

/*@::Append(), Delete()@******************************************************/
/*                                                                           */
/*  OBJECT Append(x, y, dir)                                                 */
/*                                                                           */
/*  Return the append of lists x and y (dir is PARENT or CHILD).             */
/*                                                                           */
/*****************************************************************************/

#define	Append(x, y, dir)						\
( zz_res = (x),	zz_hold = (y),						\
  zz_hold == nilobj ? zz_res  :						\
  zz_res  == nilobj ? zz_hold :						\
  ( zz_tmp = pred(zz_hold, dir),					\
    pred(zz_hold, dir) = pred(zz_res, dir),				\
    succ(pred(zz_res, dir), dir) = zz_hold,				\
    pred(zz_res, dir) = zz_tmp,						\
    succ(zz_tmp, dir) = zz_res						\
  )									\
)


/*****************************************************************************/
/*                                                                           */
/*  OBJECT Delete(x, dir)                                                    */
/*                                                                           */
/*  Delete x from its dir list, and return succ(x, dir) or nilobj if none.   */
/*                                                                           */
/*****************************************************************************/

#define Delete(x, dir)							\
( zz_hold = (x),							\
  succ(zz_hold, dir) == zz_hold ? nilobj :				\
  ( zz_res = succ(zz_hold, dir),					\
    pred(zz_res, dir) = pred(zz_hold, dir),				\
    succ(pred(zz_hold, dir), dir) = zz_res,				\
    pred(zz_hold, dir) = succ(zz_hold, dir) = zz_hold,			\
    zz_res								\
  )									\
)

#define Down(x)		succ(x, CHILD)
#define NextDown(x)	succ(x, CHILD)
#define LastDown(x)	pred(x, CHILD)
#define PrevDown(x)	pred(x, CHILD)
#define	Up(x)		succ(x, PARENT)
#define	NextUp(x)	succ(x, PARENT)
#define	LastUp(x)	pred(x, PARENT)
#define	PrevUp(x)	pred(x, PARENT)

#define	Child(y, link)							\
for( y = pred(link, PARENT);  type(y) == LINK;  y = pred(y, PARENT) )

#define	Parent(y, link)							\
for( y = pred(link, CHILD);   type(y) == LINK;  y = pred(y, CHILD) )


/*@::UpDim(), DownDim(), Link(), DeleteLink(), etc.@**************************/
/*                                                                           */
/*  UpDim(x, dim)                                                            */
/*  DownDim(x, dim)                                                          */
/*                                                                           */
/*  Returns the dim child or parent link of node x (dim == COLM or ROWM).    */
/*                                                                           */
/*****************************************************************************/

#define UpDim(x, dim)	( (dim) == COLM ? succ(x, PARENT) : pred(x, PARENT) )
#define DownDim(x, dim)	( (dim) == COLM ? succ(x, CHILD) : pred(x, CHILD) )


/*****************************************************************************/
/*                                                                           */
/*  OBJECT Link(x, y)                                                        */
/*                                                                           */
/*  Make y a child of x in the directed graph, using a new link.             */
/*  The link node is returned.                                               */
/*                                                                           */
/*****************************************************************************/

#define Link(x, y)							\
{ New(xx_link, LINK);							\
  Append(xx_link, (x), CHILD);						\
  Append(xx_link, (y), PARENT);						\
}


/*****************************************************************************/
/*                                                                           */
/*  DeleteLink(link)                                                         */
/*                                                                           */
/*  Cut the link between nodes x and y of the directed graph.                */
/*  Returns the link node of the next child of x, or x if none.              */
/*                                                                           */
/*****************************************************************************/

#define DeleteLink(link)						\
{ xx_link = (link);							\
  Delete(xx_link, PARENT);						\
  Delete(xx_link, CHILD);						\
  Dispose(xx_link);							\
}


/*****************************************************************************/
/*                                                                           */
/*  DisposeChild(link)                                                       */
/*                                                                           */
/*  Delete link, and if its child is thereby unattached, dispose it.         */
/*                                                                           */
/*****************************************************************************/

#define DisposeChild(link)						\
{ xx_link = (link);							\
  xx_tmp = Delete(xx_link, PARENT);					\
  Delete(xx_link, CHILD);						\
  Dispose(xx_link);							\
  if( succ(xx_tmp, PARENT) == xx_tmp )  DisposeObject(xx_tmp);		\
} /* end DisposeChild */


/*****************************************************************************/
/*                                                                           */
/*  MoveLink(link, x, dir)                                                   */
/*                                                                           */
/*  Move the dir end of link from wherever it is now to node x.              */
/*                                                                           */
/*****************************************************************************/

#define MoveLink(link, x, dir)						\
( xx_link = (link),							\
  Delete(xx_link, 1 - (dir) ),						\
  Append(xx_link, (x), 1 - (dir) )					\
) /* end MoveLink */


/*@::TransferLinks(), DeleteNode(), etc.@*************************************/
/*                                                                           */
/*  TransferLinks(start_link, stop_link, dest_link)                          */
/*                                                                           */
/*  Move parent end of links start_link (inclusive) to stop_link (exclusive) */
/*  to just before dest_link.                                                */
/*                                                                           */
/*****************************************************************************/

#define TransferLinks(start_link, stop_link, dest_link)			\
{ OBJECT xxstart = start_link, xxstop = stop_link, xxdest = dest_link;	\
  if( xxstart != xxstop )						\
  {	assert( type(xxstart) == LINK, "TransferLinks: start_link!" );	\
	Append(xxstart, xxstop, CHILD); /* actually a split */		\
	Append(xxstart, xxdest, CHILD);					\
  }									\
}


/*****************************************************************************/
/*                                                                           */
/*  DeleteNode(x)                                                            */
/*                                                                           */
/*  Delete node x and every edge attaching to x.                             */
/*                                                                           */
/*****************************************************************************/

#define DeleteNode(x)							\
{ xx_hold = (x);							\
  while( Up(xx_hold)   != xx_hold ) DeleteLink( Up(xx_hold) );		\
  while( Down(xx_hold) != xx_hold ) DeleteLink( Down(xx_hold) );	\
  Dispose(xx_hold);							\
}


/*****************************************************************************/
/*                                                                           */
/*  MergeNode(x, y)                                                          */
/*                                                                           */
/*  Take all the children of y and make them children of x.                  */
/*  Take all the parents of y and make them parents of x.  Dispose y.        */
/*                                                                           */
/*****************************************************************************/

#define MergeNode(x, y)							\
{ xx_res = (x); xx_hold = (y);						\
  xx_tmp = Delete(xx_hold, PARENT);					\
  Append(xx_res, xx_tmp, PARENT);					\
  xx_tmp = Delete(xx_hold, CHILD);					\
  Append(xx_res, xx_tmp, CHILD);					\
  Dispose(xx_hold);							\
}  /* end MergeNode */


/*****************************************************************************/
/*                                                                           */
/*  ReplaceNode(x, y)                                                        */
/*                                                                           */
/*  Move all the parent links of y to x.                                     */
/*                                                                           */
/*****************************************************************************/

#define ReplaceNode(x, y)						\
( xx_tmp = Delete((y), PARENT),						\
  Append((x), xx_tmp, PARENT)						\
) /* end ReplaceNode */


/*@::FirstDefinite(), NextDefinite(), etc.@***********************************/
/*                                                                           */
/*  FirstDefinite(x, link, y, jn)                                            */
/*                                                                           */
/*  On input, x is an object and link and y are undefined.  On output there  */
/*  are two cases:                                                           */
/*                                                                           */
/*  link != x.  Then y is first definite child of x and link is its link;    */
/*              jn is TRUE iff all gaps on the way to link were joined.      */
/*                                                                           */
/*  link == x.  Then x has no definite child and y is undefined.             */
/*                                                                           */
/*  A SPLIT object is considered to be definite if both its children are     */
/*  definite.  This condition is returned by SplitIsDefinite.                */
/*                                                                           */
/*****************************************************************************/

#define FirstDefinite(x, link, y, jn)					\
{ jn = TRUE;								\
  for( link = Down(x);  link != x;  link = NextDown(link) )		\
  { Child(y, link);							\
    if( type(y) == GAP_OBJ )  jn = jn && join(gap(y));			\
    else if( type(y)==SPLIT ? SplitIsDefinite(y) : is_definite(type(y)))\
      break;								\
  }									\
} /* end FirstDefinite */


/*****************************************************************************/
/*                                                                           */
/*  NextDefinite(x, link, y)                                                 */
/*                                                                           */
/*  On input, x is an object and link is a link to one of its children; y    */
/*  is undefined.  On output there are two cases:                            */
/*                                                                           */
/*  link != x.  Then y is the first definite child of x following link, and  */
/*              link is changed to be the link of y.                         */
/*                                                                           */
/*  link == x.  Then x has no definite child following link, and y remains   */
/*              undefined.                                                   */
/*                                                                           */
/*****************************************************************************/

#define NextDefinite(x, link, y)					\
{ for( link = NextDown(link);  link != x;  link = NextDown(link) )	\
  { Child(y, link);							\
    if( type(y) == SPLIT ? SplitIsDefinite(y) : is_definite(type(y)) )	\
	break;								\
  }									\
} /* end NextDefinite */


/*****************************************************************************/
/*                                                                           */
/*  NextDefiniteWithGap(x, link, y, g, jn)                                   */
/*                                                                           */
/*  On input, x is an object and link is a link to one of its children; y    */
/*  and g are undefined.  On output there are two cases:                     */
/*                                                                           */
/*  link != x.  Then y is the first definite child of x following link, and  */
/*              link is changed to be the link of y.  Also, g is defined     */
/*              to be the gap just before y; this must exist and is tested   */
/*              by an assert test; and jn is true iff all of the gaps on     */
/*              the way from old link to new link are join gaps.             */
/*                                                                           */
/*  link == x.  Then x has no definite child following link, and y and g     */
/*              remain undefined.                                            */
/*                                                                           */
/*****************************************************************************/

#define NextDefiniteWithGap(x, link, y, g, jn)				\
{ g = nilobj;  jn = TRUE;						\
  for( link = NextDown(link);  link != x;  link = NextDown(link) )	\
  { Child(y, link);							\
    if( type(y) == GAP_OBJ )  g = y, jn = jn && join(gap(y));		\
    else if( type(y)==SPLIT ? SplitIsDefinite(y):is_definite(type(y)) )	\
    { assert( g != nilobj, "NextDefinite: g == nilobj!" );		\
      break;								\
    }									\
  }									\
} /* end NextDefiniteWithGap */

/*@@**************************************************************************/
/*                                                                           */
/*  LastDefinite(x, link, y)                                                 */
/*                                                                           */
/*  On input, x is an object and link and y are undefined.  On output there  */
/*  are two cases:                                                           */
/*                                                                           */
/*  link != x.  Then y is the last definite child of x and link is its link. */
/*                                                                           */
/*  link == x.  Then x has no definite child and y is undefined.             */
/*                                                                           */
/*  A SPLIT object is considered to be definite if both its children are     */
/*  definite.  This condition is returned by SplitIsDefinite.                */
/*                                                                           */
/*****************************************************************************/

#define LastDefinite(x, link, y)					\
{ for( link = LastDown(x);  link != x;  link = PrevDown(link) )		\
  { Child(y, link);							\
    if( type(y) == SPLIT ? SplitIsDefinite(y) : is_definite(type(y)) )	\
	break;								\
  }									\
} /* end LastDefinite */


/*****************************************************************************/
/*                                                                           */
/*  PrevDefinite(x, link, y)                                                 */
/*                                                                           */
/*  On input, x is an object and link is a link to one of its children; y    */
/*  is undefined.  On output there are two cases:                            */
/*                                                                           */
/*  link != x.  Then y is the first definite child of x preceding link, and  */
/*              link is changed to be the link of y.                         */
/*                                                                           */
/*  link == x.  Then x has no definite child preceding link, and y remains   */
/*              undefined.                                                   */
/*                                                                           */
/*****************************************************************************/

#define PrevDefinite(x, link, y)					\
{ for( link = PrevDown(link);  link != x;  link = PrevDown(link) )	\
  { Child(y, link);							\
    if( type(y) == SPLIT ? SplitIsDefinite(y) : is_definite(type(y)) )	\
	break;								\
  }									\
} /* end PrevDefinite */


/*@::Module Declarations@*****************************************************/
/*                                                                           */
/*  MODULE DECLARATIONS                                                      */
/*                                                                           */
/*****************************************************************************/

/*****  z01.c	  Supervise		**************************************/
extern		  main(int argc, char *argv[]);
extern	POINTER	  MemCheck;
extern	OBJECT	  StartSym;
extern	OBJECT	  GalleySym;
extern	OBJECT	  ForceGalleySym;
extern	OBJECT	  InputSym;
extern	OBJECT	  PrintSym;
extern	OBJECT	  FilterInSym;
extern	OBJECT	  FilterOutSym;
extern	OBJECT	  FilterErrSym;
extern	OBJECT	  OptGallSym;
extern	OBJECT	  CommandOptions;
extern	BOOLEAN	  AllowCrossDb;
extern	BOOLEAN	  Encapsulated;
extern	BOOLEAN	  Kern;
extern	BOOLEAN	  SafeExecution;
extern	BOOLEAN   AltErrorFormat;
extern	int	  BackEnd;
extern	int	  TotalWordCount;
extern	FULL_CHAR *BackEndWord;
extern	FULL_LENGTH	  PlainCharWidth;
extern	FULL_LENGTH	  PlainCharHeight;
extern	BOOLEAN	  PlainFormFeed;
extern	BOOLEAN	  InitializeAll;
#if LOCALE_ON
extern	nl_catd	  MsgCat;
#endif

/*****  z02.c	  Lexical Analyser	**************************************/
extern	BOOLEAN	  LexLegalName(FULL_CHAR *str);
extern	void	  LexInit(void);
extern	void	  LexPush(FILE_NUM x, int offs, int ftyp, int lnum, BOOLEAN same);
extern	void	  LexPop(void);
extern	long	  LexNextTokenPos(void);
extern	OBJECT	  LexGetToken(void);
extern	void	  LexScanFilter(FILE *fp, BOOLEAN end_stop, FILE_POS *err_pos);

/*****  z03.c	  File Service	        **************************************/
extern	FILE_POS  *no_fpos;
extern	void	  InitFiles(void);
extern	void	  AddToPath(int fpath, OBJECT dirname);
extern	FILE_NUM  DefineFile(FULL_CHAR *str, FULL_CHAR *suffix,
		    FILE_POS *xfpos, int ftype, int fpath);
extern	FILE_NUM  FirstFile(int ftype);
extern	FILE_NUM  NextFile(FILE_NUM i);
extern	FILE_NUM  FileNum(FULL_CHAR *str, FULL_CHAR *suffix);
extern	FILE_NUM  DatabaseFileNum(FILE_POS *xfpos);
extern	FULL_CHAR *FileName(FILE_NUM fnum);
extern	FULL_CHAR *FullFileName(FILE_NUM fnum);
extern	FULL_CHAR *EchoFilePos(FILE_POS *pos);
extern	FULL_CHAR *EchoAltFilePos(FILE_POS *pos);
extern	FULL_CHAR *EchoFileSource(FILE_NUM fnum);
extern	FULL_CHAR *EchoFileLine(FILE_POS *pos);
extern	FILE_POS  *PosOfFile(FILE_NUM fnum);
extern	FILE	  *OpenFile(FILE_NUM fnum, BOOLEAN check_ld, BOOLEAN check_lt);
extern	FILE	  *OpenIncGraphicFile(FULL_CHAR *str, unsigned char typ,
		    OBJECT *full_name, FILE_POS *xfpos, BOOLEAN *compressed);
extern	void	  FileSetUpdated(FILE_NUM fnum, int newlines);
extern	int	  FileGetLineCount(FILE_NUM fnum);
extern	BOOLEAN	  FileTestUpdated(FILE_NUM fnum);

/*****  z04.c	  Token Service	        **************************************/
extern	OBJECT	  NewToken(unsigned char xtype, FILE_POS *xfpos,
		    unsigned char xvspace, unsigned char xhspace,
		    unsigned char xprec, OBJECT xactual);
extern	OBJECT	  CopyTokenList(OBJECT x, FILE_POS *pos);
extern	FULL_CHAR *EchoCatOp(unsigned xtype, BOOLEAN xmark, BOOLEAN xjoin);
extern	FULL_CHAR *EchoToken(OBJECT x);

/*****  z05.c	  Read Definitions  	**************************************/
extern	void	  ReadPrependDef(unsigned typ, OBJECT encl);
extern	void	  ReadDatabaseDef(unsigned typ, OBJECT encl);
extern	void	  ReadDefinitions(OBJECT *token, OBJECT encl,
		    unsigned char res_type);

/*****  z06.c	  Object Parser	        **************************************/
extern	void	  SetScope(OBJECT env, int *count, BOOLEAN vis_only);
extern	void	  InitParser(FULL_CHAR *cross_db);
extern	OBJECT	  Parse(OBJECT *token, OBJECT encl, BOOLEAN defs_allowed,
		    BOOLEAN transfer_allowed);

/*****  z07.c	  Object Service	**************************************/
extern	BOOLEAN	  SplitIsDefinite(OBJECT x);
extern	int	  DisposeObject(OBJECT x);
extern	OBJECT	  MakeWord(unsigned typ, FULL_CHAR *str, FILE_POS *pos);
extern	OBJECT	  MakeWordTwo(unsigned typ, FULL_CHAR *str1, FULL_CHAR *str2,
		    FILE_POS *pos);
extern	OBJECT	  MakeWordThree(FULL_CHAR *s1, FULL_CHAR *s2, FULL_CHAR *s3);
extern	OBJECT	  CopyObject(OBJECT x, FILE_POS *pos);
extern	OBJECT	  InsertObject(OBJECT x, OBJECT *ins, STYLE *style);

/*****  z08.c	  Object Manifest	**************************************/
extern	OBJECT	  ReplaceWithTidy(OBJECT x, BOOLEAN one_word);
extern	OBJECT	  Manifest(OBJECT x, OBJECT env, STYLE *style, OBJECT bthr[2],
		    OBJECT fthr[2], OBJECT *target, OBJECT *crs, BOOLEAN ok,
		    BOOLEAN need_expand, OBJECT *enclose, BOOLEAN fcr);

/*****  z09.c	  Closure Expansion	**************************************/
extern	OBJECT	  SearchEnv(OBJECT env, OBJECT sym);
extern	OBJECT	  SetEnv(OBJECT x, OBJECT y);
extern	void	  AttachEnv(OBJECT env, OBJECT x);
extern	OBJECT	  GetEnv(OBJECT x);
extern	OBJECT	  DetachEnv(OBJECT x);
extern	OBJECT	  ClosureExpand(OBJECT x, OBJECT env, BOOLEAN crs_wanted,
		    OBJECT *crs, OBJECT *res_env);
extern	OBJECT	  ParameterCheck(OBJECT x, OBJECT env);

/*****  z10.c	  Cross References	**************************************/
extern	void	  CrossInit(OBJECT sym);
extern	OBJECT	  CrossMake(OBJECT sym, OBJECT val, int ctype);
extern	OBJECT	  GallTargEval(OBJECT sym, FILE_POS *dfpos);
extern	void	  CrossAddTag(OBJECT x);
extern	OBJECT	  CrossExpand(OBJECT x, OBJECT env, STYLE *style,
		    OBJECT *crs, OBJECT *res_env);
extern	void	  CrossSequence(OBJECT x);
extern	void	  CrossClose(void);

/*****  z11.c	  Style Service		**************************************/
extern	FULL_CHAR *EchoStyle(STYLE *style);
extern	void	  SpaceChange(STYLE *style, OBJECT x);
extern	void	  BreakChange(STYLE *style, OBJECT x);
extern	void	  YUnitChange(STYLE *style, OBJECT x);
extern	void	  ZUnitChange(STYLE *style, OBJECT x);

/*****  z12.c	  Size Finder		**************************************/
extern	OBJECT	  MinSize(OBJECT x, int dim, OBJECT *extras);

/*****  z13.c	  Object Breaking	**************************************/
extern	OBJECT	  BreakObject(OBJECT x, CONSTRAINT *c);

/*****  z14.c	  Object Filling        **************************************/
extern	OBJECT	  FillObject(OBJECT x, CONSTRAINT *c, OBJECT multi,
		     BOOLEAN can_hyphenate, BOOLEAN allow_shrink,
		     BOOLEAN extend_unbreakable, BOOLEAN *hyph_used);

/*****  z15.c	  Size Constraints	**************************************/
extern	void	  MinConstraint(CONSTRAINT *xc, CONSTRAINT *yc);
extern	void	  EnlargeToConstraint(FULL_LENGTH *b, FULL_LENGTH *f,
		    CONSTRAINT *c);
extern	int	  ScaleToConstraint(FULL_LENGTH b, FULL_LENGTH f,
		    CONSTRAINT *c);
extern	void	  InvScaleConstraint(CONSTRAINT *yc, FULL_LENGTH sf,
		    CONSTRAINT*xc);
extern	void	  RotateConstraint(CONSTRAINT *c, OBJECT y, FULL_LENGTH angle,
		    CONSTRAINT *hc, CONSTRAINT *vc, int dim);
extern	BOOLEAN	  InsertScale(OBJECT x, CONSTRAINT *c);
extern	void	  Constrained(OBJECT x, CONSTRAINT *xc, int dim, OBJECT *why);
extern	FULL_CHAR *EchoConstraint(CONSTRAINT *c);
extern	void	  DebugConstrained(OBJECT x);

/*****  z16.c	  Size Adjustments	**************************************/
extern	FULL_LENGTH	  FindShift(OBJECT x, OBJECT y, int dim);
extern	void	  SetNeighbours(OBJECT link, BOOLEAN ratm, OBJECT *pg,
		    OBJECT *pdef, OBJECT *sg, OBJECT *sdef, int *side);
extern	void	  AdjustSize(OBJECT x, FULL_LENGTH b, FULL_LENGTH f, int dim);

/*****  z17.c	  Gap Widths		**************************************/
extern	void	  GetGap(OBJECT x, STYLE *style, GAP *res_gap,
		    unsigned *res_inc);
extern	FULL_LENGTH  MinGap(FULL_LENGTH a, FULL_LENGTH b, FULL_LENGTH c, GAP *xgap);
extern	FULL_LENGTH  ExtraGap(FULL_LENGTH a, FULL_LENGTH b, GAP *xgap, int dir);
extern	FULL_LENGTH  ActualGap(FULL_LENGTH a, FULL_LENGTH b, FULL_LENGTH c,
		       GAP *xgap, FULL_LENGTH f, FULL_LENGTH mk);
extern	FULL_CHAR *EchoGap(GAP *xgap);

/*****  z18.c	  Galley Transfer	**************************************/
extern	STYLE	  InitialStyle;
extern	OBJECT	  InitialEnvironment;
extern	void	  TransferInit(OBJECT InitEnv);
extern	OBJECT	  TransferBegin(OBJECT x);
extern	void	  TransferComponent(OBJECT x);
extern	void	  TransferEnd(OBJECT x);
extern	void	  TransferClose(void);

/*****  z19.c	  Galley Attaching	**************************************/
extern	void	  DetachGalley(OBJECT hd);
extern	OBJECT	  SearchGalley(OBJECT start, OBJECT sym, BOOLEAN forwards,
		    BOOLEAN subgalleys, BOOLEAN closures, BOOLEAN input);
extern	int	  AttachGalley(OBJECT hd, OBJECT *inners, OBJECT *suspend_pt);

/*****  z20.c	  Galley Flushing	**************************************/
extern	FULL_CHAR *DebugInnersNames(OBJECT inners);
extern	void	  FlushGalley(OBJECT hd);

/***    z21.c	  Galley Maker		**************************************/
extern	void	  SizeGalley(OBJECT hd, OBJECT env, BOOLEAN rows,
		    BOOLEAN joined, BOOLEAN nonblock, BOOLEAN trig,
		    STYLE *style, CONSTRAINT *c, OBJECT target,
		    OBJECT *dest_index, OBJECT *recs, OBJECT *inners,
		    OBJECT enclose);

/***    z22.c	  Galley Service	**************************************/
extern	void	  Interpose(OBJECT z, int typ, OBJECT x, OBJECT y);
extern	void	  FlushInners(OBJECT inners, OBJECT hd);
extern	void	  ExpandRecursives(OBJECT recs);
extern	void	  Promote(OBJECT hd, OBJECT stop_link, OBJECT dest_index);
extern	void	  KillGalley(OBJECT hd, BOOLEAN optimize);
extern	void	  FreeGalley(OBJECT hd, OBJECT stop_link, OBJECT *inners,
		    OBJECT relocate_link, OBJECT sym);
extern	void	  SetTarget(OBJECT hd);
extern	int	  CheckComponentOrder(OBJECT preceder, OBJECT follower);

/*****  z23.c	  Galley Printer	**************************************/
extern	void	  FixAndPrintObject(OBJECT x, FULL_LENGTH xmk, FULL_LENGTH xb,
		    FULL_LENGTH xf, int dim, BOOLEAN suppress, FULL_LENGTH pg,int count);

/*****  z24.c	  Print Service         **************************************/
extern	void	  PrintInit(FILE *file_ptr);
extern	void	  PrintBeforeFirst(FULL_LENGTH h, FULL_LENGTH v, FULL_CHAR *label);
extern	void	  PrintBetween(FULL_LENGTH h, FULL_LENGTH v, FULL_CHAR *label);
extern	void	  PrintWord(OBJECT x, int hpos, int vpos);
extern	void	  PrintUnderline(FONT_NUM fnum, FULL_LENGTH xstart,
		    FULL_LENGTH xstop, FULL_LENGTH ymk);
extern	void	  PrintAfterLast(void);
extern	void	  CoordTranslate(FULL_LENGTH xdist, FULL_LENGTH ydist);
extern	void	  CoordRotate(FULL_LENGTH amount);
extern	void	  CoordScale(float hfactor, float vfactor);
extern	void	  SaveGraphicState(OBJECT x);
extern	void	  RestoreGraphicState(void);
extern	void	  PrintGraphicObject(OBJECT x);
extern	void	  DefineGraphicNames(OBJECT x);
extern	void	  SaveTranslateDefineSave(OBJECT x,
		    FULL_LENGTH xdist, FULL_LENGTH ydist);
extern	void	  PrintGraphicInclude(OBJECT x, FULL_LENGTH colmark,
		    FULL_LENGTH rowmark);

/*****  z25.c	  Object Echo	        **************************************/
extern	FULL_CHAR *EchoObject(OBJECT x);
extern	void	  DebugObject(OBJECT x);
extern	FULL_CHAR *EchoIndex(OBJECT index);
extern	void	  DebugGalley(OBJECT hd, OBJECT pinpt, int indent);

/*****  z26.c	  Echo Service	        **************************************/
extern	void	  BeginString(void);
extern	void	  AppendString(FULL_CHAR *str);
extern	FULL_CHAR *EndString(void);
extern	FULL_CHAR *EchoLength(int len);
extern	FULL_CHAR *Image(unsigned int c);

/*****	z27.c	  Debug Service		**************************************/
#if DEBUG_ON
extern	void	  DebugInit(FULL_CHAR *str);
extern	void	  Debug(int category, int urgency, char *str, ...);
extern	void	  ProfileOn(char *str);
extern	void	  ProfileOff(char *str);
extern	void	  ProfilePrint(void);
#endif

/*****	z28.c	  Error Service		**************************************/
extern	void	  ErrorInit(FULL_CHAR *str);
extern	BOOLEAN	  ErrorSeen(void);
extern	void	  EnterErrorBlock(BOOLEAN ok_to_print);
extern	void	  LeaveErrorBlock(BOOLEAN commit);
extern	void	  CheckErrorBlocks(void);
extern	POINTER	  Error(int set_num, int msg_num, char *str, int etype,
		    FILE_POS *pos, ...);

/*****  z29.c	  Symbol Table		**************************************/
extern	void	  InitSym(void);
extern	void	  PushScope(OBJECT x, BOOLEAN npars, BOOLEAN vis);
extern	void	  PopScope(void);
extern	void	  SuppressVisible(void);
extern	void	  UnSuppressVisible(void);
extern	void	  SuppressScope(void);
extern	void	  UnSuppressScope(void);
extern	void	  SwitchScope(OBJECT sym);
extern	void	  UnSwitchScope(OBJECT sym);
extern	void	  BodyParAllowed(void);
extern	void	  BodyParNotAllowed(void);
extern	OBJECT	  InsertSym(FULL_CHAR *str, unsigned char xtype,
		    FILE_POS *xfpos, unsigned char xprecedence,
		    BOOLEAN xindefinite, BOOLEAN xrecursive,
		    unsigned xpredefined, OBJECT xenclosing, OBJECT xbody);
extern	OBJECT	  SearchSym(FULL_CHAR *str, int len);
extern	FULL_CHAR *SymName(OBJECT s);
extern	FULL_CHAR *FullSymName(OBJECT x, FULL_CHAR *str);
extern	OBJECT	  ChildSym(OBJECT s, unsigned typ);
extern	OBJECT	  ChildSymWithCode(OBJECT s, unsigned char code);
extern	void	  CheckSymSpread(void);
extern	void	  DeleteEverySym(void);
extern	void	  DebugScope(void);

/*****  z30.c	  Symbol Uses		**************************************/
extern	void	  InsertUses(OBJECT x, OBJECT y);
extern	void	  FlattenUses(void);
extern	BOOLEAN	  SearchUses(OBJECT x, OBJECT y);
extern	OBJECT	  FirstExternTarget(OBJECT sym, OBJECT *cont);
extern	OBJECT	  NextExternTarget(OBJECT sym, OBJECT *cont);

/*****  z31.c	  Memory Allocator	**************************************/
extern	void	  DebugRegisterUsage(int typ, int delta_num, int delta_size);
extern	void	  DebugMemory(void);
extern	void	  MemInit(void);
extern	OBJECT	  GetMemory(int siz, FILE_POS *pos);
extern	OBJECT	  zz_free[];
extern	unsigned char	zz_lengths[];
extern	int	  zz_newcount;
extern	int	  zz_disposecount;
extern	int	  zz_listcount;
extern	OBJECT	  zz_hold;
extern	OBJECT	  zz_tmp;
extern	OBJECT	  zz_res;
extern	int	  zz_size;
extern	OBJECT	  xx_link, xx_tmp;
extern	OBJECT	  xx_hold, xx_res;

/*****  z32.c	  Counter Service           **********************************/
extern	OBJECT	  Next(OBJECT x, int inc, BOOLEAN *done);

/*****  z33.c	  Database Service	**************************************/
extern	OBJECT	  OldCrossDb;
extern	OBJECT	  NewCrossDb;
extern	OBJECT	  DbCreate(OBJECT x);
extern	void	  DbInsert(OBJECT db, BOOLEAN gall, OBJECT sym, FULL_CHAR *tag,
		    FILE_POS *tagfpos, FULL_CHAR *seq, FILE_NUM dfnum,
		    long dfpos, int dlnum, BOOLEAN check);
extern	void	  DbConvert(OBJECT db, BOOLEAN full_name);
extern	void	  DbClose(OBJECT db);
extern	OBJECT	  DbLoad(OBJECT stem, int fpath, BOOLEAN create, OBJECT symbs);
extern	BOOLEAN	  DbRetrieve(OBJECT db, BOOLEAN gall, OBJECT sym,
		    FULL_CHAR *tag, FULL_CHAR *seq, FILE_NUM *dfnum,
		    long *dfpos, int *dlnum, long *cont);
extern	BOOLEAN	  DbRetrieveNext(OBJECT db, BOOLEAN *gall, OBJECT *sym,
		    FULL_CHAR *tag, FULL_CHAR *seq, FILE_NUM *dfnum,
		    long *dfpos, int *dlnum, long *cont);

/*****  z34.c	  Rotation Service    	**************************************/
extern	void	  RotateSize(FULL_LENGTH *xcb, FULL_LENGTH *xcf,
		    FULL_LENGTH *xrb, FULL_LENGTH *xrf, OBJECT y,
		    FULL_LENGTH theta);

/*****  z35.c	  Time Keeper     	**************************************/
extern	FULL_CHAR *TimeString(void);
extern	void	  InitTime(void);
extern	OBJECT	  MomentSym;
extern	OBJECT	  StartMoment(void);

/*****  z36.c	  Hyphenation     	**************************************/
extern	BOOLEAN	  ReadHyphTable(LANGUAGE_NUM lnum);
extern	OBJECT	  Hyphenate(OBJECT x);

/*****  z37.c	  Font Service           *************************************/
extern	FONT_INFO *finfo;
extern	int	  font_curr_page;
extern	void	  FontInit(void);
extern	void	  FontDefine(OBJECT family, OBJECT face, OBJECT inside);
extern	void	  FontChange(STYLE *style, OBJECT x);
extern	void	  FontWordSize(OBJECT x);
extern	FULL_LENGTH  FontSize(FONT_NUM fnum, OBJECT x);
extern	FULL_LENGTH  FontHalfXHeight(FONT_NUM fnum);
extern	MAPPING	  FontMapping(FONT_NUM fnum, FILE_POS *xfpos);
extern	FULL_CHAR *FontName(FONT_NUM fnum);
extern	FULL_CHAR *FontFamily(FONT_NUM fnum);
extern	FULL_CHAR *FontFace(FONT_NUM fnum);
extern	FULL_CHAR *FontFamilyAndFace(FONT_NUM fnum);
extern	void	  FontPrintAll(FILE *fp);
extern	void	  FontPrintPageSetup(FILE *fp);
extern	void	  FontPrintPageResources(FILE *fp);
extern	void	  FontAdvanceCurrentPage(void);
extern	void	  FontPageUsed(OBJECT face);
extern	BOOLEAN	  FontNeeded(FILE *fp);

/*****  z38.c	  Character Mappings    **************************************/
extern	MAP_VEC	  MapTable[];
extern	MAPPING	  MapLoad(OBJECT filename, BOOLEAN must_print);
extern	FULL_CHAR MapCharEncoding(FULL_CHAR *str, MAPPING m);
extern	FULL_CHAR *MapEncodingName(MAPPING m);
extern	void	  MapPrintEncodings(FILE *fp);
extern	void	  MapPrintResources(FILE *fp);
extern	OBJECT	  MapSmallCaps(OBJECT x, STYLE *style);
extern	BOOLEAN	  MapIsLowerCase(FULL_CHAR ch, MAPPING m);


/*****  z39.c	  String Handler        **************************************/
#define		  AsciiToFull(x)	( (FULL_CHAR *) (x) )
#if COLLATE
#define		  StringEqual(a, b)	(strcoll((char *)(a), (char *)(b))==0)
#define		  StringLessEqual(a, b) (strcoll((char*)(a),(char*)(b))<=0)
#else
#define		  StringEqual(a, b)	(strcmp((char *)(a), (char *)(b))==0)
#define		  StringLessEqual(a, b) (strcmp((char*)(a),(char*)(b))<=0)
#endif
#define		  StringCat(a, b)	strcat((char *)(a),(char *)(b))
#define		  StringCopy(a, b)	strcpy((char *)(a),(char *)(b))
#define		  StringLength(a)	strlen((char *)(a))
#define		  StringFOpen(a, b)	fopen( (char *) (a), (b) )
#define		  StringFPuts(a, b)	fputs( (char *) (a), (b) )
#define		  StringFGets(a, b, c)	fgets( (char *) (a), (b), (c) )
#define		  StringRemove(a)	remove((char *)(a))
#define		  StringRename(a, b)	rename((char *)(a),(char *)(b))
extern	BOOLEAN	  StringBeginsWith(FULL_CHAR *str, FULL_CHAR *pattern);
extern	BOOLEAN	  StringEndsWith(FULL_CHAR *str, FULL_CHAR *pattern);
extern	BOOLEAN	  StringContains(FULL_CHAR *str, FULL_CHAR *pattern);
extern	FULL_CHAR *StringInt(int i);
extern	FULL_CHAR *StringFiveInt(int i);
extern	FULL_CHAR *StringQuotedWord(OBJECT x);

/*****  z40.c	  Filter Handler        **************************************/
extern	void	  FilterInit(void);
extern	OBJECT	  FilterCreate(BOOLEAN use_begin, OBJECT act, FILE_POS *xfpos);
extern	void	  FilterSetFileNames(OBJECT x);
extern	OBJECT	  FilterExecute(OBJECT x, FULL_CHAR *command, OBJECT env);
extern	void	  FilterWrite(OBJECT x, FILE *fp, int *linecount);
extern	void	  FilterScavenge(BOOLEAN all);

/*****  z41.c	  Object Input-Output   **************************************/
extern	OBJECT	  ReadFromFile(FILE_NUM fnum, long pos, int lnum);
extern	void	  AppendToFile(OBJECT x, FILE_NUM fnum, int *pos, int *lnum);
extern	void	  CloseFiles(void);

/*****  z42.c	  Colour Service        **************************************/
extern	void	  ColourInit(void);
extern	void	  ColourChange(STYLE *style, OBJECT x);
extern	FULL_CHAR *ColourCommand(COLOUR_NUM cnum);

/*****  z43.c	  Language Service      **************************************/
extern	void	  LanguageInit(void);
extern	BOOLEAN	  LanguageWordEndsSentence(OBJECT wd, BOOLEAN lc_prec);
extern	void	  LanguageDefine(OBJECT names, OBJECT hyph_file);
extern	void	  LanguageChange(STYLE *style, OBJECT x);
extern	FULL_CHAR *LanguageString(LANGUAGE_NUM lnum);
extern	OBJECT	  LanguageHyph(LANGUAGE_NUM lnum);
extern	BOOLEAN	  LanguageSentenceEnds[];

/*****  z44.c	  Vertical Hyphenation  **************************************/
extern	BOOLEAN	  VerticalHyphenate(OBJECT y);
extern	OBJECT	  ConvertGalleyList(OBJECT x);
extern	OBJECT	  BuildEnclose(OBJECT hd);

/*****  z45.c	  External Sort         **************************************/
extern	void	  SortFile(char *infile, char *outfile);

/*****  z46.c	  Optimal Galleys       **************************************/
extern	BOOLEAN	  FindOptimize(OBJECT x, OBJECT env);
extern	void	  SetOptimize(OBJECT hd, STYLE *style);
extern	void	  GazumpOptimize(OBJECT hd, OBJECT dest);
extern	void	  CalculateOptimize(OBJECT hd);
extern	void	  DebugOptimize(OBJECT hd);

/*****  z47.c	  Environment Table     **************************************/
extern	void	  EnvInit(void);
extern	BOOLEAN	  EnvWriteRetrieve(OBJECT env, FILE_NUM fnum, int *offset,
		    int *lnum);
extern	void	  EnvWriteInsert(OBJECT env, FILE_NUM fnum, int offset,int lnum);
extern	BOOLEAN	  EnvReadRetrieve(FILE_NUM fnum, int offset, OBJECT *env);
extern	void	  EnvReadInsert(FILE_NUM fnum, int offset, OBJECT env);
extern	void	  EnvDebug(void);

/*****  z48.c	  PDF back end          **************************************/
extern	void      PDFFile_Init(FILE* in_fp, int in_h_bound, int in_v_bound,
				int in_IN, int in_CM, int in_PT, int in_EM);
extern	void      PDFFile_BeginFontEncoding(FILE* in_fp,
				const char* in_encoding_name);
extern	void      PDFFile_EndFontEncoding(FILE* in_fp);
extern	void      PDFFile_Cleanup(FILE* in_fp);
extern	void      PDFPage_Init(FILE* in_fp, float in_scale_factor,
				int in_line_width);
extern	void      PDFPage_Cleanup(FILE* in_fp);
extern	void	  PDFPage_Write(FILE* in_fp, char* in_str);
extern	void      PDFPage_Push(FILE* in_fp);
extern	void      PDFPage_Pop(FILE* in_fp);
extern	void	  PDFPage_Scale(FILE* in_fp, float in_h_scale_factor,
				float in_v_scale_factor);
extern	void	  PDFPage_Translate(FILE* in_fp, float in_delta_h,
				float in_delta_v);
extern	void	  PDFPage_Rotate(FILE* in_fp, float in_angle_in_radians);
extern	void	  PDFPage_SetVars(int xsize, int ysize, int xmark, int ymark,
				int loutf, int loutv, int louts);
extern	void	  PDFPage_WriteGraphic(FILE* in_fp, FULL_CHAR* in_str);
extern	void	  PDFPage_PrintUnderline(FILE* in_fp, int in_x1, int in_x2,
				int in_y, int in_thickness);

extern	void      PDFFont_AddFont(
				FILE* in_fp,
				const FULL_CHAR* in_short_font_name,
				const FULL_CHAR* in_real_font_name,
				const FULL_CHAR* in_font_encoding_name);
extern	void      PDFFont_Set(FILE* in_fp, FULL_LENGTH in_font_size,
				FULL_CHAR * in_short_font_name);
extern	void    PDFText_OpenXY(FILE* in_fp, int hpos, int vpos);
extern	void    PDFText_OpenX(FILE* in_fp, int hpos);
extern	void    PDFText_Open(FILE* in_fp);
extern	void    PDFText_Kern(FILE* in_fp, int in_kern);
extern	void    PDFText_Close(FILE* in_fp);
extern	BOOLEAN PDFHasValidTextMatrix(void);


/*@::assert(), debug(), debug flags@******************************************/
/*                                                                           */
/*  ASSERT AND DEBUG CODE                                                    */
/*                                                                           */
/*****************************************************************************/

#if ASSERT_ON
#define assert(c, m)							\
   { if( !(c) )  Error(1, 2, "assert failed in %s", INTERN, no_fpos, m); }
#define assert1(c, m, p1)						\
   { if( !(c) )  Error(1, 3, "assert failed in %s %s", INTERN,no_fpos,m, p1); }
#else
#define assert(c, m)
#define assert1(c, m, p1)
#endif

#if DEBUG_ON

struct dbs
{	char	*flag;			/* external names for debug flags    */
	BOOLEAN	on[3];			/* the debug flags                   */
};
extern	struct dbs 	dbg[];

/* debug routines */
#define debug0(cat, urg, str)                				\
    { if( dbg[cat].on[urg] ) Debug(cat, urg, str); }
#define debug1(cat, urg, str, p1)					\
    { if( dbg[cat].on[urg] ) Debug(cat, urg, str, p1); }
#define debug2(cat, urg, str, p1, p2)					\
    { if( dbg[cat].on[urg] ) Debug(cat, urg, str, p1, p2); }
#define debug3(cat, urg, str, p1, p2, p3)				\
    { if( dbg[cat].on[urg] ) Debug(cat, urg, str, p1, p2, p3); }
#define debug4(cat, urg, str, p1, p2, p3, p4)				\
    { if( dbg[cat].on[urg] ) Debug(cat, urg, str, p1, p2, p3, p4); }
#define debug5(cat, urg, str, p1, p2, p3, p4, p5)			\
    { if( dbg[cat].on[urg] ) Debug(cat, urg, str, p1, p2, p3, p4, p5); }
#define debug6(cat, urg, str, p1, p2, p3, p4, p5, p6)			\
    { if( dbg[cat].on[urg] ) Debug(cat, urg, str, p1, p2, p3, p4, p5, p6); }
#define debug7(cat, urg, str, p1, p2, p3, p4, p5, p6, p7)		\
    { if( dbg[cat].on[urg] ) Debug(cat, urg, str, p1, p2, p3, p4, p5,p6,p7); }
#define debug8(cat, urg, str, p1, p2, p3, p4, p5, p6, p7, p8)		\
    { if( dbg[cat].on[urg] ) Debug(cat, urg, str, p1, p2,p3,p4,p5,p6,p7,p8); }
#define	ifdebug(cat, urg, x)						\
    { if( dbg[cat].on[urg] ) { x; } }

#define debugcond0(cat, urg, cond, str)                			\
    { if( dbg[cat].on[urg] && cond ) Debug(cat, urg, str); }
#define debugcond1(cat, urg, cond, str, p1)				\
    { if( dbg[cat].on[urg] && cond ) Debug(cat, urg, str, p1); }
#define debugcond2(cat, urg, cond, str, p1, p2)				\
    { if( dbg[cat].on[urg] && cond ) Debug(cat, urg, str, p1, p2); }
#define debugcond3(cat, urg, cond, str, p1, p2, p3)			\
    { if( dbg[cat].on[urg] && cond ) Debug(cat, urg, str, p1, p2, p3); }
#define debugcond4(cat, urg, cond, str, p1, p2, p3, p4)			\
    { if( dbg[cat].on[urg] && cond ) Debug(cat, urg, str, p1, p2, p3, p4); }
#define debugcond5(cat, urg, cond, str, p1, p2, p3, p4, p5)		\
    { if( dbg[cat].on[urg] && cond ) Debug(cat, urg, str, p1, p2, p3, p4, p5);}
#define	ifdebugcond(cat, urg, cond, x)					\
    { if( dbg[cat].on[urg] && cond ) { x; } }
#define	debug_init(str)							\
    DebugInit(str)

/* debug styles */
#define	D	 0
#define	DD	 1
#define	DDD	 2

/* debug flags */
#define	DSP	 1		/*  z01.c   -dsp   Supervise                 */
#define	DLA	 2		/*  z02.c   -dla   Lexical Analyser          */
#define	DFS	 3		/*  z03.c   -dfs   File Service              */
#define	DTS	 4		/*  z04.c   -dts   Token Service             */
#define	DRD	 5		/*  z05.c   -drd   Read Definitions          */
#define	DOP	 6		/*  z06.c   -dop   Object Parser             */
#define	DOS	 7		/*  z07.c   -dos   Object Service            */
#define	DOM	 8		/*  z08.c   -dom   Object Manifest           */
#define	DCE	 9		/*  z09.c   -dce   Closure Expansion         */
#define	DCR	10		/*  z10.c   -dcr   Cross References	     */
#define	DSS	11		/*  z11.c   -dss   Style Service	     */
#define	DSF	12		/*  z12.c   -dsf   Size Finder               */
#define	DOB	13		/*  z13.c   -dob   Object Breaking	     */
#define	DOF	14		/*  z14.c   -dof   Object Filling	     */
#define	DSC	15		/*  z15.c   -dsc   Size Constraints          */
#define	DSA	16		/*  z16.c   -dsa   Size Adjustments	     */
#define	DGW	17		/*  z17.c   -dgw   Gap Widths                */
#define	DGT	18		/*  z18.c   -dgt   Galley Transfer           */
#define	DGA	19		/*  z19.c   -dgf   Galley Attaching          */
#define	DGF	20		/*  z20.c   -dgf   Galley Flushing           */
#define	DGM	21		/*  z21.c   -dgm   Galley Maker              */
#define	DGS	22		/*  z22.c   -dgs   Galley Service            */
#define	DGP	23		/*  z23.c   -dgp   Galley Printer            */
#define	DPS	24		/*  z24.c   -dps   Print Service             */
#define	DOE	25		/*  z25.c   -doe   Object Echo               */
#define	DES	26		/*  z26.c   -des   Echo Service		     */
#define	DZZ	27		/*  z27.c   -dzz   Debug Service             */
#define	DYY	28		/*  z28.c   -dyy   Error Service             */
#define	DST	29		/*  z29.c   -dst   Symbol Table              */
#define	DSU	30		/*  z30.c   -dsu   Symbol Uses               */
#define	DMA	31		/*  z31.c   -dma   Memory Allocator          */
#define	DCS	32		/*  z32.c   -dcs   Counter Service           */
#define	DBS	33		/*  z33.c   -dbs   Database Service          */
#define	DRS	34		/*  z34.c   -drs   Rotation Service          */
#define	DTK	35		/*  z35.c   -dtk   Time Keeper               */
#define	DHY	36		/*  z36.c   -dhy   Hyphenation               */
#define	DFT	37		/*  z37.c   -dft   Font Service              */
#define	DCM	38		/*  z38.c   -dcm   Character Mapping         */
#define	DSH	39		/*  z39.c   -dsh   String Handler            */
#define	DFH	40		/*  z40.c   -dsh   Filter Handler            */
#define	DIO	41		/*  z41.c   -dio   Object Input-Output       */
#define	DCO	42		/*  z42.c   -dco   Colour Service            */
#define	DLS	43		/*  z43.c   -dls   Language Service          */
#define	DVH	44		/*  z44.c   -dvh   Vertical Hyphenation      */
#define	DEX	45		/*  z45.c   -dex   External Sort             */
#define	DOG	46		/*  z46.c   -dex   Optimal Galleys           */
#define	DET	47		/*  z47.c   -det   Environment Table         */
#define	DPD	48		/*  z48.c   -dpd   PDF Back End              */
#define	DPP	49		/*          -dpp   Profiling                 */
#define	ANY	50		/*          -d     any                       */

#else
#define ifdebug(cat, urg, x)
#define debug0(cat, urg, str)
#define debug1(cat, urg, str, p1)
#define debug2(cat, urg, str, p1, p2)
#define debug3(cat, urg, str, p1, p2, p3)
#define debug4(cat, urg, str, p1, p2, p3, p4)
#define debug5(cat, urg, str, p1, p2, p3, p4, p5)
#define debug6(cat, urg, str, p1, p2, p3, p4, p5, p6)
#define debug7(cat, urg, str, p1, p2, p3, p4, p5, p6, p7)
#define debug8(cat, urg, str, p1, p2, p3, p4, p5, p6, p7, p8)

#define debugcond0(cat, urg, cond, str)
#define debugcond1(cat, urg, cond, str, p1)
#define debugcond2(cat, urg, cond, str, p1, p2)
#define debugcond3(cat, urg, cond, str, p1, p2, p3)
#define debugcond4(cat, urg, cond, str, p1, p2, p3, p4)
#define debugcond5(cat, urg, cond, str, p1, p2, p3, p4, p5)
#define	ifdebugcond(cat, urg, cond, x)
#define	debug_init(str)	Error(1, 4, "%s - debug flags not implemented", \
	FATAL, no_fpos, str)
#endif
