/*@z24.c:Back End:FontDefine(), FontChange(), FontAtomSize()@*****************/
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
/*  FILE:         z24.c                                                      */
/*  MODULE:       PostScript Back End                                        */
/*  EXTERNS:      PrintInit(), FontStripQuotes(), FontDefine(),              */
/*                FontChange(), FontAtomSize(), FontSize(),                  */
/*                PrintPrologue(), PrintOriginIncrement(), PrintAtom(),      */
/*                PrintClose()                                               */
/*                CoordTranslate(), CoordRotate(), CoordScale(),             */
/*                SaveGraphicState(), RestoreGraphicState(),                 */
/*                DefineGraphicNames, PrintGraphicObject()                   */
/*                                                                           */
/*  This module implements Lout's PostScript back end, by reading Adobe      */
/*  font metrics files (.AFM files, version 2) and writing PostScript.       */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define DEFAULT_XHEIGHT 500

#define printnum(x, fp)							\
{ unsigned char buff[20];  register int i, y;				\
  if( x < 0 )								\
  { y = -x;								\
    putc('-', fp);							\
  }									\
  else y = x;								\
  i = 0;								\
  do									\
  { buff[i++] = (y % 10) + '0';						\
  } while( y = y / 10 );						\
  do									\
  { putc(buff[--i], fp);						\
  } while( i );								\
}


/*****************************************************************************/
/*                                                                           */
/*  FirstChar(str, ch, xfpos)                                                */
/*  NextChar(str, ch, xfpos)                                                 */
/*                                                                           */
/*  FirstChar sets ch to the true (interpreted) value of the first           */
/*  character of Lout string str.  NextChar sets ch to the true value of     */
/*  the next character, repeatedly.  Both set ch to '\0' at end of string.   */
/*                                                                           */
/*****************************************************************************/

#define FirstChar(str, ch, xfpos)	p = str;  NextChar(str, ch, xfpos)

#define NextChar(str, ch, xfpos)					\
  while( *p == '"' )  p++;						\
  if( *p != '\\' )  ch = *p++;						\
  else if( *++p >= '0' && *p <= '7' )					\
  { int count;								\
    count = ch = 0;							\
    do									\
    { ch = ch * 8 + *p++ - '0';						\
      count++;								\
    } while( *p >= '0' && *p <= '7' && count < 3 );			\
    if( ch == '\0' )  Error(WARN, xfpos, "null character \\0 in word");	\
  }									\
  else if( *p == '"'  )  ch = '"',  ++p;				\
  else if( *p == '\\' )  ch = '\\', ++p;				\
  else									\
  { Error(WARN, xfpos, "unknown ecape sequence \\%c in word", *p);	\
    ch = *p++;								\
  }


/*****************************************************************************/
/*                                                                           */
/*  CheckLigature(str, ch, lig)                                              */
/*                                                                           */
/*  Check whether character ch from string str starts a ligature.  Use lig   */
/*  as the source of information about what ligatures there are.  If ch      */
/*  does start a ligature, skip it and change ch to the ligature character.  */
/*                                                                           */
/*  CheckLigature also modifies any ligature it finds to the form \?"""      */
/*  where ? denotes the ligature character (assumed to be different from     */
/*  the characters which normally may follow a \, i.e. 0123456789"\), and    */
/*  " is used to fill up space.                                              */
/*                                                                           */
/*****************************************************************************/

#define CheckLigature(str, ch, lig)					\
  if( lig[ch] )								\
  { unsigned char *a, *b;						\
    a = &lig[lig[ch] + MAX_CHARS];					\
    debug3(DFT, D, "  CheckLigature(%s, %c, %s)", str, ch, a);		\
    while( *a++ == ch )							\
    { b = p;								\
      debug2(DFT, D, "  checking a = %s, b = %s", a, b);		\
      while( *a == *b && *(a+1) != '\0' && *b != '\0' )  a++, b++;	\
      if( *(a+1) == '\0' )						\
      { *(p-1) = '\\';							\
	*p = ch = *a;							\
	while( ++p < b ) *p = '"';					\
        debug1(DFT, D, "  success: now str = %s", str);			\
	break;								\
      }									\
      else								\
      { while( *++a );  a++;						\
        debug0(DFT, D, "  failure");					\
      }									\
    }									\
  }


/*****************************************************************************/
/*                                                                           */
/*  FontStripQuotes(str, xfpos)                                              */
/*                                                                           */
/*  Destructively replace str by its unquoted version.                       */
/*                                                                           */
/*****************************************************************************/

FontStripQuotes(str, xfpos)
unsigned char *str;  FILE_POS *xfpos;
{ unsigned char *p, *q;
  int ch;
  debug1(DFT, D, "FontStripQuotes( %s )", str);
  q = str;  FirstChar(str, ch, xfpos);
  while( ch != '\0' )
  { *q++ = ch;
    NextChar(str, ch, xfpos);
  }
  *q++ = '\0';
  debug1(DFT, D, "FontStripQuotes returning, result is %s", str);
} /* end FontStripQuotes */


/*@@**************************************************************************/
/*                                                                           */
/*  Definitions for metrics                                                  */
/*                                                                           */
/*****************************************************************************/

#define	NO_FONT		  0	/* the not-a-font font number                */
#define	MAX_CHARS	256	/* maximum number of chars in a font         */
#define SZ_DFT	       1000	/* default lout size is 50p                  */

#define font_num(x)		word_font(x)
#define	font_size(x)		back(x, COL)
#define	font_xheight2(x)	fwd(x, COL)

struct metrics {
  LENGTH up;
  LENGTH down;
  LENGTH left;
  LENGTH right;
};

static struct metrics	*size_table[MAX_FONT];	/* metrics of sized fonts    */
static unsigned char	*lig_table[MAX_FONT];	/* ligatures                 */
static OBJECT		font_table[MAX_FONT];	/* record of sized fonts     */
static OBJECT		font_root;		/* root of tree of fonts     */
static FONT_NUM		fontcount;		/* number of sized fonts     */

static FILE	*out_fp;		/* output file                       */
static short	currentfont;		/* font of most recent atom          */
static BOOLEAN	cpexists;		/* true if a current point exists    */
static LENGTH	currenty;		/* if cpexists, its y coordinate     */
static int	wordcount;		/* atoms printed since last newline  */
static int	pagecount;		/* total number of pages printed     */
static BOOLEAN	prologue_done;		/* TRUE after prologue is printed    */
static OBJECT	needs;			/* Resource needs of included EPSFs  */


/*****************************************************************************/
/*                                                                           */
/*  PrintInit(file_ptr)                                                      */
/*                                                                           */
/*  Initialise this module.  Output is to go to FILE file_ptr.               */
/*                                                                           */
/*****************************************************************************/

PrintInit(file_ptr)
FILE *file_ptr;
{ debug0(DFT, D, "PrintInit()");
  out_fp	= file_ptr;
  prologue_done	= FALSE;
  currentfont	= NO_FONT;
  cpexists	= FALSE;
  wordcount	= pagecount = 0;
  fontcount	= 0;
  font_root	= New(ACAT);
  needs		= New(ACAT);
  debug0(DFT, D, "PrintInit returning.");
}


/*****************************************************************************/
/*                                                                           */
/*  FontDebug()                        	                                     */
/*                                                                           */
/*  Print out font tree.                                                     */
/*                                                                           */
/*****************************************************************************/

#if DEBUG_ON
static FontDebug()
{ OBJECT family, face, filename, link, flink;  int i;
  assert( font_root != nil && type(font_root)==ACAT, "FontDebug: font_root!" );
  for( link = Down(font_root);  link != font_root;  link = NextDown(link) )
  { Child(family, link);
    assert( type(family) == WORD, "FontDebug: family!" );
    fprintf(stderr, "family %s:\n", string(family));
    for( flink = Down(family);  flink != family;  flink = NextDown(flink) )
    { Child(face, flink);
      assert( type(face) == WORD, "FontDebug: face!" );
      fprintf(stderr, "   face %s in file ", string(face));
      assert( Down(face) != face, "FontDebug: Down(face)!");
      Child(filename, Down(face));
      assert( type(filename) == WORD, "FontDebug: filename!" );
      fprintf(stderr, "%s\n", string(filename));
    }
  }
  for( i = 1;  i <= fontcount;  i++ )
    fprintf(stderr, "  font_table[%d] = %s\n",
		i, EchoObject(null, font_table[i]));
} /* end FontDebug */
#endif

/*@@**************************************************************************/
/*                                                                           */
/*  FontDefine(family, face, filename) 		                             */
/*                                                                           */
/*  Insert a font with this family, face and file name into the font tree.   */
/*                                                                           */
/*****************************************************************************/

FontDefine(family, face, filename)
OBJECT family, face, filename;
{ OBJECT link, y;
  debug3(DFT, D, "FontDefine( %s, %s, %s )",
	string(family), string(face), string(filename) );

  /* insert family into font tree if not already present */
  for( link = Down(font_root);  link != font_root;  link = NextDown(link) )
  { Child(y, link);
    if( strcmp(string(y), string(family)) == 0 )
    { Dispose(family);
      family = y;
      break;
    }
  }
  if( link == font_root )  Link(font_root, family);

  /* insert face into family, or error if already present */
  for( link = Down(family);  link != family;  link = NextDown(link) )
  { Child(y, link);
    if( strcmp(string(y), string(face)) == 0 )
    { Error(WARN, &fpos(face), "font %s %s already defined at%s",
	string(family), string(face), EchoFilePos(&fpos(y)));
      debug0(DFT, D, "FontDefine returning: font already defined");
      return;
    }
  }
  Link(family, face);

  assert( type(filename) == WORD, "FontDefine: filename!" );
  Link(face, filename);
  Child(filename, Down(face));
  assert( type(filename) == WORD, "FontDefine: filename!" );
  debug0(DFT, D, "FontDefine returning.");
  /* ifdebug(DFT, DD, FontDebug()); */
} /* end FontDefine */


/*@@**************************************************************************/
/*                                                                           */
/*  static ReadFont(face, err)                                               */
/*                                                                           */
/*  Read in a font file.  Object err is used only for error reporting.       */
/*                                                                           */
/*****************************************************************************/

#define is_letter(ch) ( ((ch)>='A' && (ch)<='Z') || ((ch)>='a' && (ch)<='z') )

static int find_ch(str, char_name)
unsigned char *str;  OBJECT char_name[];
{ int i;
  for( i = 0;  i < MAX_CHARS;  i++ )
    if( char_name[i] != nil && strcmp(string(char_name[i]), str) == 0 )
      return i;
  return -1;
} /* end find_ch */

static ReadFont(face, err)
OBJECT face, err;
{ OBJECT filename, fontname;
  unsigned char buff[MAX_LINE], command[MAX_LINE];
  int wx, llx, lly, urx, ury, xheight2, ch, i, lnum, offset;
  BOOLEAN xhfound, chfound, wxfound, bfound;
  FILE *fp;
  struct metrics *fnt;
  unsigned char *lig;
  OBJECT char_name[MAX_CHARS], lig_list[MAX_CHARS];
  OBJECT x, y, z, link, zlink;
  char *malloc();
  assert( type(face) == WORD, "ReadFont: type(face) != WORD!" );
  debug1(DFT, DD, "ReadFont( %s, err )", string(face));

  /* initialize font number and font_table entries, char_name and lig_list */
  if( ++fontcount >= MAX_FONT )
     Error(FATAL, &fpos(err), "too many different fonts and sizes (max is %d)",
       MAX_FONT - 1);
  for( i = 0;  i < MAX_CHARS;  i++ )  char_name[i] = lig_list[i] = nil;

  /* open Adobe font metrics (.AFM) file */
  assert( Down(face) != face, "ReadFont: filename missing!" );
  Child(filename, Down(face));
  fp = OpenFile(DefineFile(filename, FONT_FILE, FONT_PATH), FALSE);
  if( fp == NULL )  Error(FATAL, &fpos(filename),
    "Cannot open font file %s", string(filename));
  fnt = (struct metrics *) malloc(MAX_CHARS * sizeof(struct metrics));
  if( ( (unsigned char *) fgets(buff, MAX_LINE, fp) ) != buff ||
	sscanf(buff, "%s", command) != 1 ||
	strcmp(command, "StartFontMetrics") != 0 )
  { Error(FATAL, &fpos(filename),
      "font file %s does not begin with StartFontMetrics", string(filename));
  }

  /* read font metrics file */
  xhfound = FALSE;  fontname = nil;  lnum = 1;
  while ( ( (unsigned char *) fgets(buff, MAX_LINE, fp) ) == buff )
  {
    lnum++;
    sscanf(buff, "%s", command);
    switch( command[0] )
    {

      case 'X':

	if( strcmp(command, "XHeight") == 0 ) 
	{
	  if( xhfound )
	  { Error(FATAL, &fpos(filename),
	      "XHeight found twice in font file (line %d)", lnum);
	  }
	  sscanf(buff, "XHeight %d", &xheight2);
	  xheight2 = xheight2 / 2;
	  xhfound = TRUE;
	}
	break;


      case 'F':

	if( strcmp(command, "FontName") == 0 )
	{ if( fontname != nil )
	  { Error(FATAL, &fpos(filename),
	      "FontName found twice in font file %s (line %d)",
	      string(filename), lnum);
	  }
	  sscanf(buff, "FontName %s", command);
	  fontname = MakeWord(command, &fpos(filename));
	  if( string(fontname)[0] == '\0' )
	  { Error(FATAL, &fpos(filename),
	      "FontName empty in font file %s (line %d)",
	      string(filename), lnum);
	  }
	}
	break;


      case 'S':

	if( strcmp(command, "StartCharMetrics") == 0 )
	{
	  if( fontname == nil )  Error(FATAL, &fpos(filename),
		"FontName missing in file %s", string(filename));
	  if( !xhfound )  xheight2 = DEFAULT_XHEIGHT / 2;
	  while( ( (unsigned char *) fgets(buff, MAX_LINE, fp) ) == buff )
	  {
	    debug1(DFT, DD, "ReadFont reading %s", buff);
	    lnum++;
	    sscanf(buff, "%s", command);
	    if( strcmp(command, "EndCharMetrics") == 0 )
	    {
	      /* make a new font record and insert into font tree */
	      font_size(fontname) = SZ_DFT;
	      font_xheight2(fontname) = xheight2;
	      font_num(fontname) = fontcount;
	      font_table[fontcount] = fontname;
	      size_table[fontcount] = fnt;
	      Link(face, fontname);

	      /* construct ligature table */
	      ifdebug(DFT, D,
		fprintf(stderr, "Ligatures for font %s\n", string(filename));
	        for( i = 0;  i < MAX_CHARS;  i++ )
	        { fprintf(stderr, "%3d (%c) %s:\t", i, is_letter(i) ? i : '?',
			char_name[i] != nil ? string(char_name[i])
					    : (unsigned char *) "<nil>" );
		  x = lig_list[i];
		  if( x != nil )
		  for( link = Down(x);  link != x;  link = NextDown(link) )
		  { fprintf(stderr, " L ");
		    Child(y, link);
		    for( zlink= Down(y);  zlink != y;  zlink = NextDown(zlink) )
		    { Child(z, zlink);
		      fprintf(stderr, " %s", string(z));
		    }
		    fprintf(stderr, " ;");
		  }
		  fprintf(stderr, "\n");
	        }
	      );
	      lig = (unsigned char *) malloc(2*MAX_CHARS);
	      if( lig == NULL )  Error(FATAL, &fpos(filename),
		"run out of memory reading font file %s", string(filename));
	      for( i = 0;  i < MAX_CHARS;  i++ )  lig[i] = 0;
	      offset = MAX_CHARS+1;
	      for( ch = 0;  ch < MAX_CHARS;  ch++ )
	      { if( lig_list[ch] == nil )  continue;
		lig[ch] = offset - MAX_CHARS;
		for( link = Down(lig_list[ch]);  link != lig_list[ch];  link =
			NextDown(link) )
		{ lig[offset++] = ch;
		  Child(y, link);
		  for( zlink = Down(y);  zlink != y;  zlink = NextDown(zlink) )
		  { Child(z, zlink);
		    if( offset >= 2*MAX_CHARS-3 )  Error(FATAL, &fpos(filename),
			"too many ligatures in font file %s", string(filename));
		    i = find_ch(string(z), char_name);
		    if( i == -1 )  Error(FATAL, &fpos(filename),
			"unknown character name %s in font file %s", string(z),
			string(filename));
		    lig[offset++] = i;
		  }
		  lig[offset++] = '\0';
		}
		lig[offset++] = '\0';
	      }
	      lig_table[fontcount] = lig;

	      /* debug and exit */
	      fclose(fp);
	      for( i = 0;  i < MAX_CHARS;  i++ )
	      {	if( char_name[i] != nil )  Dispose(char_name[i]);
		if( lig_list[i] != nil )  DisposeObject(lig_list[i]);
	      }
	      debug4(DFT, D, "ReadFont returning: %d, name %s, fs %d, xh2 %d",
		fontcount, string(fontname), font_size(fontname), xheight2);
	      return;
	    }
	    ch = -1;  
	    chfound = wxfound = bfound = FALSE;
	    i = 0;  while( buff[i] == ' ' )  i++;
	    while( buff[i] != '\n' )
	    {
	      sscanf(&buff[i], "%s", command);
	      if( strcmp(command, "C") == 0 )
	      {	sscanf(&buff[i], "C %d", &ch);
		chfound = TRUE;
	      }
	      if( strcmp(command, "N") == 0 )
	      { if( !chfound )  Error(FATAL, &fpos(filename),
		  "N precedes C in font file %s (line %d)",
		  string(filename), lnum);
		sscanf(&buff[i], "N %s", command);
		char_name[ch] = MakeWord(command, no_fpos);
	      }
	      else if( strcmp(command, "WX") == 0 )
	      {	sscanf(&buff[i], "WX %d", &wx);
		wxfound = TRUE;
	      }
	      else if( strcmp(command, "B") == 0 )
	      { sscanf(&buff[i], "B %d %d %d %d", &llx, &lly, &urx, &ury);
		bfound = TRUE;
	      }
	      else if( strcmp(command, "L") == 0 )
	      { if( !chfound )  Error(FATAL, &fpos(filename),
		  "L precedes C in font file %s (line %d)",
		  string(filename), lnum);
		if( lig_list[ch] == nil )  lig_list[ch] = New(ACAT);
		y = New(ACAT);
		Link(lig_list[ch], y);
		i++; /* skip L */
		while( buff[i] == ' ' )  i++;
		while( buff[i] != ';' && buff[i] != '\n' )
		{ sscanf(&buff[i], "%s", command);
		  z = MakeWord(command, no_fpos);
		  Link(y, z);
		  while( buff[i] != ' ' && buff[i] != ';' )  i++;
		  while( buff[i] == ' ' ) i++;
		}
	      }
	      while( buff[i] != ';' && buff[i] != '\n' )  i++;
	      if( buff[i] == ';' )
	      { i++;  while( buff[i] == ' ' ) i++;
	      }
	    }
	    if( !chfound )
	    { Error(FATAL, &fpos(filename),
	      "C missing in font file %s (line %d)", string(filename), lnum);
	    }
	    if( !wxfound )
	    { Error(FATAL, &fpos(filename),
	     "WX missing in font file %s (line %d)", string(filename), lnum);
	    }
	    if( !bfound )
	    { Error(FATAL, &fpos(filename),
	      "B missing in font file %s (line %d)", string(filename), lnum);
	    }
	    if( ch >= 0 && ch < MAX_CHARS )
	    { fnt[ch].left  = llx;
	      fnt[ch].down  = lly - xheight2;
	      fnt[ch].right = wx;
	      fnt[ch].up    = ury - xheight2;
	      debug5(DFT, DD, "  fnt[%c] = (%d,%d,%d,%d)", ch, fnt[ch].left,
		fnt[ch].down, fnt[ch].right, fnt[ch].up);
	    }
	  }
	  Error(FATAL, &fpos(filename),
		"EndCharMetrics missing from font file %s", string(filename));
	}
	break;


      default:

	break;

    }
  }
  Error(FATAL, &fpos(filename),
	"StartCharMetrics missing from font file %s", string(filename));
} /* end ReadFont */


/*@@**************************************************************************/
/*                                                                           */
/*  FontChange(style, x)                                                     */
/*                                                                           */
/*  Returns an internal font number which is the current font changed        */
/*  according to word object x.  e.g. if current font is Roman 12p and x is  */
/*  "-3p", then FontChange returns the internal font number of Roman 9p.     */
/*                                                                           */
/*****************************************************************************/

FontChange(style, x)
STYLE *style;  OBJECT x;
{ /* register */ int i;
  OBJECT par[3], family, face, fsize, y, link, new, old, tmpf;
  GAP gp;  LENGTH flen;  int num, c;  unsigned inc;
  struct metrics *newfnt, *oldfnt;  char *malloc();
  debug2(DFT, D, "FontChange( %s, %s )", EchoStyle(style), EchoObject(null, x));
  assert( font(*style)>=0 && font(*style)<=fontcount, "FontChange: fontcount!");
  /* ifdebug(DFT, DD, FontDebug()); */

  /* set par[0..num-1] to the 1, 2 or 3 parameters of the font operator */
  num = 0;
  if( type(x) == WORD )
  { par[num++] = x;
    FontStripQuotes(string(x), &fpos(x));
  }
  else if( type(x) == ACAT )
  { for( link = Down(x);  link != x;  link = NextDown(link) )
    { Child(y, link);
      debug1(DFT, DD, "  pars examining y = %s", EchoObject(null, y));
      if( type(y) == GAP_OBJ )  continue;
      if( type(y) != WORD || num >= 3 )
      {	Error(WARN, &fpos(x), "error in left parameter of %s", KW_FONT);
	debug0(DFT, D, "FontChange returning: ACAT children");
	return;
      }
      par[num++] = y;
      FontStripQuotes(string(y), &fpos(x));
    }
  }
  else
  { Error(WARN, &fpos(x), "error in left parameter of %s", KW_FONT);
    debug0(DFT, D, "FontChange returning: wrong type");
    return;
  }
  debug1(DFT, DD, " found pars, num = %d", num);

  /* extract fsize parameter, if any */
  assert( num >= 1 && num <= 3, "FontChange: num!" );
  c = string(par[num-1])[0];
  if( c == '+' || c == '-' || (c >= '0' && c <= '9') )
  { fsize = par[num-1];  num--;
  }
  else fsize = nil;

  /* check for initial font case: must have family, face, and size */
  if( font(*style) == NO_FONT && (fsize == nil || num < 2) )
    Error(FATAL, &fpos(x), "initial font must have family, face and size");

  /* get font family */
  if( num == 2 )
  {
    /* par[0] contains a new family name */
    for( link = Down(font_root);  link != font_root;  link = NextDown(link) )
    { Child(family, link);
      if( strcmp(string(family), string(par[0])) == 0 )  break;
    }
    if( link == font_root )
    { Error(WARN,&fpos(par[0]), "font family %s not defined", string(par[0]));
      return;
    }
  }
  else
  { /* preserve current family */
    assert( Up(font_table[font(*style)]) != font_table[font(*style)],
      "FontChange: Up(font_table[font(*style)]) !" );
    Parent(face, Up(font_table[font(*style)]));
    assert( type(face) == WORD, "FontChange: type(face)!" );
    assert( Up(face) != face, "FontChange: Up(face)!" );
    Parent(family, Up(face));
    assert( type(family) == WORD, "FontChange: type(family)!" );
  }

  /* get font face */
  if( num != 0 )
  {
    /* par[num-1] contains a new face name */
    for( link = Down(family);  link != family;  link = NextDown(link) )
    { Child(face, link);
      if( strcmp(string(face), string(par[num-1])) == 0 )  break;
    }
    if( link == family )
    {
      /* missing face name; first check whether a family name was intended */
      for( link = Down(font_root);  link != font_root;  link = NextDown(link) )
      {	Child(tmpf, link);
	if( strcmp(string(tmpf), string(par[num-1])) == 0 )  break;
      }
      if( font_root == Down(font_root) )
      {	Error(FATAL, &fpos(par[num-1]), "there are no fonts");
      }
      else if( link != font_root )
      {	Error(WARN, &fpos(par[num-1]),
	    "font family name %s must be accompanied by a face name",
	    string(par[num-1]));
      }
      else Error(WARN, &fpos(par[num-1]),
	    "font face name %s not defined in font family %s",
		string(par[num-1]), string(family));
      return;
    }
  }
  else
  {
    /* preserve current face name */
    Parent(face, Up(font_table[font(*style)]));
    assert( type(face) == WORD, "FontChange: type(face)!" );
    assert( Up(face) != face, "FontChange: Up(face)!" );
  }

  /* get font size */
  if( fsize == nil )  flen = font_size(font_table[font(*style)]);
  else 
  { GetGap(fsize, style, &gp, &inc);
    if( inc == ABS )  flen = width(gp);
    else if( font(*style) == NO_FONT )
      Error(FATAL, &fpos(fsize), "no font encloses this %s", string(fsize));
    else if( inc==INC )  flen = font_size(font_table[font(*style)])+width(gp);
    else if( inc==DEC )  flen = font_size(font_table[font(*style)])-width(gp);
    else Error(INTERN, &fpos(x), "GetGap returned inc = %d!", inc);
  }

  if( flen <= 0 )
  { Error(WARN, &fpos(fsize), "%s %s ignored: result is not positive",
      string(fsize), KW_FONT);
    return;
  }

  /* if the font file has not been read before, read it now */
  assert( Down(face) != face && type(Down(face)) == LINK, "FontChange: dn!" );
  if( Down(face) == LastDown(face) )  ReadFont(face, x);
  assert( Down(face) != LastDown(face), "FontChange: after ReadFont!" );

  /* search fonts of face for desired size; return if already present */
  for( link = NextDown(Down(face));  link != face;  link = NextDown(link) )
  { Child(fsize, link);
    if( font_size(fsize) == flen )
    { font(*style) = font_num(fsize);
      SetGap( space_gap(*style), FALSE, TRUE, FIXED_UNIT, EDGE_MODE,
			size_table[font_num(fsize)][' '].right);
      debug2(DFT, D,"FontChange returning (old) %d (XHeight2 = %d)",
			font(*style), font_xheight2(font_table[font(*style)]));
      return;
    }
  }

  /* insert new sized font record into tree */
  if( ++fontcount >= MAX_FONT )
    Error(FATAL, &fpos(x), "too many different fonts and sizes (max is %d)",
      MAX_FONT - 1);
  assert( Down(face) != face && NextDown(Down(face)) != face, "FontChange!!" );
  Child(old, NextDown(Down(face)));
  assert( type(old) == WORD, "FontChange: old!" );
  new = MakeWord(string(old), no_fpos);
  Link(face, new);
  font_size(new)        = flen;
  font_xheight2(new)    = font_xheight2(old) * font_size(new) / font_size(old);
  font_num(new)         = fontcount;
  font_table[fontcount] = new;
  size_table[fontcount] =
	(struct metrics *) malloc(MAX_CHARS * sizeof(struct metrics));
  lig_table[fontcount]  = (unsigned char *) malloc(2*MAX_CHARS);

  /* scale old font to new size */
  newfnt = size_table[font_num(new)];
  oldfnt = size_table[font_num(old)];
  for( i = 0;  i < MAX_CHARS;  i++ )
  { newfnt[i].left  = (oldfnt[i].left  * font_size(new)) / font_size(old);
    newfnt[i].right = (oldfnt[i].right * font_size(new)) / font_size(old);
    newfnt[i].down  = (oldfnt[i].down  * font_size(new)) / font_size(old);
    newfnt[i].up    = (oldfnt[i].up    * font_size(new)) / font_size(old);
  }
  for( i = 0;  i < 2*MAX_CHARS;  i++ )
    lig_table[font_num(new)][i] = lig_table[font_num(old)][i];

  /* return new font number and exit */
  font(*style) = fontcount;
  SetGap( space_gap(*style), FALSE, TRUE, FIXED_UNIT, EDGE_MODE,
    size_table[fontcount][' '].right);
  debug2(DFT, D,"FontChange returning (scaled) %d (XHeight2 = %d)",
    font(*style), font_xheight2(font_table[font(*style)]));
  /* FontDebug(); */
} /* end FontChange */

/*@@**************************************************************************/
/*                                                                           */
/*  FontAtomSize(x)                                                          */
/*                                                                           */
/*  Set the horizontal and vertical sizes of literal atom x.                 */
/*                                                                           */
/*****************************************************************************/

FontAtomSize(x)
OBJECT x;
{ 
  /* register */ unsigned char *p;
  /* register */ int r, u, d, ch, newch;
  struct metrics *fnt;  unsigned char *lig;

  debug2(DFT, D, "FontAtomSize( %s ), font = %d", string(x), word_font(x));
  FirstChar(string(x), ch, &fpos(x));
  debug1(DFT, DDD, "  ch = %d", ch);
  if( ch == '\0' )
  { back(x, COL) = fwd(x, COL) = 0;
    back(x, ROW) = fwd(x, ROW) = 0;
  }
  else
  { if ( word_font(x) < 1 || word_font(x) > fontcount )
      Error(FATAL, &fpos(x), "%s operator missing, word is %s",
	KW_FONT, string(x));
    fnt = size_table[word_font(x)];
    lig = lig_table[word_font(x)];
    CheckLigature(string(x), ch, lig);
    d = fnt[ch].down;
    u = fnt[ch].up;
    r = fnt[ch].right;
    NextChar(string(x), ch, &fpos(x));
    CheckLigature(string(x), ch, lig);
    debug1(DFT, DDD, "  ch = %d", ch);
    while( ch )
    { if( fnt[ch].up   > u )  u = fnt[ch].up;
      if( fnt[ch].down < d )  d = fnt[ch].down;
      r += fnt[ch].right;
      NextChar(string(x), ch, &fpos(x));
      CheckLigature(string(x), ch, lig);
      debug1(DFT, DDD, "  ch = %d", ch);
    }
    back(x, COL) = 0;
    fwd(x, COL)  = max(r, 0);
    back(x, ROW) = max(u, 0);
    fwd(x, ROW)  = max(-d, 0);
  } 
  debug4(DFT, D, "FontAtomSize returning %hd %hd %hd %hd",
	  back(x, COL), fwd(x, COL), back(x, ROW), fwd(x, ROW));
} /* end FontAtomSize */


/*****************************************************************************/
/*                                                                           */
/*  LENGTH FontSize(fnum, x)                                                 */
/*                                                                           */
/*  Return the size of this font.  x is for error messages only.             */
/*                                                                           */
/*****************************************************************************/

LENGTH FontSize(fnum, x)
FONT_NUM fnum;  OBJECT x;
{
  debug1(DFT, D, "FontSize( %d )", fnum);
  assert( fnum <= fontcount, "FontSize!" );
  if( fnum <= 0 )  Error(FATAL, &fpos(x), "no current font at this point");
  debug1(DFT, D, "FontSize returning %d", font_size(font_table[fnum]));
  return font_size(font_table[fnum]);
} /* end FontSize */


/*@@**************************************************************************/
/*                                                                           */
/*  PrintPrologue(h, v)                                                      */
/*                                                                           */
/*  Generate the standard PostScript prologue, augmented with any @Prologue  */
/*  or @SysPrologue files specified by the user.                             */
/*  The first non-empty page has width h and height v in Lout units.         */
/*  The following PostScript operators are defined:                          */
/*                                                                           */
/*      scale_factor  fnt       scale and set font                           */
/*      x_coordinate  x         move to x_coordinate, current y coordinate   */
/*      string        s         show string                                  */
/*      number        in        result is number inches                      */
/*      number        cm        result is number centimetres                 */
/*      number        pt        result is number points                      */
/*      number        sp        result is number spaces                      */
/*      number        vs        result is number v's                         */
/*      number        ft        result is number font-sizes                  */
/*                                                                           */
/*  as well as loutgr, for use with Lout's @Graphic operator:                */
/*                                                                           */
/*      xsize ysize xmark ymark fr vs sp loutgr -                            */
/*                                                                           */
/*  Define xmark, ymark, xsize, ysize to be the positions of                 */
/*  these features of x, and define symbols ft, vs and sp                    */
/*  to be the current font size, line separation, and space width.           */
/*                                                                           */
/*****************************************************************************/

PrintPrologue(h, v)
LENGTH h, v;
{ FILE_NUM fnum;
  debug2(DGP, DD, "PrintPrologue: v = %d   h = %d", v, h);

  /* print header comments for PostScript DSC 3.0 output */
  if( Encapsulated )
    fprintf(out_fp, "%%%!PS-Adobe-3.0 EPSF-3.0\n");
  else
    fprintf(out_fp, "%%%!PS-Adobe-3.0\n");
  fprintf(out_fp, "%%%%Creator: %s\n", LOUT_VERSION);
  fprintf(out_fp, "%%%%CreationDate: %s", TimeString());
  fprintf(out_fp, "%%%%DocumentNeededResources: (atend)\n");
  fprintf(out_fp, "%%%%Pages: (atend)\n");
  fprintf(out_fp, "%%%%BoundingBox: 0 0 %d %d\n", h/PT, v/PT);
  fprintf(out_fp, "%%%%EndComments\n");

  /* print procedure definitions part of header */
  fprintf(out_fp, "%%%%BeginProlog\n");
  fprintf(out_fp, "%%%%BeginResource: procset LoutStartUp\n");
  fprintf(out_fp, "/fnt { exch findfont exch scalefont setfont } def\n");
  fprintf(out_fp, "/x { currentpoint exch pop moveto } def\n");
  fprintf(out_fp, "/s { show } def\n");
  fprintf(out_fp, "/in { %d mul } def\n", IN);
  fprintf(out_fp, "/cm { %d mul } def\n", CM);
  fprintf(out_fp, "/pt { %d mul } def\n", PT);
  fprintf(out_fp, "/em { %d mul } def\n", EM);
  fprintf(out_fp, "/sp { louts mul } def\n");
  fprintf(out_fp, "/vs { loutv mul } def\n");
  fprintf(out_fp, "/ft { loutf mul } def\n");
  fprintf(out_fp, "/dg {           } def\n");

  fputs("/loutgr {\n",						out_fp);
  fputs("  /louts exch def\n",					out_fp);
  fputs("  /loutv exch def\n",					out_fp);
  fputs("  /loutf exch def\n",					out_fp);
  fputs("  /ymark exch def\n",					out_fp);
  fputs("  /xmark exch def\n",					out_fp);
  fputs("  /ysize exch def\n",					out_fp);
  fputs("  /xsize exch def\n} def\n",				out_fp);

  /* print definitions used by Lout output when including EPSF files */
  /* copied from PostScript Language Reference Manual (2nd Ed.), page 726 */
  fputs("/BeginEPSF {\n",					out_fp);
  fputs("  /LoutEPSFState save def\n",				out_fp);
  fputs("  /dict_count countdictstack def\n",			out_fp);
  fputs("  /op_count count 1 sub def\n",			out_fp);
  fputs("  userdict begin\n",					out_fp);
  fputs("  /showpage { } def\n",				out_fp);
  fputs("  0 setgray 0 setlinecap\n",				out_fp);
  fputs("  1 setlinewidth 0 setlinejoin\n",			out_fp);
  fputs("  10 setmiterlimit [] 0 setdash newpath\n",		out_fp);
  fputs("  /languagelevel where\n",				out_fp);
  fputs("  { pop languagelevel\n",				out_fp);
  fputs("    1 ne\n",						out_fp);
  fputs("    { false setstrokeadjust false setoverprint\n",	out_fp);
  fputs("    } if\n",						out_fp);
  fputs("  } if\n",						out_fp);
  fputs("} bind def\n",						out_fp);

  fputs("/EndEPSF {\n",						out_fp);
  fputs("  count op_count sub { pop } repeat\n",		out_fp);
  fputs("  countdictstack dict_count sub { end } repeat\n",	out_fp);
  fputs("  LoutEPSFState restore\n",				out_fp);
  fputs("} bind def\n",						out_fp);

  fputs("%%EndResource\n",					out_fp);

  /* print prepend files (assumed to be organized as DSC 3.0 Resources) */
  for( fnum=FirstFile(PREPEND_FILE);  fnum != NO_FILE;  fnum=NextFile(fnum) )
  { char buff[MAX_LINE];  FILE *fp;
    if( (fp = OpenFile(fnum, FALSE)) == null )
      Error(WARN, PosOfFile(fnum), "cannot open %s file %s",
	KW_PREPEND, FileName(fnum));
    else if( fgets(buff, MAX_LINE, fp) == NULL )
      Error(WARN, PosOfFile(fnum), "%s file %s is empty",
	KW_PREPEND, FileName(fnum));
    else
    {
      if( !StringBeginsWith(buff, "%%BeginResource:") )
	Error(WARN, PosOfFile(fnum),
	  "%s file %s lacks PostScript DSC 3.0 \"%%%%BeginResource:\" comment",
	  KW_PREPEND, FileName(fnum));
      fputs(buff, out_fp);
      fprintf(out_fp, "\n%% %s file %s\n", KW_PREPEND, FileName(fnum));
      while( fgets(buff, MAX_LINE, fp) != NULL )  fputs(buff, out_fp);
    }
  }

  fputs("\n%%EndProlog\n\n", out_fp);
  fprintf(out_fp, "%%%%Page: ? %d\n", ++pagecount);
  fprintf(out_fp, "%%%%BeginPageSetup\n");
  fprintf(out_fp, "%.4f dup scale %d setlinewidth\n", 1.0 / PT, PT/2);
  fprintf(out_fp, "/pgsave save def\n");
  fprintf(out_fp, "%%%%EndPageSetup\n");
  prologue_done = TRUE;
} /* end PrintPrologue */

/*@@**************************************************************************/
/*                                                                           */
/*  PrintOriginIncrement(y)                                                  */
/*                                                                           */
/*  Move current vertical origin down by y.                                  */
/*                                                                           */
/*****************************************************************************/

PrintOriginIncrement(y)
LENGTH y;
{ debug1(DGP, D, "PrintOriginIncrement( %d )", y );
  fprintf(out_fp, "\npgsave restore\nshowpage\n");
  cpexists = FALSE;
  currentfont = NO_FONT;
  if( Encapsulated )
  { PrintClose();
    Error(FATAL, no_fpos, "truncating -EPS document at end of first page");
  }
  fprintf(out_fp, "\n%%%%Page: ? %d\n", ++pagecount);
  fprintf(out_fp, "%%%%BeginPageSetup\n");
  fprintf(out_fp, "%.4f dup scale %d setlinewidth\n", 1.0 / PT, PT/2);
  fprintf(out_fp, "/pgsave save def\n");
  fprintf(out_fp, "%%%%EndPageSetup\n");
  wordcount = 0;
}


/*****************************************************************************/
/*                                                                           */
/*  PrintAtom(x, hpos, vpos)                                                 */
/*                                                                           */
/*  Print word x; its marks cross at the point (hpos, vpos).                 */
/*                                                                           */
/*****************************************************************************/

PrintAtom(x, hpos, vpos)
OBJECT x;  int hpos, vpos;
{ unsigned char *p;

  debug4(DGP, DD, "PrintAtom( %s, %d, %d ) font %d", string(x),
	hpos, vpos, word_font(x));

  /* if font is different to previous word then print change */
  if (word_font(x) != currentfont)
  { currentfont = word_font(x);
    assert( type(font_table[currentfont])==WORD, "PrintAtom: font_table!" );
    if( string(font_table[currentfont])[0] == '\0' )
    { Error(INTERN, &fpos(font_table[currentfont]),
	"font bug: font %d, addr %d, string addr %d (hex 0x%x)",
	currentfont, font_table[currentfont],
	string(font_table[currentfont]), string(font_table[currentfont]));
    }
    fprintf(out_fp, "\n/%s %hd fnt\n",
      string(font_table[currentfont]), font_size(font_table[currentfont]));
  }

  /* move to coordinate of x */
  debug1(DGP, DDD, "  xheight2 = %d", font_xheight2(font_table[currentfont]));
  vpos = vpos - font_xheight2(font_table[currentfont]);
  if( cpexists && currenty == vpos )
  { printnum(hpos, out_fp);
    fputs(" x", out_fp);
  }
  else
  { currenty = vpos;
    printnum(hpos, out_fp);
    putc(' ', out_fp);
    printnum(currenty, out_fp);
    fputs(" moveto", out_fp);
    cpexists = TRUE;
  }

  /* show string(x) */
  putc('(', out_fp);
  p = string(x);
  while( *p != '\0' )  switch( *p )
  {
    case '"':	p++;
		break;

    case '\\':	switch( *++p )
		{
		  case '\0':	break;
      
		  case '"':	putc(*p++, out_fp);
				break;

		  case '\\':
		  case '0':
		  case '1':
		  case '2':
		  case '3':
		  case '4':
		  case '5':
		  case '6':
		  case '7':	putc('\\', out_fp);
				putc(*p++,   out_fp);
				break;

		  default:	/* denotes print in octal e.g. ligature */
				putc('\\', out_fp);
				fprintf(out_fp, "%03o", *p++);
		}
		break;

    case '(':	
    case ')':	putc('\\', out_fp);
		putc(*p++, out_fp);
		break;

    default:	putc(*p++, out_fp);
		break;
  }
  if( ++wordcount >= 5 )
  { fputs(")s\n", out_fp);  wordcount = 0;
  }
  else fputs(")s ", out_fp);

  debug0(DGP, DDD, "PrintAtom returning");
} /* end PrintAtom */


/*@@**************************************************************************/
/*                                                                           */
/*  PrintClose()                                                             */
/*                                                                           */
/*  Clean up this module and close output stream.                            */
/*                                                                           */
/*****************************************************************************/

PrintClose()
{ OBJECT family, face, x, link, flink;  BOOLEAN first_need;
  if( prologue_done )
  { fprintf(out_fp, "\npgsave restore\nshowpage\n");
    fprintf(out_fp, "%%%%Trailer\n");

    /* print document fonts line */
    /* *** obsolete DSC 1.0 version
    fprintf(out_fp, "%%%%DocumentFonts:");
    for( link = Down(font_root); link != font_root; link = NextDown(link) )
    { Child(family, link);
      for( flink = Down(family);  flink != family;  flink = NextDown(flink) )
      {	Child(face, flink);
	if( LastDown(face) != Down(face) )
	{ Child(x, LastDown(face));
	  fprintf(out_fp, " %s", string(x));
	}
      }
    }
    fprintf(out_fp, "\n");
    *** */

    /* print resource requirements (DSC 3.0 version) - fonts */
    first_need = TRUE;
    for( link = Down(font_root); link != font_root; link = NextDown(link) )
    { Child(family, link);
      for( flink = Down(family);  flink != family;  flink = NextDown(flink) )
      { Child(face, flink);
	if( LastDown(face) != Down(face) )
	{ Child(x, LastDown(face));
	  fprintf(out_fp, "%s font %s\n",
	    first_need ? "%%DocumentNeededResources:" : "%%+", string(x));
	  first_need = FALSE;
	}
      }
    }

    /* print resource requirements (DSC 3.0 version) - included EPSFs  */
    for( link = Down(needs); link != needs; link = NextDown(link) )
    { Child(x, link);
      assert(type(x) == WORD, "PrintClose: needs!" );
      fprintf(out_fp, "%s %s",
	first_need ? "%%DocumentNeededResources:" : "%%+", string(x));
      first_need = FALSE;
    }

    fprintf(out_fp, "%%%%Pages: %d\n", pagecount);
    fprintf(out_fp, "%%%%EOF\n");
  }
  DisposeObject(font_root);
} /* end PrintClose */


/*****************************************************************************/
/*                                                                           */
/*  CoordTranslate(xdist, ydist)                                             */
/*                                                                           */
/*  Translate coordinate system by the given x and y distances.              */
/*                                                                           */
/*****************************************************************************/

CoordTranslate(xdist, ydist)
LENGTH xdist, ydist;
{ debug2(DRS,D,"CoordTranslate(%s, %s)",
    EchoLength(xdist), EchoLength(ydist));
  fprintf(out_fp, "%d %d translate\n", xdist, ydist);
  cpexists = FALSE;
  currentfont = NO_FONT;
  debug0(DRS, D, "CoordTranslate returning.");
} /* end CoordTranslate */


/*@@**************************************************************************/
/*                                                                           */
/*  CoordRotate(amount)                                                      */
/*                                                                           */
/*  Rotate coordinate system by given amount (in internal DG units)          */
/*                                                                           */
/*****************************************************************************/

CoordRotate(amount)
LENGTH amount;
{ debug1(DRS, D, "CoordRotate(%.1f degrees)", (float) amount / DG);
  fprintf(out_fp, "%.4f rotate\n", (float) amount / DG);
  cpexists = FALSE;
  currentfont = NO_FONT;
  debug0(DRS, D, "CoordRotate returning.");
} /* end CoordRotate */


/*****************************************************************************/
/*                                                                           */
/*  CoordScale(ratio, dim)                                                   */
/*                                                                           */
/*  Scale coordinate system by ratio in the given dimension.                 */
/*                                                                           */
/*****************************************************************************/

CoordScale(hfactor, vfactor)
float hfactor, vfactor;
{ unsigned char buff[20];
  ifdebug(DRS, D, sprintf(buff, "%.3f, %.3f", hfactor, vfactor));
  debug1(DRS, D, "CoordScale(%s)", buff);
  fprintf(out_fp, "%.4f %.4f scale\n", hfactor, vfactor);
  cpexists = FALSE;
  currentfont = NO_FONT;
  debug0(DRS, D, "CoordScale returning.");
} /* end CoordScale */


/*****************************************************************************/
/*                                                                           */
/*  SaveGraphicState()                                                       */
/*                                                                           */
/*  Save current coord system on stack for later restoration.                */
/*                                                                           */
/*****************************************************************************/

SaveGraphicState()
{ debug0(DRS, D, "SaveGraphicState()");
  fprintf(out_fp, "gsave\n");
  debug0(DRS, D, "SaveGraphicState returning.");
} /* end SaveGraphicState */


/*****************************************************************************/
/*                                                                           */
/*  RestoreGraphicState()                                                    */
/*                                                                           */
/*  Restore previously saved coordinate system.  NB we normally assume that  */
/*  no white space is needed before any item of output, but since this       */
/*  procedure is sometimes called immediately after PrintGraphicObject(),    */
/*  which does not append a concluding space, we prepend one here.           */
/*                                                                           */
/*****************************************************************************/

RestoreGraphicState()
{ debug0(DRS, D, "RestoreGraphicState()");
  fprintf(out_fp, "\ngrestore\n");
  cpexists = FALSE;
  currentfont = NO_FONT;
  debug0(DRS, D, "RestoreGraphicState returning.");
} /* end RestoreGraphicState */


/*@@**************************************************************************/
/*                                                                           */
/*  PrintGraphicObject(x)                                                    */
/*                                                                           */
/*  Print object x on out_fp                                                 */
/*                                                                           */
/*****************************************************************************/

PrintGraphicObject(x)
OBJECT x;
{ OBJECT y, link;  unsigned char *p;
  switch( type(x) )
  {
    case WORD:
    
      for( p = string(x);  *p != '\0';  p++ )
      {	if( *p == '"' )
	  continue;
	else if( *p != '\\' )
	  putc(*p, out_fp);
	else if( *++p != '\0' )
	{ putc('\\', out_fp);
	  putc(*p, out_fp);
	}
      }
      break;
	

    case ACAT:
    
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	if( type(y) == GAP_OBJ )
	{ if( vspace(y) > 0 )  putc('\n', out_fp);
	  else if( hspace(y) > 0 ) putc(' ', out_fp);
	}
	else if( type(y) == WORD || type(y) == ACAT )  PrintGraphicObject(y);
	else if( type(y) != WIDE && !is_index(type(y)) )
		/* @Wide, indexes are sometimes inserted by Manifest */
	{ Error(WARN, &fpos(x), "error in left parameter of %s", KW_GRAPHIC);
	  debug1(DGP, D, "  type(y) = %s, y =", Image(type(y)));
	  ifdebug(DGP, D, EchoObject(stderr, y));
	}
      }
      break;


    default:
    
      Error(WARN, &fpos(x), "error in left parameter of %s", KW_GRAPHIC);
      debug1(DGP, D, "  type(x) = %s, x =", Image(type(x)));
      ifdebug(DGP, D, EchoObject(stderr, x));
      break;
  }
} /* end PrintGraphicObject */


/*****************************************************************************/
/*                                                                           */
/*  DefineGraphicNames(x)                                                    */
/*                                                                           */
/*  Generate PostScript for xsize, ysize etc. names of graphic object.       */
/*                                                                           */
/*****************************************************************************/

DefineGraphicNames(x)
OBJECT x;
{ OBJECT y;
  assert( type(x) == GRAPHIC, "PrintGraphic: type(x) != GRAPHIC!" );
  debug1(DRS, D, "DefineGraphicNames( %s )", EchoObject(null, x));
  debug1(DRS, DD, "  style = %s", EchoStyle(&save_style(x)));

  fprintf(out_fp, "%d %d %d %d %d %d %d loutgr\n",
    size(x, COL), size(x, ROW), back(x, COL), fwd(x, ROW),
    font(save_style(x)) <= 0 ? 12*PT : FontSize(font(save_style(x)), x),
    width(line_gap(save_style(x))), width(space_gap(save_style(x))));

  debug0(DRS, D, "DefineGraphicNames returning.");
} /* end DefineGraphicNames */


/*****************************************************************************/
/*                                                                           */
/*  PrintGraphicInclude(x, colmark, rowmark)                                 */
/*                                                                           */
/*  Print graphic include file, with appropriate surrounds.  This code       */
/*  closely follows the PostScript Language Reference Manual, 2n ed.,        */
/*  pages 733-5, except we don't clip the included EPSF.                     */
/*                                                                           */
/*  Note to porters: Version 3.0 of the EPSF standard is not compatible      */
/*  with previous versions.  Thus, Lout's output may crash your system.      */
/*  If you can find out which comment line(s) are causing the trouble,       */
/*  you can add to procedure strip_out to strip them out during the          */
/*  file inclusion step.  e.g. on my system %%EOF causes problems, so I      */
/*  strip it out.                                                            */
/*                                                                           */
/*****************************************************************************/
#define	SKIPPING	0
#define	READING_DNR	1
#define FINISHED	2

static BOOLEAN strip_out(buff)
unsigned char *buff;
{ if( StringBeginsWith(buff, "%%EOF") )  return TRUE;
  return FALSE;
} /* end strip_out */

PrintGraphicInclude(x, colmark, rowmark)
OBJECT x; LENGTH colmark, rowmark;
{ OBJECT y, full_name;  unsigned char buff[MAX_LINE];
  FILE *fp;  int state;
  debug0(DRS, D, "PrintGraphicInclude(x)");
  assert(type(x)==INCGRAPHIC || type(x)==SINCGRAPHIC, "PrintGraphicInclude!");
  assert(sparec(constraint(x)), "PrintGraphicInclude: sparec(constraint(x))!");

  /* open the include file and get its full path name */
  Child(y, Down(x));
  fp = OpenIncGraphicFile(string(y), type(x), &full_name, &fpos(y));
  assert( fp != NULL, "PrintGraphicInclude: fp!" );

  /* generate appropriate header code */
  fprintf(out_fp, "BeginEPSF\n");
  CoordTranslate(colmark - back(x, COL), rowmark - fwd(x, ROW));
  CoordScale( (float) PT, (float) PT );
  CoordTranslate(-back(y, COL), -back(y, ROW));
  fprintf(out_fp, "%%%%BeginDocument: %s\n", string(full_name));

  /* copy through the include file, except divert resources lines to needs */
  /* and strip out some comment lines that cause problems                  */
  state = (fgets(buff, MAX_LINE, fp) == NULL) ? FINISHED : SKIPPING;
  while( state != FINISHED ) switch(state)
  {
    case SKIPPING:

      if( StringBeginsWith(buff, "%%DocumentNeededResources:") &&
	  !StringContains(buff, "(atend)") == NULL )
      { x = MakeWord(&buff[strlen("%%DocumentNeededResources:")], no_fpos);
        Link(needs, x);
	state = (fgets(buff, MAX_LINE, fp) == NULL) ? FINISHED : READING_DNR;
      }
      else
      { if( StringBeginsWith(buff, "%%LanguageLevel:") )
	  Error(WARN, &fpos(x), "ignoring \"%%%%LanguageLevel\" in %s file %s",
		KW_INCGRAPHIC, string(full_name));
	if( StringBeginsWith(buff, "%%Extensions:") )
	  Error(WARN, &fpos(x), "ignoring \"%%%%Extensions\" in %s file %s",
		KW_INCGRAPHIC, string(full_name));
	if( !strip_out(buff) )  fputs(buff, out_fp);
	state = (fgets(buff, MAX_LINE, fp) == NULL) ? FINISHED : SKIPPING;
      }
      break;

    case READING_DNR:

      if( StringBeginsWith(buff, "%%+") )
      {	x = MakeWord(&buff[strlen("%%+")], no_fpos);
	Link(needs, x);
	state = (fgets(buff, MAX_LINE, fp) == NULL) ? FINISHED : READING_DNR;
      }
      else
      { if( !strip_out(buff) )  fputs(buff, out_fp);
	state = (fgets(buff, MAX_LINE, fp) == NULL) ? FINISHED : SKIPPING;
      }
      break;
  }

  /* wrapup */
  DisposeObject(full_name);
  fclose(fp);
  fprintf(out_fp, "%%%%EndDocument\nEndEPSF\n");
  debug0(DRS, D, "PrintGraphicInclude returning.");
} /* end PrintGraphicInclude */
