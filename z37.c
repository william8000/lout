/*@z37.c:Font Service:Declarations@*******************************************/
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
/*  FILE:         z37.c                                                      */
/*  MODULE:       Font Service                                               */
/*  EXTERNS:      FontInit(), FontDefine(), FontChange(), FontWordSize(),    */
/*                FontSize(), FontHalfXHeight(), FontEncoding(),             */
/*                FontEncoding(), FontFamilyAndFace(), FontNeeded()          */
/*                                                                           */
/*  This module implements fonts, using encoding vectors and Adobe font      */
/*  metrics files (.AFM files, version 2).                                   */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define DEFAULT_XHEIGHT 500	/* the default XHeight if font has none      */
#define	NO_FONT		  0	/* the not-a-font font number                */
#define	MAX_CHARS	256	/* maximum number of chars in a font         */
#define SZ_DFT	       1000	/* default lout size is 50p                  */

struct metrics {
  LENGTH up;
  LENGTH down;
  LENGTH left;
  LENGTH right;
};

static struct metrics	*size_table[MAX_FONT];	/* metrics of sized fonts    */
static FULL_CHAR	*lig_table[MAX_FONT];	/* ligatures                 */
static OBJECT		font_table[MAX_FONT];	/* record of sized fonts     */
static OBJECT		font_root;		/* root of tree of fonts     */
static FONT_NUM		fontcount;		/* number of sized fonts     */
static int		font_seqnum;		/* unique number for a font  */


/*@::FontInit(), FontDebug()@*************************************************/
/*                                                                           */
/*  FontInit()                                                               */
/*                                                                           */
/*  Initialise this module.                                                  */
/*                                                                           */
/*****************************************************************************/

FontInit()
{ debug0(DFT, D, "FontInit()");
  fontcount	= 0;
  font_root	= New(ACAT);
  font_seqnum	= 0;
  debug0(DFT, D, "FontInit returning.");
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
    assert( is_word(type(family)), "FontDebug: family!" );
    fprintf(stderr, "family %s:\n", string(family));
    for( flink = Down(family);  flink != family;  flink = NextDown(flink) )
    { Child(face, flink);
      assert( is_word(type(face)), "FontDebug: face!" );
      fprintf(stderr, "   face %s in file ", string(face));
      assert( Down(face) != face, "FontDebug: Down(face)!");
      Child(filename, Down(face));
      assert( is_word(type(filename)), "FontDebug: filename!" );
      fprintf(stderr, "%s\n", string(filename));
    }
  }
  for( i = 1;  i <= fontcount;  i++ )
    fprintf(stderr, "  font_table[%d] = %s\n", i, EchoObject(font_table[i]));
} /* end FontDebug */
#endif


/*@::FontDefine()@************************************************************/
/*                                                                           */
/*  FontDefine(family, face, inside)                                         */
/*                                                                           */
/*  Insert a font defined by fontdef <family> <face> { <inside> } into the   */
/*  font tree; <inside> ::= <fontname> <AFMfilename> <CEVfilename> <recode>  */
/*                                                                           */
/*****************************************************************************/

FontDefine(family, face, inside)
OBJECT family, face, inside;
{ OBJECT font_name, AFMfilename, CEVfilename, recode;
  OBJECT short_name, link, y, val[4]; int i;
  debug3(DFT, D, "FontDefine( %s, %s, %s )", string(family),
    string(face), EchoObject(inside));

  /* extract font_name, AFMfilename, CEVfilename, and recode */
  if( type(inside) != ACAT )
  { Error(WARN, &fpos(inside), "fontdef is not a sequence of words");
    DisposeObject(inside);  return;
  }
  for( i = 0;  Down(inside) != inside && i != 4;  i++ )
  { Child(val[i], Down(inside));
    DeleteLink(Up(val[i]));
    if( type(val[i]) == GAP_OBJ )  DisposeObject(val[i--]);
    else if( !is_word(type(val[i])) )
    { Error(WARN, &fpos(val[i]), "fontdef contains a non-word");
      DisposeObject(inside);  return;
    }
  }
  if( Down(inside) != inside || i != 4 )
  { Error(WARN, &fpos(inside), "fontdef does not contain exactly four words");
    DisposeObject(inside);  return;
  }
  font_name = val[0];    AFMfilename = val[1];
  CEVfilename = val[2];  recode = val[3];

  /* insert family into font tree if not already present */
  for( link = Down(font_root);  link != font_root;  link = NextDown(link) )
  { Child(y, link);
    if( StringEqual(string(y), string(family)) )
    { Dispose(family);  family = y; break; }
  }
  if( link == font_root )  Link(font_root, family);

  /* insert face into family, or error if already present */
  for( link = Down(family);  link != family;  link = NextDown(link) )
  { Child(y, link);
    if( StringEqual(string(y), string(face)) )
    { Error(WARN, &fpos(face), "font %s %s already defined at%s",
	string(family), string(face), EchoFilePos(&fpos(y)));
      debug0(DFT, D, "FontDefine returning: font already defined");
      Dispose(face);
      return;
    }
  }
  Link(family, face);

  /* add AFMfilename as first size of font, and PostScript name as its child */
  Link(face, AFMfilename);
  short_name = MakeWordTwo(WORD, AsciiToFull("fnt"), StringInt(++font_seqnum),
    no_fpos);
  Link(AFMfilename, short_name);  Link(AFMfilename, font_name);

  /* load encoding vector */
  if( StringEqual(string(recode), STR_FONT_RECODE) )
  { font_recoded(face) = TRUE;
    font_encoding(AFMfilename) = EvLoad(CEVfilename, TRUE);
  }
  else if( StringEqual(string(recode), STR_FONT_NORECODE) )
  { font_recoded(face) = FALSE;
    font_encoding(AFMfilename) = EvLoad(CEVfilename, FALSE);
  }
  else Error(FATAL, &fpos(recode), "expecting either Recode or NoRecode here");
  debug0(DFT, D, "FontDefine returning.");
} /* end FontDefine */


/*@::ReadFont()@**************************************************************/
/*                                                                           */
/*  static ReadFont(face, err)                                               */
/*                                                                           */
/*  Read in a font file.  Object err is used only for error reporting.       */
/*                                                                           */
/*****************************************************************************/

static ReadFont(face, err)
OBJECT face, err;
{ OBJECT filename, fontname;
  FULL_CHAR buff[MAX_LINE], command[MAX_LINE], ch;
  int wx, llx, lly, urx, ury, xheight2, i, lnum, ligtop;
  BOOLEAN xhfound, wxfound, bfound;
  FILE_NUM fnum;  FILE *fp;
  struct metrics *fnt;
  FULL_CHAR *lig, ligchar;
  OBJECT x;
  char *malloc();
  assert( is_word(type(face)), "ReadFont: !is_word(type(face))!" );
  debug1(DFT, DD, "ReadFont( %s, err )", string(face));

  /* get a new font number for this font */
  if( ++fontcount >= MAX_FONT )  Error(FATAL, &fpos(err),
    "too many different fonts and sizes (max is %d)", MAX_FONT - 1);

  /* open the Adobe font metrics (AFM) file of the font */
  assert( Down(face) != face, "ReadFont: filename missing!" );
  Child(filename, Down(face));
  assert( Down(filename) != filename, "ReadFont: filename child missing!" );
  fnum = DefineFile(string(filename), STR_EMPTY, &fpos(filename),
    FONT_FILE, FONT_PATH);
  fp = OpenFile(fnum, FALSE, FALSE);
  if( fp == NULL )
    Error(FATAL, &fpos(filename), "cannot open font file %s", FileName(fnum));

  /* check that the AFM file begins, as it should, with "StartFontMetrics" */
  if( StringFGets(buff, MAX_LINE, fp) == NULL ||
	sscanf( (char *) buff, "%s", command) != 1 ||
	!StringEqual(command, "StartFontMetrics")  )
  { debug1(DFT, D, "first line of AFM file:%s", buff);
    debug1(DFT, D, "command:%s", command);
    Error(FATAL, &fpos(filename),
      "font file %s does not begin with StartFontMetrics", FileName(fnum));
  }

  /* initialise font metrics and ligature tables for the new font */
  fnt = (struct metrics *) malloc(MAX_CHARS * sizeof(struct metrics));
  if( fnt == (struct metrics *) NULL )  Error(FATAL, &fpos(err),
    "run out of memory reading font file %s", FileName(fnum));
  lig = (FULL_CHAR *) malloc(2*MAX_CHARS*sizeof(FULL_CHAR));
  if( lig == (FULL_CHAR *) NULL )  Error(FATAL, &fpos(err),
    "run out of memory reading font file %s", FileName(fnum));
  for( i = 0;  i < MAX_CHARS;  i++ )  lig[i] = 1;	/* i.e. char unknown */
  ligtop = MAX_CHARS+2;		/* must avoid ligtop - MAX_CHARS == 0 or 1 */

  /* read font metrics file */
  xhfound = FALSE;  fontname = nil;  lnum = 1;
  while ( ( StringFGets(buff, MAX_LINE, fp) ) != NULL )
  {
    lnum++;
    sscanf( (char *) buff, "%s", command);
    switch( command[0] )
    {

      case 'X':

	if( StringEqual(command, AsciiToFull("XHeight")) ) 
	{ if( xhfound )
	  { Error(FATAL, &fpos(filename),
	      "XHeight found twice in font file (line %d)", lnum);
	  }
	  sscanf( (char *) buff, "XHeight %d", &xheight2);
	  xheight2 = xheight2 / 2;
	  xhfound = TRUE;
	}
	break;


      case 'F':

	if( StringEqual(command, AsciiToFull("FontName")) )
	{ if( fontname != nil )
	  { Error(FATAL, &fpos(filename),
	      "FontName found twice in font file %s (line %d)",
	      FileName(fnum), lnum);
	  }
	  sscanf( (char *) buff, "FontName %s", command);
	  if( StringEqual(command, STR_EMPTY) )
	  { Error(FATAL, &fpos(filename),
	      "FontName empty in font file %s (line %d)",
	      FileName(fnum), lnum);
	  }
	  Child(x, LastDown(filename));
	  if( !StringEqual(command, string(x)) )
	  Error(FATAL, &fpos(filename),
	    "FontName in AFM file (%s) and in fontdef (%s) disagree",
	    command, string(x));
	  fontname = MakeWord(WORD, command, &fpos(filename));
	}
	break;


      case 'S':

	if( !StringEqual(command, AsciiToFull("StartCharMetrics")) )
	  continue;
	if( fontname == nil )  Error(FATAL, &fpos(filename),
	  "FontName missing in file %s", FileName(fnum));
	if( !xhfound )  xheight2 = DEFAULT_XHEIGHT / 2;
	while( StringFGets(buff, MAX_LINE, fp) != NULL &&
	       !StringBeginsWith(buff, AsciiToFull("EndCharMetrics")) )
	{
	  /* read one line containing metric info for one character */
	  debug1(DFT, DD, "ReadFont reading %s", buff);
	  lnum++;  ch = '\0';  
	  wxfound = bfound = FALSE;
	  i = 0;  while( buff[i] == ' ' )  i++;
	  while( buff[i] != '\n' )
	  {
	      debug2(DFT, DD, "  ch = %d, &buff[i] = %s", ch, &buff[i]);
	      sscanf( (char *) &buff[i], "%s", command);
	      if( StringEqual(command, "N") )
	      { sscanf( (char *) &buff[i], "N %s", command);
		ch = EvRetrieve(command, font_encoding(filename));
	      }
	      else if( StringEqual(command, "WX") )
	      {	sscanf( (char *) &buff[i], "WX %d", &wx);
		wxfound = TRUE;
	      }
	      else if( StringEqual(command, "B") )
	      { sscanf( (char *) &buff[i], "B %d %d %d %d",
		  &llx, &lly, &urx, &ury);
		bfound = TRUE;
	      }
	      else if( StringEqual(command, "L") && ch != '\0' )
	      { if( lig[ch] == 1 )  lig[ch] = ligtop - MAX_CHARS;
		lig[ligtop++] = ch;
		i++;  /* skip L */
		while( buff[i] == ' ' )  i++;
		while( buff[i] != ';' && buff[i] != '\n' )
		{ sscanf( (char *) &buff[i], "%s", command);
		  ligchar = EvRetrieve(command, font_encoding(filename));
		  if( ligchar != '\0' )  lig[ligtop++] = ligchar;
		  else
		  { Error(WARN, &fpos(filename),
		    "ignoring ligature character %s in font file %s (line %d%s",
		    command, FileName(fnum), lnum, ") as it is not encoded");
		    lig[ch] = 1;
		  }
		  if( ligtop > 2*MAX_CHARS - 5 )  Error(FATAL, &fpos(filename),
		    "too many ligature characters in font file %s (line %d)",
		    FileName(fnum), lnum);
		  while( buff[i] != ' ' && buff[i] != ';' )  i++;
		  while( buff[i] == ' ' ) i++;
		}
		lig[ligtop++] = '\0';
	      }
	      while( buff[i] != ';' && buff[i] != '\n' )  i++;
	      if( buff[i] == ';' )
	      { i++;  while( buff[i] == ' ' ) i++;
	      }
	  }
	  if( ch > '\0' )
	  { 
	    if( !wxfound )
	    { Error(FATAL, &fpos(filename),
	        "WX missing in font file %s (line %d)", FileName(fnum), lnum);
	    }
	    if( !bfound )
	    { Error(FATAL, &fpos(filename),
	        "B missing in font file %s (line %d)", FileName(fnum), lnum);
	    }
	    if( lig[ch] == 1 )  lig[ch] = 0;	/* set to known if unknown */
	    else if( lig[ch] > 1 )		/* add '\0' to end of ligs */
	      lig[ligtop++] = '\0';
	    fnt[ch].left  = llx;
	    fnt[ch].down  = lly - xheight2;
	    fnt[ch].right = wx;
	    fnt[ch].up    = ury - xheight2;
	    debug5(DFT, DD, "  fnt[%c] = (%d,%d,%d,%d)", ch, fnt[ch].left,
	      fnt[ch].down, fnt[ch].right, fnt[ch].up);
	  }
	}

	/* make a new font record and insert into font tree */
	font_num(fontname) = fontcount;
	font_size(fontname) = SZ_DFT;
	font_xheight2(fontname) = xheight2;
	font_encoding(fontname) = font_encoding(filename);
	ch = EvRetrieve(STR_PS_SPACENAME, font_encoding(fontname));
	font_spacewidth(fontname) = ch == '\0' ? 0 : fnt[ch].right;
	font_table[fontcount] = fontname;
	size_table[fontcount] = fnt;
	lig_table[fontcount] = lig;
	Link(face, fontname);

	/* close file, debug and exit */
	fclose(fp);
	debug4(DFT, D, "ReadFont returning: %d, name %s, fs %d, xh2 %d",
		fontcount, string(fontname), font_size(fontname), xheight2);
	return;
	break;


      default:

	break;

    }
  }
  Error(FATAL, &fpos(filename),
	"StartCharMetrics missing from font file %s", FileName(fnum));
} /* end ReadFont */


/*@::FontChange()@************************************************************/
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
  struct metrics *newfnt, *oldfnt;  FULL_CHAR *lig;  char *malloc();
  debug2(DFT, D, "FontChange( %s, %s )", EchoStyle(style), EchoObject(x));
  assert( font(*style) <= fontcount, "FontChange: fontcount!");
  /* ifdebug(DFT, DD, FontDebug()); */

  /* set par[0..num-1] to the 1, 2 or 3 parameters of the font operator */
  num = 0;
  if( is_word(type(x)) )  par[num++] = x; 
  else if( type(x) == ACAT )
  { for( link = Down(x);  link != x;  link = NextDown(link) )
    { Child(y, link);
      debug1(DFT, DD, "  pars examining y = %s", EchoObject(y));
      if( type(y) == GAP_OBJ )  continue;
      if( !is_word(type(y)) || num >= 3 )
      {	Error(WARN, &fpos(x), "error in left parameter of %s", KW_FONT);
	debug0(DFT, D, "FontChange returning: ACAT children");
	return;
      }
      par[num++] = y;
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
  if( c == CH_INCGAP || c == CH_DECGAP || decimaldigit(c) )
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
      if( StringEqual(string(family), string(par[0])) )  break;
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
    assert( is_word(type(face)), "FontChange: type(face)!" );
    assert( Up(face) != face, "FontChange: Up(face)!" );
    Parent(family, Up(face));
    assert( is_word(type(family)), "FontChange: type(family)!" );
  }

  /* get font face */
  if( num != 0 )
  {
    /* par[num-1] contains a new face name */
    for( link = Down(family);  link != family;  link = NextDown(link) )
    { Child(face, link);
      if( StringEqual(string(face), string(par[num-1])) )  break;
    }
    if( link == family )
    {
      /* missing face name; first check whether a family name was intended */
      for( link = Down(font_root);  link != font_root;  link = NextDown(link) )
      {	Child(tmpf, link);
	if( StringEqual(string(tmpf), string(par[num-1])) )  break;
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
    assert( is_word(type(face)), "FontChange: type(face)!" );
    assert( Up(face) != face, "FontChange: Up(face)!" );
  }

  /* get font size */
  if( fsize == nil )  flen = font_size(font_table[font(*style)]);
  else 
  { GetGap(fsize, style, &gp, &inc);
    if( inc == GAP_ABS )  flen = width(gp);
    else if( font(*style) == NO_FONT )
      Error(FATAL, &fpos(fsize), "no font encloses this %s", string(fsize));
    else if( inc==GAP_INC )  flen = font_size(font_table[font(*style)])+width(gp);
    else if( inc==GAP_DEC )  flen = font_size(font_table[font(*style)])-width(gp);
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
      SetGap(space_gap(*style), FALSE, TRUE, FIXED_UNIT, EDGE_MODE,
			font_spacewidth(fsize));
      debug2(DFT, D,"FontChange returning (old) %d (XHeight2 = %d)",
			font(*style), font_xheight2(font_table[font(*style)]));
      return;
    }
  }

  /* now need to rescale the font; first create a sized font record */
  if( ++fontcount >= MAX_FONT )  Error(FATAL, &fpos(x),
    "too many different fonts and sizes (max is %d)", MAX_FONT - 1);
  assert( Down(face) != face && NextDown(Down(face)) != face, "FontChange!!" );
  Child(old, NextDown(Down(face)));
  assert( is_word(type(old)), "FontChange: old!" );
  new = MakeWord(WORD, string(old), no_fpos);
  Link(face, new);
  font_size(new)        = flen;
  font_xheight2(new)    = font_xheight2(old) * font_size(new) / font_size(old);
  font_encoding(new)	= font_encoding(old);
  font_spacewidth(new)	= font_spacewidth(old) * font_size(new)/font_size(old);
  font_num(new)         = fontcount;
  font_table[fontcount] = new;
  size_table[fontcount] =
	(struct metrics *) malloc(MAX_CHARS * sizeof(struct metrics));
  if( size_table[fontcount] == (struct metrics *) NULL )
    Error(FATAL, &fpos(x), "run out of memory when changing font or font size");
  lig_table[fontcount]  = lig = lig_table[font_num(old)];

  /* scale old font to new size */
  newfnt = size_table[font_num(new)];
  oldfnt = size_table[font_num(old)];
  for( i = 0;  i < MAX_CHARS;  i++ )  if( lig[i] != 1 )
  { newfnt[i].left  = (oldfnt[i].left  * font_size(new)) / font_size(old);
    newfnt[i].right = (oldfnt[i].right * font_size(new)) / font_size(old);
    newfnt[i].down  = (oldfnt[i].down  * font_size(new)) / font_size(old);
    newfnt[i].up    = (oldfnt[i].up    * font_size(new)) / font_size(old);
  }

  /* return new font number and exit */
  font(*style) = fontcount;
  SetGap(space_gap(*style), FALSE, TRUE, FIXED_UNIT, EDGE_MODE,
    font_spacewidth(new));
  debug2(DFT, D,"FontChange returning (scaled) %d (XHeight2 = %d)",
    font(*style), font_xheight2(font_table[font(*style)]));
  /* FontDebug(); */
} /* end FontChange */


/*@::FontWordSize()@**********************************************************/
/*                                                                           */
/*  FontWordSize(x)                                                          */
/*                                                                           */
/*  Calculate the horizontal and vertical size of WORD or QWORD x, replacing */
/*  ligature sequences by ligature characters wherever they occur.           */
/*                                                                           */
/*****************************************************************************/

FontWordSize(x)
OBJECT x;
{ FULL_CHAR *p, *q, *a, *b, *lig;  OBJECT tmp;
  int r, u, d; struct metrics *fnt;
  debug2(DFT, D, "FontWordSize( %s ), font = %d", string(x), word_font(x));
  assert( is_word(type(x)), "FontWordSize: !is_word(type(x))!" );

  p = q = string(x);
  if( *p )
  { if ( word_font(x) < 1 || word_font(x) > fontcount )
      Error(FATAL, &fpos(x), "no current font at word %s", string(x));
    fnt = size_table[word_font(x)];
    lig = lig_table[word_font(x)];
    d = u = r = 0;
    do
    { 
      /* check for missing glyph (lig[] == 1) or ligatures (lig[] > 1) */
      if( lig[*q = *p++] )
      {
	if( lig[*q] == 1 )
	{ tmp = MakeWord(QWORD, STR_SPACE, &fpos(x));
	  string(tmp)[0] = *q;
	  Error(WARN, &fpos(x),
	    "character %s left out (it has no glyph in font %s)",
	     StringQuotedWord(tmp), FontFamilyAndFace(word_font(x)));
	  Dispose(tmp);
	  continue;
	}
	else
	{ a = &lig[ lig[*(p-1)] + MAX_CHARS ];
	  while( *a++ == *(p-1) )
	  { b = p;
	    while( *a == *b && *(a+1) != '\0' && *b != '\0' )  a++, b++;
	    if( *(a+1) == '\0' )
	    { *q = *a;
	      p = b;
	      break;
	    }
	    else
	    { while( *++a );
	      a++;
	    }
	  }
	}
      }

      /* accumulate size of *q */
      if( fnt[*q].up   > u )  u = fnt[*q].up;
      if( fnt[*q].down < d )  d = fnt[*q].down;
      r += fnt[*q++].right;
    } while( *p );
    *q++ = '\0';
    back(x, COL) = 0; fwd(x, COL)  = r;
    back(x, ROW) = u; fwd(x, ROW)  = -d;
  } 
  else back(x, COL) = fwd(x, COL) = back(x, ROW) = fwd(x, ROW) = 0;
  debug4(DFT, D, "FontWordSize returning %hd %hd %hd %hd",
	  back(x, COL), fwd(x, COL), back(x, ROW), fwd(x, ROW));
} /* end FontWordSize */


/*@::FontSize(), FontHalfXHeight(), FontEncoding(), FontName()@***************/
/*                                                                           */
/*  LENGTH FontSize(fnum, x)                                                 */
/*                                                                           */
/*  Return the size of this font.  x is for error messages only.             */
/*                                                                           */
/*****************************************************************************/

LENGTH FontSize(fnum, x)
FONT_NUM fnum;  OBJECT x;
{ debug1(DFT, D, "FontSize( %d )", fnum);
  assert( fnum <= fontcount, "FontSize!" );
  if( fnum <= 0 )  Error(FATAL, &fpos(x), "no current font at this point");
  debug1(DFT, D, "FontSize returning %d", font_size(font_table[fnum]));
  return font_size(font_table[fnum]);
} /* end FontSize */


/*****************************************************************************/
/*                                                                           */
/*  LENGTH FontHalfXHeight(fnum)                                             */
/*                                                                           */
/*  Return the xheight2 value of this font.                                  */
/*                                                                           */
/*****************************************************************************/

LENGTH FontHalfXHeight(fnum)
FONT_NUM fnum;
{ debug1(DFT, D, "FontHalfXHeight( %d )", fnum);
  assert( fnum <= fontcount, "FontHalfXHeight!" );
  debug1(DFT,D,"FontHalfXHeight returning %d", font_xheight2(font_table[fnum]));
  return font_xheight2(font_table[fnum]);
} /* end FontSize */


/*****************************************************************************/
/*                                                                           */
/*  ENCODING FontEncoding(fnum)                                              */
/*                                                                           */
/*  Return the encoding of this font.                                        */
/*                                                                           */
/*****************************************************************************/

ENCODING FontEncoding(fnum)
FONT_NUM fnum;
{ debug1(DFT, D, "FontEncoding( %d )", fnum);
  assert( fnum <= fontcount, "FontSize!" );
  debug1(DFT, D, "FontEncoding returning %d", font_encoding(font_table[fnum]));
  return font_encoding(font_table[fnum]);
} /* end FontSize */


/*****************************************************************************/
/*                                                                           */
/*  FULL_CHAR *FontName(fnum)                                                */
/*                                                                           */
/*  Return the short PostScript name of this font.                           */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *FontName(fnum)
FONT_NUM fnum;
{ OBJECT face, AFMfilename, short_name;
  debug1(DFT, D, "FontName( %d )", fnum);
  assert( fnum <= fontcount, "FontName!" );
  Parent(face, Up(font_table[fnum]));
  Child(AFMfilename, Down(face));
  Child(short_name, Down(AFMfilename));
  assert( is_word(type(short_name)), "FontName: short_name!" );
  debug1(DFT, D, "FontName returning %s", string(short_name));
  return string(short_name);
} /* end FontSize */


/*@::FontFamilyAndFace(), FontPrintAll()@*************************************/
/*                                                                           */
/*  FULL_CHAR *FontFamilyAndFace(fnum)                                       */
/*                                                                           */
/*  Return a static string of the current font family and face.              */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *FontFamilyAndFace(fnum)
FONT_NUM fnum;
{ OBJECT face, family; static FULL_CHAR buff[80];
  debug1(DFT, D, "FontFamilyAndFace( %d )", fnum);
  assert( fnum <= fontcount, "FontName!" );
  Parent(face, Up(font_table[fnum]));
  Parent(family, Up(face));
  if( StringLength(string(family)) + StringLength(string(face)) + 1 > 80 )
    Error(FATAL, no_fpos, "family and face names %s %s are too long",
      string(family), string(face));
  StringCopy(buff, string(family));
  StringCat(buff, STR_SPACE);
  StringCat(buff, string(face));
  debug1(DFT, D, "FontName returning %s", buff);
  return buff;
} /* end FontFamilyAndFace */


/*****************************************************************************/
/*                                                                           */
/*  FontPrintAll(fp)                   	                                     */
/*                                                                           */
/*  Print all font encoding commands on output file fp                       */
/*                                                                           */
/*****************************************************************************/

FontPrintAll(fp)
FILE *fp;
{ OBJECT family, face, AFMfilename, short_name, ps_name, link, flink;
  assert( font_root != nil && type(font_root)==ACAT, "FontDebug: font_root!" );
  debug0(DFT, DD, "FontPrintAll(fp)");
  for( link = Down(font_root);  link != font_root;  link = NextDown(link) )
  { Child(family, link);
    assert( is_word(type(family)), "FontPrintAll: family!" );
    for( flink = Down(family);  flink != family;  flink = NextDown(flink) )
    { Child(face, flink);
      assert( is_word(type(face)), "FontPrintAll: face!" );
      assert( Down(face) != face, "FontDebug: Down(face)!");
      Child(AFMfilename, Down(face));
      assert( is_word(type(AFMfilename)), "FontPrintAll: filename!" );
      assert( Down(AFMfilename) != AFMfilename, "FontPrintAll: 1!" );
      assert( LastDown(AFMfilename) != Down(AFMfilename), "FontPrintAll: 2!" );
      Child(short_name, Down(AFMfilename));
      assert( is_word(type(short_name)), "FontPrintAll: short_name!" );
      Child(ps_name, LastDown(AFMfilename));
      assert( is_word(type(ps_name)), "FontPrintAll: ps_name!" );
      if( font_recoded(face) )
      { fprintf(fp, "/%s%s %s /%s LoutRecode\n",
	  string(ps_name), string(short_name),
	  EvName(font_encoding(AFMfilename)), string(ps_name));
        fprintf(fp, "/%s { /%s%s LoutFont } def\n", string(short_name),
	  string(ps_name), string(short_name));
      }
      else fprintf(fp, "/%s { /%s LoutFont } def\n", string(short_name),
	  string(ps_name));
    }
  }
  fputs("\n", fp);
  debug0(DFT, DD, "FontPrintAll returning.");
} /* end FontPrintAll */


/*@::FontNeeded()@************************************************************/
/*                                                                           */
/*  OBJECT FontNeeded(out_fp);                                               */
/*                                                                           */
/*  Writes font needed resources onto file out_fp.  Returns TRUE if none.    */
/*                                                                           */
/*****************************************************************************/

BOOLEAN FontNeeded(out_fp)
FILE *out_fp;
{ BOOLEAN first_need = TRUE;
  OBJECT link, flink, family, face, x;
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
  return first_need;
} /* end FontNeeded */
