/*@z37.c:Font Service:Declarations@*******************************************/
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
/*  FILE:         z37.c                                                      */
/*  MODULE:       Font Service                                               */
/*  EXTERNS:      FontInit(), FontDefine(), FontChange(), FontWordSize(),    */
/*                FontSize(), FontHalfXHeight(), FontEncoding(),             */
/*                FontMapping(), FontFamilyAndFace(), FontNeeded()           */
/*                                                                           */
/*  This module implements fonts, using encoding vectors and Adobe font      */
/*  metrics files (.AFM files, version 2).                                   */
/*                                                                           */
/*****************************************************************************/
#include "externs.h"
#define DEFAULT_XHEIGHT 500	/* the default XHeight if font has none      */
#define	NO_FONT		  0	/* the not-a-font font number                */
#define SZ_DFT	       1000	/* default lout size is 50p                  */
#define	INIT_FINFO_SIZE	100	/* initial number of sized fonts set aside   */

/*****************************************************************************/
/*                                                                           */
/* these definitions have been placed in "externs" because z24.c needs them  */
/*                                                                           */
/*  struct metrics {							     */
/*    SHORT_LENGTH up;							     */
/*    SHORT_LENGTH down;						     */
/*    SHORT_LENGTH left;						     */
/*    SHORT_LENGTH right;						     */
/*    SHORT_LENGTH last_adjust;						     */
/*  };							     		     */
/*  									     */
/*  typedef struct font_rec {						     */
/*    struct metrics	*size_table;		   metrics of sized fonts    */
/*    FULL_CHAR		*lig_table;		   ligatures                 */
/*    OBJECT		font_table;		   record of sized fonts     */
/*    OBJECT            original_font;             font rec before resizing  */
/*    SHORT_LENGTH	underline_pos;             position of underline     */
/*    SHORT_LENGTH	underline_thick;           thickness of underline    */
/*    unsigned short	*kern_table;		   first kerning chars       */
/*    FULL_CHAR		*kern_chars;		   second kerning chars      */
/*    unsigned char	*kern_value;		   points into kern_lengths  */
/*    SHORT_LENGTH	*kern_sizes;		   sizes of kernings         */
/*  } FONT_INFO;							     */
/*                                                                           */
/*****************************************************************************/

	int		font_curr_page;		/* current page number       */
	FONT_INFO	*finfo;			/* all the font table info   */
static	int		finfo_size;		/* current finfo array size  */
static	OBJECT		font_root;		/* root of tree of fonts     */
static	OBJECT		font_used;		/* fonts used on this page   */
static	FONT_NUM	font_count;		/* number of sized fonts     */
static	int		font_seqnum;		/* unique number for a font  */


/*@::FontInit(), FontDebug()@*************************************************/
/*                                                                           */
/*  FontInit()                                                               */
/*                                                                           */
/*  Initialise this module.                                                  */
/*                                                                           */
/*****************************************************************************/

void FontInit(void)
{ debug0(DFT, D, "FontInit()");
  font_curr_page = 1;
  font_count	= 0;
  New(font_root, ACAT);
  New(font_used, ACAT);
  font_seqnum	= 0;
  finfo         = (FONT_INFO *) malloc(INIT_FINFO_SIZE * sizeof(FONT_INFO));
  finfo_size    = INIT_FINFO_SIZE;
  ifdebug(DMA, D,
    DebugRegisterUsage(MEM_FONTS, 1, INIT_FINFO_SIZE * sizeof(FONT_INFO)));
  debug0(DFT, D, "FontInit returning.");
}


/*****************************************************************************/
/*                                                                           */
/*  FontDebug()                        	                                     */
/*                                                                           */
/*  Print out font tree (not currectly used).                                */
/*                                                                           */
/*****************************************************************************/

#if DEBUG_ON
static void FontDebug(void)
{ OBJECT family, face, filename, link, flink;  int i;
  assert(font_root!=nilobj && type(font_root)==ACAT, "FontDebug: font_root!");
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
  for( i = 1;  i <= font_count;  i++ )
    fprintf(stderr, "  finfo[%d].font_table = %s\n", i,
      EchoObject(finfo[i].font_table));
} /* end FontDebug */
#endif


/*@::FontDefine()@************************************************************/
/*                                                                           */
/*  FontDefine(family, face, inside)                                         */
/*                                                                           */
/*  Insert a font defined by fontdef <family> <face> { <inside> }, where     */
/*  <inside> ::= fontname AFMfilename LCMfilename recode                     */
/*                                                                           */
/*****************************************************************************/
static void ReadFont(OBJECT face, OBJECT err);

void FontDefine(OBJECT family, OBJECT face, OBJECT inside)
{ OBJECT font_name, AFMfilename, LCMfilename, recode;
  OBJECT short_name, link, y, val[5]; int i;
  debug3(DFT, D, "FontDefine( %s, %s, %s )", string(family),
    string(face), EchoObject(inside));

  /* extract font_name, AFMfilename, LCMfilename, and recode */
  if( type(inside) != ACAT )
  { Error(37, 1, "font definition does not contain a sequence of words",
      WARN, &fpos(inside));
    DisposeObject(inside);  return;
  }
  for( i = 0;  Down(inside) != inside && i != 5;  i++ )
  { Child(val[i], Down(inside));
    DeleteLink(Up(val[i]));
    if( type(val[i]) == GAP_OBJ )  DisposeObject(val[i--]);
    else if( !is_word(type(val[i])) )
    { Error(37, 2, "font definition contains a non-word", WARN, &fpos(val[i]));
      DisposeObject(inside);  return;
    }
  }
  if( Down(inside) != inside || i != 4 )
  { Error(37, 3, "font definition does not contain exactly four words",
      WARN, &fpos(inside));
    DisposeObject(inside);  return;
  }
  font_name = val[0];    AFMfilename = val[1];
  LCMfilename = val[2];  recode = val[3];

  /* insert family into font tree if not already present */
  for( link = Down(font_root);  link != font_root;  link = NextDown(link) )
  { Child(y, link);
    if( StringEqual(string(y), string(family)) )
    { Dispose(family);  family = y; break; }
  }
  if( link == font_root )  Link(font_root, family);

  /* insert face into family, or error if already present and different */
  for( link = Down(family);  link != family;  link = NextDown(link) )
  { Child(y, link);
    if( StringEqual(string(y), string(face)) )
    { OBJECT other_name, other_AFMname;
      Child(other_AFMname, Down(y));
      Child(other_name, NextDown(Down(other_AFMname)));
      if( StringEqual(string(other_name), string(font_name)) &&
          StringEqual(string(other_AFMname), string(AFMfilename)) )
      { debug0(DFT, D, "FontDefine returning: font redefined");
        Dispose(face);
        return;
      }
      Error(37, 4, "font %s %s already defined at%s", WARN, &fpos(face),
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

  /* load character mapping file */
  if( StringEqual(string(recode), STR_FONT_RECODE) )
  { font_recoded(face) = TRUE;
    font_mapping(AFMfilename) = MapLoad(LCMfilename, TRUE);
  }
  else if( StringEqual(string(recode), STR_FONT_NORECODE) )
  { font_recoded(face) = FALSE;
    font_mapping(AFMfilename) = MapLoad(LCMfilename, FALSE);
  }
  else Error(37, 5, "expecting either Recode or NoRecode here",
	 FATAL, &fpos(recode));

  /* say that this font is currently unused on any page */
  font_page(face) = 0;
  font_firstpage(face) = FALSE;

  /* if initializing run, read the font just to make sure */
  if( InitializeAll )
  { ReadFont(face, face);
  }

  debug0(DFT, D, "FontDefine returning.");
} /* end FontDefine */


/*****************************************************************************/
/*                                                                           */
/*  DebugKernTable(fnum)                                                     */
/*                                                                           */
/*  Print debug output of kern table for font fnum.                          */
/*                                                                           */
/*****************************************************************************/
#if DEBUG_ON

static void DebugKernTable(FONT_NUM fnum)
{ int i, j;
  unsigned short *kt = finfo[fnum].kern_table;
  FULL_CHAR      *kc = finfo[fnum].kern_chars;
  unsigned char  *kv = finfo[fnum].kern_value;
  SHORT_LENGTH   *ks = finfo[fnum].kern_sizes;
  debug1(DFT, DD, "DebugKernTable(%d)", fnum);
  for( i = 0;  i < MAX_CHARS;  i++ )
  { if( kt[i] != 0 )
    { debug1(DFT, DD, "kt[%d]:", i);
      for( j = kt[i];  kc[j] != '\0';  j++ )
      { debug3(DFT, DD, "KPX %c %c %d", i, kc[j], ks[kv[j]]);
      }
    }
  }
  debug1(DFT, DD, "DebugKernTable(%d) returning", fnum);
} /* DebugKernTable */
#endif


/*@::ReadFont()@**************************************************************/
/*                                                                           */
/*  static ReadFont(face, err)                                               */
/*                                                                           */
/*  Read in a font file.  Object err is used only for error reporting.       */
/*                                                                           */
/*****************************************************************************/

static void ReadFont(OBJECT face, OBJECT err)
{ OBJECT filename, fontname;
  FULL_CHAR buff[MAX_BUFF], command[MAX_BUFF], ch;
  int wx, llx, lly, urx, ury, xheight2, i, lnum, ligtop;
  float fl_wx, fl_llx, fl_lly, fl_urx, fl_ury, fl_xheight2, fl_under_pos,
	fl_under_thick;
  int under_pos, under_thick;
  BOOLEAN upfound, utfound, xhfound, wxfound, bfound;
  BOOLEAN fixed_pitch = FALSE;
  FILE_NUM fnum;  FILE *fp;
  struct metrics *fnt;
  FULL_CHAR *lig, ligchar;
  unsigned short *kt;  FULL_CHAR *kc;  unsigned char *kv;  SHORT_LENGTH *ks;
  OBJECT x;
  assert( is_word(type(face)), "ReadFont: !is_word(type(face))!" );
  debug1(DFT, DD, "ReadFont( %s, err )", string(face));

  /* get a new font number for this font, possibly requiring realloc */
  if( ++font_count >= finfo_size )
  { if( font_count > MAX_FONT )
      Error(37, 6, "too many different fonts and sizes (maximum is %d)",
	FATAL, &fpos(err),MAX_FONT);
    ifdebug(DMA, D,
      DebugRegisterUsage(MEM_FONTS, -1, -finfo_size * sizeof(FONT_INFO)));
    finfo_size *= 2;
    ifdebug(DMA, D,
      DebugRegisterUsage(MEM_FONTS, 1, finfo_size * sizeof(FONT_INFO)));
    finfo = (FONT_INFO *) realloc(finfo, finfo_size * sizeof(FONT_INFO));
    if( finfo == (FONT_INFO *) NULL )
      Error(37, 7, "run out of memory when increasing font table size",
	FATAL, &fpos(err));
  }

  /* open the Adobe font metrics (AFM) file of the font */
  assert( Down(face) != face, "ReadFont: filename missing!" );
  Child(filename, Down(face));
  assert( Down(filename) != filename, "ReadFont: filename child missing!" );
  debug0(DFS, D, "  calling DefineFile from ReadFont");
  fnum = DefineFile(string(filename), STR_EMPTY, &fpos(filename),
    FONT_FILE, FONT_PATH);
  fp = OpenFile(fnum, FALSE, FALSE);
  if( fp == NULL )
    Error(37, 8, "cannot open font file %s", FATAL, &fpos(filename),
      FileName(fnum));

  /* check that the AFM file begins, as it should, with "StartFontMetrics" */
  if( StringFGets(buff, MAX_BUFF, fp) == NULL ||
	sscanf( (char *) buff, "%s", command) != 1 ||
	!StringEqual(command, "StartFontMetrics")  )
  { debug1(DFT, DD, "first line of AFM file:%s", buff);
    debug1(DFT, DD, "command:%s", command);
    Error(37, 9, "font file %s does not begin with StartFontMetrics",
      FATAL, &fpos(filename), FileName(fnum));
  }

  /* initialise font metrics table for the new font */
  ifdebug(DMA, D,
      DebugRegisterUsage(MEM_FONTS, 1, MAX_CHARS * sizeof(struct metrics)));
  fnt = (struct metrics *) malloc(MAX_CHARS * sizeof(struct metrics));
  if( fnt == (struct metrics *) NULL )
    Error(37, 10, "run out of memory while reading font file %s",
      FATAL, &fpos(err), FileName(fnum));
  ifdebug(DMA, D,
      DebugRegisterUsage(MEM_FONTS, 0, 2*MAX_CHARS*sizeof(FULL_CHAR)));
  lig = (FULL_CHAR *) malloc(2*MAX_CHARS*sizeof(FULL_CHAR));

  /* initialise ligature table for the new font */
  if( lig == (FULL_CHAR *) NULL )
    Error(37, 11, "run out of memory while reading font file %s",
      FATAL, &fpos(err), FileName(fnum));
  for( i = 0;  i < MAX_CHARS;  i++ )  lig[i] = 1;	/* i.e. char unknown */
  ligtop = MAX_CHARS+2;		/* must avoid ligtop - MAX_CHARS == 0 or 1 */

  /* initialise kerning table for the new font */
  ifdebug(DMA, D,
      DebugRegisterUsage(MEM_FONTS, 0, MAX_CHARS * sizeof(unsigned short)));
  kt = (unsigned short *) malloc(MAX_CHARS * sizeof(unsigned short));
  if( kt == (unsigned short *) NULL )
    Error(37, 12, "run out of memory while reading font file %s",
      FATAL, &fpos(err), FileName(fnum));
  for( i = 0;  i < MAX_CHARS;  i++ )  kt[i] = 0;  /* i.e. no kerns */
  ks = (SHORT_LENGTH *) NULL;			  /* i.e. no kern sizes */

  /* read font metrics file */
  xhfound = upfound = utfound = FALSE;
  fontname = nilobj;  lnum = 1;
  while ( ( StringFGets(buff, MAX_BUFF, fp) ) != NULL )
  {
    lnum++;
    sscanf( (char *) buff, "%s", command);
    switch( command[0] )
    {

      case 'U':

	if( StringEqual(command, AsciiToFull("UnderlinePosition")) ) 
	{ if( upfound )
	  { Error(37, 13, "UnderlinePosition found twice in font file (line %d)",
	      FATAL, &fpos(filename), lnum);
	  }
	  sscanf( (char *) buff, "UnderlinePosition %f", &fl_under_pos);
	  under_pos = fl_under_pos;
	  upfound = TRUE;
	}
	else if( StringEqual(command, AsciiToFull("UnderlineThickness")) ) 
	{ if( utfound )
	  { Error(37, 14, "UnderlineThickness found twice in font file (line %d)",
	      FATAL, &fpos(filename), lnum);
	  }
	  sscanf( (char *) buff, "UnderlineThickness %f", &fl_under_thick);
	  under_thick = fl_under_thick;
	  utfound = TRUE;
	}
	break;


      case 'X':

	if( StringEqual(command, AsciiToFull("XHeight")) ) 
	{ if( xhfound )
	  { Error(37, 15, "XHeight found twice in font file (line %d)",
	      FATAL, &fpos(filename), lnum);
	  }
	  sscanf( (char *) buff, "XHeight %f", &fl_xheight2);
	  xheight2 = fl_xheight2 / 2;
	  xhfound = TRUE;
	}
	break;


      case 'F':

	if( StringEqual(command, AsciiToFull("FontName")) )
	{ if( fontname != nilobj )
	  { Error(37, 16, "FontName found twice in font file %s (line %d)",
	      FATAL, &fpos(filename), FileName(fnum), lnum);
	  }
	  sscanf( (char *) buff, "FontName %s", command);
	  if( StringEqual(command, STR_EMPTY) )
	  { Error(37, 17, "FontName empty in font file %s (line %d)",
	      FATAL, &fpos(filename), FileName(fnum), lnum);
	  }
	  Child(x, LastDown(filename));
	  if( !StringEqual(command, string(x)) )
	  Error(37, 18, "FontName in font file (%s) and fontdef (%s) disagree",
	    WARN, &fpos(filename), command, string(x));
	  fontname = MakeWord(WORD, command, &fpos(filename));
	}
	break;


      case 'I':

	if( StringEqual(command, AsciiToFull("IsFixedPitch")) )
	{ 
	  sscanf( (char *) buff, "IsFixedPitch %s", command);
	  if( StringEqual(command, AsciiToFull("true")) )
	  { fixed_pitch = TRUE;
	  }
	}
	break;


      case 'S':

	if( StringEqual(command, AsciiToFull("StartCharMetrics")) )
	{
	  if( fontname == nilobj )
	    Error(37, 19, "FontName missing in file %s",
	      FATAL, &fpos(filename), FileName(fnum));
	  if( !xhfound )  xheight2 = DEFAULT_XHEIGHT / 2;
	  while( StringFGets(buff, MAX_BUFF, fp) != NULL &&
	         !StringBeginsWith(buff, AsciiToFull("EndCharMetrics")) )
	  {
	    /* read one line containing metric info for one character */
	    debug1(DFT, DDD, "ReadFont reading %s", buff);
	    lnum++;  ch = '\0';  
	    wxfound = bfound = FALSE;
	    i = 0;  while( buff[i] == ' ' )  i++;
	    while( buff[i] != '\n' )
	    {
	      debug2(DFT, DDD, "  ch = %d, &buff[i] = %s", ch, &buff[i]);
	      sscanf( (char *) &buff[i], "%s", command);
	      if( StringEqual(command, "N") )
	      { sscanf( (char *) &buff[i], "N %s", command);
		ch = MapCharEncoding(command, font_mapping(filename));
	      }
	      else if( StringEqual(command, "WX") )
	      {	sscanf( (char *) &buff[i], "WX %f", &fl_wx);
		wx = fl_wx;
		wxfound = TRUE;
	      }
	      else if( StringEqual(command, "B") )
	      { sscanf( (char *) &buff[i], "B %f %f %f %f",
		  &fl_llx, &fl_lly, &fl_urx, &fl_ury);
		llx = fl_llx;
		lly = fl_lly;
		urx = fl_urx;
		ury = fl_ury;
		bfound = TRUE;
	      }
	      else if( StringEqual(command, "L") &&
		BackEnd != PLAINTEXT && ch != '\0' )
	      { if( lig[ch] == 1 )  lig[ch] = ligtop - MAX_CHARS;
		lig[ligtop++] = ch;
		i++;  /* skip L */
		while( buff[i] == ' ' )  i++;
		while( buff[i] != ';' && buff[i] != '\n' )
		{ sscanf( (char *) &buff[i], "%s", command);
		  ligchar = MapCharEncoding(command, font_mapping(filename));
		  if( ligchar != '\0' )  lig[ligtop++] = ligchar;
		  else
		  { Error(37, 20, "ignoring unencoded ligature character %s in font file %s (line %d)",
		      WARN, &fpos(filename), command, FileName(fnum), lnum);
		    lig[ch] = 1;
		  }
		  if( ligtop > 2*MAX_CHARS - 5 )
		    Error(37, 21, "too many ligature characters in font file %s (line %d)",
		    FATAL, &fpos(filename), FileName(fnum), lnum);
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
	      { Error(37, 22, "WX missing in font file %s (line %d)",
		  FATAL, &fpos(filename), FileName(fnum), lnum);
	      }
	      if( !bfound )
	      { Error(37, 23, "B missing in font file %s (line %d)",
		  FATAL, &fpos(filename), FileName(fnum), lnum);
	      }
	      if( lig[ch] == 1 )  lig[ch] = 0;	/* set to known if unknown */
	      else if( lig[ch] > 1 )		/* add '\0' to end of ligs */
	        lig[ligtop++] = '\0';
	      switch( BackEnd )
	      {
		case POSTSCRIPT:
		case PDF:	 fnt[ch].left  = llx;
				 fnt[ch].down  = lly - xheight2;
				 fnt[ch].right = wx;
				 fnt[ch].up    = ury - xheight2;
				 fnt[ch].last_adjust =
				   (urx == 0 || wx == 0 || fixed_pitch) ? 0 : urx - wx;
				 break;

		case PLAINTEXT:  fnt[ch].left  = 0;
				 fnt[ch].down  = - PlainCharHeight / 2;
				 fnt[ch].right = PlainCharWidth;
				 fnt[ch].up    = PlainCharHeight / 2;
				 fnt[ch].last_adjust = 0;
				 break;
	      }
	      debug6(DFT, DDD, "  fnt[%c] = (%d,%d,%d,%d,%d)",ch, fnt[ch].left,
	        fnt[ch].down, fnt[ch].right, fnt[ch].up, fnt[ch].last_adjust);
	    }
	  }
	}
	else if( BackEnd != PLAINTEXT && Kern &&
	  StringEqual(command, AsciiToFull("StartKernPairs")) )
	{ FULL_CHAR ch1, ch2, last_ch1;
	  FULL_CHAR name1[30], name2[30];
	  int kc_top, ks_top, pos, num_pairs, ksize;  float fl_ksize;

	  if( sscanf( (char *) buff, "StartKernPairs %d", &num_pairs) != 1 )
	    Error(37, 24, "syntax error on StartKernPairs line in font file %s (line %d)",
	      FATAL, &fpos(filename), FileName(fnum), lnum);
	  kc_top = 1;  ks_top = 1;
	  ifdebug(DMA, D,
	    DebugRegisterUsage(MEM_FONTS, 0, 2*num_pairs * sizeof(FULL_CHAR)));
	  kc = (FULL_CHAR *) malloc(2 * num_pairs * sizeof(FULL_CHAR));
	  ifdebug(DMA, D, DebugRegisterUsage(MEM_FONTS, 0,
	    2 * num_pairs * sizeof(unsigned char)));
	  kv = (unsigned char *) malloc(2 * num_pairs * sizeof(unsigned char));
	  ifdebug(DMA, D, DebugRegisterUsage(MEM_FONTS, 0,
	    num_pairs * sizeof(SHORT_LENGTH)));
	  ks = (SHORT_LENGTH *) malloc(num_pairs * sizeof(SHORT_LENGTH));
	  last_ch1 = '\0';
	  while( StringFGets(buff, MAX_BUFF, fp) == (char *) buff &&
	    !StringBeginsWith(buff, AsciiToFull("EndKernPairs")) )
	  {
	    debug1(DFT, DD, "ReadFont reading %s", buff);
	    lnum++;
	    if( StringBeginsWith(buff, AsciiToFull("KPX")) )
	    {
	      /* get the two character names and kern size from buff */
	      if( sscanf((char *)buff, "KPX %s %s %f",name1,name2,&fl_ksize)!=3 )
		Error(37, 25, "syntax error in font file %s (line %d): %s",
		  FATAL, &fpos(filename), FileName(fnum), lnum, buff);

	      /* ignore size 0 kern pairs (they are frequent, why?) */
	      ksize = fl_ksize;
	      if( ksize == 0 )  continue;

	      /* check that both characters are encoded */
	      ch1 = MapCharEncoding(name1, font_mapping(filename));
	      if( ch1 == '\0' )
	      {
		/* ***
		Error(37, 26, "unencoded kern character %s in font file %s (line %d)",
		  WARN, &fpos(filename), name1, FileName(fnum), lnum);
		*** */
		continue;
	      }
	      ch2 = MapCharEncoding(name2, font_mapping(filename));
	      if( ch2 == '\0' )
	      {
		/* ***
		Error(37, 27, "unencoded kern character %s in font file %s (line %d)",
		  WARN, &fpos(filename), name2, FileName(fnum), lnum);
		*** */
		continue;
	      }

	      /* check that ch1 is contiguous with previous occurrences */
	      if( ch1 != last_ch1 && kt[ch1] != 0 )
	      { Error(37, 28, "non-contiguous kerning pair %s %s in font file %s (line %d)",
		  WARN, &fpos(filename), name1, name2, FileName(fnum), lnum);
		continue;
	      }
	      last_ch1 = ch1;

	      /* if ch1 never seen before, make new entry in kt[] and kc[] */
	      if( kt[ch1] == 0 )
	      { debug2(DFT, DD, "  kt[%d] = %d", ch1, kc_top);
		kt[ch1] = kc_top;
		kc[kc_top] = (FULL_CHAR) '\0';
		kv[kc_top] = 0;
		kc_top++;
	      }

	      /* find kerning size in ks[] or else add it to the end */
	      for( pos = 1;  pos < ks_top;  pos++ )
	      { if( ks[pos] == ksize )  break;
	      }
	      if( pos == ks_top )
	      { if( ks_top == num_pairs )
		  Error(37, 29, "too many kerning pairs in font file %s (line %d)",
		    FATAL, &fpos(filename), FileName(fnum), lnum);
		debug2(DFT, DD, "  ks[%d] = %d", pos, ksize);
		ks[pos] = ksize;
		ks_top++;
	      }

	      /* insert ch2 into the kc entries (sorted decreasing) for ch1 */
	      for( i = kc_top-1; i >= kt[ch1] && kc[i] < ch2;  i-- )
	      { kc[i+1] = kc[i];
		kv[i+1] = kv[i];
	      }
	      if( i >= kt[ch1] && kc[i] == ch2 )
		Error(37, 30, "kerning pair %s %s appears twice in font file %s (line %d)",
		  FATAL, &fpos(filename), name1, name2, FileName(fnum), lnum);
	      kc[i+1] = ch2;
	      kv[i+1] = pos;
	      kc_top++;
	    }
	  }
	  ks[0] = ks_top;
	}
	break;


      case 'E':

	if( StringEqual(command, AsciiToFull("EndFontMetrics")) )
	{
	  /* make a new font record and insert into font tree */
	  font_num(face) = font_num(fontname) = font_count;
	  font_size(fontname) =
	    (BackEnd != PLAINTEXT) ? SZ_DFT : PlainCharHeight;
	  font_xheight2(fontname) =
	    (BackEnd != PLAINTEXT) ? xheight2 : PlainCharHeight / 4;
	  font_mapping(fontname) = font_mapping(filename);
	  ch = MapCharEncoding(STR_PS_SPACENAME, font_mapping(fontname));
	  font_spacewidth(fontname) = ch == '\0' ? 0 : fnt[ch].right;
	  finfo[font_count].font_table = fontname;
	  finfo[font_count].original_font = face;
	  finfo[font_count].underline_pos = xheight2 - under_pos;
	  finfo[font_count].underline_thick = under_thick;
	  finfo[font_count].size_table = fnt;
	  finfo[font_count].lig_table = lig;
	  finfo[font_count].kern_table = kt;
	  finfo[font_count].kern_chars = kc;
	  finfo[font_count].kern_value = kv;
	  finfo[font_count].kern_sizes = ks;
	  Link(face, fontname);
	  ifdebug(DFT, DD, DebugKernTable(font_count));
	  /* *** either no errors or too many, so killing this now
	  if( InitializeAll )
	  { OBJECT family;  FULL_CHAR *str;
	    Parent(family, Up(face));
	    for( i = 0;  i < MAX_CHARS;  i++ )
	    {
	      if( lig[i] == 1 )
	      { str = string(MapTable[font_mapping(fontname)]->vector[i]);
		if( !StringEqual(str, AsciiToFull(".notdef")) &&
		     MapCharEncoding(str, font_mapping(fontname)) == i )
		  Error(37, 31, "font %s %s has no glyph for character %s",
		    WARN, &fpos(filename), string(family), string(face), str);
	      }
	    }
	  }
	  *** */

	  /* close file, debug and exit */
	  fclose(fp);
	  debug4(DFT, D, "ReadFont returning: %d, name %s, fs %d, xh2 %d",
		  font_count, string(fontname), font_size(fontname), xheight2);
	  return;
	}
	break;


      default:

	break;

    }
  }
  Error(37, 32, "EndFontMetrics missing from font file %s",
    FATAL, &fpos(filename), FileName(fnum));
} /* end ReadFont */


/*@::FontChange()@************************************************************/
/*                                                                           */
/*  FontChange(style, x)                                                     */
/*                                                                           */
/*  Returns an internal font number which is the current font changed        */
/*  according to word object x.  e.g. if current font is Roman 12p and x is  */
/*  "-3p", then FontChange returns the internal font number of Roman 9p.     */
/*                                                                           */
/*  FontChange permits empty and null objects within x; these have no        */
/*  effect.                                                                  */
/*                                                                           */
/*****************************************************************************/

void FontChange(STYLE *style, OBJECT x)
{ /* register */ int i;
  OBJECT par[3], family, face, fsize, y, link, new, old, tmpf;
  GAP gp;  SHORT_LENGTH flen;  int num, c;  unsigned inc;
  struct metrics *newfnt, *oldfnt;  FULL_CHAR *lig;
  SHORT_LENGTH *oldks, *newks;  int klen;
  debug2(DFT, D, "FontChange( %s, %s )", EchoStyle(style), EchoObject(x));
  assert( font(*style) <= font_count, "FontChange: font_count!");
  /* ifdebug(DFT, DD, FontDebug()); */

  /* set par[0..num-1] to the 1, 2 or 3 parameters of the font operator */
  num = 0;
  if( type(x) == NULL_CLOS )
  { /* acceptable, but do nothing */
  }
  else if( is_word(type(x)) )
  {
    if( StringEqual(string(x), STR_SMALL_CAPS_ON) )
      small_caps(*style) = SMALL_CAPS_ON;
    else if( StringEqual(string(x), STR_SMALL_CAPS_OFF) )
      small_caps(*style) = SMALL_CAPS_OFF;
    else if( !StringEqual(string(x), STR_EMPTY) )
      par[num++] = x; 
  }
  else if( type(x) == ACAT )
  { for( link = Down(x);  link != x;  link = NextDown(link) )
    { Child(y, link);
      debug1(DFT, DDD, "  pars examining y = %s", EchoObject(y));
      if( type(y) == GAP_OBJ || type(y)  == NULL_CLOS )  continue;
      if( is_word(type(y)) ) 
      {
        if( StringEqual(string(y), STR_SMALL_CAPS_ON) )
          small_caps(*style) = SMALL_CAPS_ON;
        else if( StringEqual(string(y), STR_SMALL_CAPS_OFF) )
          small_caps(*style) = SMALL_CAPS_OFF;
        else if( !StringEqual(string(y), STR_EMPTY) )
	{
	  if( num >= 3 )
	  { Error(37, 33, "error in left parameter of %s",
	      WARN, &fpos(x), KW_FONT);
	    debug0(DFT, D, "FontChange returning: ACAT children");
	    return;
	  }
          par[num++] = y; 
	}
      }
      else
      {	Error(37, 34, "error in left parameter of %s",
	  WARN, &fpos(x), KW_FONT);
	debug0(DFT, D, "FontChange returning: ACAT children");
	return;
      }
    }
  }
  else
  { Error(37, 35, "error in left parameter of %s", WARN, &fpos(x), KW_FONT);
    debug0(DFT, D, "FontChange returning: wrong type");
    return;
  }
  debug1(DFT, DDD, " found pars, num = %d", num);
  if( num == 0 )
  { debug1(DFT, D, "FontChange returning %s", EchoStyle(style));
    return;
  }

  /* extract fsize parameter, if any */
  assert( num >= 1 && num <= 3, "FontChange: num!" );
  fsize = nilobj;
  for( i = 0;  i < num;  i++ )
  {
    c = string(par[i])[0];
    if( c == CH_INCGAP || c == CH_DECGAP || decimaldigit(c) )
    {
      /* extract fsize, shuffle the rest down */
      fsize = par[i];
      for( i = i + 1;  i < num;  i++ )
	par[i-1] = par[i];
      num--;
    }
  }

  /* *** old now
  c = string(par[num-1])[0];
  if( c == CH_INCGAP || c == CH_DECGAP || decimaldigit(c) )
  { fsize = par[num-1];  num--;
  }
  else fsize = nilobj;
  *** */

  /* check for initial font case: must have family, face, and size */
  if( font(*style) == NO_FONT && (fsize == nilobj || num < 2) )
    Error(37, 36, "initial font must have family, face and size",
      FATAL, &fpos(x));

  /* get font family */
  if( num == 2 )
  {
    /* par[0] contains a new family name */
    for( link = Down(font_root);  link != font_root;  link = NextDown(link) )
    { Child(family, link);
      if( StringEqual(string(family), string(par[0])) )  break;
    }
    if( link == font_root )
    { Error(37, 37, "font family %s not defined",
	WARN, &fpos(par[0]), string(par[0]));
      return;
    }
  }
  else
  { /* preserve current family */
    assert( Up(finfo[font(*style)].font_table)!=finfo[font(*style)].font_table,
      "FontChange: Up(finfo[font(*style)].font_table) !" );
    Parent(face, Up(finfo[font(*style)].font_table));
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
      {	Error(37, 38, "there are no fonts", FATAL, &fpos(par[num-1]));
      }
      else if( link != font_root )
      {	Error(37, 39, "font family name %s must be accompanied by a face name",
	  WARN, &fpos(par[num-1]), string(par[num-1]));
      }
      else Error(37, 40, "font face name %s not defined in font family %s",
	     WARN, &fpos(par[num-1]), string(par[num-1]), string(family));
      return;
    }
  }
  else
  {
    /* preserve current face name */
    Parent(face, Up(finfo[font(*style)].font_table));
    assert( is_word(type(face)), "FontChange: type(face)!" );
    assert( Up(face) != face, "FontChange: Up(face)!" );
  }

  /* get font size */
  if( fsize == nilobj )  flen = font_size(finfo[font(*style)].font_table);
  else 
  { GetGap(fsize, style, &gp, &inc);
    if( mode(gp) != EDGE_MODE || units(gp) != FIXED_UNIT )
    { Error(37, 56, "syntax error in font size %s; ignoring it",
	WARN, &fpos(fsize), string(fsize));
      flen = font_size(finfo[font(*style)].font_table);
    }
    else if( inc == GAP_ABS )
      flen = width(gp);
    else if( font(*style) == NO_FONT )
    { Error(37, 41, "no current font on which to base size change %s",
	FATAL, &fpos(fsize), string(fsize));
    }
    else if( inc == GAP_INC )
      flen = font_size(finfo[font(*style)].font_table) + width(gp);
    else if( inc == GAP_DEC )
      flen = font_size(finfo[font(*style)].font_table) - width(gp);
    else Error(37, 42, "FontChange: %d", INTERN, &fpos(x), inc);
  }

  if( flen <= 0 )
  { Error(37, 43, "%s %s ignored (result is not positive)",
      WARN, &fpos(fsize), string(fsize), KW_FONT);
    return;
  }

  /* if the font file has not been read before, read it now */
  assert( Down(face) != face && type(Down(face)) == LINK, "FontChange: dn!" );
  if( Down(face) == LastDown(face) )  ReadFont(face, x);
  assert( Down(face) != LastDown(face), "FontChange: after ReadFont!" );

  /* search fonts of face for desired size; return if already present */
  if( BackEnd == PLAINTEXT )  flen = PlainCharHeight;
  for( link = NextDown(Down(face));  link != face;  link = NextDown(link) )
  { Child(fsize, link);
    if( font_size(fsize) == flen )
    { font(*style) = font_num(fsize);
      SetGap(space_gap(*style), nobreak(space_gap(*style)), FALSE, TRUE,
	FIXED_UNIT, EDGE_MODE, font_spacewidth(fsize));
      debug2(DFT, D,"FontChange returning (old) %d (XHeight2 = %d)",
	font(*style), font_xheight2(finfo[font(*style)].font_table));
      return;
    }
  }

  /* now need to rescale the font; first create a sized font record */
  if( ++font_count >= finfo_size )
  { if( font_count > MAX_FONT )
      Error(37, 44, "too many different fonts and sizes (max is %d)",
        FATAL, &fpos(x), MAX_FONT);
    ifdebug(DMA, D, DebugRegisterUsage(MEM_FONTS, -1,
      -finfo_size * sizeof(FONT_INFO)));
    finfo_size *= 2;
    ifdebug(DMA, D, DebugRegisterUsage(MEM_FONTS, 1,
      finfo_size * sizeof(FONT_INFO)));
    finfo = (FONT_INFO *) realloc(finfo, finfo_size * sizeof(FONT_INFO));
    if( finfo == (FONT_INFO *) NULL )
      Error(37, 45, "run out of memory when increasing font table size",
	FATAL, &fpos(x));
  }

  assert( Down(face) != face && NextDown(Down(face)) != face, "FontChange!!" );
  Child(old, NextDown(Down(face)));
  assert( is_word(type(old)), "FontChange: old!" );
  new = MakeWord(WORD, string(old), no_fpos);
  Link(face, new);
  font_size(new)        = BackEnd != PLAINTEXT ? flen : font_size(old);
  font_xheight2(new)    = font_xheight2(old) * font_size(new) / font_size(old);
  font_mapping(new)	= font_mapping(old);
  font_spacewidth(new)	= font_spacewidth(old) * font_size(new)/font_size(old);
  font_num(new)         = font_count;
  finfo[font_count].font_table = new;
  finfo[font_count].original_font = face;
  finfo[font_count].underline_pos =
    (finfo[font_num(old)].underline_pos * font_size(new)) / font_size(old);
  finfo[font_count].underline_thick =
    (finfo[font_num(old)].underline_thick * font_size(new)) / font_size(old);
  ifdebug(DMA, D, DebugRegisterUsage(MEM_FONTS, 1,
      MAX_CHARS * sizeof(struct metrics)));
  finfo[font_count].size_table =
    (struct metrics *) malloc(MAX_CHARS * sizeof(struct metrics));
  if( finfo[font_count].size_table == (struct metrics *) NULL )
    Error(37, 46, "run out of memory when changing font or font size",
      FATAL, &fpos(x));
  finfo[font_count].lig_table  = lig = finfo[font_num(old)].lig_table;

  /* scale old font to new size */
  newfnt = finfo[font_num(new)].size_table;
  oldfnt = finfo[font_num(old)].size_table;
  for( i = 0;  i < MAX_CHARS;  i++ )  if( lig[i] != 1 )
  { newfnt[i].left  = (oldfnt[i].left  * font_size(new)) / font_size(old);
    newfnt[i].right = (oldfnt[i].right * font_size(new)) / font_size(old);
    newfnt[i].down  = (oldfnt[i].down  * font_size(new)) / font_size(old);
    newfnt[i].up    = (oldfnt[i].up    * font_size(new)) / font_size(old);
    newfnt[i].last_adjust = (oldfnt[i].last_adjust * font_size(new)) / font_size(old);
  }

  /* copy and scale kerning tables */
  finfo[font_count].kern_table = finfo[font_num(old)].kern_table;
  finfo[font_count].kern_chars = finfo[font_num(old)].kern_chars;
  finfo[font_count].kern_value = finfo[font_num(old)].kern_value;
  oldks = finfo[font_num(old)].kern_sizes;
  if( oldks != (SHORT_LENGTH *) NULL )
  { klen = oldks[0];
    ifdebug(DMA, D, DebugRegisterUsage(MEM_FONTS, 0, klen * sizeof(SHORT_LENGTH)));
    finfo[font_count].kern_sizes = newks =
      (SHORT_LENGTH *) malloc(klen * sizeof(SHORT_LENGTH));
    if( newks == (SHORT_LENGTH *) NULL )
      Error(37, 47, "run out of memory when changing font or font size",
	FATAL, &fpos(x));
    newks[0] = klen;
    for( i = 1;  i < klen;  i++ )
      newks[i] = (oldks[i] * font_size(new)) / font_size(old);
  }
  else finfo[font_count].kern_sizes = (SHORT_LENGTH *) NULL;

  /* return new font number and exit */
  font(*style) = font_count;
  SetGap(space_gap(*style), nobreak(space_gap(*style)), FALSE, TRUE,
    FIXED_UNIT, EDGE_MODE, font_spacewidth(new));
  debug2(DFT, D,"FontChange returning (scaled) %d (XHeight2 = %d)",
    font(*style), font_xheight2(finfo[font(*style)].font_table));
  /* FontDebug(); */
} /* end FontChange */


/*****************************************************************************/
/*                                                                           */
/*  KernLength(fnum, ch1, ch2, res)                                          */
/*                                                                           */
/*  Set res to the kern length between ch1 and ch2 in font fnum, or 0 if     */
/*  none.  Actually we first convert ch1 and ch2 to corresponding unaccented */
/*  characters, because metrics files don't seem to contain kerning pairs    */
/*  for accented characters.                                                 */
/*                                                                           */
/*****************************************************************************/

#define KernLength(fnum, mp, ch1, ch2, res)				\
{ int ua_ch1 = mp[ch1];							\
  int ua_ch2 = mp[ch2];							\
  int i = finfo[fnum].kern_table[ua_ch1], j;				\
  if( i == 0 )  res = 0;						\
  else									\
  { FULL_CHAR *kc = finfo[fnum].kern_chars;				\
    for( j = i;  kc[j] > ua_ch2;  j++ );				\
    res = (kc[j] == ua_ch2) ?						\
      finfo[fnum].kern_sizes[finfo[fnum].kern_value[j]] : 0;		\
  }									\
} /* end KernLength */


/*@::FontWordSize()@**********************************************************/
/*                                                                           */
/*  FontWordSize(x)                                                          */
/*                                                                           */
/*  Calculate the horizontal and vertical size of WORD or QWORD x, including */
/*  the effect of ligature sequences but not replacing them with ligatures.  */
/*                                                                           */
/*****************************************************************************/

void FontWordSize(OBJECT x)
{ FULL_CHAR *p, *q, *a, *b, *lig, *unacc, *acc;  OBJECT tmp;
  FULL_CHAR buff[MAX_BUFF];  MAPPING m;
  int r, u, d, ksize; struct metrics *fnt;
  debug2(DFT, D, "FontWordSize( %s ), font = %d", string(x), word_font(x));
  assert( is_word(type(x)), "FontWordSize: !is_word(type(x))!" );

  p = string(x);
  q = buff;
  if( *p )
  { if ( word_font(x) < 1 || word_font(x) > font_count )
      Error(37, 48, "no current font at word %s", FATAL, &fpos(x), string(x));
    if ( word_colour(x) == 0 )
      Error(37, 49, "no current colour at word %s", FATAL, &fpos(x), string(x));
    if ( word_language(x) == 0 )
      Error(37, 50, "no current language at word %s", FATAL, &fpos(x),string(x));
    fnt = finfo[word_font(x)].size_table;
    lig = finfo[word_font(x)].lig_table;
    m = font_mapping(finfo[word_font(x)].font_table);
    unacc = MapTable[m]->map[MAP_UNACCENTED];
    acc   = MapTable[m]->map[MAP_ACCENT];
    d = u = r = 0;
    do
    { 
      /* check for missing glyph (lig[] == 1) or ligatures (lig[] > 1) */
      if( lig[*q = *p++] )
      {
	if( lig[*q] == 1 )
	{ tmp = MakeWord(QWORD, STR_SPACE, &fpos(x));
	  string(tmp)[0] = *q;
	  if( unacc[*q] != '\0' )
	  {
	    /* *** this is acceptable now, let this char through
	    Error(37, 51, "accent dropped from character %s (it has no glyph in font %s)",
	      WARN, &fpos(x),
	      StringQuotedWord(tmp), FontFamilyAndFace(word_font(x)));
	    *(p-1) = *q = unacc[*q];
	    *** */
	    fnt[*q].up = fnt[unacc[*q]].up;
	    fnt[*q].down = fnt[unacc[*q]].down;
	    fnt[*q].right = fnt[unacc[*q]].right;
	  }
	  else
	  {
	    Error(37, 52, "character %s replaced by space (it has no glyph in font %s)",
	      WARN, &fpos(x),
	      StringQuotedWord(tmp), FontFamilyAndFace(word_font(x)));
	    *(p-1) = *q = CH_SPACE;
	  }
	  Dispose(tmp);
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
    *q = '\0';

    /* adjust for last character */
    r += fnt[*(q-1)].last_adjust;

    /* add kern lengths to r */
    for( p = buff, q = p+1;  *q;  p++, q++ )
    { KernLength(word_font(x), unacc, *p, *q, ksize);
      debugcond3(DFT, D, ksize != 0, "  KernLength(fnum, %c, %c) = %d",
	*p, *q, ksize);
      r += ksize;
    }
    /* set sizes of x */
    back(x, COLM) = 0;
    fwd(x, COLM)  = r;
    back(x, ROWM) = u;
    fwd(x, ROWM)  = -d;
  } 
  else back(x, COLM) = fwd(x, COLM) = back(x, ROWM) = fwd(x, ROWM) = 0;
  debug4(DFT, D, "FontWordSize returning %hd %hd %hd %hd",
	  back(x, COLM), fwd(x, COLM), back(x, ROWM), fwd(x, ROWM));
} /* end FontWordSize */


/*@::FontSize(), FontHalfXHeight(), FontEncoding(), FontName()@***************/
/*                                                                           */
/*  FULL_LENGTH FontSize(fnum, x)                                            */
/*                                                                           */
/*  Return the size of this font.  x is for error messages only.             */
/*                                                                           */
/*****************************************************************************/

FULL_LENGTH FontSize(FONT_NUM fnum, OBJECT x)
{ debug1(DFT, DD, "FontSize( %d )", fnum);
  assert( fnum <= font_count, "FontSize!" );
  if( fnum <= 0 )
    Error(37, 53, "no current font at this point", FATAL, &fpos(x));
  debug1(DFT, DD, "FontSize returning %d", font_size(finfo[fnum].font_table));
  return font_size(finfo[fnum].font_table);
} /* end FontSize */


/*****************************************************************************/
/*                                                                           */
/*  FULL_LENGTH FontHalfXHeight(fnum)                                        */
/*                                                                           */
/*  Return the xheight2 value of this font.                                  */
/*                                                                           */
/*****************************************************************************/

FULL_LENGTH FontHalfXHeight(FONT_NUM fnum)
{ debug1(DFT, DD, "FontHalfXHeight( %d )", fnum);
  assert( fnum <= font_count, "FontHalfXHeight!" );
  debug1(DFT, DD, "FontHalfXHeight returning %d",
    font_xheight2(finfo[fnum].font_table));
  return font_xheight2(finfo[fnum].font_table);
} /* end FontHalfXHeight */


/*****************************************************************************/
/*                                                                           */
/*  MAPPING FontMapping(fnum, xfpos)                                         */
/*                                                                           */
/*  Return the character mapping of this font, to use for small caps, etc.   */
/*  xfpos is the file position for error messages.                           */
/*                                                                           */
/*****************************************************************************/

MAPPING FontMapping(FONT_NUM fnum, FILE_POS *xfpos)
{ debug1(DFT, DD, "FontMapping( %d )", fnum);
  assert( fnum <= font_count, "FontMapping!" );
  if( fnum <= 0 )
    Error(37, 54, "no current font at this point", FATAL, xfpos);
  debug1(DFT, DD, "FontMapping returning %d",
    font_mapping(finfo[fnum].font_table));
  return font_mapping(finfo[fnum].font_table);
} /* end FontMapping */


/*****************************************************************************/
/*                                                                           */
/*  FULL_CHAR *FontName(fnum)                                                */
/*                                                                           */
/*  Return the short PostScript name of this font.                           */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *FontName(FONT_NUM fnum)
{ OBJECT face, AFMfilename, short_name;
  debug1(DFT, D, "FontName( %d )", fnum);
  assert( fnum <= font_count, "FontName!" );
  Parent(face, Up(finfo[fnum].font_table));
  Child(AFMfilename, Down(face));
  Child(short_name, Down(AFMfilename));
  assert( is_word(type(short_name)), "FontName: short_name!" );
  debug1(DFT, D, "FontName returning %s", string(short_name));
  return string(short_name);
} /* end FontName */


/*@::FontFamily(), FontFace@**************************************************/
/*                                                                           */
/*  FULL_CHAR *FontFamilyAndFace(fnum)                                       */
/*                                                                           */
/*  Return a static string of the current font family and face.              */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *FontFamily(FONT_NUM fnum)
{ OBJECT face, family;
  debug1(DFT, D, "FontFamily( %d )", fnum);
  assert( fnum <= font_count, "FontFamiliy!" );
  Parent(face, Up(finfo[fnum].font_table));
  Parent(family, Up(face));
  debug1(DFT, D, "FontFamily returning %s", string(family));
  return string(family);
} /* end FontFamilyAndFace */


FULL_CHAR *FontFace(FONT_NUM fnum)
{ OBJECT face, family;
  debug1(DFT, D, "FontFacec( %d )", fnum);
  assert( fnum <= font_count, "FontFamiliy!" );
  Parent(face, Up(finfo[fnum].font_table));
  Parent(family, Up(face));
  debug1(DFT, D, "FontFace returning %s", string(face));
  return string(face);
} /* end FontFamilyAndFace */


/*@::FontFamilyAndFace(), FontPrintAll()@*************************************/
/*                                                                           */
/*  FULL_CHAR *FontFamilyAndFace(fnum)                                       */
/*                                                                           */
/*  Return a static string of the current font family and face.              */
/*                                                                           */
/*****************************************************************************/

FULL_CHAR *FontFamilyAndFace(FONT_NUM fnum)
{ OBJECT face, family; static FULL_CHAR buff[80];
  debug1(DFT, D, "FontFamilyAndFace( %d )", fnum);
  assert( fnum <= font_count, "FontName!" );
  Parent(face, Up(finfo[fnum].font_table));
  Parent(family, Up(face));
  if( StringLength(string(family)) + StringLength(string(face)) + 1 > 80 )
    Error(37, 55, "family and face names %s %s are too long",
      FATAL, no_fpos, string(family), string(face));
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

void FontPrintAll(FILE *fp)
{ OBJECT family, face, AFMfilename, short_name, ps_name, link, flink;
  assert(font_root!=nilobj && type(font_root)==ACAT, "FontDebug: font_root!");
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
	  MapEncodingName(font_mapping(AFMfilename)), string(ps_name));
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


/*@@**************************************************************************/
/*                                                                           */
/*  FontPrintPageSetup(fp)             	                                     */
/*                                                                           */
/*  Print all font encoding commands needed for the current page onto fp.    */
/*                                                                           */
/*****************************************************************************/

void FontPrintPageSetup(FILE *fp)
{ OBJECT face, AFMfilename, short_name, ps_name, link;
  assert(font_root!=nilobj && type(font_root)==ACAT, "FontDebug: font_root!");
  assert(font_used!=nilobj && type(font_used)==ACAT, "FontDebug: font_used!");
  debug0(DFT, DD, "FontPrintPageSetup(fp)");
  for( link = Down(font_used);  link != font_used;  link = NextDown(link) )
  {
    Child(face, link);
    assert( is_word(type(face)), "FontPrintPageSetup: face!" );
    assert( Down(face) != face, "FontDebug: Down(face)!");

    /* record that face is used on the first page, if this is the first page */
    if( font_curr_page == 1 )  font_firstpage(face) = TRUE;

    /* print font encoding command unless already done */
    if( !font_firstpage(face) || font_curr_page == 1 )
    { Child(AFMfilename, Down(face));
      assert( is_word(type(AFMfilename)), "FontPrintPageSetup: filename!" );
      assert( Down(AFMfilename) != AFMfilename, "FontPrintPageSetup: 1!" );
      assert( LastDown(AFMfilename)!=Down(AFMfilename), "FontPrintPageSetup!");
      Child(short_name, Down(AFMfilename));
      assert( is_word(type(short_name)), "FontPrintPageSetup: short_name!" );
      Child(ps_name, LastDown(AFMfilename));
      assert( is_word(type(ps_name)), "FontPrintPageSetup: ps_name!" );
      fprintf(fp, "%%%%IncludeResource: font %s\n", string(ps_name));

      switch( BackEnd )
      {
	case POSTSCRIPT:

          if( font_recoded(face) )
          { fprintf(fp, "/%s%s %s /%s LoutRecode\n",
	      string(ps_name), string(short_name),
	      MapEncodingName(font_mapping(AFMfilename)), string(ps_name));
            fprintf(fp, "/%s { /%s%s LoutFont } def\n", string(short_name),
	      string(ps_name), string(short_name));
          }
          else fprintf(fp, "/%s { /%s LoutFont } def\n", string(short_name),
	    string(ps_name));
	  break;

	
	case PDF:

	  PDFFont_AddFont(fp, string(short_name), string(ps_name),
	    MapEncodingName(font_mapping(AFMfilename)));
	  break;
      }
    }
  }
  debug0(DFT, DD, "FontPrintPageSetup returning.");
} /* end FontPrintPageSetup */


/*@@**************************************************************************/
/*                                                                           */
/*  FontPrintPageResources(fp)        	                                     */
/*                                                                           */
/*  Print all page resources (i.e. fonts needed or supplied) onto fp.        */
/*                                                                           */
/*****************************************************************************/

void FontPrintPageResources(FILE *fp)
{ OBJECT face, AFMfilename, short_name, ps_name, link;
  BOOLEAN first;
  assert(font_root!=nilobj && type(font_root)==ACAT, "FontDebug: font_root!");
  assert(font_used!=nilobj && type(font_used)==ACAT, "FontDebug: font_used!");
  debug0(DFT, DD, "FontPrintPageResources(fp)");
  first = TRUE;
  for( link = Down(font_used);  link != font_used;  link = NextDown(link) )
  {
    Child(face, link);
    assert( is_word(type(face)), "FontPrintPageResources: face!" );
    assert( Down(face) != face, "FontDebug: Down(face)!");

    Child(AFMfilename, Down(face));
    assert( is_word(type(AFMfilename)), "FontPrintPageResources: filename!" );
    assert( Down(AFMfilename) != AFMfilename, "FontPrintPageResources: 1!" );
    assert( LastDown(AFMfilename)!=Down(AFMfilename), "FontPrintPageRes!");
    Child(short_name, Down(AFMfilename));
    assert( is_word(type(short_name)), "FontPrintPageResources: short_name!" );
    Child(ps_name, LastDown(AFMfilename));
    assert( is_word(type(ps_name)), "FontPrintPageResources: ps_name!" );

    switch( BackEnd )
    {
      case POSTSCRIPT:

        fprintf(fp, "%s font %s\n",
          first ? "%%PageResources:" : "%%+", string(ps_name));
        first = FALSE;
	break;


      case PDF:

	/* PDFWriteFontResource(fp, string(ps_name)); */
	break;
    }
  }
  debug0(DFT, DD, "FontPrintPageResources returning.");
} /* end FontPrintPageResources */


/*@@**************************************************************************/
/*                                                                           */
/*  FontAdvanceCurrentPage()        	                                     */
/*                                                                           */
/*  Advance the current page.                                                */
/*                                                                           */
/*****************************************************************************/

void FontAdvanceCurrentPage(void)
{ debug0(DFT, DD, "FontAdvanceCurrentPage()");
  while( Down(font_used) != font_used )  DeleteLink(Down(font_used));
  font_curr_page++;
  debug0(DFT, DD, "FontAdvanceCurrentPage() returning.");
} /* end FontAdvanceCurrentPage */


/*@::FontPageUsed()@**********************************************************/
/*                                                                           */
/*  OBJECT FontPageUsed(face)                                                */
/*                                                                           */
/*  Declares that font face is used on the current page.                     */
/*                                                                           */
/*****************************************************************************/

void FontPageUsed(OBJECT face)
{ debug1(DFT, DD, "FontPageUsed(%d)", font_num(face));
  assert( font_page(face) < font_curr_page, "FontPageUsed!" );
  Link(font_used, face);
  font_page(face) = font_curr_page;
  debug0(DFT, DD, "FontPageUsed returning");
} /* end FontPageUsed */


/*@::FontNeeded()@************************************************************/
/*                                                                           */
/*  OBJECT FontNeeded(fp)                                                    */
/*                                                                           */
/*  Writes font needed resources onto file out_fp.  Returns TRUE if none.    */
/*                                                                           */
/*****************************************************************************/

BOOLEAN FontNeeded(FILE *fp)
{ BOOLEAN first_need = TRUE;
  OBJECT link, flink, family, face, x;
  for( link = Down(font_root); link != font_root; link = NextDown(link) )
  { Child(family, link);
    for( flink = Down(family);  flink != family;  flink = NextDown(flink) )
    { Child(face, flink);
      if( LastDown(face) != Down(face) )
      { Child(x, LastDown(face));
        fprintf(fp, "%s font %s\n",
            first_need ? "%%DocumentNeededResources:" : "%%+", string(x));
        first_need = FALSE;
      }
    }
  }
  return first_need;
} /* end FontNeeded */
