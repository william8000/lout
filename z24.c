/*@z24.c:Print Service:PrintInit()@*******************************************/
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
/*  FILE:         z24.c                                                      */
/*  MODULE:       Print Service                                              */
/*  EXTERNS:      PrintInit(), PrintPrologue(), PrintOriginIncrement(),      */
/*                PrintWord(), PrintClose(), CoordTranslate(),               */
/*                CoordRotate(), CoordScale(), SaveGraphicState(),           */
/*                RestoreGraphicState(), PrintGraphicObject(),               */
/*                DefineGraphicNames(), PrintGraphicInclude()                */
/*                                                                           */
/*  This module implements the PostScript back end.                          */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define DEFAULT_XHEIGHT 500
#define	NO_FONT	0			/* actually stolen from z37.c        */

#define printnum(x, fp)							\
{ char buff[20];  register int i, y;					\
  if( x >= 0 )  y = x;							\
  else { y = -x; putc(CH_MINUS, fp); }					\
  i = 0;								\
  do { buff[i++] = numtodigitchar(y % 10);				\
     } while( y = y / 10 );						\
  do { putc(buff[--i], fp);						\
     } while( i );							\
}

static FILE	*out_fp;		/* output file                       */
static FONT_NUM	currentfont;		/* font of most recent atom          */
static short	currentxheight2;	/* half xheight in current font      */
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
  out_fp = file_ptr;  prologue_done = FALSE;
  currentfont = NO_FONT;  cpexists = FALSE;
  wordcount = pagecount = 0;  needs = New(ACAT);
  debug0(DFT, D, "PrintInit returning.");
}


/*@::PrintPrologue@***********************************************************/
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
/*      number        vs        result is number vspaces                     */
/*      number        ft        result is number font-sizes                  */
/*                                                                           */
/*  as well as LoutGraphic, for use with the @Graphic operator:              */
/*                                                                           */
/*      xsize ysize xmark ymark fr vs sp LoutGraphic -                       */
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
  fprintf(out_fp, "%%%%EndComments\n\n");

  /* print procedure definitions part of header */
  fprintf(out_fp, "%%%%BeginProlog\n");
  fprintf(out_fp, "%%%%BeginResource: procset LoutStartUp\n");
  fprintf(out_fp, "/x { currentpoint exch pop moveto } def\n");
  fprintf(out_fp, "/s { show } def\n");
  fprintf(out_fp, "/in { %d mul } def\n", IN);
  fprintf(out_fp, "/cm { %d mul } def\n", CM);
  fprintf(out_fp, "/pt { %d mul } def\n", PT);
  fprintf(out_fp, "/em { %d mul } def\n", EM);
  fprintf(out_fp, "/sp { louts mul } def\n");
  fprintf(out_fp, "/vs { loutv mul } def\n");
  fprintf(out_fp, "/ft { loutf mul } def\n");
  fprintf(out_fp, "/dg {           } def\n\n");

  fputs("/LoutGraphic {\n",					  out_fp);
  fputs("  /louts exch def\n",					  out_fp);
  fputs("  /loutv exch def\n",					  out_fp);
  fputs("  /loutf exch def\n",					  out_fp);
  fputs("  /ymark exch def\n",					  out_fp);
  fputs("  /xmark exch def\n",					  out_fp);
  fputs("  /ysize exch def\n",					  out_fp);
  fputs("  /xsize exch def\n} def\n\n",				  out_fp);

  /* print definition used by Lout output to recode fonts                 */
  /* adapted from PostScript Language Reference Manual (2nd Ed), page 275 */
  /* usage: /<fullname> <encodingvector> /<originalname> LoutRecode -     */

  fputs("/LoutFont\n",                                            out_fp);
  fputs("{ findfont exch scalefont setfont\n",                    out_fp);
  fputs("} bind def\n\n",					  out_fp);

  fputs("/LoutRecode {\n",                                        out_fp);
  fputs("  { findfont dup length dict begin\n",                   out_fp);
  fputs("    {1 index /FID ne {def} {pop pop} ifelse} forall\n",  out_fp);
  fputs("    /Encoding exch def\n",                               out_fp);
  fputs("    currentdict end definefont pop\n",                   out_fp);
  fputs("  }\n",                                                  out_fp);
  fputs("  stopped {}\n",                                         out_fp);
  fputs("} bind def\n\n",                                         out_fp);

  /* print definitions used by Lout output when including EPSF files      */
  /* copied from PostScript Language Reference Manual (2nd Ed.), page 726 */

  fputs("/BeginEPSF {\n",					  out_fp);
  fputs("  /LoutEPSFState save def\n",				  out_fp);
  fputs("  /dict_count countdictstack def\n",			  out_fp);
  fputs("  /op_count count 1 sub def\n",			  out_fp);
  fputs("  userdict begin\n",					  out_fp);
  fputs("  /showpage { } def\n",				  out_fp);
  fputs("  0 setgray 0 setlinecap\n",				  out_fp);
  fputs("  1 setlinewidth 0 setlinejoin\n",			  out_fp);
  fputs("  10 setmiterlimit [] 0 setdash newpath\n",		  out_fp);
  fputs("  /languagelevel where\n",				  out_fp);
  fputs("  { pop languagelevel\n",				  out_fp);
  fputs("    1 ne\n",						  out_fp);
  fputs("    { false setstrokeadjust false setoverprint\n",	  out_fp);
  fputs("    } if\n",						  out_fp);
  fputs("  } if\n",						  out_fp);
  fputs("} bind def\n\n",					  out_fp);

  fputs("/EndEPSF {\n",						  out_fp);
  fputs("  count op_count sub { pop } repeat\n",		  out_fp);
  fputs("  countdictstack dict_count sub { end } repeat\n",	  out_fp);
  fputs("  LoutEPSFState restore\n",				  out_fp);
  fputs("} bind def\n",						  out_fp);

  fputs("%%EndResource\n\n",					  out_fp);

  /* print encoding vectors and font recoding commands */
  EvPrintAll(out_fp);
  FontPrintAll(out_fp);

  /* print prepend files (assumed to be organized as DSC 3.0 Resources) */
  for( fnum=FirstFile(PREPEND_FILE);  fnum != NO_FILE;  fnum=NextFile(fnum) )
  { FULL_CHAR buff[MAX_LINE];  FILE *fp;
    if( (fp = OpenFile(fnum, FALSE, FALSE)) == null )
      Error(WARN, PosOfFile(fnum), "cannot open %s file %s",
	KW_PREPEND, FileName(fnum));
    else if( StringFGets(buff, MAX_LINE, fp) == NULL )
      Error(WARN, PosOfFile(fnum), "%s file %s is empty",
	KW_PREPEND, FileName(fnum));
    else
    {
      if( !StringBeginsWith(buff, AsciiToFull("%%BeginResource:")) )
	Error(WARN, PosOfFile(fnum),
	  "%s file %s lacks PostScript DSC 3.0 \"%%%%BeginResource:\" comment",
	  KW_PREPEND, FileName(fnum));
      StringFPuts(buff, out_fp);
      fprintf(out_fp, "\n%% %s file %s\n", KW_PREPEND, FileName(fnum));
      while( StringFGets(buff, MAX_LINE, fp) != NULL )
	StringFPuts(buff, out_fp);
    }
  }

  fputs("\n%%EndProlog\n\n", out_fp);
  fprintf(out_fp, "%%%%Page: ? %d\n", ++pagecount);
  fprintf(out_fp, "%%%%BeginPageSetup\n");
  fprintf(out_fp, "/pgsave save def\n");
  fprintf(out_fp, "%.4f dup scale %d setlinewidth\n", 1.0 / PT, PT/2);
  fprintf(out_fp, "%%%%EndPageSetup\n");
  prologue_done = TRUE;
} /* end PrintPrologue */


/*@::PrintOriginIncrement(), EightBitsToPrintForm[]@**************************/
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
  fprintf(out_fp, "/pgsave save def\n");
  fprintf(out_fp, "%.4f dup scale %d setlinewidth\n", 1.0 / PT, PT/2);
  fprintf(out_fp, "%%%%EndPageSetup\n");
  wordcount = 0;
}

/*@::EightBitToPrintForm()@***************************************************/
/*                                                                           */
/*  static char *EightBitToPrintForm[]                                       */
/*                                                                           */
/*  Given 8-bit character i, returns a string of characters that will be     */
/*  interpreted by PostScript as character i when read within a string.      */
/*                                                                           */
/*      CHAR_OUT==1    Printable ASCII literal, others as escape sequences   */
/*      CHAR_OUT==2    Printable ISO-LATIN-1 literal, others escaped         */
/*                                                                           */
/*****************************************************************************/

static char *EightBitToPrintForm[] = {
#if CHAR_OUT==0
    "",      "\\001", "\\002", "\\003", "\\004", "\\005", "\\006", "\\007",
    "\\010", "\\011", "\\012", "\\013", "\\014", "\\015", "\\016", "\\017",
    "\\020", "\\021", "\\022", "\\023", "\\024", "\\025", "\\026", "\\027",
    "\\030", "\\031", "\\032", "\\033", "\\034", "\\035", "\\036", "\\037",
    " ",     "!",     "\"",    "#",     "$",     "%",     "&",     "'",
    "\\(",   "\\)",   "*",     "+",     ",",     "-",     ".",     "/",
    "0",     "1",     "2",     "3",     "4",     "5",     "6",     "7",
    "8",     "9",     ":",     ";",     "<",     "=",     ">",     "?",
    "@",     "A",     "B",     "C",     "D",     "E",     "F",     "G",
    "H",     "I",     "J",     "K",     "L",     "M",     "N",     "O",
    "P",     "Q",     "R",     "S",     "T",     "U",     "V",     "W",
    "X",     "Y",     "Z",     "[",     "\\\\",  "]",     "^",     "_",
    "`",     "a",     "b",     "c",     "d",     "e",     "f",     "g",
    "h",     "i",     "j",     "k",     "l",     "m",     "n",     "o",
    "p",     "q",     "r",     "s",     "t",     "u",     "v",     "w",
    "x",     "y",     "z",     "{",     "|",     "}",     "~",     "\\177",
    "\\200", "\\201", "\\202", "\\203", "\\204", "\\205", "\\206", "\\207",
    "\\210", "\\211", "\\212", "\\213", "\\214", "\\215", "\\216", "\\217",
    "\\220", "\\221", "\\222", "\\223", "\\224", "\\225", "\\226", "\\227",
    "\\230", "\\231", "\\232", "\\233", "\\234", "\\235", "\\236", "\\237",
    "\\240", "\\241", "\\242", "\\243", "\\244", "\\245", "\\246", "\\247",
    "\\250", "\\251", "\\252", "\\253", "\\254", "\\255", "\\256", "\\257",
    "\\260", "\\261", "\\262", "\\263", "\\264", "\\265", "\\266", "\\267",
    "\\270", "\\271", "\\272", "\\273", "\\274", "\\275", "\\276", "\\277",
    "\\300", "\\301", "\\302", "\\303", "\\304", "\\305", "\\306", "\\307",
    "\\310", "\\311", "\\312", "\\313", "\\314", "\\315", "\\316", "\\317",
    "\\320", "\\321", "\\322", "\\323", "\\324", "\\325", "\\326", "\\327",
    "\\330", "\\331", "\\332", "\\333", "\\334", "\\335", "\\336", "\\337",
    "\\340", "\\341", "\\342", "\\343", "\\344", "\\345", "\\346", "\\347",
    "\\350", "\\351", "\\352", "\\353", "\\354", "\\355", "\\356", "\\357",
    "\\360", "\\361", "\\362", "\\363", "\\364", "\\365", "\\366", "\\367",
    "\\370", "\\371", "\\372", "\\373", "\\374", "\\375", "\\376", "\\377"
#else
#if CHAR_OUT==1
    "",      "\\001", "\\002", "\\003", "\\004", "\\005", "\\006", "\\007",
    "\\010", "\\011", "\\012", "\\013", "\\014", "\\015", "\\016", "\\017",
    "\\020", "\\021", "\\022", "\\023", "\\024", "\\025", "\\026", "\\027",
    "\\030", "\\031", "\\032", "\\033", "\\034", "\\035", "\\036", "\\037",
    " ",     "!",     "\"",    "#",     "$",     "%",     "&",     "'",
    "\\(",   "\\)",   "*",     "+",     ",",     "-",     ".",     "/",
    "0",     "1",     "2",     "3",     "4",     "5",     "6",     "7",
    "8",     "9",     ":",     ";",     "<",     "=",     ">",     "?",
    "@",     "A",     "B",     "C",     "D",     "E",     "F",     "G",
    "H",     "I",     "J",     "K",     "L",     "M",     "N",     "O",
    "P",     "Q",     "R",     "S",     "T",     "U",     "V",     "W",
    "X",     "Y",     "Z",     "[",     "\\\\",  "]",     "^",     "_",
    "`",     "a",     "b",     "c",     "d",     "e",     "f",     "g",
    "h",     "i",     "j",     "k",     "l",     "m",     "n",     "o",
    "p",     "q",     "r",     "s",     "t",     "u",     "v",     "w",
    "x",     "y",     "z",     "{",     "|",     "}",     "~",     "\\177",
    "\\200", "\\201", "\\202", "\\203", "\\204", "\\205", "\\206", "\\207",
    "\\210", "\\211", "\\212", "\\213", "\\214", "\\215", "\\216", "\\217",
    "\220",  "\221",  "\222",  "\223",  "\224",  "\225",  "\226",  "\227",
    "\230",  "\\231", "\232",  "\233",  "\\234", "\235",  "\236",  "\237",
    "\240",  "\241",  "\242",  "\243",  "\244",  "\245",  "\246",  "\247",
    "\250",  "\251",  "\252",  "\253",  "\254",  "\255",  "\256",  "\257",
    "\260",  "\261",  "\262",  "\263",  "\264",  "\265",  "\266",  "\267",
    "\270",  "\271",  "\272",  "\273",  "\274",  "\275",  "\276",  "\277",
    "\300",  "\301",  "\302",  "\303",  "\304",  "\305",  "\306",  "\307",
    "\310",  "\311",  "\312",  "\313",  "\314",  "\315",  "\316",  "\317",
    "\320",  "\321",  "\322",  "\323",  "\324",  "\325",  "\326",  "\327",
    "\330",  "\331",  "\332",  "\333",  "\334",  "\335",  "\336",  "\337",
    "\340",  "\341",  "\342",  "\343",  "\344",  "\345",  "\346",  "\347",
    "\350",  "\351",  "\352",  "\353",  "\354",  "\355",  "\356",  "\357",
    "\360",  "\361",  "\362",  "\363",  "\364",  "\365",  "\366",  "\367",
    "\370",  "\371",  "\372",  "\373",  "\374",  "\375",  "\376",  "\377"
#else
If you are trying to compile this you have the wrong CHAR_OUT value!
#endif
#endif
};

/*@::PrintWord()@*************************************************************/
/*                                                                           */
/*  PrintWord(x, hpos, vpos)                                                 */
/*                                                                           */
/*  Print word x; its marks cross at the point (hpos, vpos).                 */
/*                                                                           */
/*****************************************************************************/

PrintWord(x, hpos, vpos)
OBJECT x;  int hpos, vpos;
{ FULL_CHAR *p;

  debug4(DGP, DD, "PrintWord( %s, %d, %d ) font %d", string(x),
	hpos, vpos, word_font(x));

  /* if font is different to previous word then print change */
  if( word_font(x) != currentfont )
  { currentfont = word_font(x);
    currentxheight2 = FontHalfXHeight(currentfont);
    fprintf(out_fp, "\n%hd %s\n", FontSize(currentfont, x), FontName(currentfont));
  }

  /* move to coordinate of x */
  debug1(DGP, DDD, "  currentxheight2 = %d", currentxheight2);
  vpos = vpos - currentxheight2;
  if( cpexists && currenty == vpos )
  { printnum(hpos, out_fp);
    fputs(" x", out_fp);
  }
  else
  { currenty = vpos;
    printnum(hpos, out_fp);
    fputs(" ", out_fp);
    printnum(currenty, out_fp);
    fputs(" moveto", out_fp);
    cpexists = TRUE;
  }

  /* show string(x) */
  fputs("(", out_fp);
  for( p = string(x);  *p;  p++ )  fputs(EightBitToPrintForm[*p], out_fp);
  if( ++wordcount >= 5 )
  { fputs(")s\n", out_fp);  wordcount = 0;
  }
  else fputs(")s ", out_fp);

  debug0(DGP, DDD, "PrintWord returning");
} /* end PrintWord */


/*@::PrintClose(), CoordTranslate()@******************************************/
/*                                                                           */
/*  PrintClose()                                                             */
/*                                                                           */
/*  Clean up this module and close output stream.                            */
/*                                                                           */
/*****************************************************************************/

PrintClose()
{ OBJECT x, link;  BOOLEAN first_need;
  if( prologue_done )
  { fprintf(out_fp, "\npgsave restore\nshowpage\n");
    fprintf(out_fp, "%%%%Trailer\n");

    /* print document fonts line */
    /* *** obsolete DSC 1.0 version
    fprintf(out_fp, "%%%%DocumentFonts:");
    for( link = Down(font_root); link != font_root; link = NextDown(link) )
    { OBJECT flink, family, face;
      Child(family, link);
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
    first_need = FontNeeded(out_fp);

    /* print resource requirements (DSC 3.0 version) - included EPSFs  */
    for( link = Down(needs); link != needs; link = NextDown(link) )
    { Child(x, link);
      assert(is_word(type(x)), "PrintClose: needs!" );
      fprintf(out_fp, "%s %s",
	first_need ? "%%DocumentNeededResources:" : "%%+", string(x));
      first_need = FALSE;
    }

    fprintf(out_fp, "%%%%Pages: %d\n", pagecount);
    fprintf(out_fp, "%%%%EOF\n");
  }
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
  cpexists = FALSE;  currentfont = NO_FONT;
  debug0(DRS, D, "CoordTranslate returning.");
} /* end CoordTranslate */

/*@::CoordRotate(), CoordScale(), SaveGraphicsState(), etc.@******************/
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
{ char buff[20];
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


/*@::PrintGraphicObject(), DefineGraphicNames()@******************************/
/*                                                                           */
/*  PrintGraphicObject(x)                                                    */
/*                                                                           */
/*  Print object x on out_fp                                                 */
/*                                                                           */
/*****************************************************************************/

PrintGraphicObject(x)
OBJECT x;
{ OBJECT y, link;
  switch( type(x) )
  {
    case WORD:
    case QWORD:
    
      StringFPuts(string(x), out_fp);
      break;
	

    case ACAT:
    
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	if( type(y) == GAP_OBJ )
	{ if( vspace(y) > 0 )  fputs("\n", out_fp);
	  else if( hspace(y) > 0 ) fputs(" ", out_fp);
	}
	else if( is_word(type(y)) || type(y) == ACAT )  PrintGraphicObject(y);
	else if( type(y) != WIDE && !is_index(type(y)) )
		/* @Wide, indexes are sometimes inserted by Manifest */
	{ Error(WARN, &fpos(x), "error in left parameter of %s", KW_GRAPHIC);
	  debug1(DGP, D, "  type(y) = %s, y =", Image(type(y)));
	  ifdebug(DGP, D, DebugObject(y));
	}
      }
      break;


    default:
    
      Error(WARN, &fpos(x), "error in left parameter of %s", KW_GRAPHIC);
      debug1(DGP, D, "  type(x) = %s, x =", Image(type(x)));
      ifdebug(DGP, D, DebugObject(x));
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
{ assert( type(x) == GRAPHIC, "PrintGraphic: type(x) != GRAPHIC!" );
  debug1(DRS, D, "DefineGraphicNames( %s )", EchoObject(x));
  debug1(DRS, DD, "  style = %s", EchoStyle(&save_style(x)));

  fprintf(out_fp, "%d %d %d %d %d %d %d LoutGraphic\n",
    size(x, COL), size(x, ROW), back(x, COL), fwd(x, ROW),
    font(save_style(x)) <= 0 ? 12*PT : FontSize(font(save_style(x)), x),
    width(line_gap(save_style(x))), width(space_gap(save_style(x))));

  debug0(DRS, D, "DefineGraphicNames returning.");
} /* end DefineGraphicNames */


/*@::PrintGraphicIncldue()@***************************************************/
/*                                                                           */
/*  PrintGraphicInclude(x, colmark, rowmark)                                 */
/*                                                                           */
/*  Print graphic include file, with appropriate surrounds.  This code       */
/*  closely follows the PostScript Language Reference Manual, 2n ed.,        */
/*  pages 733-5, except we do not clip the included EPSF.                    */
/*                                                                           */
/*  Note to porters: Version 3.0 of the EPSF standard is not compatible      */
/*  with previous versions.  Thus, this output may crash your system.        */
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
FULL_CHAR *buff;
{ if( StringBeginsWith(buff, AsciiToFull("%%EOF")) )  return TRUE;
  return FALSE;
} /* end strip_out */

PrintGraphicInclude(x, colmark, rowmark)
OBJECT x; LENGTH colmark, rowmark;
{ OBJECT y, full_name;  FULL_CHAR buff[MAX_LINE];
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
  state = (StringFGets(buff, MAX_LINE, fp) == NULL) ? FINISHED : SKIPPING;
  while( state != FINISHED ) switch(state)
  {
    case SKIPPING:

      if( StringBeginsWith(buff, AsciiToFull("%%DocumentNeededResources:")) &&
	  !StringContains(buff, AsciiToFull("(atend)")) )
      { x = MakeWord(WORD, &buff[StringLength("%%DocumentNeededResources:")],
	      no_fpos);
        Link(needs, x);
	state = (StringFGets(buff,MAX_LINE,fp)==NULL) ? FINISHED : READING_DNR;
      }
      else
      { if( StringBeginsWith(buff, AsciiToFull("%%LanguageLevel:")) )
	  Error(WARN, &fpos(x), "ignoring \"%%%%LanguageLevel\" in %s file %s",
		KW_INCGRAPHIC, string(full_name));
	if( StringBeginsWith(buff, AsciiToFull("%%Extensions:")) )
	  Error(WARN, &fpos(x), "ignoring \"%%%%Extensions\" in %s file %s",
		KW_INCGRAPHIC, string(full_name));
	if( !strip_out(buff) )  StringFPuts(buff, out_fp);
	state = (StringFGets(buff, MAX_LINE, fp) == NULL) ? FINISHED : SKIPPING;
      }
      break;

    case READING_DNR:

      if( StringBeginsWith(buff, AsciiToFull("%%+")) )
      {	x = MakeWord(WORD, &buff[StringLength(AsciiToFull("%%+"))], no_fpos);
	Link(needs, x);
	state = (StringFGets(buff,MAX_LINE,fp)==NULL) ? FINISHED : READING_DNR;
      }
      else
      { if( !strip_out(buff) )  StringFPuts(buff, out_fp);
	state = (StringFGets(buff, MAX_LINE, fp) == NULL) ? FINISHED : SKIPPING;
      }
      break;
  }

  /* wrapup */
  DisposeObject(full_name);
  fclose(fp);
  fprintf(out_fp, "%%%%EndDocument\nEndEPSF\n");
  debug0(DRS, D, "PrintGraphicInclude returning.");
} /* end PrintGraphicInclude */
