/*@z07.c:Character Mapping:LoadMapping(), SmallCaps()@************************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.06)                       */
/*  COPYRIGHT (C) 1994 Jeffrey H. Kingston                                   */
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
/*  FILE:         z45.c                                                      */
/*  MODULE:       Character Mapping                                          */
/*  EXTERNS:      LoadMapping(), SmallCaps()                                 */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define MAX_MAP		 20		/* max number of encoding vectors    */
#define MAX_CHAR	256		/* max chars represented in one char */
#define	MAX_HASH	353		/* size of hash table                */

typedef struct mapvec {
  OBJECT	file_name;		/* name of file containing the vec   */
  FILE_NUM	fnum;			/* the file number of this file      */
  ENCODING	enc;			/* encoding used by this mapping     */
  FULL_CHAR	mapping[MAX_CHAR];	/* mapped value                      */
  FULL_CHAR	transform[MAX_CHAR];	/* transform after mapping           */
} *MAP_VEC;

static	MAP_VEC	map_table[MAX_MAP];	/* the mappings                      */
static	int	maptop = 1;		/* first free slot in map_table[]    */
					/* save 0 for "no mapping"           */

/*@::LoadMapping()@***********************************************************/
/*                                                                           */
/*  MAPPING LoadMapping(file_name, enc)                                      */
/*                                                                           */
/*  Declare file_name to be a character mapping file.  A file may be so      */
/*  declared more than once.  The character names are from encoding enc.     */
/*                                                                           */
/*****************************************************************************/

MAPPING LoadMapping(OBJECT file_name, ENCODING enc)
{ FILE *fp;  MAP_VEC map;  MAPPING m;  int i;
  FULL_CHAR charname[MAX_BUFF], mapname[MAX_BUFF], alg[MAX_BUFF];
  FULL_CHAR charval, mapval;
  debug2(DCM, D, "LoadMapping(%s, %d)", EchoObject(file_name), enc);

  /* if the file name is "-", it means no mapping file is supplied */
  if( StringEqual(string(file_name), AsciiToFull("-")) )
  { debug1(DCM, D, "LoadMapping returning 0 (file name is %s)",
      string(file_name));
    return (MAPPING) 0;
  }

  /* find out whether we've seen this file name before */
  for( m = 1; m < maptop; m++ )
  { if( map_table[m]->enc == enc &&
        StringEqual(string(map_table[m]->file_name), string(file_name)) )
      break;
  }

  /* if seen before, just return the previously assigned mapping number */
  if( m < maptop )
  { Dispose(file_name);
    debug1(DCM, D, "LoadMapping returning %d (not new)", m);
    return m;
  }

  /* new, so allocate a new slot in map_table for this new mapping */
  if( maptop++ == MAX_MAP )
    Error(45, 1, "too many character mappings", FATAL, &fpos(file_name));
  ifdebug(DMA, D, DebugRegisterUsage(MEM_CMAPS, 1, sizeof(struct mapvec)));
  map_table[m] = map = (MAP_VEC) malloc( sizeof(struct mapvec) );
  if( map == (MAP_VEC) NULL )
    Error(45, 2, "run out of memory when loading character mapping",
      FATAL, &fpos(file_name));
  map->file_name = file_name;
  map->enc       = enc;
  for( i = 0;  i < MAX_CHAR; i++ )
  { map->mapping[i] = i;
    map->transform[i] = (FULL_CHAR) '.';
  }

  /* define and open the file */
  map->fnum = DefineFile(string(file_name), STR_EMPTY, &fpos(file_name),
    MAPPING_FILE, ENCODING_PATH);
  fp = OpenFile(map->fnum, FALSE, FALSE);
  if( fp == NULL )  Error(45, 3, "cannot open character mapping file %s",
      FATAL, PosOfFile(map->fnum), FileName(map->fnum));

  /* read triples of the form (charname, charname, algorithm) and insert */
  while( fscanf(fp, "%s %s %s", charname, mapname, alg) == 3 )
  { charval = EvRetrieve(charname, enc);
    if( charval == 0 )
      Error(45, 4, "unknown character name %s in character mapping file %s",
        FATAL, PosOfFile(map->fnum), charname, FileName(map->fnum));
    mapval = EvRetrieve(mapname, enc);
    if( mapval == 0 )
      Error(45, 5, "unknown character name %s in character mapping file %s",
	    FATAL, PosOfFile(map->fnum), mapname, FileName(map->fnum));
    if( (alg[0] != '.' && alg[0] != '-') || alg[1] != '\0' )
      Error(45, 6, "unknown algorithm %s in character mapping file %s",
	    FATAL, PosOfFile(map->fnum), alg, FileName(map->fnum));
    map->mapping[charval] = mapval;
    map->transform[charval] = alg[0];
  }
  fclose(fp);
  debug1(DCM, D, "LoadMapping returning %d (new mapping)", m);
  return m;
} /* end LoadMapping */

/*@@**************************************************************************/
/*                                                                           */
/*  OBJECT DoWord(buff, q, x, fnum)                                          */
/*                                                                           */
/*  Replace WORD or QWORD x by a small caps version, based on word_font(x).  */
/*                                                                           */
/*****************************************************************************/

static OBJECT DoWord(FULL_CHAR *buff, FULL_CHAR *q, OBJECT x, FONT_NUM fnum)
{ OBJECT res;
  *q++ = '\0';
  res = MakeWord(type(x), buff, &fpos(x));
  word_font(res) = fnum;
  word_colour(res) = word_colour(x);
  word_language(res) = word_language(x);
  word_hyph(res) = word_hyph(x);
  return res;
} /* end DoWord */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT DoVShift(x, vshift, chld)                                         */
/*                                                                           */
/*  Make an new VSHIFT object with the given shift and child.                */
/*                                                                           */
/*****************************************************************************/

static OBJECT DoVShift(OBJECT x, LENGTH vshift, OBJECT chld)
{ OBJECT res;
  res = New(VSHIFT);
  FposCopy(fpos(res), fpos(x));
  shift_type(res) = GAP_DEC;
  units(shift_gap(res)) = FIXED_UNIT;
  mode(shift_gap(res)) = EDGE_MODE;
  width(shift_gap(res)) = vshift;
  Link(res, chld);
  return res;
}

/*****************************************************************************/
/*                                                                           */
/*  void DoAddGap(new_acat)                                                  */
/*                                                                           */
/*  Add a new 0i gap object to new_acat.                                     */
/*                                                                           */
/*****************************************************************************/

static void DoAddGap(OBJECT new_acat)
{ OBJECT new_g;
  new_g = New(GAP_OBJ);
  FposCopy(fpos(new_g), fpos(new_acat));
  hspace(new_g) = vspace(new_g) = 0;
  SetGap(gap(new_g), FALSE, TRUE, FIXED_UNIT, EDGE_MODE, 0*IN);
  Link(new_acat, new_g);
}

/*@::SmallCaps()@*************************************************************/
/*                                                                           */
/*  OBJECT SmallCaps(x, style)                                               */
/*                                                                           */
/*  Replace WORD or QWORD x by a small caps version, based on word_font(x).  */
/*                                                                           */
/*****************************************************************************/
#define	INIT		0
#define	ALL_NON		1
#define	ALL_TRANS	2
#define	MIXED_NON	3
#define	MIXED_TRANS	4
#define transformable(ch)	(transform[ch] == '-')

OBJECT SmallCaps(OBJECT x, STYLE *style)
{ MAPPING m;  int i;  OBJECT new_y, new_x, new_acat, tmp;
  FULL_CHAR *mapping, *transform, buff[MAX_BUFF], *p, *q;
  FONT_NUM small_font;  LENGTH vshift;  int state;  STYLE new_style;
  static OBJECT font_change_word = nilobj;
  assert( is_word(type(x)), "SmallCaps: !is_word(type(x))" );
  debug2(DCM, D, "SmallCaps(%s %s)", Image(type(x)), string(x));

  /* get the mapping and return if there isn't one for this font */
  m = FontMapping(font_num(x));
  if( m == 0 )
  { debug0(DCM, D, "SmallCaps returning unchanged (mapping is 0)");
    return x;
  }
  assert( 1 <= m && m < maptop, "SmallCaps: mapping out of range!" );
  mapping = map_table[m]->mapping;
  transform = map_table[m]->transform;

  /* apply the mapping to each character in turn */

  /* if plain text, apply the mapping and exit */
  if( BackEnd == PLAINTEXT )
  {
    for( i = 0;  string(x)[i] != '\0';  i++ )
      string(x)[i] = mapping[string(x)[i]];
    debug1(DCM, D, "SmallCaps returning (plain text) %s", EchoObject(x));
    return x;
  }

  /* set up the font change word if not already done */
  if( font_change_word == nilobj )
  { font_change_word = MakeWord(WORD, AsciiToFull("0.7f"), no_fpos);
  }

  state = INIT;  q = buff;
  for( p = string(x);  *p != '\0';  p++ )
  {
    debug2(DCM, DD, " examining %c (%s)", *p,
      transformable(*p) ? "transformable" : "not transformable");
    switch( state )
    {
      case INIT:

        /* this state is for when we are at the first character */
        if( transformable(*p) )
        { *q++ = mapping[*p];

	  /* work out what the smaller font is going to be, and the vshift */
	  StyleCopy(new_style, *style);
	  FontChange(&new_style, font_change_word);
	  small_font = font(new_style);
	  vshift = FontHalfXHeight(word_font(x)) - FontHalfXHeight(small_font);

          state = ALL_TRANS;
        }
        else
        { *q++ = mapping[*p];
          state = ALL_NON;
        }
        break;


      case ALL_NON:

        /* in this state, all characters so far are non-transformable */
        if( transformable(*p) )
        { 
	  /* work out what the smaller font is going to be */
	  StyleCopy(new_style, *style);
	  FontChange(&new_style, font_change_word);
	  small_font = font(new_style);
	  vshift = FontHalfXHeight(word_font(x)) - FontHalfXHeight(small_font);

	  /* make a new WORD out of the current contents of buff */
	  new_y = DoWord(buff, q, x, word_font(x));

	  /* construct the skeleton of the result to replace x */
	  new_x = New(ONE_COL);
	  FposCopy(fpos(new_x), fpos(x));
	  new_acat = New(ACAT);
	  FposCopy(fpos(new_acat), fpos(x));
	  Link(new_x, new_acat);
	  Link(new_acat, new_y);
	  DoAddGap(new_acat);

	  /* start off a new buffer with *p */
	  q = buff;
	  *q++ = mapping[*p];
	  state = MIXED_TRANS;
        }
        else *q++ = mapping[*p];
        break;


      case ALL_TRANS:

        /* in this state, all characters so far are transformable */
        if( transformable(*p) ) *q++ = mapping[*p];
        else
        {
	  /* make a new @VShift WORD out of the current contents of buff */
	  tmp = DoWord(buff, q, x, small_font);
	  new_y = DoVShift(x, vshift, tmp);

	  /* construct the skeleton of the result to replace x */
	  new_x = New(ONE_COL);
	  FposCopy(fpos(new_x), fpos(x));
	  new_acat = New(ACAT);
	  FposCopy(fpos(new_acat), fpos(x));
	  Link(new_x, new_acat);
	  Link(new_acat, new_y);
	  DoAddGap(new_acat);

	  /* start off a new buffer with *p */
	  q = buff;
	  *q++ = mapping[*p];
	  state = MIXED_NON;
        }
        break;


      case MIXED_NON:

        /* in this state the previous char was non-transformable, but */
        /* there have been characters before that that were transformable */
        if( transformable(*p) )
        {
	  /* make a new WORD out of the current contents of buff */
	  new_y = DoWord(buff, q, x, word_font(x));

	  /* link the new word into the growing structure that replaces x */
	  Link(new_acat, new_y);
	  DoAddGap(new_acat);

	  /* start off a new buffer with *p */
	  q = buff;
	  *q++ = mapping[*p];
	  state = MIXED_TRANS;
        }
        else *q++ = mapping[*p];
        break;


      case MIXED_TRANS:

        /* in this state the previous char was transformable, but there */
        /* have been characters before that that were non-transformable */
        if( transformable(*p) ) *q++ = mapping[*p];
        else
        {
	  /* make a new @VShift WORD out of the current contents of buff */
	  tmp = DoWord(buff, q, x, small_font);
	  new_y = DoVShift(x, vshift, tmp);

	  /* link the new word into the growing structure that replaces x */
	  Link(new_acat, new_y);
	  DoAddGap(new_acat);

	  /* start off a new buffer with *p */
	  q = buff;
	  *q++ = mapping[*p];
	  state = MIXED_NON;
        }
        break;

    }
  }

  /* now at termination, clean up the structure */
  switch( state )
  {
    case INIT:
    case ALL_NON:

      /* original x is OK as is: either empty or all non-transformable */
      break;


    case ALL_TRANS:

      /* make a new @VShift WORD and replace x with it */
      tmp = DoWord(buff, q, x, small_font);
      new_x = DoVShift(x, vshift, tmp);
      ReplaceNode(new_x, x);
      Dispose(x);
      x = new_x;
      break;


    case MIXED_NON:

      /* make a new WORD, add to new_acat, and replace x */
      new_y = DoWord(buff, q, x, word_font(x));
      Link(new_acat, new_y);
      ReplaceNode(new_x, x);
      Dispose(x);
      x = new_x;
      break;


    case MIXED_TRANS:

      /* make a new @VShift WORD, add to new_acat, and replace x */
      tmp = DoWord(buff, q, x, small_font);
      new_y = DoVShift(x, vshift, tmp);
      Link(new_acat, new_y);
      ReplaceNode(new_x, x);
      Dispose(x);
      x = new_x;
      break;
  }
  debug1(DCM, D, "SmallCaps returning %s", EchoObject(x));
  return x;
} /* end SmallCaps */
