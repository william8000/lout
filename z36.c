/*@z36.c:Hyphenation: Hyphenate()@********************************************/
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
/*  FILE:         z36.c                                                      */
/*  MODULE:       Hyphenation                                                */
/*  EXTERNS:      Hyphenate()                                                */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define MAX_CHAR	256		/* max chars represented in one char */
#define TRIE_MAGIC	5361534
#define KILL_CLASS	0		/* characters which prevent hyphen'n */
#define PUNCT_CLASS	1		/* characters which delimit hyphen'n */

typedef struct trie_rec
{ int	magic;				/* a magic number to make sure ok    */
  int	class_count;			/* the number of character classes   */
  unsigned char	class[MAX_CHAR];	/* the character classes             */
  short	*node_mem;			/* the node memory                   */
  int	node_lim;			/* top of node memory                */
  int	node_free;			/* first free space in node memory   */
  unsigned char	*string_mem;		/* the string memory                 */
  int	string_lim;			/* top of string memory              */
  int	string_first;			/* the first (last inserted) string  */
} *TRIE;


#ifdef DEBUG_ON
/*****************************************************************************/
/*                                                                           */
/*  findrep(i, T)     Returns one character whose class in T is i.           */
/*                                                                           */
/*****************************************************************************/

static unsigned char findrep(i, T)
int i;  TRIE T;
{ int ch;
  for( ch = 0;  ch < MAX_CHAR;  ch++ )  if( T->class[ch] == i )  return ch;
  Error(INTERN, no_fpos, "hyph DoTriePrint: findrep failed");
} /* end findrep */
#endif


/*****************************************************************************/
/*                                                                           */
/*  TRIE T                                                                   */
/*                                                                           */
/*  The packed hyphenation table, or NULL if not yet read in.                */
/*                                                                           */
/*****************************************************************************/

static TRIE	T = (TRIE) NULL;	/* the compressed hyphenation table  */

/*@@**************************************************************************/
/*                                                                           */
/*  TRIE NewTrie(node_lim, string_lim)                                       */
/*                                                                           */
/*  Initialize a new trie with the given amount of space for nodes and       */
/*  strings.                                                                 */
/*                                                                           */
/*****************************************************************************/

static TRIE NewTrie(node_lim, string_lim)
int node_lim, string_lim;
{ TRIE T;  int i;  char *malloc();
  debug2(DHY, D, "NewTrie(%d, %d)", node_lim, string_lim);
  T = (TRIE) malloc( sizeof(struct trie_rec)
		     + node_lim*sizeof(short) + string_lim*sizeof(char));
  T->magic = TRIE_MAGIC;  T->class_count = 1;
  for( i = 0;  i < MAX_CHAR;  i++ )  T->class[i] = 0;
  T->node_mem = (short *) ( (char *) T + sizeof(struct trie_rec));
  T->node_lim = node_lim;  T->node_free = 0;
  T->string_mem = (unsigned char *) &(T->node_mem[node_lim]);
  T->string_lim = T->string_first = string_lim;
  debug0(DHY, D, "NewTrie returning.");
  return T;
} /* end NewTrie */


/*****************************************************************************/
/*                                                                           */
/*  ClassConvert(in, out, T)                                                 */
/*                                                                           */
/*  Set out[i] to the character class of in[i] in T, for all i.              */
/*                                                                           */
/*****************************************************************************/

#define ClassConvert(in, out, T)					\
{ int i;								\
  for( i = 0;  in[i] != '\0';  i++ )					\
    if( T->class[in[i]] != 0 )  out[i] = T->class[in[i]];		\
    else Error(INTERN, no_fpos, "hyph: \"%s\" has illegal class", in);	\
  out[i] = '\0';							\
} /* end ClassConvert */


/*****************************************************************************/
/*                                                                           */
/*  short NewTrieString(str, T)                                              */
/*                                                                           */
/*  Copy a new string into T, and return its offset in string_mem;           */
/*                                                                           */
/*****************************************************************************/

static short NewTrieString(str, T)
unsigned char *str;  TRIE T;
{ int i;  short res = T->string_first - strlen(str) - 1;
  if( res < 0 )  Error(INTERN, no_fpos, "hyph: trie string limit exceeded");
  T->string_first = res;  strcpy(&(T->string_mem[res]), str);
  return res;
} /* end NewTrieString */


/*****************************************************************************/
/*                                                                           */
/*  int NewTrieNode(T)                                                       */
/*                                                                           */
/*  Allocate a new empty trie node in T, and return its offset in node_mem.  */
/*                                                                           */
/*****************************************************************************/

static int NewTrieNode(T)
TRIE T;
{ int i;  int res;
  if( T->node_free + T->class_count > T->node_lim )
    Error(INTERN, no_fpos, "hyph: trie node limit exceeded");
  res = T->node_free;  T->node_free += T->class_count;
  for( i = res;  i < T->node_free;  i++ )  T->node_mem[i] = 0;
  return res;
} /* end NewTrieNode */


/*@@**************************************************************************/
/*                                                                           */
/*  AddClassToTrie(str, T)                                                   */
/*                                                                           */
/*  Add a new character class, whose members are the characters of str, to   */
/*  trie T.  This cannot occur after the first insertion.                    */
/*                                                                           */
/*****************************************************************************/

static AddClassToTrie(str, T)
unsigned char *str; TRIE T;
{ int i;
  if( T->string_first != T-> string_lim )
    Error(INTERN, no_fpos, "hyph AddClassToTrie after first insertion!");
  for( i = 0;  str[i] != '\0';  i++ )
    if( T->class[str[i]] == 0 ) T->class[str[i]] = T->class_count;
    else Error(INTERN,no_fpos, "hyph: class of %c may not be changed!", str[i]);
  T->class_count++;
} /* end AddClassToTrie */


/*****************************************************************************/
/*                                                                           */
/*  TrieInsert(key, value, T)                                                */
/*                                                                           */
/*  Insert a new key and value into trie T.                                  */
/*                                                                           */
/*****************************************************************************/

TrieInsert(key, value, T)
unsigned char *key, *value;  TRIE T;
{ unsigned char str[MAX_LINE];  int i, curr_node, next_node, pos, ch;
  debug2(DHY, D, "TrieInsert(%s, %s, T)", key, value);

  /* if first insertion, add one node after making sure class_count is even */
  if( T->node_free == 0 )
  { T->class_count = 2 * ceiling(T->class_count, 2);
    ch = NewTrieNode(T);
  }

  /* invariant: curr_node is an existing node of T with prefix str[0..i-1] */
  ClassConvert(key, str, T);
  curr_node = i = 0;
  for(;;)
  {
    /* if str is ended, add value only to string memory */
    if( str[i] == '\0' )
    { if( T->node_mem[curr_node] != 0 )
	Error(INTERN, no_fpos, "hyph string %s already inserted", key);
      else T->node_mem[curr_node] = - NewTrieString(value, T);
      debug0(DHY, D, "TrieInsert returning (empty suffix).");
      return;
    }

    /* if next position is unoccupied, store remainder of str and value */
    next_node = T->node_mem[curr_node + str[i]];
    if( next_node == 0 )
    { ch = NewTrieString(value, T);
      T->node_mem[curr_node + str[i]] = - NewTrieString(&str[i+1], T);
      debug0(DHY, D, "TrieInsert returning (non-empty suffix).");
      return;
    }

    /* if next position is occupied by a non-empty string, move that */
    /* string down one level and replace it by a trie node           */
    if( next_node < 0 )
    { pos = - next_node;
      ch = T->string_mem[pos];
      if( T->string_first == pos )  T->string_first++;
      T->node_mem[curr_node + str[i]] = next_node = NewTrieNode(T)/2;
      T->node_mem[2*next_node + ch] = -(pos+1);
    }

    /* now next is the offset of the next node to be searched */
    curr_node = 2*next_node;  i++;
  }
} /* end TrieInsert */


/*@@**************************************************************************/
/*                                                                           */
/*  BeGetChar(fp, pv)                                                        */
/*  BePutChar(fp, v)                                                         */
/*  BeGetShort(fp, pv)                                                       */
/*  BePutShort(fp, v)                                                        */
/*  BeGetInt(fp, pv)                                                         */
/*  BePutInt(fp, v)                                                          */
/*                                                                           */
/*  Get char, short, or int pv from file fp, and put char, short, or int     */
/*  onto file fp.  These routines are designed so that the file can be       */
/*  written or read safely by big-endian and little-endian architectures;    */
/*  this is accomplished by reading and writing one byte at a time to and    */
/*  from a big-endian format file.  All return 0 on success, -1 on failure.  */
/*  Thanks to David W. Sanderson for this code.                              */
/*                                                                           */
/*****************************************************************************/

#define BeGetChar(fp, pv)  ( (c = getc(fp)) == EOF ? -1 : (*pv = c & 0xFF, 0) )
#define BePutChar(fp, v)   ( putc(v & 0xFF, fp), 0 )

#define BeGetShort(fp, pv)						\
(  (c = getc(fp)) == EOF ? -1 :						\
   (  *pv = (c & 0xFF) << 8,						\
      (c = getc(fp)) == EOF ? -1 : (*pv |= c & 0xFF, 0)			\
   )									\
)

#define BePutShort(fp, v)						\
( putc((v >> 8) & 0xFF, fp), putc(v & 0xFF, fp), 0 )

int BeGetInt(fp, pv)
FILE *fp; int *pv;
{ int c;
  if ((c = getc(fp)) == EOF) return -1;
  *pv = (c & 0xFF) << 24;
  if ((c = getc(fp)) == EOF) return -1;
  *pv |= (c & 0xFF) << 16;
  if ((c = getc(fp)) == EOF) return -1;
  *pv |= (c & 0xFF) << 8;
  if ((c = getc(fp)) == EOF) return -1;
  *pv |= c & 0xFF;
  return 0;
}

int BePutInt(fp, v)
FILE *fp; int v;
{
  putc((v >> 24) & 0xFF, fp);
  putc((v >> 16) & 0xFF, fp);
  putc((v >> 8) & 0xFF, fp);
  putc(v & 0xFF, fp);
  return 0;
}


/*@@**************************************************************************/
/*                                                                           */
/*  TRIE TrieRead()                                                          */
/*                                                                           */
/*  Read in a packed trie if possible, otherwise pack an unpacked one.       */
/*                                                                           */
/*****************************************************************************/

TRIE TrieRead()
{ TRIE T;  FILE_NUM unpacked_fnum, packed_fnum;
  FILE *unpacked_fp, *packed_fp;  int len, prev, i, j, c;
  char *malloc();
  debug0(DHY, D, "TrieRead()");

  /* open file, using name stored in file handler */
  packed_fnum = FirstFile(HYPH_PACKED_FILE);
  assert( packed_fnum != NO_FILE, "TrieRead: packed_fnum!" );
  packed_fp = OpenFile(packed_fnum, FALSE);
  if( packed_fp == NULL )
  {
    /* no packed file, so open unpacked one instead */
    unsigned char str[MAX_LINE], key[MAX_LINE], value[MAX_LINE],
		  buff[MAX_LINE+10];
    unpacked_fnum = FirstFile(HYPH_FILE);
    assert( unpacked_fnum != NO_FILE, "TrieRead: unpacked unpacked_fnum!" );
    unpacked_fp = OpenFile(unpacked_fnum, FALSE);
    if( unpacked_fp == NULL )
    { Error(WARN, no_fpos, "cannot open hyphenation file %s",
	FileName(unpacked_fnum));
      return (TRIE) NULL;
    }

    /* read in unpacked hyphenation trie from unpacked_fp and compress it */
    T = NewTrie(60000, 32767);
    while( fgets(str, MAX_LINE, unpacked_fp) != NULL && str[0] != '\n' )
    { str[strlen(str)-1] = '\0';
      debug1(DHY, D, "adding class %s", str);
      AddClassToTrie(str, T);
    }
    while( fgets(str, MAX_LINE, unpacked_fp) != NULL && str[0] != '\n' )
    { prev = '0'; j = 0;
      for( i = 0;  str[i] != '\n' && str[i] != '\0';  i++ )
      { if( str[i] >= '0' && str[i] <= '9' )  prev = str[i];
        else key[j] = str[i], value[j++] = prev, prev = '0';
      }
      key[j] = '\0';  value[j] = prev;  value[j+1] = '\0';
      TrieInsert(key, value, T);
    }
    fclose(unpacked_fp);
    len = CompressTrie(T);

    /* write the compressed trie out to the packed file */
    strcpy(buff, FileName(unpacked_fnum));
    strcat(buff, HYPH_SUFFIX);
    packed_fp = fopen(buff, "w");
    if( packed_fp == NULL )  Error(FATAL, no_fpos,
      "cannot write to hyphenation file %s", buff);
    BePutInt(packed_fp, T->magic);
    BePutInt(packed_fp, T->class_count);
    for( i = 0; i < MAX_CHAR; i++ )  BePutChar(packed_fp, T->class[i]);
    BePutInt(packed_fp, 0);  /* placeholder for node_mem */
    BePutInt(packed_fp, T->node_lim);
    BePutInt(packed_fp, T->node_free);
    BePutInt(packed_fp, 0);  /* placeholder for string_mem */
    BePutInt(packed_fp, T->string_lim);
    BePutInt(packed_fp, T->string_first);
    for( i = 0; i < T->node_free; i++ )  BePutShort(packed_fp, T->node_mem[i]);
    for( i = 0; i < T->string_lim; i++)  BePutChar(packed_fp, T->string_mem[i]);
    fclose(packed_fp);
    /***OLD*VERSION*********************************************************
    if( fwrite( (char *) T, len, 1, packed_fp) != 1 )  Error(FATAL, no_fpos,
      "error on write to hyphenation file %s", buff);
    ***********************************************************************/

    /* now try again to open packed_fnum, the file just written */
    packed_fp = OpenFile(packed_fnum, FALSE);
    if( packed_fp == NULL )  Error(FATAL, no_fpos,
      "cannot open hyphenation file %s", FileName(packed_fnum));
  }

  /* now packed hyphenation file is open, read it in */
  fseek(packed_fp, 0L, 2);  len = (int) ftell(packed_fp);  rewind(packed_fp);
  T = (TRIE) malloc(len);
  /***OLD*VERSION**********************************************************
  if( fread( (char *) T, len, 1, packed_fp) != 1 )  Error(FATAL, no_fpos,
      "error on read of hyphenation file %s", FileName(packed_fnum));
  ************************************************************************/
  if( BeGetInt(packed_fp, &T->magic) != 0 )  Error(FATAL, no_fpos,
      "error on read from packed hyphenation file %s", FileName(packed_fnum));
  if( T->magic != TRIE_MAGIC )  Error(FATAL, no_fpos,
      "bad magic number in hyphenation file %s", FileName(packed_fnum));
  BeGetInt(packed_fp, &T->class_count);
  for( i = 0; i < MAX_CHAR; i++ )  BeGetChar(packed_fp, &T->class[i]);
  BeGetInt(packed_fp, &i);  /* placeholder for node_mem */
  BeGetInt(packed_fp, &T->node_lim);
  BeGetInt(packed_fp, &T->node_free);
  BeGetInt(packed_fp, &i);  /* placeholder for string_mem */
  BeGetInt(packed_fp, &T->string_lim);
  BeGetInt(packed_fp, &T->string_first);
  T->node_mem = (short *) ( (char *) T + sizeof(struct trie_rec) );
  T->string_mem = (unsigned char *) &(T->node_mem[T->node_lim]);
  for( i = 0; i < T->node_free; i++ )  BeGetShort(packed_fp, &T->node_mem[i]);
  for( i = 0; i < T->string_lim; i++ ) BeGetChar(packed_fp, &T->string_mem[i]);

  /* debug and exit */
  debug0(DHY, D, "TrieRead returning, T =");
  ifdebug(DHY, DD, TriePrint(T, stderr));
  return T;
} /* end TrieRead */


/*@@**************************************************************************/
/*                                                                           */
/*  int CompressTrie(T)                                                      */
/*                                                                           */
/*  Compress trie T and return its length in characters.                     */
/*                                                                           */
/*****************************************************************************/

int CompressTrie(T)
TRIE T;
{ unsigned char *p, *q;  int len, i;
  debug0(DHY, D, "CompressTrie(T), T =");
  ifdebug(DHY, DD, TriePrint(T, stderr));
  T->node_lim = T->node_free;
  for( i = 0;  i < T->node_lim;  i++ )
    if( T->node_mem[i] < 0 )
      T->node_mem[i] = - ( -T->node_mem[i] - T->string_first);
  p = (unsigned char *) &(T->node_mem[T->node_free]);
  q = &(T->string_mem[T->string_first]);
  len = T->string_lim - T->string_first;
  for( i = 0;  i < len;  i++ )  *p++ = *q++;
  T->string_mem = (unsigned char *) &(T->node_mem[T->node_lim]);
  T->string_first = 0;
  T->string_lim = len;
  len = sizeof(struct trie_rec) + T->node_lim * sizeof(short)
				+ T->string_lim * sizeof(unsigned char);
  debug1(DHY, D, "CompressTrie returning %d, T =", len);
  ifdebug(DHY, DD, TriePrint(T, stderr));
  return len;
} /* end CompressTrie */


/*****************************************************************************/
/*                                                                           */
/*  AccumulateRating(x, y)                                                   */
/*                                                                           */
/*  Accumulate the hyphenation rating string x into y.                       */
/*                                                                           */
/*****************************************************************************/

#define AccumulateRating(x, y)						\
{ unsigned char *p = x, *q = y;							\
  while( *p )								\
  { if( *p > *q )  *q = *p;						\
    p++, q++;								\
  }									\
} /* end AccumulateRating */


/*@@**************************************************************************/
/*                                                                           */
/*  OBJECT Hyphenate(x)                                                      */
/*                                                                           */
/*  Hyphenate ACAT object x, returning the hyphenated result.                */
/*                                                                           */
/*****************************************************************************/

OBJECT Hyphenate(x)
OBJECT x;
{ OBJECT link, y, z, next_link;
  unsigned char str[MAX_LINE+2], rate[MAX_LINE+3],
		*class, *key, *ss, *s, *rem, *p, *q;
  int start, stop, i, j, curr_node, next_node, pos;
  BOOLEAN hyphenated;  static ShowRate();
  static BOOLEAN tried_file = FALSE;
  assert( type(x) == ACAT, "Hyphenate: type(x) != ACAT!" );
  debug1(DHY, DD, "Hyphenate(%s)", EchoObject(null, x));

  /* if no trie is present, try to get it from a file */
  if( T == (TRIE) NULL )
  { if( !tried_file )  T = TrieRead();
    tried_file = TRUE;
    if( T == (TRIE) NULL )
    { debug0(DHY, DD, "Hyphenate returning (no trie).");
      return x;
    }
  }

  /* for each word y of x, try to hyphenate it */
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link);
    if( type(y) != WORD )  continue;
    debug1(DHY, DD, "Hyphenate() examining %s", EchoObject(null, y));

    /* start := index of y's first letter, stop := index following last */
    key = string(y);  class = T->class;
    for( start = 0;  class[key[start]] == PUNCT_CLASS;  start++ );
    for( stop = start;  class[key[stop]] > PUNCT_CLASS;  stop++ );

    /* if a - ended the run, hyphenate there only */
    if( key[stop] == '-' )
    { next_link = NextDown(link);
      z = MakeWord(&key[stop+1], &fpos(y));
      word_font(z) = word_font(y);
      FontAtomSize(z);
      Link(NextDown(link), z);
      z = New(GAP_OBJ);
      SetGap(gap(z), FALSE, TRUE, FIXED_UNIT, HYPH_MODE, 0);
      Link(NextDown(link), z);
      Link(z, MakeWord("0ch", &fpos(y)));
      key[stop + 1] = '\0';
      FontAtomSize(y);
      link = PrevDown(next_link);
      continue;
    }

    /* don't hyphenate if less than 5 letters, or a kill char is nearby */
    if( stop - start < 5 )  continue;
    if( key[stop] != '\0' && class[key[stop]] == KILL_CLASS )  continue;

    /* let str[] be the converted substring, let rate[] be all '0' */
    str[0] = PUNCT_CLASS;  rate[0] = '0';
    for( i = 0;  i < stop - start;  i++ )
    { str[i+1] = class[key[start + i]];
      rate[i+1] = '0';
    }
    str[i+1] = PUNCT_CLASS;  rate[i+1] = '0';
    str[i+2] = '\0';  rate[i+2] = '0';
    rate[i+3] = '\0';
    ifdebug(DHY, DD, ShowRate(key, start, stop, rate, stderr));

    /* for each suffix of str[], accumulate patterns matching its prefixes */
    ss = str;
    do
    {
      ifdebug(DHY, DD,
	fprintf(stderr, "trying suffix \"");
	for( p = ss; *p != 0;  p++ )  fprintf(stderr, "%c", findrep(*p, T));
	fprintf(stderr, "\"\n");
      );
	
      /* accumulate all prefixes of ss */
      curr_node = 0;  s = ss;
      for(;;)
      {
	/* if curr_node has empty string, that is one prefix */
	pos = T->node_mem[curr_node];
	if( pos < 0 )
	{ AccumulateRating(&T->string_mem[- pos], rate+(ss-str));
	  debug1(DHY, DD, " found %s", &(T->string_mem[- pos]));
	}

	/* if ss is finished, no other prefixes are possible */
	if( *s == '\0' )  break;

	/* determine next_node and break if empty */
	next_node = T->node_mem[curr_node + *s];
	if( next_node == 0 )  break;

	/* if next_node is a string, check whether it is a prefix of ss */
	if( next_node < 0 )
	{ rem = &(T->string_mem[-next_node]);
	  do
	  { if( *rem == '\0' )
	    { AccumulateRating(rem+1, rate+(ss-str));
	      debug1(DHY, DD, " found %s", rem+1);
	      break;
	    }
	  } while( *++s == *rem++ );
	  break;
	}

	/* otherwise go on to the next trie node */
	curr_node = 2*next_node;  s++;
      }
    } while( *(++ss + 2) != PUNCT_CLASS );
    ifdebug(DHY, DD, ShowRate(key, start, stop, rate, stderr));

    /* now rate[] has accumulated ratings; use it to perform hyphenations */
    hyphenated = FALSE;
    /* hyphenate after any concluding - */
    /* *** now doing this at start only (see above)
    if( key[stop] == '-' )
    {
      z = MakeWord(&key[stop+1], &fpos(y));
      word_font(z) = word_font(y);
      FontAtomSize(z);
      Link(NextDown(link), z);
      z = New(GAP_OBJ);
      SetGap(gap(z), FALSE, TRUE, FIXED_UNIT, HYPH_MODE, 0);
      Link(NextDown(link), z);
      Link(z, MakeWord("0ch", &fpos(y)));
      key[stop + 1] = '\0';
      hyphenated = TRUE;
    }
    *** */
    next_link = NextDown(link);
    for( i = stop - start - 1;  i >= 3;  i-- )
    {
      /* hyphenate at i if rate[i] is odd */
      if( is_odd(rate[i]) )
      {	z = MakeWord(&key[start+i-1], &fpos(y));
	word_font(z) = word_font(y);
	FontAtomSize(z);
	Link(NextDown(link), z);
	z = New(GAP_OBJ);
	SetGap(gap(z), FALSE, TRUE, FIXED_UNIT, HYPH_MODE, 0);
	Link(NextDown(link), z);
	Link(z, MakeWord("0ch", &fpos(y)));
	key[start + i - 1] = '\0';
	hyphenated = TRUE;
      }
    }
    if( hyphenated )
    { FontAtomSize(y);
      link = PrevDown(next_link);
    }

  } /* end for each word */

  debug1(DHY, DD, "Hyphenate returning %s", EchoObject(null, x));
  return x;
} /* end Hyphenate */


/*@@**************************************************************************/
/*                                                                           */
/*  unsigned char *TrieRetrieve(key, T)                                      */
/*                                                                           */
/*  Retrieve the value associated with key in T, or NULL if not present.     */
/*                                                                           */
/*****************************************************************************/
#if DEBUG_ON

unsigned char *TrieRetrieve(key, T)
unsigned char *key;  TRIE T;
{ unsigned char str[MAX_LINE];  int i, curr_node, next_node, pos;
  debug1(DHY, DD, "TrieRetrieve(%s, T)", key);
  ClassConvert(key, str, T);

  /* invariant: curr_node is an existing node of T with prefix str[0..i-1] */
  curr_node = i = 0;
  for(;;)
  {
    /* if next_node is 0, the string was never inserted */
    next_node = T->node_mem[curr_node + str[i]];
    if( next_node == 0 )  return (unsigned char *) NULL;

    /* if next_node < 0 it represents an offset into the string memory */
    if( next_node < 0 )
    { pos = - next_node;
      if( str[i] != '\0' )
      {	do
	{ if( str[++i] != T->string_mem[pos++] )  return (unsigned char *) NULL;
	} while( str[i] != '\0' );
      }
      return &(T->string_mem[pos]);
    }

    /* otherwise next_node is the trie node to be searched next */
    curr_node = 2*next_node;  i++;
  }
} /* end TrieRetrieve */


/*****************************************************************************/
/*                                                                           */
/*  ShowRate(key, start, stop, rate, fp)                                     */
/*                                                                           */
/*  Debug print of key[] and rate[] on file fp.                              */
/*                                                                           */
/*****************************************************************************/

static ShowRate(key, start, stop, rate, fp)
unsigned char *key;  int start, stop;  unsigned char *rate;  FILE *fp;
{ int i;
  fprintf(fp, "key:    ");
  for( i = start;  i < stop;  i++ )  fprintf(fp, " %c", key[i]);
  fprintf(fp, "\nrate:");
  for( i = 0;  rate[i] != '\0';  i++ )  fprintf(fp, " %c", rate[i]);
  fprintf(fp, "\n");
} /* end ShowRate */


/*@@**************************************************************************/
/*                                                                           */
/*  DoTriePrint(T, node, len, fp)                                            */
/*                                                                           */
/*  Print on file fp the subset of the entries of trie T stored in node and  */
/*  its descendants.  The node has prefix prefix[0..len-1].                  */
/*                                                                           */
/*****************************************************************************/

static unsigned char prefix[MAX_LINE];

static DoTriePrint(T, node, len, fp)
TRIE T; int node, len; FILE *fp;
{ int i, next_node, pos;
  for( i = 0;  i < T->class_count;  i++ )
  {
    /* if next_node < 0, have string to print */
    next_node = T->node_mem[node + i];
    if( next_node < 0 )
    {
      prefix[len] = '\0';
      fprintf(fp, "%s", prefix);
      pos = - next_node;
      if( i != 0 )
      {
	fprintf(fp, "%c", findrep(i, T));
	while( T->string_mem[pos] != '\0' )
	{ fprintf(fp, "%c", findrep(T->string_mem[pos], T));
	  pos++;
	}
	pos++;
      }
      fprintf(fp, " %s\n", &(T->string_mem[pos]));
    }

    /* else if next_node > 0 have a child node to explore */
    else if( next_node > 0 )
    { assert( i > 0, "DoTriePrint: i == 0!" );
      prefix[len] = findrep(i, T);
      prefix[len+1] = '\0';
      DoTriePrint(T, 2*next_node, len+1, fp);
    }
  }
} /* end DoTriePrint */


/*****************************************************************************/
/*                                                                           */
/*  TriePrint(T, fp)                                                         */
/*                                                                           */
/*  Print trie T on file fp.                                                 */
/*                                                                           */
/*****************************************************************************/

TriePrint(T, fp)
TRIE T;  FILE *fp;
{ int i, j, ch;
  assert( T-> magic == TRIE_MAGIC, "TriePrint: magic!" );
  fprintf(fp, "Classes:");
  for( i = 1;  i < T->class_count;  i++ )
  { fprintf(fp, " ");
    for( ch = 0;  ch < MAX_CHAR;  ch++ )
      if( T->class[ch] == i )  fprintf(fp, "%c", ch);
  }
  fprintf(fp, "\n");
  fprintf(fp, "Node space: %d capacity, %d used\n", T->node_lim, T->node_free);
  fprintf(fp, "String space: %d capacity, %d used\n", T->string_lim,
	T->string_lim - T->string_first);
  prefix[0] = '\0';
  DoTriePrint(T, 0, 0, fp);
} /* end TriePrint */
#endif
