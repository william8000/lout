/*@z36.c:Hyphenation: Declarations@*******************************************/
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
/*  FILE:         z36.c                                                      */
/*  MODULE:       Hyphenation                                                */
/*  EXTERNS:      Hyphenate()                                                */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define	NODE_MULT	4		/* what to multiply node indexes by  */
#define MAX_CHAR	256		/* max chars represented in one char */
#define TRIE_MAGIC	5361534
#define KILL_CLASS	0		/* characters preventing hyphenation */
#define PUNCT_CLASS	1		/* characters delimiting hyphenation */

typedef struct trie_rec
{ int		magic;			/* a magic number to make sure ok    */
  int		class_count;		/* the number of character classes   */
  unsigned char	class[MAX_CHAR];	/* the character classes             */
  short		*node_mem;		/* the node memory                   */
  int		node_lim;		/* top of node memory                */
  int		node_free;		/* first free space in node memory   */
  FULL_CHAR	*string_mem;		/* the string memory                 */
  int		string_lim;		/* top of string memory              */
  int		string_first;		/* the first (last inserted) string  */
} *TRIE;


/*****************************************************************************/
/*                                                                           */
/*  static TRIE HyphTables[]                                                 */
/*                                                                           */
/*  The packed hyphenation tables, indexed by language.  An entry is NULL    */
/*  when the table for that language has not yet been read in; TriedFile     */
/*  is TRUE after we have tried to read that file, whether or not we were    */
/*  successful.                                                              */
/*                                                                           */
/*****************************************************************************/

static TRIE	HyphTables[MAX_LANGUAGE] = { NULL };
static BOOLEAN	TriedFile[MAX_LANGUAGE]  = { FALSE };


/*@::CompressValue(), UncompressValue()@**************************************/
/*                                                                           */
/*  CompressValue(p, q)                                                      */
/*                                                                           */
/*  Compress value string p, placing the result in q.                        */
/*                                                                           */
/*****************************************************************************/
#define FirstHalf(y)		( (y) >> 4 )
#define LastHalf(y)		( (y) & 15 )
#define AssignFirstHalf(x, y)	( (x) = ((y) << 4) )
#define AssignLastHalf(x, y)	( (x) |= (y) )

#define CompressValue(compressed, uncompressed)				\
/* FULL_CHAR *compressed, *uncompressed; */				\
{ register FULL_CHAR *p, *q;						\
  p = compressed;  q = uncompressed;					\
  for( ; ; )								\
  {									\
    if( *q == (FULL_CHAR) '\0' )					\
    { *p = (FULL_CHAR) '\0';						\
      break;								\
    }									\
    AssignFirstHalf(*p, *q++ - '0' + 2);				\
    if( *q == (FULL_CHAR) '\0' )					\
    { *++p = (FULL_CHAR) '\0';						\
      break;								\
    }									\
    AssignLastHalf(*p, *q++ - '0' + 2);					\
    p++;								\
  }									\
}

/*****************************************************************************/
/*                                                                           */
/*  UncompressValue(q, p)                                                    */
/*                                                                           */
/*  Uncompress value string q, placing the result in p.                      */
/*                                                                           */
/*****************************************************************************/

#define UncompressValue(compressed, uncompressed)			\
/* FULL_CHAR *compressed, *uncompressed; */				\
{ register FULL_CHAR *p, *q;						\
  p = compressed; q = uncompressed;					\
  for( ; ; )								\
  {									\
    if( FirstHalf(*p) == '\0' )  break;					\
    *q++ = FirstHalf(*p) + '0' - 2;					\
    if( LastHalf(*p) == '\0' )  break;					\
    *q++ = LastHalf(*p) + '0' - 2;					\
    p++;								\
  }									\
  *q = (FULL_CHAR) '\0';						\
}

/*****************************************************************************/
/*                                                                           */
/*  ClassConvert(in, out, T, hline_num)                                      */
/*                                                                           */
/*  Set out[i] to the character class of in[i] in T, for all i.              */
/*                                                                           */
/*****************************************************************************/

#define ClassConvert(in, out, T, hline_num)				\
{ int i;								\
  for( i = 0;  in[i] != '\0';  i++ )					\
    if( T->class[in[i]] != 0 )  out[i] = T->class[in[i]];		\
    else								\
      Error(36, 1, "in hyphenation file, line %d: character (octal %o) is not in any class",	\
	FATAL, no_fpos, hline_num, in[i]);				\
  out[i] = '\0';							\
} /* end ClassConvert */


/*@::findrep(), TrieRetrieve(), ShowRate()@***********************************/
/*                                                                           */
/*  findrep(i, T)     Returns one character whose class in T is i.           */
/*                                                                           */
/*****************************************************************************/
#if DEBUG_ON

static FULL_CHAR findrep(int i, TRIE T)
{ int ch;
  for( ch = 0;  ch < MAX_CHAR;  ch++ )
    if( T->class[ch] == i ) return (FULL_CHAR) ch;
  Error(36, 2, "DoTriePrint: findrep failed", INTERN, no_fpos);
  return (FULL_CHAR) ch;  /* never reached, but gcc doesn't know that */
} /* end findrep */


/*****************************************************************************/
/*                                                                           */
/*  static FULL_CHAR *TrieRetrieve(key, T)                                   */
/*                                                                           */
/*  Retrieve the value associated with key in T, or NULL if not present.     */
/*  This procedure is not presently in use.                                  */
/*                                                                           */
/*****************************************************************************/

static FULL_CHAR *TrieRetrieve(FULL_CHAR *key, TRIE T)
{ FULL_CHAR str[MAX_BUFF];  int i, curr_node, next_node, pos;
  debug1(DHY, DD, "TrieRetrieve(%s, T)", key);
  ClassConvert(key, str, T, 0);

  /* invariant: curr_node is an existing node of T with prefix str[0..i-1] */
  curr_node = i = 0;
  for(;;)
  {
    /* if next_node is 0, the string was never inserted */
    next_node = T->node_mem[curr_node + str[i]];
    if( next_node == 0 )  return (FULL_CHAR *) NULL;

    /* if next_node < 0 it represents an offset into the string memory */
    if( next_node < 0 )
    { pos = - next_node;
      if( str[i] != '\0' )
      {	do
	{ if( str[++i] != T->string_mem[pos++] )  return (FULL_CHAR *) NULL;
	} while( str[i] != '\0' );
      }
      return &(T->string_mem[pos]);
    }

    /* otherwise next_node is the trie node to be searched next */
    curr_node = NODE_MULT*next_node;  i++;
  }
} /* end TrieRetrieve */


/*****************************************************************************/
/*                                                                           */
/*  static ShowRate(key, start, stop, rate, fp)                              */
/*                                                                           */
/*  Debug print of key[] and rate[] on file fp.                              */
/*                                                                           */
/*****************************************************************************/

static void ShowRate(FULL_CHAR *key, int start, int stop, FULL_CHAR *rate,
FILE *fp)
{ int i;
  fprintf(fp, "key:    ");
  for( i = start;  i < stop;  i++ )  fprintf(fp, " %c", key[i]);
  fprintf(fp, "\nrate:");
  for( i = 0;  rate[i] != '\0';  i++ )  fprintf(fp, " %c", rate[i]);
  fprintf(fp, "\n");
} /* end ShowRate */


/*@::DoTriePrint(), TriePrint()@**********************************************/
/*                                                                           */
/*  static DoTriePrint(T, node, len, fp)                                     */
/*                                                                           */
/*  Print on file fp the subset of the entries of trie T stored in node and  */
/*  its descendants.  The node has prefix prefix[0..len-1].                  */
/*                                                                           */
/*****************************************************************************/

static FULL_CHAR prefix[MAX_BUFF];

static void DoTriePrint(TRIE T, int node, int len, FILE *fp)
{ int i, next_node, pos;  FULL_CHAR str[20];
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
      UncompressValue(&(T->string_mem[pos]), str);
      fprintf(fp, " %s\n", str);
    }

    /* else if next_node > 0 have a child node to explore */
    else if( next_node > 0 )
    { assert( i > 0, "DoTriePrint: i == 0!" );
      prefix[len] = findrep(i, T);
      prefix[len+1] = '\0';
      DoTriePrint(T, NODE_MULT*next_node, len+1, fp);
    }
  }
} /* end DoTriePrint */


/*****************************************************************************/
/*                                                                           */
/*  static TriePrint(T, fp)                                                  */
/*                                                                           */
/*  Print trie T on file fp.                                                 */
/*                                                                           */
/*****************************************************************************/

static void TriePrint(TRIE T, FILE *fp)
{ int i, ch;
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


/*@::NewTrie(), ClassConvert(), NewTrieString(), NewTrieNode()@***************/
/*                                                                           */
/*  static TRIE NewTrie(node_lim, string_lim)                                */
/*                                                                           */
/*  Initialize a new trie with the this much space for nodes and strings.    */
/*                                                                           */
/*****************************************************************************/

static TRIE NewTrie(unsigned node_lim, unsigned string_lim)
{ TRIE T;  int i;
  debug2(DHY, D, "NewTrie(%d, %d)", node_lim, string_lim);
  ifdebug(DMA, D, DebugRegisterUsage(MEM_HYPH_PATS, 1,
    sizeof(struct trie_rec) + node_lim*sizeof(short)+string_lim*sizeof(char)));
  T = (TRIE) malloc( sizeof(struct trie_rec)
		     + node_lim*sizeof(short) + string_lim*sizeof(char));
  if( T == (TRIE) NULL )
    Error(36, 3, "run out of memory while constructing hyphenation table",
      FATAL, no_fpos);
  T->magic = TRIE_MAGIC;  T->class_count = 1;
  for( i = 0;  i < MAX_CHAR;  i++ )  T->class[i] = 0;
  T->node_mem = (short *) ( (char *) T + sizeof(struct trie_rec));
  T->node_lim = node_lim;  T->node_free = 0;
  T->string_mem = (FULL_CHAR *) &(T->node_mem[node_lim]);
  T->string_lim = T->string_first = string_lim;
  debug0(DHY, D, "NewTrie returning.");
  return T;
} /* end NewTrie */


/*****************************************************************************/
/*                                                                           */
/*  static short NewTrieString(str, T)                                       */
/*                                                                           */
/*  Copy a new string into T, and return its offset in string_mem;           */
/*                                                                           */
/*****************************************************************************/

static short NewTrieString(FULL_CHAR *str, TRIE T)
{ short res = T->string_first - StringLength(str) - 1;
  if( res >= 0 )
  { T->string_first = res;  StringCopy(&(T->string_mem[res]), str);
  }
  return res;
} /* end NewTrieString */


/*****************************************************************************/
/*                                                                           */
/*  ststic int NewTrieNode(T)                                                */
/*                                                                           */
/*  Allocate a new empty trie node in T, and return its offset in node_mem.  */
/*                                                                           */
/*****************************************************************************/

static int NewTrieNode(TRIE T)
{ int i;  int res;
  if( T->node_free + T->class_count > T->node_lim )
    Error(36, 4, "hyphenation trie node limit exceeded", INTERN, no_fpos);
  res = T->node_free;  T->node_free += T->class_count;
  for( i = res;  i < T->node_free;  i++ )  T->node_mem[i] = 0;
  return res;
} /* end NewTrieNode */


/*@::AddClassToTrie(), TrieInsert()@******************************************/
/*                                                                           */
/*  static AddClassToTrie(str, T)                                            */
/*                                                                           */
/*  Add a new character class, whose members are the characters of str, to   */
/*  trie T.  This cannot occur after the first insertion.                    */
/*                                                                           */
/*****************************************************************************/

static void AddClassToTrie(FULL_CHAR *str, TRIE T)
{ int i;
  assert( T->string_first == T->string_lim, "AddClassToTrie: after insertion");
  for( i = 0;  str[i] != '\0';  i++ )
    if( T->class[str[i]] == 0 ) T->class[str[i]] = T->class_count;
    else Error(36, 5, "hyphenation class of %c may not be changed",
      INTERN, no_fpos, str[i]);
  T->class_count++;
} /* end AddClassToTrie */


/*****************************************************************************/
/*                                                                           */
/*  static BOOLEAN TrieInsert(key, value, T, hline_num)                      */
/*                                                                           */
/*  Insert a new key and value into trie T (originating on line hline_num).  */
/*                                                                           */
/*****************************************************************************/

static BOOLEAN TrieInsert(FULL_CHAR *key, FULL_CHAR *value, TRIE T, int hline_num)
{ FULL_CHAR str[MAX_BUFF], compressed_value[MAX_BUFF];
  int i, curr_node, next_node, pos, ch;  short strpos;
  debug2(DHY, D, "TrieInsert(%s, %s, T)", key, value);

  /* if first insertion, add one node after making sure class_count is even */
  if( T->node_free == 0 )
  { T->class_count = NODE_MULT * ceiling(T->class_count, NODE_MULT);
    ch = NewTrieNode(T);
  }

  CompressValue(compressed_value, value);

  /* invariant: curr_node is an existing node of T with prefix str[0..i-1] */
  ClassConvert(key, str, T, hline_num);
  curr_node = i = 0;
  for(;;)
  {
    /* if str is ended, add compressed_value only to string memory */
    if( str[i] == '\0' )
    { if( T->node_mem[curr_node] != 0 )
	Error(36, 6, "hyphenation string %s already inserted",
	  INTERN, no_fpos, key);
      else
      {
	strpos = NewTrieString(compressed_value, T);
	if( strpos < 0 )
	{ debug0(DHY, D, "TrieInsert returning FALSE (trie full)");
	  return FALSE;
	}
	T->node_mem[curr_node] = - strpos;
      }
      debug0(DHY, D, "TrieInsert returning TRUE (empty suffix).");
      return TRUE;
    }

    /* if next position is unoccupied, store remainder of str and value */
    next_node = T->node_mem[curr_node + str[i]];
    if( next_node == 0 )
    { ch = NewTrieString(compressed_value, T);
      if( ch < 0 )
      { debug0(DHY, D, "TrieInsert returning FALSE (trie full)");
	return FALSE;
      }
      strpos = NewTrieString(&str[i+1], T);
      if( strpos < 0 )
      { debug0(DHY, D, "TrieInsert returning FALSE (trie full)");
	return FALSE;
      }
      T->node_mem[curr_node + str[i]] = - strpos;
      debug0(DHY, D, "TrieInsert returning (non-empty suffix).");
      return TRUE;
    }

    /* if next position is occupied by a non-empty string, move that */
    /* string down one level and replace it by a trie node           */
    if( next_node < 0 )
    { pos = - next_node;
      ch = T->string_mem[pos];
      if( T->string_first == pos )  T->string_first++;
      T->node_mem[curr_node + str[i]] = next_node = NewTrieNode(T)/NODE_MULT;
      T->node_mem[NODE_MULT*next_node + ch] = -(pos+1);
    }

    /* now next is the offset of the next node to be searched */
    curr_node = NODE_MULT*next_node;  i++;
  }
} /* end TrieInsert */


/*@::BeGetChar(), BePutChar(), BeGetShort(), BePutShort(), etc.@**************/
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
#define BePutChar(fp, v)   ( putc( (char) (v & 0xFF), fp), 0 )

#define BeGetShort(fp, pv)						\
(  (c = getc(fp)) == EOF ? -1 :						\
   (  *pv = (c & 0xFF) << 8,						\
      (c = getc(fp)) == EOF ? -1 : (*pv |= c & 0xFF, 0)			\
   )									\
)

#define BePutShort(fp, v)						\
( putc((v >> 8) & 0xFF, fp), putc(v & 0xFF, fp), 0 )

static int BeGetInt(FILE *fp, int *pv)
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

static int BePutInt(FILE *fp, int v)
{
  putc((v >> 24) & 0xFF, fp);
  putc((v >> 16) & 0xFF, fp);
  putc((v >> 8) & 0xFF, fp);
  putc(v & 0xFF, fp);
  return 0;
}


/*@::CompressTrie(), TrieRead(), AccumulateRating()@**************************/
/*                                                                           */
/*  static CompressTrie(T)                                                   */
/*                                                                           */
/*  Compress trie T and return its length in characters.                     */
/*                                                                           */
/*****************************************************************************/

static void CompressTrie(TRIE T)
{ FULL_CHAR *p, *q;  int len, i;
  debug0(DHY, D, "CompressTrie(T), T =");
  debug2(DHY, D, "Node space: %d capacity, %d used\n",
    T->node_lim, T->node_free);
  debug2(DHY, D, "String space: %d capacity, %d used\n",
    T->string_lim, T->string_lim - T->string_first);
  ifdebug(DHY, DD, TriePrint(T, stderr));
  T->node_lim = T->node_free;
  for( i = 0;  i < T->node_lim;  i++ )
    if( T->node_mem[i] < 0 )
      T->node_mem[i] = - ( -T->node_mem[i] - T->string_first);
  p = (FULL_CHAR *) &(T->node_mem[T->node_free]);
  q = &(T->string_mem[T->string_first]);
  len = T->string_lim - T->string_first;
  for( i = 0;  i < len;  i++ )  *p++ = *q++;
  T->string_mem = (FULL_CHAR *) &(T->node_mem[T->node_lim]);
  T->string_first = 0;
  T->string_lim = len;
  len = sizeof(struct trie_rec) + T->node_lim * sizeof(short)
				+ T->string_lim * sizeof(FULL_CHAR);
  debug1(DHY, D, "CompressTrie returning; len = %d, T =", len);
  ifdebug(DHY, DD, TriePrint(T, stderr));
} /* end CompressTrie */


/*****************************************************************************/
/*                                                                           */
/*  static TRIE TrieRead(lnum, success)                                      */
/*                                                                           */
/*  Read in a packed trie if possible, otherwise pack an unpacked one.       */
/*  The trie is to be for language lnum.                                     */
/*                                                                           */
/*  Boolean success is set to true if no errors were encountered.  If the    */
/*  file read was a placeholder, success will be true but still a null       */
/*  TRIE will be returned.                                                   */
/*                                                                           */
/*****************************************************************************/
#define START_STATE		0
#define CLASSES_STATE		1
#define EXCEPTIONS_STATE	2
#define PATTERNS_STATE		3

static TRIE TrieRead(LANGUAGE_NUM lnum, BOOLEAN *success)
{ TRIE T;  FILE_NUM unpacked_fnum, packed_fnum;  OBJECT fname;
  FILE *unpacked_fp, *packed_fp;  unsigned len;
  int prev, i, j, c, state, hline_num;
#if DEBUG_ON
  int icount = 0;
#endif
  debug2(DHY, D, "TrieRead(%d %s)", lnum,
    lnum == 0 ? STR_NONE : LanguageString(lnum));

  /* get hyphenation file name from language module */
  fname = LanguageHyph(lnum);
  assert( fname == nilobj || is_word(type(fname)), "TrieRead: fname!" );
  if( fname == nilobj )
  { *success = FALSE;
    return (TRIE) NULL;
  }

  /* define and open packed file */
  debug0(DFS, D, "  calling DefineFile from TrieRead (1)");
  packed_fnum = DefineFile(string(fname), HYPH_PACKED_SUFFIX,
    &fpos(fname), HYPH_PACKED_FILE, HYPH_PATH);
  packed_fp = OpenFile(packed_fnum, FALSE, FALSE);
  if( packed_fp == NULL )
  {
    /* no packed file, so define and open unpacked one instead */
    FULL_CHAR str[MAX_BUFF], key[MAX_BUFF], value[MAX_BUFF],
		  buff[MAX_BUFF+10];
    debug0(DFS, D, "  calling DefineFile from TrieRead (2)");
    unpacked_fnum = DefineFile(string(fname), HYPH_SUFFIX,
      &fpos(fname), HYPH_FILE, HYPH_PATH);
    unpacked_fp = OpenFile(unpacked_fnum, FALSE, FALSE);
    if( unpacked_fp == NULL )
    { Error(36, 7, "cannot open hyphenation file %s",
	WARN, no_fpos, FileName(unpacked_fnum));
      *success = FALSE;
      return (TRIE) NULL;
    }
    hline_num = 1;

    /* check that first line contains magic header or stub */
    if( StringFGets(str, MAX_BUFF, unpacked_fp) == NULL ||
        ( !StringEqual(str, AsciiToFull("Lout hyphenation information\n")) &&
	  !StringEqual(str, AsciiToFull("Lout hyphenation placeholder\n")) )
      )
      Error(36, 8, "header line of hyphenation file %s missing",
	FATAL, no_fpos, FileName(unpacked_fnum));

    /* if file is just a placeholder, exit silently with success */
    if( !StringEqual(str, AsciiToFull("Lout hyphenation information\n")) )
    { *success = TRUE;
      return (TRIE) NULL;
    }

    /* read the classes, exceptions, and patterns from the unpacked file */
    T = NewTrie( (unsigned) 120000,  (unsigned) 32767);
    state = START_STATE;
    while( fscanf(unpacked_fp, "%s", str) == 1 )
    {
      hline_num++;
      if( str[0] == '%' ) /* comment to end of line */
      {	StringFGets(str, MAX_BUFF, unpacked_fp);
	continue;
      }

      switch( state )
      {
	case START_STATE:

	  if( !StringEqual(str, AsciiToFull("Classes:")) )
	    Error(36, 9, "Classes heading of hyphenation file %s missing",
	      FATAL, no_fpos, FileName(unpacked_fnum));
	  state = CLASSES_STATE;
	  break;


	case CLASSES_STATE:

	  if( StringEqual(str, AsciiToFull("Exceptions:")) )
	  { state = EXCEPTIONS_STATE;
	  }
	  else if( StringEqual(str, AsciiToFull("Patterns:")) )
	  { state = PATTERNS_STATE;
	  }
	  else
	  { debug1(DHY, D, "adding class %s", str);
	    AddClassToTrie(str, T);
	  }
	  break;


	case EXCEPTIONS_STATE:

	  if( StringEqual(str, AsciiToFull("Patterns:")) )
	  { state = PATTERNS_STATE;
	  }
	  else
	  { prev = CH_EIGHT; j = 0;
	    key[j] = '.', value[j++] = prev, prev = CH_EIGHT;
	    for( i = 0;  str[i] != '\0';  i++ )
	    { if( str[i] == CH_HYPHEN )  prev = CH_NINE;
	      else key[j] = str[i], value[j++] = prev, prev = CH_EIGHT;
	    }
	    key[j] = '.', value[j++] = prev, prev = CH_EIGHT;
	    key[j] = '\0';  value[j] = prev;  value[j+1] = '\0';
	    if( !TrieInsert(key, value, T, hline_num) )
	    {
	      Error(36, 10, "hyphenation file %s%s is too large", WARN,
		&fpos(fname), string(fname), HYPH_SUFFIX);
	      *success = FALSE;
	      return (TRIE) NULL;
	    }
	  }
	  break;


	case PATTERNS_STATE:

	  prev = CH_ZERO; j = 0;
	  for( i = 0;  str[i] != '\0';  i++ )
	  { if( decimaldigit(str[i]) )  prev = str[i];
	    else key[j] = str[i], value[j++] = prev, prev = CH_ZERO;
	  }
	  key[j] = '\0';  value[j] = prev;  value[j+1] = '\0';
	  debug3(DHY, D, "TrieInsert(%s, %s, T) [%d]", key, value, ++icount);
	  if( !TrieInsert(key, value, T, hline_num) )
	  {
	    Error(36, 11, "hyphenation file %s%s is too large", WARN,
	      &fpos(fname), string(fname), HYPH_SUFFIX);
	    *success = FALSE;
	    return (TRIE) NULL;
	  }
	  break;


	default:

	  assert(FALSE, "TrieRead: state");
	  break;

      } /* end switch */
    } /* end while */

    if( state != PATTERNS_STATE )
      Error(36, 12, "format error in hyphenation file %s",
	FATAL, no_fpos, FileName(unpacked_fnum));
    fclose(unpacked_fp);
    CompressTrie(T);

    /* write the compressed trie out to the packed file */
    /* cannot use FileName(packed_fnum) because path won't be right */
    StringCopy(buff, FileName(unpacked_fnum));
    StringCopy(&buff[StringLength(buff) - StringLength(HYPH_SUFFIX)],
      HYPH_PACKED_SUFFIX);
    packed_fp = StringFOpen(buff, WRITE_BINARY);
    if( packed_fp == NULL )
      Error(36, 13, "cannot write to hyphenation file %s", FATAL,no_fpos,buff);
    BePutInt(packed_fp, T->magic);
    BePutInt(packed_fp, T->class_count);
    for( i = 0; i < MAX_CHAR; i++ )  BePutChar(packed_fp, T->class[i]);
    /* BePutInt(packed_fp, 0); */ /* placeholder for node_mem now omitted */
    BePutInt(packed_fp, T->node_lim);
    BePutInt(packed_fp, T->node_free);
    /* BePutInt(packed_fp, 0); */  /* placeholder for string_mem now omitted */
    BePutInt(packed_fp, T->string_lim);
    BePutInt(packed_fp, T->string_first);
    for( i=0; i < T->node_free; i++ )  BePutShort(packed_fp, T->node_mem[i]);
    for( i=0; i < T->string_lim; i++)  BePutChar(packed_fp, T->string_mem[i]);
    fclose(packed_fp);

    /* free T */
    ifdebug(DMA, D, DebugRegisterUsage(MEM_HYPH_PATS, 1,
      sizeof(struct trie_rec) + 120000*sizeof(short)+32767*sizeof(char)));
    free(T);

    /* now try again to open packed_fnum, the file just written */
    packed_fp = OpenFile(packed_fnum, FALSE, FALSE);
    if( packed_fp == NULL )
      Error(36, 14, "cannot open hyphenation file %s",
	FATAL, no_fpos, FileName(packed_fnum));
  } /* end if( packed_fp == NULL ) */

  /* now packed hyphenation file is open, read it in */
  fseek(packed_fp,0L,2); len = (unsigned) ftell(packed_fp); rewind(packed_fp);
  ifdebug(DMA, D, DebugRegisterUsage(MEM_HYPH_PATS, 1, len));
  /* the 2*sizeof(void*) is for the sizes of node_mem and string_mem */
  T = (TRIE) malloc(len + 2*sizeof(void*));
  if( T == (TRIE) NULL )
    Error(36, 15, "run out of memory while reading hyphenation table",
      FATAL, no_fpos);
  if( BeGetInt(packed_fp, &T->magic) != 0 )
    Error(36, 16, "error on read from packed hyphenation file %s",
      FATAL, no_fpos, FileName(packed_fnum));
  if( T->magic != TRIE_MAGIC )
    Error(36, 17, "bad magic number in hyphenation file %s",
      FATAL, no_fpos, FileName(packed_fnum));
  BeGetInt(packed_fp, &T->class_count);
  for( i = 0; i < MAX_CHAR; i++ )  BeGetChar(packed_fp, &T->class[i]);
  /* BeGetInt(packed_fp, &i); */  /* placeholder for node_mem now omitted */
  BeGetInt(packed_fp, &T->node_lim);
  BeGetInt(packed_fp, &T->node_free);
  /* BeGetInt(packed_fp, &i); */ /* placeholder for string_mem now omitted */
  BeGetInt(packed_fp, &T->string_lim);
  BeGetInt(packed_fp, &T->string_first);
  T->node_mem = (short *) ( (char *) T + sizeof(struct trie_rec) );
  T->string_mem = (FULL_CHAR *) &(T->node_mem[T->node_lim]);
  for( i = 0; i < T->node_free; i++ )  BeGetShort(packed_fp, &T->node_mem[i]);
  for( i = 0; i < T->string_lim; i++ ) BeGetChar(packed_fp, &T->string_mem[i]);
  fclose(packed_fp);

  /* debug and exit */
  debug0(DHY, D, "TrieRead returning, T =");
  *success = TRUE;
  ifdebug(DHY, DD, TriePrint(T, stderr));
  return T;
} /* end TrieRead */


/*****************************************************************************/
/*                                                                           */
/*  AccumulateRating(x, y)                                                   */
/*                                                                           */
/*  Accumulate the hyphenation rating string x into y.                       */
/*                                                                           */
/*****************************************************************************/

#define AccumulateRating(x, y)						\
{ FULL_CHAR *p = x, *q = y;						\
  while( *p )								\
  { if( *p > *q )  *q = *p;						\
    p++, q++;								\
  }									\
} /* end AccumulateRating */


/*@::ReadHyphTable()@*********************************************************/
/*                                                                           */
/*  BOOLEAN ReadHyphTable(lnum)                                              */
/*                                                                           */
/*  Read hyphenation table for language lnum.                                */
/*                                                                           */
/*****************************************************************************/

BOOLEAN ReadHyphTable(LANGUAGE_NUM lnum)
{ BOOLEAN res;
  debug1(DHY, D, "ReadHyphTable(%d)", lnum);
  assert(lnum > 0, "ReadHyphTable: lnum <= 0!");
  assert(HyphTables[lnum]==(TRIE) NULL && !TriedFile[lnum], "ReadHyphTable!");
  HyphTables[lnum] = TrieRead(lnum, &res);
  TriedFile[lnum] = TRUE;
  debug2(DHY, D, "ReadHyphTable(%d) returning %s", lnum, bool(res));
  return res;
} /* end ReadHyphTable */


/*@::Hyphenate@***************************************************************/
/*                                                                           */
/*  OBJECT Hyphenate(x)                                                      */
/*                                                                           */
/*  Hyphenate ACAT object x, returning the hyphenated result.                */
/*                                                                           */
/*****************************************************************************/

OBJECT Hyphenate(OBJECT x)
{ OBJECT link, y, z, next_link;  TRIE T;  LANGUAGE_NUM lnum;
  FULL_CHAR str[MAX_WORD+2], rate[MAX_WORD+3], val[MAX_WORD+3],
    *class, *key, *ss, *s, *p, *rem, *lig, *a, *b;
  int start, stop, i, curr_node, next_node, pos;
  BOOLEAN hyphenated, success;
  assert( type(x) == ACAT, "Hyphenate: type(x) != ACAT!" );
  debug1(DHY, DD, "Hyphenate(%s)", EchoObject(x));

  /* for each word y of x, try to hyphenate it */
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link);
    if( !is_word(type(y)) || string(y)[0] == '\0' || !word_hyph(y) )
      continue;
    debug1(DHY, DD, "Hyphenate() examining %s", EchoObject(y));

    /* determine T, the trie to use */
    lnum = word_language(y);
    if( lnum == 0 )
      Error(36, 18, "no current language for word %s",
	FATAL, &fpos(y), string(y));
    T = HyphTables[lnum];

    /* if no trie is present, try to get it from a file */
    if( T == (TRIE) NULL )
    { if( !TriedFile[lnum] )
      { T = HyphTables[lnum] = TrieRead(lnum, &success);
        TriedFile[lnum] = TRUE;
      }
      if( T == (TRIE) NULL )
      { debug1(DHY, DD, "Hyphenate continuing (no trie for %s)", string(y));
        continue;
      }
    }

    /* start := index of first letter of y, stop := index following last */
    key = string(y);  class = T->class;
    for( start = 0;  class[key[start]] == PUNCT_CLASS;  start++ );
    for( stop = start;  class[key[stop]] > PUNCT_CLASS;  stop++ );

    /* if a - ended the run, hyphenate there only */
    if( key[stop] == CH_HYPHEN )
    { next_link = NextDown(link);
      z = MakeWord(WORD, &key[stop+1], &fpos(y));
      word_font(z) = word_font(y);
      word_colour(z) = word_colour(y);
      word_language(z) = word_language(y);
      word_hyph(z) = word_hyph(y);
      underline(z) = underline(y);
      FontWordSize(z);
      Link(NextDown(link), z);
      New(z, GAP_OBJ);
      hspace(z) = vspace(z) = 0;
      SetGap(gap(z), FALSE, TRUE, FIXED_UNIT, HYPH_MODE, 0);
      underline(z) = underline(y);
      Link(NextDown(link), z);
      Link(z, MakeWord(WORD, STR_GAP_ZERO_HYPH, &fpos(y)));
      key[stop + 1] = '\0';
      FontWordSize(y);
      link = PrevDown(next_link);
      continue;
    }

    /* do not hyphenate if less than 5 letters, or a kill char is nearby */
    if( stop - start < 5 )  continue;
    if( key[stop] != '\0' && class[key[stop]] == KILL_CLASS )  continue;

    /* let str[] be the converted substring, let rate[] be all CH_ZERO */
    str[0] = PUNCT_CLASS;  rate[0] = CH_ZERO;
    for( i = 0;  i < stop - start;  i++ )
    { str[i+1] = class[key[start + i]];
      rate[i+1] = CH_ZERO;
    }
    str[i+1] = PUNCT_CLASS;  rate[i+1] = CH_ZERO;
    str[i+2] = '\0';  rate[i+2] = CH_ZERO;
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
	{ UncompressValue(&T->string_mem[- pos], val);
	  AccumulateRating(val, rate+(ss-str));
	  debug1(DHY, DD, " found %s", val);
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
	    { UncompressValue(rem+1, val);
	      AccumulateRating(val, rate+(ss-str));
	      debug1(DHY, DD, " found %s", val);
	      break;
	    }
	  } while( *++s == *rem++ );
	  break;
	}

	/* otherwise go on to the next trie node */
	curr_node = NODE_MULT*next_node;  s++;
      }
    } while( *(++ss + 1) != PUNCT_CLASS );
    ifdebug(DHY, DD, ShowRate(key, start, stop, rate, stderr));

    /* set rate[i] to CH_ZERO whenever key[start+i-1] lies within a ligature */
    lig = finfo[word_font(y)].lig_table;
    for( p = key, i = 2;  *p != '\0';  p++, i++ )
    { if( lig[*p] > 1 )
      { a = &lig[ lig[*p] + MAX_CHARS ];
        while( *a++ == *p )
        { b = p+1;
          while( *a == *b && *(a+1) != '\0' && *b != '\0' )  a++, b++;
          if( *(a+1) == '\0' )
          { rate[i] = CH_ZERO;
            break;
          }
          else
          { while( *++a );
            a++;
          }
        }
      }
    }
    ifdebug(DHY, DD, ShowRate(key, start, stop, rate, stderr));

    /* now rate[] has accumulated ratings; use it to perform hyphenations */
    hyphenated = FALSE;
    next_link = NextDown(link);
    for( i = stop - start - 1;  i >= 3;  i-- )
    {
      /* hyphenate at i if rate[i] is odd */
      if( is_odd(rate[i]) )
      {	z = MakeWord(WORD, &key[start+i-1], &fpos(y));
	word_font(z) = word_font(y);
	word_colour(z) = word_colour(y);
	word_language(z) = word_language(y);
	word_hyph(z) = word_hyph(y);
	underline(z) = underline(y);
	FontWordSize(z);
	Link(NextDown(link), z);
	New(z, GAP_OBJ);
	hspace(z) = vspace(z) = 0;
	SetGap(gap(z), FALSE, TRUE, FIXED_UNIT, HYPH_MODE, 0);
	underline(z) = underline(y);
	Link(NextDown(link), z);
	Link(z, MakeWord(WORD, STR_GAP_ZERO_HYPH, &fpos(y)));
	key[start + i - 1] = '\0';
	hyphenated = TRUE;
      }
    }
    if( hyphenated )
    { FontWordSize(y);
      link = PrevDown(next_link);
    }

  } /* end for each word */

  debug3(DHY, DD, "Hyphenate returning %s,%s %s",
    EchoLength(back(x, COLM)), EchoLength(fwd(x, COLM)), EchoObject(x));
  return x;
} /* end Hyphenate */
