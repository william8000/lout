/*@z45.c:External Sort:SortFile()@********************************************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.43)                       */
/*  COPYRIGHT (C) 1991, 2008 Jeffrey H. Kingston                             */
/*                                                                           */
/*  Jeffrey H. Kingston (jeff@it.usyd.edu.au)                                */
/*  School of Information Technologies                                       */
/*  The University of Sydney 2006                                            */
/*  AUSTRALIA                                                                */
/*                                                                           */
/*  This program is free software; you can redistribute it and/or modify     */
/*  it under the terms of the GNU General Public License as published by     */
/*  the Free Software Foundation; either Version 3, or (at your option)      */
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
/*  FILE:         z45.c                                                      */
/*  MODULE:       External Sort                                              */
/*  EXTERNS:      SortFile()                                                 */
/*                                                                           */
/*  This simple sort utility assumes that the source file can all be read    */
/*  into memory.  If not, you get an "out of memory" error message.          */
/*                                                                           */
/*****************************************************************************/
#include "externs.h"

#define BUFF_SIZE	4096		/* size of one memory buffer */
#define LINES_GUESS     2000		/* initial guess of number of lines */


/*****************************************************************************/
/*                                                                           */
/*  int ReadOneLine(FILE *fp, char *buff, int buff_len)                      */
/*                                                                           */
/*  Read one line of fp, up to and including the following newline           */
/*  sequence, which may be a CH_LF/CH_CR pair as well as singleton.          */
/*  Place the contents of the line into *buff, including a concluding        */
/*  \0 but never the concluding newline sequence.                            */
/*                                                                           */
/*  The buffer has space for buff_len characters including the concluding    */
/*  \0.  If space runs out, terminate the read early; the unread portion     */
/*  will be read by the next call to ReadOneLine().  This code assumes       */
/*  that buff_len is at least 2.                                             */
/*                                                                           */
/*  Return values are as follows:                                            */
/*                                                                           */
/*     0   Did not read a line because EOF was encountered immediately       */
/*     1   Successfully read a line, empty or otherwise, into *buff          */
/*     2   Read a line but had to stop early owing to buffer overflow        */
/*                                                                           */
/*****************************************************************************/

int ReadOneLine(FILE *fp, FULL_CHAR *buff, int buff_len)
{ int ch;
  int len1 = buff_len - 1;

  /* read characters up to the end of line or file */
  int count = 0;
  while( (ch = getc(fp)) != EOF && ch != CH_LF && ch != CH_CR )
  {
    buff[count++] = ch;
    if( count >= len1 )
    {
      /* out of space, have to stop early */
      buff[count++] = '\0';
      return 2;
    }
  }

  /* terminate gracefully depending what character we stopped at */
  if( ch == EOF )
  {
    if( count == 0 )
      return 0;
  }
  else if( ch == CH_LF )
  {
    /* consume any immediately following CH_CR too */
    ch = getc(fp);
    if( ch != CH_CR )
      ungetc(ch, fp);
  }
  else /* ch == CH_CR */
  {
    /* consume any immediately following CH_LF too */
    ch = getc(fp);
    if( ch != CH_LF )
      ungetc(ch, fp);
  }
  buff[count++] = '\0';
  return 1;
} /* end ReadOneLine */


/*****************************************************************************/
/*                                                                           */
/*  int ReadOneBinaryLine(FILE *fp, char *buff, int buff_len, int *count)    */
/*                                                                           */
/*  Contributed by William Bader, July 2008, mainly to allow included EPS    */
/*  files to contain null characters.                                        */
/*                                                                           */
/*  Read one line of fp, up to and including the following newline           */
/*  sequence, which may be a CH_LF/CH_CR pair as well as singleton.          */
/*  Place the contents of the line into *buff including the line ending.     */
/*  Set count to the total number of characters written into *buff.          */
/*  Adds a terminating null (not included in count).                         */
/*                                                                           */
/*  If remaining_len is >= 0, then read at most that many characters and     */
/*  decrement remaining_len by the number of characters read.                */
/*                                                                           */
/*  The buffer has space for buff_len characters.                            */
/*  If space runs out, terminate the read early; the unread portion          */
/*  will be read by the next call to ReadOneLine().  This code assumes       */
/*  that buff_len is at least 3.                                             */
/*                                                                           */
/*  Return values are as follows:                                            */
/*                                                                           */
/*     0   Did not read a line because EOF was encountered immediately       */
/*     1   Successfully read a line, empty or otherwise, into *buff          */
/*     2   Read a line but had to stop early owing to buffer overflow        */
/*                                                                           */
/*****************************************************************************/

int ReadOneBinaryLine(FILE *fp, FULL_CHAR *buff, int buff_len, int *count_ptr,
  long *remaining_len)
{ int ch;
  int len1 = buff_len - 3;

  /* read characters up to the end of line or file or remaining_len */
  int count = 0;
  while( (ch = getc(fp)) != EOF && ch != CH_LF && ch != CH_CR )
  {
    if( *remaining_len >= 0 )
    {
      if( *remaining_len == 0 )
      {
       ch = EOF;
       break;
      }
      (*remaining_len)--;
    }
    buff[count++] = ch;
    if( count >= len1 )
    {
      *count_ptr = count;
      return 2;
    }
  }

  /* terminate gracefully depending what character we stopped at */
  if( ch == EOF )
  {
    if( count == 0 ) {
      *count_ptr = 0;
      return 0;
    }
  }
  else if( ch == CH_LF )
  {
    /* consume any immediately following CH_CR too */
    if( *remaining_len > 0 )
    {
      (*remaining_len)--;
    }
    buff[count++] = ch;
    ch = getc(fp);
    if( ch != CH_CR )
      ungetc(ch, fp);
    else
    {
      if( *remaining_len > 0 )
      {
        (*remaining_len)--;
      }
      buff[count++] = ch;
    }
  }
  else /* ch == CH_CR */
  {
    /* consume any immediately following CH_LF too */
    if( *remaining_len > 0 )
    {
      (*remaining_len)--;
    }
    buff[count++] = ch;
    ch = getc(fp);
    if( ch != CH_LF )
      ungetc(ch, fp);
    else
    {
      if( *remaining_len > 0 )
      {
        (*remaining_len)--;
      }
      buff[count++] = ch;
    }
  }
  buff[count] = '\0';
  *count_ptr = count;
  return 1;
} /* end ReadOneBinaryLine */


/*****************************************************************************/
/*                                                                           */
/*  LINE *ReadLines(FILE *fp, FULL_CHAR *fname, FULL_CHAR *first_line, *len) */
/*                                                                           */
/*  Read all of the lines of fp into memory and return a null-terminated     */
/*  array of pointers to these lines, and set *len to the number of lines.   */
/*  Make sure the lines themselves are null-terminated, also.                */
/*                                                                           */
/*  As for ReadOneLine above, lines may be terminated by a one or two        */
/*  character newline sequence, but these characters are never included      */
/*  in the lines returned.                                                   */
/*                                                                           */
/*  fname is the name of the file being sorted, and is used for error        */
/*  messages only.                                                           */
/*                                                                           */
/*  if first_line is non-null then it is a pointer to a string which is      */
/*  to become the first line of the result.  This string needs copying.      */
/*                                                                           */
/*****************************************************************************/

LINE *ReadLines(FILE *fp, FULL_CHAR *fname, FULL_CHAR *first_line, int *len)
{
  char *buff;				/* the current input line buffer     */
  char *buff_top;			/* first spot off end of buffer      */
  char *bp;				/* first free spot in buff           */

  LINE *lines;				/* the array of pointers to lines    */
  int  lines_length;			/* the length of the lines array     */
  LINE *lines_top;			/* first spot off end of lines       */
  LINE *lp;				/* first free spot in lines          */

  char *p, *q;
  int ch;
  debug1(DEX, D, "ReadLines(-, %s, -)", fname);

  /* initialize buff to be empty with size BUFF_SIZE */
  buff = malloc(BUFF_SIZE * sizeof(char));
  if( buff == NULL )
    Error(45, 1, "run out of memory when reading index file %s",
      FATAL, no_fpos, fname);
  buff_top = buff + BUFF_SIZE;
  bp = buff;

  /* initialize the lines buffer to be the first line  */
  lines_length = LINES_GUESS;
  lines = malloc(lines_length * sizeof(LINE *));
  lines_top = &lines[lines_length];
  lp = lines;

  /* add first_line to lines buffer if required */
  if( first_line != (FULL_CHAR *) null )
  {
    *lp = malloc((StringLength(first_line) + 1) * sizeof(char));
    StringCopy( (char *) *lp, first_line);
    lp++;
  }

  *lp++ = bp;
  while( (ch = getc(fp)) != EOF )
  {
    debug4(DEX, DD, "lines: [%d  %d(%d)  %d]",
      (int) lines, (int) (lp-1), (int) *(lp-1), (int) lines_top -1);
    debug3(DEX, DD, " buff: [%d   bp %d   %d]",
      (int) buff, (int) bp, (int) buff_top - 1);
    assert( (long) buff >= (long) lines_top ||
	    (long) buff_top <= (long) lines,
	    "ReadLines: lines and buff overlap!" );

    /* get new buffer and copy current line across if out of buff space */
    if( bp == buff_top )
    {
      debug0(DEX, D, "  getting new buff");
      buff = malloc(BUFF_SIZE * sizeof(char));
      if( buff == NULL )
	Error(45, 2, "run out of memory when reading index file %s",
	  FATAL, no_fpos, fname);
      buff_top = buff + BUFF_SIZE;
      for( p = buff, q = *(lp-1);  q != bp;  *p++ = *q++ );
      bp = p; *bp = '\0';
      debug1(DEX, D, "  copied into new buff: %s", buff);
      *(lp-1) = buff;
      if( bp == buff_top )
	Error(45, 3, "line too long when reading index file %s",
	  FATAL, no_fpos, fname);
    }

    /* if newline char, end this line and start the next */
    if( ch == CH_LF || ch == CH_CR )
    {
      *bp++ = '\0';
      debug1(DEX, D, "  finished line: %s", *(lp-1));

      /* get rid of following character if part of two-character line ending */
      if( ch == CH_LF )
      {
	ch = getc(fp);
	if( ch != CH_CR )
	  ungetc(ch, fp);
      }
      else /* ch == CH_CR */
      {
	ch = getc(fp);
	if( ch != CH_LF )
	  ungetc(ch, fp);
      }

      /* if no room in lines for next line, double its size */
      if( lp == lines_top )
      {
        debug1(DEX, D, "  realloc(lines, %d)", 2 * lines_length);
	lines = realloc(lines, 2 * lines_length * sizeof(LINE *));
	if( lines == NULL )
	  Error(45, 4, "run out of memory when reading index file %s",
	    FATAL, no_fpos, fname);
	lp = &lines[lines_length];
	lines_length = 2 * lines_length;
	lines_top = &lines[lines_length];
      }

      *lp++ = bp;
    }
    else /* ordinary char with space available, so just add it */
    {
      *bp++ = ch;
    }
  }

  *len = (lp - lines - 1);
  debug1(DEX, D, "ReadLines returning (len = %d)", *len);
  return lines;

} /* end ReadLines */


/*****************************************************************************/
/*                                                                           */
/*  WriteLines(FILE *fp, LINE *lines, int len)                               */
/*                                                                           */
/*  Write array of lines "lines", of length len, to file fp.                 */
/*                                                                           */
/*****************************************************************************/

void WriteLines(FILE *fp, LINE *lines, int len)
{ int i;
  for( i = 0;  i < len;  i++ )
  { fputs(lines[i], fp);
    fputs((char *) STR_NEWLINE, fp);
  }
}


/*****************************************************************************/
/*                                                                           */
/*  Line comparison functions (for qsort)                                    */
/*                                                                           */
/*  By Jeff Kingston and Valery Ushakov (uwe).                               */
/*                                                                           */
/*****************************************************************************/

static int pstrcmp(const void *a, const void *b)	/* !UseCollate */
{
  return strcmp (*(char **)a, *(char **)b);
}

static int pstrcollcmp(const void *a, const void *b)	/* UseCollate */
{
  return strcollcmp (*(char **)a, *(char**)b);
}


/*****************************************************************************/
/*                                                                           */
/*  void SortLines(LINE *lines, int lines_len)                               */
/*                                                                           */
/*  Sort the given lines.                                                    */
/*                                                                           */
/*****************************************************************************/

void SortLines(LINE *lines, int lines_len)
{
  qsort(lines, lines_len, sizeof(LINE), (UseCollate ? pstrcollcmp : pstrcmp));
}


/*****************************************************************************/
/*                                                                           */
/*  void SortFile(char *infile, char *outfile)                               */
/*                                                                           */
/*  Sort file infile, placing the result on file outfile.                    */
/*                                                                           */
/*****************************************************************************/

void SortFile(FULL_CHAR *infile, FULL_CHAR *outfile)
{
  LINE *lines;
  int lines_len;
  FILE *in_fp, *out_fp; 
  debug2(DEX, D, "SortFile(%s, %s)", infile, outfile);
  
  /* open input file */
  in_fp = fopen( (char *) infile, READ_FILE);
  if( in_fp == (FILE *) NULL )
    Error(45, 5, "cannot open index file %s for reading",
	    FATAL, no_fpos, outfile);

  /* open output file */
  out_fp = fopen( (char *) outfile, WRITE_FILE);
  if( out_fp == (FILE *) NULL )
    Error(45, 6, "cannot open index file %s for writing",
	    FATAL, no_fpos, outfile);

  /* read lines, sort them, and write them out again sorted */
  lines = ReadLines(in_fp, infile, (FULL_CHAR *) NULL, &lines_len);
  SortLines(lines, lines_len);
  fclose(in_fp);
  WriteLines(out_fp, lines, lines_len);
  fclose(out_fp);
}
