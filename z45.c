/*@z45.c:External Sort:SortFile()@********************************************/
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
/*  FILE:         z45.c                                                      */
/*  MODULE:       External Sort                                              */
/*  EXTERNS:      SortFile()                                                 */
/*                                                                           */
/*  This file is a cut-down version of the GNU sort utility, written by      */
/*  Mike Haertel.  The cutting was done by Franck Arnaud and Jeff Kingston.  */
/*                                                                           */
/*****************************************************************************/

/* sort - sort lines of text (with all kinds of options).
   Copyright (C) 1988, 1991 Free Software Foundation

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Written December 1988 by Mike Haertel.
   The author may be reached (Email) at the address mike@gnu.ai.mit.edu,
   or (US mail) as Mike Haertel c/o Free Software Foundation. */

/* apparently not needed #include <sys/types.h> */
#include "externs.h"

#ifndef UCHAR_MAX
#define UCHAR_MAX 255
#endif

/* from system.h */

#include <string.h>   /* uwe: it also gives us strcoll() */
/* already included in externs #include <stdlib.h> */
#include <ctype.h>

/* end system.h */

#define UCHAR_LIM (UCHAR_MAX + 1)
#define UCHAR(c) ((unsigned char) (c))


/* During the merge phase, the number of files to merge at once. */
#define NMERGE 16

/* Initial buffer size for in core sorting.  Will not grow unless a
   line longer than this is seen. */
static int sortalloc =  524288;

/* Guess of average line length. */
static int linelength = 40;

/* Maximum number of elements for the array(s) of struct line's, in bytes.  */
#define LINEALLOC 262144


/* bcopy -- from lib-text-util/bcopy.c */
/* bcopy.c -- copy memory.
   renamed jeff_bcopy as a portability fix by JK
   Copy length bytes from source to dest.  Does not null-terminate.
   In the public domain.
   By David MacKenzie <djm@gnu.ai.mit.edu>.  */

static void
jeff_bcopy (source, dest, length)
     char *source, *dest;
     unsigned length;
{
  if (source < dest)
    /* Moving from low mem to hi mem; start at end.  */
    for (source += length, dest += length; length; --length)
      *--dest = *--source;
  else if (source != dest)
    /* Moving from hi mem to low mem; start at beginning.  */
    for (; length; --length)
      *dest++ = *source++;
}
/* end jeff_bcopy */

/* local error */

static void sort_error(char *err)
{
	fprintf(stderr,"sort: %s\n",err);
}

/* Lines are held in core as counted strings. */
struct line
{
  char *text;			/* Text of the line. */
  int length;			/* Length not including final newline. */
  char *keybeg;			/* Start of first key. */
  char *keylim;			/* Limit of first key. */
};

/* Arrays of lines. */
struct lines
{
  struct line *lines;		/* Dynamically allocated array of lines. */
  int used;			/* Number of slots used. */
  int alloc;			/* Number of slots allocated. */
  int limit;			/* Max number of slots to allocate.  */
};

/* Input buffers. */
struct buffer
{
  char *buf;			/* Dynamically allocated buffer. */
  int used;			/* Number of bytes used. */
  int alloc;			/* Number of bytes allocated. */
  int left;			/* Number of bytes left after line parsing. */
};

/* Allocate N bytes of memory dynamically, with error checking.  */

static char *
xmalloc (n)
     unsigned n;
{
  char *p;

  p = malloc (n);
  if (p == 0)
    {
      sort_error ("virtual memory exhausted");
      exit (2);
    }
  return p;
}

/* Change the size of an allocated block of memory P to N bytes,
   with error checking.
   If P is NULL, run xmalloc.
   If N is 0, run free and return NULL.  */

static char *
xrealloc (p, n)
     char *p;
     unsigned n;
{
  if (p == 0)
    return xmalloc (n);
  if (n == 0)
    {
      free (p);
      return 0;
    }
  p = realloc (p, n);
  if (p == 0)
    {
      sort_error ("virtual memory exhausted");
      exit (2);
    }
  return p;
}

static FILE *
xfopen (file, how)
     char *file, *how;
{
	FILE *fp;

	if(strcmp(file,"-") == 0)
	{
		sort_error("do not use standard io");
		exit(2);
	}

	fp = fopen(file,how);

  if (fp == 0)
    {
      sort_error ("can't open file "); 
      sort_error (file);
      exit (2);
    }
  return fp;
}

static void
xfclose (fp)
     FILE *fp;
{
  fflush (fp);
  if (fp != stdin && fp != stdout)
    {
      if (fclose (fp) != 0)
	{
	  sort_error ("error closing file");
	  exit (2);
	}
    }
  else
    /* Allow reading stdin from tty more than once. */
    clearerr (fp);
}

static void
xfwrite (buf, ssize, nelem, fp)
     char *buf;
     int ssize, nelem;
     FILE *fp;
{
  if (fwrite (buf, ssize, nelem, fp) != nelem)
    {
      sort_error ("write error");
      exit (2);
    }
}


/* Initialize BUF, allocating ALLOC bytes initially. */

static void
initbuf (buf, alloc)
     struct buffer *buf;
     int alloc;
{
  buf->alloc = alloc;
  buf->buf = xmalloc (buf->alloc);
  buf->used = buf->left = 0;
}

/* Fill BUF reading from FP, moving buf->left bytes from the end
   of buf->buf to the beginning first.	If EOF is reached and the
   file wasn't terminated by a newline, supply one.  Return a count
   of bytes buffered. */

static int
fillbuf (buf, fp)
     struct buffer *buf;
     FILE *fp;
{
  int cc;

  jeff_bcopy (buf->buf + buf->used - buf->left, buf->buf, buf->left);
  buf->used = buf->left;

  while (!feof (fp) && (buf->used == 0 || !memchr (buf->buf, '\n', buf->used)))
    {
      if (buf->used == buf->alloc)
	{
	  buf->alloc *= 2;
	  buf->buf = xrealloc (buf->buf, buf->alloc);
	}
      cc = fread (buf->buf + buf->used, 1, buf->alloc - buf->used, fp);
      if (ferror (fp))
	{
	  sort_error ("read error");
	  exit (2);
	}
      buf->used += cc;
    }

  if (feof (fp) && buf->used && buf->buf[buf->used - 1] != '\n')
    {
      if (buf->used == buf->alloc)
	{
	  buf->alloc *= 2;
	  buf->buf = xrealloc (buf->buf, buf->alloc);
	}
      buf->buf[buf->used++] = '\n';
    }

  return buf->used;
}

/* Initialize LINES, allocating space for ALLOC lines initially.
   LIMIT is the maximum possible number of lines to allocate space
   for, ever.  */

static void
initlines (lines, alloc, limit)
     struct lines *lines;
     int alloc;
     int limit;
{
  lines->alloc = alloc;
  lines->lines = (struct line *) xmalloc (lines->alloc * sizeof (struct line));
  lines->used = 0;
  lines->limit = limit;
}


/* Find the lines in BUF, storing pointers and lengths in LINES.
   Also replace newlines with NULs. */

static void
findlines (buf, lines)
     struct buffer *buf;
     struct lines *lines;
{
  register char *beg = buf->buf, *lim = buf->buf + buf->used, *ptr;

  lines->used = 0;

  while (beg < lim && (ptr = memchr (beg, '\n', lim - beg))
	 && lines->used < lines->limit)
    {
      /* There are various places in the code that rely on a NUL
	 being at the end of in-core lines; NULs inside the lines
	 will not cause trouble, though. */
      *ptr = '\0';

      if (lines->used == lines->alloc)
	{
	  lines->alloc *= 2;
	  lines->lines = (struct line *)
	    xrealloc ((char *) lines->lines,
		      lines->alloc * sizeof (struct line));
	}

      lines->lines[lines->used].text = beg;
      lines->lines[lines->used].length = ptr - beg;

      ++lines->used;
      beg = ptr + 1;
    }

  buf->left = lim - beg;
}

/* Compare two lines A and B, returning negative, zero, or positive
   depending on whether A compares less than, equal to, or greater than B. */

#if COLLATE
static int
compare (a, b)
     register struct line *a, *b;
{
  int tmpa, tmpb, mini;

      tmpa = a->length, tmpb = b->length;
      mini = find_min(tmpa, tmpb);
      if (mini == 0)
	return tmpa - tmpb;
      else
	return strcoll (a->text, b->text);
}

#else /* !COLLATE -- good old ASCIIbetical order */

static int
compare (a, b)
     register struct line *a, *b;
{
  int diff, tmpa, tmpb, mini;

      tmpa = a->length, tmpb = b->length;
      mini = find_min(tmpa, tmpb);
      if (mini == 0)
	diff = tmpa - tmpb;
      else
	{
	  char *ap = a->text, *bp = b->text;

	  diff = *ap - *bp;
	  if (diff == 0)
	    {
	      diff = memcmp (ap, bp, mini);
	      if (diff == 0)
		diff = tmpa - tmpb;
	    }
	}

  return diff;
}
#endif /* !COLLATE */

/* Sort the array LINES with NLINES members, using TEMP for temporary space. */

static void
sortlines (lines, nlines, temp)
     struct line *lines, *temp;
     int nlines;
{
  register struct line *lo, *hi, *t;
  register int nlo, nhi;

  if (nlines == 2)
    {
      if (compare (&lines[0], &lines[1]) > 0)
	*temp = lines[0], lines[0] = lines[1], lines[1] = *temp;
      return;
    }

  nlo = nlines / 2;
  lo = lines;
  nhi = nlines - nlo;
  hi = lines + nlo;

  if (nlo > 1)
    sortlines (lo, nlo, temp);

  if (nhi > 1)
    sortlines (hi, nhi, temp);

  t = temp;

  while (nlo && nhi)
    if (compare (lo, hi) <= 0)
      *t++ = *lo++, --nlo;
    else
      *t++ = *hi++, --nhi;
  while (nlo--)
    *t++ = *lo++;

  for (lo = lines, nlo = nlines - nhi, t = temp; nlo; --nlo)
    *lo++ = *t++;
}

/* Sort NFILES FILES onto OFP. */

void SortFile(char *infile, char *outfile)  /* name changed from sort_one by JK */
{
  struct buffer buf;
  struct lines lines;
  struct line *tmp;
  int i, ntmp;
  FILE *fp,*ofp; 
  
  ofp = xfopen (outfile, WRITE_BINARY);

  initbuf (&buf, sortalloc);
  initlines (&lines, sortalloc / linelength + 1,
	     LINEALLOC / sizeof (struct line));
  ntmp = lines.alloc;
  tmp = (struct line *) xmalloc (ntmp * sizeof (struct line));

      fp = xfopen (infile, READ_BINARY);
      while (fillbuf (&buf, fp))
	{
	  findlines (&buf, &lines);
	  if (lines.used > ntmp)
	    {
	      while (lines.used > ntmp)
		ntmp *= 2;
	      tmp = (struct line *)
		xrealloc ((char *) tmp, ntmp * sizeof (struct line));
	    }
	  sortlines (lines.lines, lines.used, tmp);

	  for (i = 0; i < lines.used; ++i)
	      {
		xfwrite (lines.lines[i].text, 1, lines.lines[i].length, ofp);
		putc ('\n', ofp);
	      }
	}
      xfclose (fp);
      xfclose (ofp);

  free (buf.buf);
  free ((char *) lines.lines);
  free ((char *) tmp);

}
