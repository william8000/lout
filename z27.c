/*@z27.c:Debug Service:DebugInit(), Debug()@**********************************/
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
/*  FILE:         z27.c                                                      */
/*  MODULE:       Debug Service                                              */
/*  EXTERNS:      dbg[], DebugInit(), Debug()                                */
/*                ProfileOn(), ProfileOff(), ProfilePrint()                  */
/*                                                                           */
/*****************************************************************************/
#include "externs"

#if DEBUG_ON

struct dbs  dbg[] = {
    (unsigned char *) "zz",    0, 0, 0,		/* - unused -                */
    (unsigned char *) "sp",    0, 0, 0,		/* Supervise                 */
    (unsigned char *) "la",    0, 0, 0,		/* Lexical Analyser          */
    (unsigned char *) "fs",    0, 0, 0,		/* File Service              */
    (unsigned char *) "ts",    0, 0, 0,		/* Token Service             */
    (unsigned char *) "rd",    0, 0, 0,		/* Read Definitions          */
    (unsigned char *) "op",    0, 0, 0,		/* Object Parser             */
    (unsigned char *) "os",    0, 0, 0,		/* Object Service            */
    (unsigned char *) "om",    0, 0, 0,		/* Object Manifest           */
    (unsigned char *) "ce",    0, 0, 0,		/* Closure Expansion         */
    (unsigned char *) "cr",    0, 0, 0,		/* Cross References          */
    (unsigned char *) "ss",    0, 0, 0,		/* Style Service             */
    (unsigned char *) "sf",    0, 0, 0,		/* Size Finder               */
    (unsigned char *) "ob",    0, 0, 0,		/* Object Breaking           */
    (unsigned char *) "of",    0, 0, 0,		/* Object Filling            */
    (unsigned char *) "sc",    0, 0, 0,		/* Size Constraints          */
    (unsigned char *) "sa",    0, 0, 0,		/* Size Adjustments          */
    (unsigned char *) "gw",    0, 0, 0,		/* Gap Widths                */
    (unsigned char *) "gt",    0, 0, 0,		/* Galley Transfer           */
    (unsigned char *) "ga",    0, 0, 0,		/* Galley Attaching          */
    (unsigned char *) "gf",    0, 0, 0,		/* Galley Flusher            */
    (unsigned char *) "gm",    0, 0, 0,		/* Galley Maker              */
    (unsigned char *) "gs",    0, 0, 0,		/* Galley Service            */
    (unsigned char *) "gp",    0, 0, 0,		/* Galley Printer            */
    (unsigned char *) "ft",    0, 0, 0,		/* Font Tables               */
    (unsigned char *) "oe",    0, 0, 0,		/* Object Echo               */
    (unsigned char *) "es",    0, 0, 0,		/* Echo Service              */
    (unsigned char *) "zz",    0, 0, 0,		/* Debug Service (unused)    */
    (unsigned char *) "yy",    0, 0, 0,		/* Error Service             */
    (unsigned char *) "st",    0, 0, 0,		/* Symbol Table              */
    (unsigned char *) "su",    0, 0, 0,		/* Symbol Uses               */
    (unsigned char *) "ma",    0, 0, 0,		/* Memory Allocator          */
    (unsigned char *) "cs",    0, 0, 0,		/* Counter Service           */
    (unsigned char *) "bs",    0, 0, 0,		/* Database Service          */
    (unsigned char *) "rs",    0, 0, 0,		/* Rotation Service          */
    (unsigned char *) "tk",    0, 0, 0,		/* Time Keeper               */
    (unsigned char *) "hy",    0, 0, 0,		/* Hyphenation               */
    (unsigned char *) "pp",    0, 0, 0,		/* Profiling                 */
    (unsigned char *) "",      0, 0, 0,		/* any                       */
};


/*****************************************************************************/
/*                                                                           */
/*  DebugInit(str)                                                           */
/*                                                                           */
/*  Turn on the debug flag given by str.                                     */
/*                                                                           */
/*****************************************************************************/

DebugInit(str)
unsigned char *str;
{ int j, urg;
  for( urg = 0;  urg < 2 && str[urg+2] == 'd';  urg++ );
  for( j = 1;  ;  j++ )
  { if( strcmp(dbg[j].flag, &str[urg+2]) == 0 )  break;
    if( strcmp(dbg[j].flag, ""         ) == 0 )
      Error(FATAL, no_fpos, "unknown debug flag %s", str);
  }
  for( ;  urg >= 0;  urg-- )  dbg[j].on[urg] = dbg[ANY].on[urg] = TRUE;
} /* end DebugInit */


/*@@**************************************************************************/
/*                                                                           */
/*  Debug(category, urgency, str, p1, p2, p3, p4, p5, p6, p7, p8)            */
/*                                                                           */
/*  Print str on debug output, if the flag corresponding to the given        */
/*  debug category and urgency is on.                                        */
/*                                                                           */
/*****************************************************************************/

/*VARARGS3*/
Debug(category, urgency, str, p1, p2, p3, p4, p5, p6, p7, p8)
int category, urgency;  unsigned char *str;  int p1, p2, p3, p4, p5, p6, p7, p8;
{ static BOOLEAN first_message = TRUE;
  if( first_message )
  { fprintf(stderr, "\nLout Debug Output:\n");
    first_message = FALSE;
  }
  fprintf(stderr, "%2s: ", dbg[category].flag);
  fprintf(stderr, str, p1, p2, p3, p4, p5, p6, p7, p8);
  fprintf(stderr, "\n");
  fflush(stderr);
} /* end Debug */


#define MAXPROF	20
#include <time.h>

struct profrec
{ unsigned char *label;			/* label of the profile              */
  int calls;			/* number of calls with this label   */
  long time;			/* total time of this label          */
};

static struct profrec profstack[MAXPROF];
static struct profrec profstore[MAXPROF];
static int proftop = 0, profsize = 0;

/*****************************************************************************/
/*                                                                           */
/*  ProfileOn(str)                                                           */
/*                                                                           */
/*  Start profiling label str.                                               */
/*                                                                           */
/*****************************************************************************/

ProfileOn(str)
unsigned char *str;
{ int i;  long raw_time;
  for( i = 0;  i < proftop;  i++ )
  { if( strcmp(profstack[i].label, str) == 0 )
    { for( i = 0;  i < proftop;  i++ )
	fprintf(stderr, "profstack[%d] = %s\n", i, profstack[i].label);
      Error(INTERN, no_fpos, "ProfileOn: %s restarted", str);
    }
  }
  if( proftop == MAXPROF )  Error(INTERN, no_fpos, "ProfileOn: overflow");
  time(&raw_time);
  profstack[proftop].label = str;
  profstack[proftop].time  = raw_time;
  proftop++;
} /* end ProfileOn */


/*****************************************************************************/
/*                                                                           */
/*  ProfileOff(str)                                                          */
/*                                                                           */
/*  Stop profiling label str.                                                */
/*                                                                           */
/*****************************************************************************/

ProfileOff(str)
unsigned char *str;
{ int i;  long raw_time;
  if( proftop == 0 || strcmp(profstack[proftop-1].label, str) != 0 )
    Error(INTERN, no_fpos, "ProfileOff: %s is not the current label", str);
  for( i = 0;  i < profsize && strcmp(profstore[i].label, str) != 0; i++ );
  if( i >= profsize )
  { if( profsize == MAXPROF )  Error(INTERN, no_fpos, "ProfileOff: overflow");
    profsize++;
    profstore[i].label = str;
    profstore[i].calls = 0;
    profstore[i].time  = 0;
  }
  time(&raw_time);
  profstore[i].calls += 1;
  profstore[i].time  += (raw_time - profstack[proftop-1].time);
  proftop--;
} /* end ProfileOff */


/*****************************************************************************/
/*                                                                           */
/*  ProfilePrint()                                                           */
/*                                                                           */
/*  Print results of profiling.                                              */
/*                                                                           */
/*****************************************************************************/

ProfilePrint()
{ int i;
  for( i = 0;  i < profsize;  i++ )
  { fprintf(stderr, "Profile %-20s  %6d secs, %3d calls, %6.2f secs/call\n",
	profstore[i].label, profstore[i].time, profstore[i].calls,
	(float) profstore[i].time / profstore[i].calls );
  }
} /* end ProfilePrint */
#endif
