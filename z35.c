/*@z35.c:Time Keeper: InitTime()@*********************************************/
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
/*  FILE:         z35.c                                                      */
/*  MODULE:       Time Keeper                                                */
/*  EXTERNS:      InitTime()                                                 */
/*                                                                           */
/*****************************************************************************/
#include <time.h>
#include "externs"

#define load(str, typ, encl)						\
  sym = InsertSym(str, typ, no_fpos, DEFAULT_PREC,  			\
  FALSE, FALSE, 0, encl, MakeWord("", no_fpos));			\
  if( typ == NPAR )  visible(sym) = TRUE

#define add_par(format, val, sym)					\
  sprintf(buff, format, val);						\
  par = New(PAR);  actual(par) = sym;					\
  Link(current_moment, par);						\
  tmp = MakeWord(buff, no_fpos);					\
  Link(par, tmp);

static OBJECT current_moment = nil;
static unsigned char time_string[30] = { '\0' };


/*****************************************************************************/
/*                                                                           */
/*  OBJECT MomentSym;                                                        */
/*                                                                           */
/*  The symbol table entry for the @Moment symbol.                           */
/*                                                                           */
/*****************************************************************************/

OBJECT MomentSym = nil;


/*@@**************************************************************************/
/*                                                                           */
/*  InitTime()                                                               */
/*                                                                           */
/*  Place a declaration of the @Moment symbol into the symbol table, and     */
/*  initialize the value of the object StartMoment.                          */
/*                                                                           */
/*****************************************************************************/

InitTime()
{ long raw_time; struct tm *now;
  unsigned char buff[20]; OBJECT par, tmp, sym, env;
  OBJECT tag, second, minute, hour, weekday,
	monthday, yearday, month, year, century, dst;
  debug0(DTK, D, "InitTime()");

  /* define @Moment symbol with its host of named parameters */
  MomentSym = load("@Moment",         LOCAL, StartSym);
  tag       = load(KW_TAG,            NPAR,  MomentSym);
  second    = load("@Second",         NPAR,  MomentSym);
  minute    = load("@Minute",         NPAR,  MomentSym);
  hour      = load("@Hour",           NPAR,  MomentSym);
  monthday  = load("@Day",            NPAR,  MomentSym);
  month     = load("@Month",          NPAR,  MomentSym);
  year      = load("@Year",           NPAR,  MomentSym);
  century   = load("@Century",        NPAR,  MomentSym);
  weekday   = load("@WeekDay",        NPAR,  MomentSym);
  yearday   = load("@YearDay",        NPAR,  MomentSym);
  dst       = load("@DaylightSaving", NPAR,  MomentSym);

  /* get current time and convert to ASCII */
  time(&raw_time);
  now = localtime(&raw_time);
  strcpy(time_string, asctime(now));

  /* start of current_moment */
  current_moment = New(CLOSURE);
  actual(current_moment) = MomentSym;

  /* attach its many parameters */
  add_par("%s",   KW_NOW,                      tag);
  add_par("%d",   now->tm_sec,                 second);
  add_par("%d",   now->tm_min,                 minute);
  add_par("%d",   now->tm_hour,                hour);
  add_par("%d",   now->tm_mday,                monthday);
  add_par("%d",   now->tm_mon + 1,             month);
  add_par("%.2d", now->tm_year % 100,          year);
  add_par("%d",   (now->tm_year+1900) / 100,   century);
  add_par("%d",   now->tm_wday + 1,            weekday);
  add_par("%d",   now->tm_yday,                yearday);
  add_par("%d",   now->tm_isdst,               dst);

  /* add a null environment */
  env = New(ENV);
  AttachEnv(env, current_moment);
  debug0(DTK, D, "InitTime() returning.");
  debug0(DTK, DD, "current_moment =");
  ifdebug(DTK, DD, EchoObject(stderr, current_moment));
} /* end InitTime */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT StartMoment()                                                     */
/*                                                                           */
/*  Returns a copy of the initial time.                                      */
/*                                                                           */
/*****************************************************************************/

OBJECT StartMoment()
{ OBJECT res;
  debug0(DTK, D, "StartMoment()");
  assert(current_moment != nil, "StartMoment: current_moment == nil!");
  res = CopyObject(current_moment, no_fpos);
  debug0(DTK, D, "StartMoment returning");
  ifdebug(DTK, D, EchoObject(stderr, res));
  return res;
}


/*****************************************************************************/
/*                                                                           */
/*  unsigned char *TimeString()                                              */
/*                                                                           */
/*  Returns a pointer to a string containing the current time.               */
/*                                                                           */
/*****************************************************************************/

unsigned char *TimeString()
{ return time_string;
} /* end TimeString */
