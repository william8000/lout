/*@z34.c:Rotation Service:Declarations@***************************************/
/*                                                                           */
/*  THE LOUT DOCUMENT FORMATTING SYSTEM (VERSION 3.02)                       */
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
/*  FILE:         z34.c                                                      */
/*  MODULE:       Rotation Service                                           */
/*  EXTERNS:      RotateSize()                                               */
/*                                                                           */
/*****************************************************************************/
#include <math.h>
#ifndef M_PI
#define M_PI       3.1415926535897931160E0
#endif
#include "externs"

typedef struct { double x, y;          } rect_coord;
typedef struct { double angle, radius; } polar_coord;

#define rect_to_polar(rect, polar)				\
polar.angle = atan2(rect.y, rect.x),				\
polar.radius = sqrt(rect.x*rect.x + rect.y*rect.y)

#define polar_to_rect(polar, rect)				\
rect.x = polar.radius * cos(polar.angle),			\
rect.y = polar.radius * sin(polar.angle)


/*@::RotateSize()@************************************************************/
/*                                                                           */
/*  RotateSize(xcb, xcf, xrb, xrf, y, theta)                                 */
/*                                                                           */
/*  Calculate the size of x, assuming that it is y rotated by theta degrees. */
/*                                                                           */
/*****************************************************************************/

RotateSize(xcb, xcf, xrb, xrf, y, theta)
LENGTH *xcb, *xcf, *xrb, *xrf;  OBJECT y;  LENGTH theta;
{ rect_coord ycorners[4], xcorner;  polar_coord pol;
  double maxx, maxy, minx, miny, ang;  int i;
  char buff1[20], buff2[20];

  /* calculate theta in radians */
  ang = (double) theta * 2 * M_PI / (double) (DG * 360);
  ifdebug(DRS, D, sprintf(buff2, "%.1f", ang));
  debug2(DRS, D, "RotateSize( %s, %s )", EchoObject(y), buff2);
  debug4(DRS, DD, "  ycb %s, ycf %s, yrb %s, yrf %s",
	EchoLength(back(y, COL)), EchoLength(fwd(y, COL)),
	EchoLength(back(y, ROW)), EchoLength(fwd(y, ROW)));

  /* set up coordinates of the four corners of y */
  ycorners[0].x =   (float) fwd(y, COL);
  ycorners[0].y =   (float) back(y, ROW);
  ycorners[1].x = - (float) back(y, COL);
  ycorners[1].y =   (float) back(y, ROW);
  ycorners[2].x = - (float) back(y, COL);
  ycorners[2].y = - (float) fwd(y, ROW);
  ycorners[3].x =   (float) fwd(y, COL);
  ycorners[3].y = - (float) fwd(y, ROW);

  /* rotate these four corners by theta and store their extremes */
  maxx = maxy = (float) - MAX_LEN;
  minx = miny = (float) MAX_LEN;
  for( i = 0;  i < 4;  i++ )
  {	
    if( ycorners[i].x == 0 && ycorners[i].y == 0 )
    {	pol.radius = 0; pol.angle  = 0; }
    else rect_to_polar(ycorners[i], pol);
    ifdebug(DRS, DD, sprintf(buff1, "%.1f", pol.angle));
    ifdebug(DRS, DD, sprintf(buff2, "%.1f", ang));
    debug5(DRS, DD, "  transforming (%s, %s) -> (%s, %s) + %s",
      EchoLength( (int) ycorners[i].x), EchoLength( (int) ycorners[i].y),
      EchoLength( (int) pol.radius), buff1, buff2);
    pol.angle += ang;
    polar_to_rect(pol, xcorner);
    ifdebug(DRS, DD, sprintf(buff1, "%.1f", pol.angle));
    debug4(DRS, DD, "    transforming (%s, %s) -> (%s, %s)",
      EchoLength( (int) pol.radius), buff1,
      EchoLength( (int) xcorner.x), EchoLength( (int) xcorner.y) );
    maxx = max(maxx, xcorner.x);    minx = min(minx, xcorner.x);
    maxy = max(maxy, xcorner.y);    miny = min(miny, xcorner.y);
  }

  /* store sizes back into x and return */
  *xcb = - (int) minx;    *xcf  =   (int) maxx;
  *xrb =   (int) maxy;    *xrf  = - (int) miny;
  debug0(DRS, D, "RotateSize returning.");
  debug4(DRS, DD, "  xcb %s, xcf %s, xrb %s, xrf %s",
    EchoLength(*xcb), EchoLength(*xcf),
    EchoLength(*xrb), EchoLength(*xrf));
} /* end RotateSize */
