/*@z17.c:Gap Widths:GetGap(), MinGap(), ExtraGap(), ActualGap()@**************/
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
/*  FILE:         z17.c                                                      */
/*  MODULE:       Gap Widths                                                 */
/*  EXTERNS:      GetGap(), MinGap(), ExtraGap(), ActualGap(), EchoGap()     */
/*                                                                           */
/*****************************************************************************/
/* #include <math.h> */
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  GetGap(x, style, res_gap, res_inc)                                       */
/*                                                                           */
/*  Object x is expected to be a WORD containing a gap:                      */
/*                                                                           */
/*      <gap>        ::=  [ <increment> ] <width> [ <mode> ]                 */
/*                   ::=                                                     */
/*      <width>      ::=  <unsigned number> <units>                          */
/*      <units>      ::=  c  |  i  |  p  |  m  |  f  |  s                    */
/*                   ::=  v  |  w  |  b  |  r  |  d                          */
/*      <mode>       ::=  e  |  h  |  x  |  o  |  k  |  t                    */
/*      <increment>  ::=  +  |  -                                            */
/*                                                                           */
/*  Set *res_gap to the gap in the strings of x; *res_inc is the increment.  */
/*  The gap is calculated using the given style.                             */
/*  If the gap is empty, this is a synonym for 0ie.                          */
/*  If there is an error, GetGap prints a message and returns 0ie.           */
/*                                                                           */
/*****************************************************************************/
#define setwidths(x, y) w = x; units(*res_gap) = y;  break;

GetGap(x, style, res_gap, res_inc)
OBJECT x;  STYLE *style;  GAP *res_gap;  unsigned *res_inc;
{ int w;  float num; 
  unsigned char *str;

  debug2(DGW, D, "GetGap( %s, %s, res_gap, res_inc )",
	EchoObject(null, x), EchoStyle(style));

  width(*res_gap) = 0;  units(*res_gap) = FIXED_UNIT;
  mode(*res_gap)  = EDGE_MODE;  *res_inc = ABS;

  /* make sure we have a WORD argument */
  if( type(x) != WORD )
  { Error(WARN, &fpos(x), "gap is not a simple word");
    debug1(DGW, D, "GetGap failing (type(x) = %s)", Image(type(x)));
    return;
  }
  str = string(x);

  /* if word is empty, return 0ie */
  if( *str == '\0' )
  { debug0(DGW, D, "GetGap returning (null word)");
    return;
  }

  /* find the gap increment */
  if( *str == '+' )       *res_inc = INC, str++;
  else if( *str == '-' )  *res_inc = DEC, str++;

  /* read the gap width */
  if( sscanf(str, "%f", &num) != 1 )
  { Error(WARN, &fpos(x), "width missing from %s", string(x));
    Error(WARN, &fpos(x), "reminder: /, | and & characters %s",
		"must be enclosed in double quotes");
    debug0(DGW, D, "GetGap failing (width missing)");
    return;
  }
  while( (*str >= '0' && *str <= '9') || *str == '.' )  str++;

  /* find the units, calculate length, and check for reasonableness */
  switch( *str )
  {
    case 'c':	setwidths( num * CM,                        FIXED_UNIT );
    case 'i':	setwidths( num * IN,                        FIXED_UNIT );
    case 'p':	setwidths( num * PT,                        FIXED_UNIT );
    case 'm':	setwidths( num * EM,                        FIXED_UNIT );
    case 'f':	setwidths( num * FontSize(font(*style), x), FIXED_UNIT );
    case 's':	setwidths( num * width(space_gap(*style)),  FIXED_UNIT );
    case 'v':	setwidths( num * width(line_gap(*style)),   FIXED_UNIT );
    case 'w':   setwidths( num * FR,                        NEXT_UNIT  );
    case 'b':	setwidths( num * FR,                        FRAME_UNIT );
    case 'r':	setwidths( num * FR,                        AVAIL_UNIT );

    case 'd':	if( *res_inc == DEC ) num = - num;
		*res_inc = ABS;
		while( num >= 360.0 ) num -= 360.0;
		while( num <= -360.0 ) num += 360.0;
		assert( (num >= -360) && (num <= 360), "GetGap: degrees!" );
		setwidths( num * DG,                        DEG_UNIT   );

    default:	Error(WARN, &fpos(x), "units letter missing from %s",string(x));
		debug0(DGW, D, "GetGap failing (units letter missing)");
		return;
  }

  if( units(*res_gap) == AVAIL_UNIT && w > FR )
  { Error(WARN, &fpos(x), "%.1fr too large; replaced with 1.0r", num);
    w = FR;
  }
  else if( w > MAX_LEN )
  { assert( units(*res_gap) != DEG_UNIT, "GetGap: oversize degrees!" );
    Error(WARN, &fpos(x), "length %s is too large - max (%dc) substituted",
		string(x), MAX_LEN/CM);
    w = MAX_LEN;
  }
  width(*res_gap) = w;

  /* find the gap mode */
  switch( *++str )
  {
    case 'e':
    case '\0':	mode(*res_gap) = EDGE_MODE;	break;
    case 'h':	mode(*res_gap) = HYPH_MODE;	break;
    case 'x':	mode(*res_gap) = MARK_MODE;	break;
    case 'o':	mode(*res_gap) = OVER_MODE;	break;
    case 'k':	mode(*res_gap) = KERN_MODE;	break;
    case 't':	mode(*res_gap) = TAB_MODE;	break;

    default:	Error(WARN, &fpos(x), "unknown gap mode in %s",string(x));
		debug0(DGW, D, "GetGap failing (spacing mode)");
		return;
  }

  if( *str != '\0' && *++str != '\0' )
    Error(WARN, &fpos(x), "invalid width or gap %s", string(x));

  debug2(DGW, D, "GetGap returning (res_gap = %s, res_inc = %s)",
    EchoGap(res_gap), Image( (int) *res_inc) );
} /* end GetGap */


/*@@**************************************************************************/
/*                                                                           */
/*  LENGTH MinGap(a, b, c, xgap)                                             */
/*                                                                           */
/*  Returns the minimum possible separation between the marks of two         */
/*  objects with the given intervening gap.                                  */
/*  The first object has fwd value a, the second has back value b and fwd c. */
/*                                                                           */
/*****************************************************************************/

LENGTH MinGap(a, b, c, xgap)
LENGTH a, b, c;  GAP *xgap;
{ LENGTH res;  int w;
  switch( units(*xgap) )
  {
    case FIXED_UNIT:	w = width(*xgap);
			break;

    case FRAME_UNIT:	w = 0;
			break;

    case AVAIL_UNIT:	w = 0;
			break;

    case NEXT_UNIT:	w = width(*xgap) * (b + c) / FR;
			break;

    default:		Error(INTERN, no_fpos, "MinGap: units = %d",
				units(*xgap));
			break;
  }
  switch( mode(*xgap) )
  {
    case NO_MODE:	Error(INTERN, no_fpos, "MinGap: NO_MODE");
			res = 0;
			break;

    case ADD_HYPH:
    case HYPH_MODE:
    case EDGE_MODE:	res = min(MAX_LEN, a + w + b);
			break;

    case MARK_MODE:	res = max(w, a + b);
			break;

    case OVER_MODE:	res = w;
			break;

    case KERN_MODE:	res = max(max(a, b), w);
			break;

    case TAB_MODE:	res = a + b;
			break;

    default:		Error(INTERN, no_fpos, "MinGap: %d", mode(*xgap));
			res = 0;
			break;

  }
  debug5(DGW, D, "MinGap( _,%s  %s  %s,%s ) = %s", EchoLength(a),
	EchoGap(xgap), EchoLength(b), EchoLength(c), EchoLength(res) );
  return res;
} /* end MinGap */


/*@@**************************************************************************/
/*                                                                           */
/*  LENGTH ExtraGap(a, b, xgap, dir)                                         */
/*                                                                           */
/*  Consider two objects, the first with forward length a, the second with   */
/*  back length b.  The objects are separated by the given gap.              */
/*  If dir == FWD, ExtraGap returns the maximum amount that a could be       */
/*  increased without increasing MinGap(a, b, c, xgap).                      */
/*  If dir == BACK, similarly for b.                                         */
/*                                                                           */
/*****************************************************************************/

LENGTH ExtraGap(a, b, xgap, dir)
LENGTH a, b; GAP *xgap;  int dir;
{ LENGTH tmp, res;
  LENGTH w = units(*xgap) == FIXED_UNIT ? width(*xgap) : 0;
  switch( mode(*xgap) )
  {
    case NO_MODE:	Error(INTERN, no_fpos, "ExtraGap: NO_MODE");
			res = 0;
			break;

    case ADD_HYPH:
    case HYPH_MODE:
    case EDGE_MODE:	res = 0;
			break;

    case MARK_MODE:	res = max(0, w - a - b);
			break;

    case OVER_MODE:	res = MAX_LEN;
			break;

    case KERN_MODE:	tmp = max(a, max(b, w));
			res = dir == BACK ? tmp - b : tmp - a;
			break;

    case TAB_MODE:	res = 0;
			break;

    default:		Error(INTERN, no_fpos, "ExtraGap: %d", mode(*xgap));
			res = 0;
			break;

  }
  debug5(DGW, DD, "ExtraGap( %s, %s, %s, %s ) = %s", EchoLength(a),
		EchoLength(b), EchoGap(xgap), Image(dir), EchoLength(res));
  return res;
} /* end ExtraGap */


/*@@**************************************************************************/
/*                                                                           */
/*  LENGTH ActualGap(a, b, c, xgap, f, mk)                                   */
/*                                                                           */
/*  Returns the actual separation between the marks of an object of size     */
/*  (?, a) and an object of size (b, c) separated by gap *xgap in a frame    */
/*  of size f; the first object lies at mk in the frame (0 <= mk <= f).      */
/*                                                                           */
/*****************************************************************************/

LENGTH ActualGap(a, b, c, xgap, f, mk)
LENGTH a, b, c;  GAP *xgap;  LENGTH f, mk;
{ LENGTH res;  int w, w2;
  switch( units(*xgap) )
  {
    case FIXED_UNIT:	w = width(*xgap);
			break;

    case FRAME_UNIT:	w = (width(*xgap) * f) / FR;
			break;

    case AVAIL_UNIT:	w = (width(*xgap) * (f - b - c)) / FR;
			w = max(w, 0);
			break;

    case NEXT_UNIT:	w = width(*xgap) * (b + c) / FR;
			break;

    default:		Error(INTERN, no_fpos, "ActualGap: units = %d",
				units(*xgap));
			break;
  }
  switch( mode(*xgap) )
  {
    case NO_MODE:	Error(INTERN, no_fpos, "ActualGap: NO_MODE");
			w2 = 0;
			break;

    case ADD_HYPH:
    case HYPH_MODE:
    case EDGE_MODE:	w2 = a + w + b;
			break;

    case MARK_MODE:	w2 = max( w, a + b );
			break;

    case OVER_MODE:	w2 = w;
			break;

    case KERN_MODE:	w2 = max( max(a, b), w);
			break;

    case TAB_MODE:	w2 = w + b - mk;
			w2 = max( w2, a + b );
			break;

    default:		Error(INTERN,no_fpos,"ActualGap: mode %d", mode(*xgap));
			w2 = 0;
			break;
  }
  res = min(MAX_LEN, w2);
  debug6(DGW, D, "ActualGap( _,%s %s %s,%s; %s ) = %s",
	EchoLength(a), EchoGap(xgap), EchoLength(b),
	EchoLength(c), EchoLength(f), EchoLength(res) );
  return res;
} /* end ActualGap */


/*@@**************************************************************************/
/*                                                                           */
/*  unsigned char *EchoGap(xgap)                                             */
/*                                                                           */
/*  Returns a static string showing the indicated xgap.                      */
/*                                                                           */
/*****************************************************************************/
#if DEBUG_ON

unsigned char *EchoGap(xgap)
GAP *xgap;
{ unsigned char *letter = (unsigned char *) "?ehxokt";  unsigned char c;
  static unsigned char buff[20];
  assert( mode(*xgap) <= 6, "EchoGap: mode(*xgap)" );
  c = letter[mode(*xgap)];
  switch( units(*xgap) )
  {
    case 0:		sprintf(buff, "(none)%c", c);
			break;

    case FIXED_UNIT:	sprintf(buff, "%.1fc%c", (float) width(*xgap) / CM, c);
			break;

    case NEXT_UNIT:	sprintf(buff, "%.1fw%c", (float) width(*xgap) / FR, c);
			break;

    case FRAME_UNIT:	sprintf(buff, "%.1fb%c", (float) width(*xgap) / FR, c);
			break;

    case AVAIL_UNIT:	sprintf(buff, "%.1fr%c", (float) width(*xgap) / FR, c);
			break;

    case DEG_UNIT:	sprintf(buff, "%.1fd", (float) width(*xgap) / DG);
			break;

    default:		Error(INTERN, no_fpos, "EchoGap: %d", units(*xgap));
			break;

  }
  return buff;
} /* end EchoGap */
#endif
