/*@z17.c:Gap Widths:GetGap()@*************************************************/
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
/*  FILE:         z17.c                                                      */
/*  MODULE:       Gap Widths                                                 */
/*  EXTERNS:      GetGap(), MinGap(), ExtraGap(), ActualGap(), EchoGap()     */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  GetGap(x, style, res_gap, res_inc)                                       */
/*                                                                           */
/*  Object x is expected to be a WORD or QWORD containing a gap:             */
/*                                                                           */
/*      <gap>        ::=  [ <increment> ] <width> [ <mode> ]                 */
/*                   ::=                                                     */
/*      <width>      ::=  <unsigned number> <units>                          */
/*      <units>      ::=  c  |  i  |  p  |  m  |  f  |  s                    */
/*                   ::=  v  |  w  |  b  |  r  |  d  |  y  |  z              */
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

void GetGap(OBJECT x, STYLE *style, GAP *res_gap, unsigned *res_inc)
{ int w;  float num; 
  FULL_CHAR *str;

  debug2(DGW, DD, "GetGap( %s, %s, res_gap, res_inc )",
	EchoObject(x), EchoStyle(style));

  width(*res_gap) = 0;  units(*res_gap) = FIXED_UNIT;
  mode(*res_gap)  = EDGE_MODE;  *res_inc = GAP_ABS;

  /* make sure we have a WORD or QWORD argument */
  if( !is_word(type(x)) )
  { Error(17, 1, "gap is not a simple word", WARN, &fpos(x));
    debug1(DGW, DD, "GetGap failing (type(x) = %s)", Image(type(x)));
    return;
  }
  str = string(x);

  /* if word is empty, return 0ie */
  if( *str == '\0' )
  { debug0(DGW, DD, "GetGap returning (null word)");
    return;
  }

  /* find the gap increment */
  if( *str == CH_INCGAP )       *res_inc = GAP_INC, str++;
  else if( *str == CH_DECGAP )  *res_inc = GAP_DEC, str++;

  /* read the gap width */
  if( sscanf((char *) str, "%f", &num) != 1 )
  { Error(17, 2, "width missing from %s", WARN, &fpos(x), string(x));
    Error(17, 3, "%s, %s and %s must be enclosed in double quotes",
      WARN, &fpos(x), KW_VCAT_NJ, KW_HCAT_NJ, KW_ACAT_NJ);
    debug0(DGW, DD, "GetGap failing (width missing)");
    return;
  }
  while( numericchar(*str) )  str++;

  /* find the units, calculate length, and check for reasonableness */
  switch( *str )
  {
    case CH_UNIT_CM:	setwidths( num*CM,                        FIXED_UNIT );
    case CH_UNIT_IN:	setwidths( num*IN,                        FIXED_UNIT );
    case CH_UNIT_PT:	setwidths( num*PT,                        FIXED_UNIT );
    case CH_UNIT_EM:	setwidths( num*EM,                        FIXED_UNIT );
    case CH_UNIT_FT:	setwidths( num*FontSize(font(*style), x), FIXED_UNIT );
    case CH_UNIT_SP:	setwidths( num*width(space_gap(*style)),  FIXED_UNIT );
    case CH_UNIT_VS:	setwidths( num*width(line_gap(*style)),   FIXED_UNIT );
    case CH_UNIT_YU:	setwidths( num*yunit(*style),             FIXED_UNIT );
    case CH_UNIT_ZU:	setwidths( num*zunit(*style),             FIXED_UNIT );
    case CH_UNIT_WD:	setwidths( num*FR,                        NEXT_UNIT  );
    case CH_UNIT_BD:	setwidths( num*FR,                        FRAME_UNIT );
    case CH_UNIT_RL:	setwidths( num*FR,                        AVAIL_UNIT );

    case CH_UNIT_DG:	if( *res_inc == GAP_DEC ) num = - num;
			*res_inc = GAP_ABS;
			while( num >  180.0 ) num -= 360.0;
			while( num < -180.0 ) num += 360.0;
			assert((num>=-180.0) && (num<=180.0), "GetGap: dg!");
			setwidths( num*DG,                        DEG_UNIT   );

    default:	Error(17, 4, "units letter missing from %s",
		  WARN, &fpos(x), string(x));
		debug0(DGW, DD, "GetGap failing (units letter missing)");
		return;
  }

  if( units(*res_gap) == AVAIL_UNIT && w > FR )
  { Error(17, 5, "%.1fr too large (1.0r substituted)", WARN, &fpos(x), num);
    w = FR;
  }
  else if( w > MAX_LEN )
  { assert( units(*res_gap) != DEG_UNIT, "GetGap: oversize degrees!" );
    Error(17, 6, "length %s is too large (maximum %dc substituted)",
      WARN, &fpos(x), string(x), MAX_LEN/CM);
    w = MAX_LEN;
  }
  width(*res_gap) = w;

  /* find the gap mode */
  switch( *++str )
  {
    case CH_MODE_EDGE:
    case '\0':		mode(*res_gap) = EDGE_MODE;	break;
    case CH_MODE_HYPH:	mode(*res_gap) = HYPH_MODE;	break;
    case CH_MODE_MARK:	mode(*res_gap) = MARK_MODE;	break;
    case CH_MODE_OVER:	mode(*res_gap) = OVER_MODE;	break;
    case CH_MODE_KERN:	mode(*res_gap) = KERN_MODE;	break;
    case CH_MODE_TABL:	mode(*res_gap) = TAB_MODE;	break;

    default:	Error(17, 7, "unknown gap mode in %s",
		  WARN, &fpos(x), string(x));
		debug0(DGW, DD, "GetGap failing (spacing mode)");
		return;
  }

  if( *str != '\0' && *++str != '\0' )
    Error(17, 8, "invalid width or gap %s", WARN, &fpos(x), string(x));

  debug2(DGW, DD, "GetGap returning (res_gap = %s, res_inc = %s)",
    EchoGap(res_gap), Image( (int) *res_inc) );
} /* end GetGap */


/*@::MinGap()@****************************************************************/
/*                                                                           */
/*  LENGTH MinGap(a, b, c, xgap)                                             */
/*                                                                           */
/*  Returns the minimum possible separation between the marks of two         */
/*  objects with the given intervening gap.                                  */
/*  The first object has fwd value a, the second has back value b and fwd c. */
/*                                                                           */
/*****************************************************************************/

LENGTH MinGap(LENGTH a, LENGTH b, LENGTH c, GAP *xgap)
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

    default:		assert(FALSE, "MinGap: units");
			break;
  }
  switch( mode(*xgap) )
  {
    case NO_MODE:	assert(FALSE, "MinGap: NO_MODE");
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

    default:		assert(FALSE, "MinGap: mode");
			res = 0;
			break;

  }
  debug5(DGW, DD, "MinGap( _,%s  %s  %s,%s ) = %s", EchoLength(a),
	EchoGap(xgap), EchoLength(b), EchoLength(c), EchoLength(res) );
  return res;
} /* end MinGap */


/*@::ExtraGap()@**************************************************************/
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

LENGTH ExtraGap(LENGTH a, LENGTH b, GAP *xgap, int dir)
{ LENGTH tmp, res;
  LENGTH w = units(*xgap) == FIXED_UNIT ? width(*xgap) : 0;
  switch( mode(*xgap) )
  {
    case NO_MODE:	assert(FALSE, "ExtraGap: NO_MODE");
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

    default:		assert(FALSE, "ExtraGap: mode");
			res = 0;
			break;

  }
  debug5(DGW, DD, "ExtraGap( %s, %s, %s, %s ) = %s", EchoLength(a),
		EchoLength(b), EchoGap(xgap), Image(dir), EchoLength(res));
  return res;
} /* end ExtraGap */


/*@::ActualGap()@*************************************************************/
/*                                                                           */
/*  LENGTH ActualGap(a, b, c, xgap, f, mk)                                   */
/*                                                                           */
/*  Returns the actual separation between the marks of an object of size     */
/*  (?, a) and an object of size (b, c) separated by gap *xgap in a frame    */
/*  of size f; the first object lies at mk in the frame (0 <= mk <= f).      */
/*                                                                           */
/*****************************************************************************/

LENGTH ActualGap(LENGTH a, LENGTH b, LENGTH c, GAP *xgap, LENGTH f, LENGTH mk)
{ LENGTH res;  int w, w2;
  switch( units(*xgap) )
  {
    case FIXED_UNIT:	w = width(*xgap);
			break;

    case FRAME_UNIT:	if( width(*xgap) > FR )
			  w = MAX_LEN;
			else
			  w = (width(*xgap) * f) / FR;
			break;

    case AVAIL_UNIT:	w = (width(*xgap) * (f - b - c)) / FR;
			w = max(w, 0);
			break;

    case NEXT_UNIT:	w = width(*xgap) * (b + c) / FR;
			break;

    default:		assert(FALSE, "ActualGap: units");
			break;
  }
  switch( mode(*xgap) )
  {
    case NO_MODE:	assert(FALSE, "ActualGap: NO_MODE");
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

    default:		assert(FALSE, "ActualGap: mode");
			w2 = 0;
			break;
  }
  res = min(MAX_LEN, w2);
  debug6(DGW, D, "ActualGap( _,%s %s %s,%s; %s ) = %s",
	EchoLength(a), EchoGap(xgap), EchoLength(b),
	EchoLength(c), EchoLength(f), EchoLength(res) );
  return res;
} /* end ActualGap */


/*@::EchoGap()@***************************************************************/
/*                                                                           */
/*  FULL_CHAR *EchoGap(xgap)                                                 */
/*                                                                           */
/*  Returns a static string showing the indicated xgap.                      */
/*                                                                           */
/*****************************************************************************/
#if DEBUG_ON

FULL_CHAR *EchoGap(GAP *xgap)
{ char *letter = "?ehxokt";  char c;  FULL_CHAR *res;
  static int i = 0;
  static char buff[3][20];
  assert( mode(*xgap) <= 6, "EchoGap: mode(*xgap)" );
  c = letter[mode(*xgap)];
  switch( units(*xgap) )
  {
    case 0:	     sprintf(buff[i], "(none)%c", c);
		     break;

    case FIXED_UNIT: sprintf(buff[i], "%.1fc%c", (float) width(*xgap) / CM, c);
		     break;

    case NEXT_UNIT:  sprintf(buff[i], "%.1fw%c", (float) width(*xgap) / FR, c);
		     break;

    case FRAME_UNIT: sprintf(buff[i], "%.1fb%c", (float) width(*xgap) / FR, c);
		     break;

    case AVAIL_UNIT: sprintf(buff[i], "%.1fr%c", (float) width(*xgap) / FR, c);
		     break;

    case DEG_UNIT:   sprintf(buff[i], "%.1fd", (float) width(*xgap) / DG);
		     break;

    default:	     assert(FALSE, "EchoGap: units");
		     break;

  }
  res = AsciiToFull(buff[i]);
  i = (i + 1) % 3;
  return res;
} /* end EchoGap */
#endif
