/*@z15.c:Size Constraints:MinConstraint(), EnlargeToConstraint()@*************/
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
/*  FILE:         z15.c                                                      */
/*  MODULE:       Size Constraints                                           */
/*  EXTERNS:      MinConstraint(), EnlargeToConstraint(),                    */
/*                ReflectConstraint(), SemiRotateConstraint(),               */
/*                RotateConstraint(), InvScaleConstraint(), Constrained(),   */
/*                EchoConstraint(), DebugConstrained()                       */
/*                                                                           */
/*****************************************************************************/
#include <math.h>
#ifndef M_PI
#define M_PI       3.1415926535897931160E0
#endif

#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  MinConstraint(xc, yc)                                                    */
/*                                                                           */
/*  Replace *xc by the minimum of the two constraints *xc and *yc.           */
/*                                                                           */
/*****************************************************************************/

void MinConstraint(CONSTRAINT *xc, CONSTRAINT *yc)
{ bc(*xc)  = min(bc(*xc),  bc(*yc));
  bfc(*xc) = min(bfc(*xc), bfc(*yc));
  fc(*xc)  = min(fc(*xc),  fc(*yc));
} /* end MinConstraint */


/*****************************************************************************/
/*                                                                           */
/*  EnlargeToConstraint(b, f, c)                                             */
/*                                                                           */
/*  Enlarge *b,*f to its largest possible value within constraint *c.        */
/*                                                                           */
/*****************************************************************************/

void EnlargeToConstraint(LENGTH *b, LENGTH *f, CONSTRAINT *c)
{
  *f = min(bfc(*c) - *b, fc(*c));
} /* end EnlargeToConstraint */


/*****************************************************************************/
/*                                                                           */
/*  float ScaleToConstraint(b, f, c)                                         */
/*                                                                           */
/*  Return the scale factor needed to scale object of size b, f down so it   */
/*  has a size which fits tightly into constraint c.                         */
/*                                                                           */
/*****************************************************************************/

int ScaleToConstraint(LENGTH b, LENGTH f, CONSTRAINT *c)
{ float scale_factor;  int res;
  debug3(DSC, DD, "ScaleToConstraint(%s, %s, %s)", EchoLength(b),
    EchoLength(f), EchoConstraint(c));
  scale_factor = 1.0;
  if( b     > 0 )  scale_factor = min(scale_factor, (float) bc(*c)/b       );
  if( b + f > 0 )  scale_factor = min(scale_factor, (float) bfc(*c)/(b + f));
  if(     f > 0 )  scale_factor = min(scale_factor, (float) fc(*c)/f       );
  res = scale_factor * SF;
  debug2(DSC, DD, "ScaleToConstraint returning %.2f (%d)", scale_factor, res);
  return res;
} /* end ScaleToConstraint */


/*@::InvScaleConstraint(), ReflectConstraint(), etc.@*************************/
/*                                                                           */
/*  InvScaleConstraint(yc, sf, xc)                                           */
/*                                                                           */
/*  Scale constraint xc to the inverse of the scale factor sf.               */
/*                                                                           */
/*****************************************************************************/

void InvScaleConstraint(CONSTRAINT *yc, LENGTH sf, CONSTRAINT *xc)
{
#if DEBUG_ON
  char buff[10];
#endif
  ifdebug(DSC, DD, sprintf(buff, "%.3f", (float) sf / SF));
  debug2(DSC, DD, "InvScaleConstraint(yc, %s, %s)", buff, EchoConstraint(xc));
  assert( sf > 0, "InvScaleConstraint: sf <= 0!" );
  bc(*yc)  = bc(*xc)  == MAX_LEN ? MAX_LEN : min(MAX_LEN, bc(*xc)  * SF / sf);
  bfc(*yc) = bfc(*xc) == MAX_LEN ? MAX_LEN : min(MAX_LEN, bfc(*xc) * SF / sf);
  fc(*yc)  = fc(*xc)  == MAX_LEN ? MAX_LEN : min(MAX_LEN, fc(*xc)  * SF / sf);
  debug1(DSC, DD, "InvScaleConstraint returning %s", EchoConstraint(yc));
} /* end InvScaleConstraint */


/*****************************************************************************/
/*                                                                           */
/*  ReflectConstraint(xc, yc)                                                */
/*                                                                           */
/*  Set xc to the constraint which is yc with its back and forward reversed. */
/*                                                                           */
/*****************************************************************************/

#define ReflectConstraint(xc, yc)  SetConstraint(xc, fc(yc), bfc(yc), bc(yc))


/*****************************************************************************/
/*                                                                           */
/*  static SemiRotateConstraint(xc, u, v, angle, yc)                         */
/*                                                                           */
/*  Used by RotateConstraint to calculate one rotated constraint.            */
/*                                                                           */
/*****************************************************************************/

static void SemiRotateConstraint(CONSTRAINT *xc, LENGTH u, LENGTH v,
float angle, CONSTRAINT *yc)
{ float cs, sn;
#if DEBUG_ON
  char buff[20];
#endif
  ifdebug(DSC, DD, sprintf(buff, "%.1f", angle * 360.0 / (2 * M_PI)));
  debug4(DSC, DD, "SemiRotateConstraint(xc, %s, %s, %sd, %s",
    EchoLength(u), EchoLength(v), buff, EchoConstraint(yc));
  cs = cos(angle);  sn = sin(angle);
  if( fabs(cs) < 1e-6 )
    SetConstraint(*xc, MAX_LEN, MAX_LEN, MAX_LEN);
  else
    SetConstraint(*xc,
      min(MAX_LEN, (bc(*yc) - u * sn) / cs),
      min(MAX_LEN, (bfc(*yc) - u * sn - v * sn) / cs),
      min(MAX_LEN, (fc(*yc) - v * sn) / cs) );
  debug1(DSC, DD, "SemiRotateConstraint returning %s", EchoConstraint(xc));
} /* end SemiRotateConstraint */


/*@::RotateConstraint()@******************************************************/
/*                                                                           */
/*  RotateConstraint(c, y, angle, hc, vc, dim)                               */
/*                                                                           */
/*  Take the object angle @Rotate y, which is supposed to be constrained     */
/*  horizontally by hc and vertically by vc, and determine a constraint      */
/*  (either horizontal or vertical, depending on dim) for y.                 */
/*                                                                           */
/*  The constraint returned is a trigonometric function of all these         */
/*  parameters, including the present size of y in dimension 1-dim.          */
/*                                                                           */
/*****************************************************************************/

void RotateConstraint(CONSTRAINT *c, OBJECT y, LENGTH angle,
CONSTRAINT *hc, CONSTRAINT *vc, int dim)
{ CONSTRAINT c1, c2, c3, dc;  float theta, psi;
#if DEBUG_ON
  char buff[20];
#endif
  ifdebug(DSC, DD, sprintf(buff, "%.1f", (float) angle / DG ));
  debug4(DSC, DD, "RotateConstraint(c, y, %sd, %s, %s, %s)",
	buff, EchoConstraint(hc), EchoConstraint(vc), dimen(dim));

  /* work out angle in radians between 0 and 2*PI */
  theta = (float) angle * 2 * M_PI / (float) (DG * 360);
  while( theta < 0 ) theta += 2 * M_PI;
  while( theta >= 2 * M_PI ) theta -= 2 * M_PI;
  assert( 0 <= theta && theta <= 2 * M_PI, "RotateConstraint: theta!" );

  /* determine theta, c1, and c2 depending on which quadrant we are in */
  if( theta <= M_PI / 2.0 )   /* first quadrant */
  { theta = theta;
    CopyConstraint(c1, *hc);
    CopyConstraint(c2, *vc);
  }
  else if ( theta <= M_PI )   /* second quadrant */
  { theta -= M_PI / 2.0;
    ReflectConstraint(c1, *vc);
    CopyConstraint(c2, *hc);
  }
  else if ( theta <= 3.0 * M_PI / 2.0 )   /* third quadrant */
  { theta -= M_PI;
    ReflectConstraint(c1, *hc);
    ReflectConstraint(c2, *vc);
  }
  else /* fourth quadrant */
  { theta -= 3.0 * M_PI / 2.0;
    CopyConstraint(c1, *vc);
    ReflectConstraint(c2, *hc);
  }
  psi = M_PI / 2.0 - theta;
  debug2(DSC, DD, "  c1: %s;  c2: %s", EchoConstraint(&c1), EchoConstraint(&c2));

  /* return the minimum of the two constraints, rotated */
  if( dim == COL )
  { SemiRotateConstraint(c, back(y, ROW), fwd(y, ROW), theta, &c1);
    ReflectConstraint(c3, c2);
    SemiRotateConstraint(&dc, fwd(y, ROW), back(y, ROW), psi, &c3);
    MinConstraint(c, &dc);
  }
  else
  { SemiRotateConstraint(c, back(y, COL), fwd(y, COL), psi, &c1);
    SemiRotateConstraint(&dc, fwd(y, COL), back(y, COL), theta, &c2);
    MinConstraint(c, &dc);
  }

  debug1(DSC, DD, "RotateConstraint returning %s", EchoConstraint(c));
} /* end RotateConstraint */


/*@::CatConstrained()@********************************************************/
/*                                                                           */
/*  static CatConstrained(x, xc, ratm, y, dim)                               */
/*                                                                           */
/*  Calculate the size constraint of object x, as for Constrained below.     */
/*  y is the enclosing VCAT etc. object;  ratm is TRUE if a ^ lies after     */
/*  x anywhere.  dim is COL or ROW.                                          */
/*                                                                           */
/*  The meaning of the key variables is as follows:                          */
/*                                                                           */
/*  be       The amount by which back(x, dim) can increase from zero         */
/*           without having any impact on size(y, dim).  Thereafter,         */
/*           any increase causes an equal increase in size(y, dim).          */
/*                                                                           */
/*  fe       The amount by which fwd(x, dim) can increase from zero          */
/*           without having any impact on size(y, dim).  Thereafter,         */
/*           any increase causes an equal increase in size(y, dim).          */
/*                                                                           */
/*  backy,   The value that back(y, dim) and fwd(y, dim) would have if x     */
/*  fwdy     was definite with size 0,0.  They will in general be larger     */
/*           than the present values if x is indefinite, and smaller         */
/*           if x is definite, although it depends on marks and gaps.        */
/*                                                                           */
/*****************************************************************************/

static void CatConstrained(OBJECT x, CONSTRAINT *xc, BOOLEAN ratm,
OBJECT y, int dim)
{ int side;			/* the size of y that x is on: BACK, ON, FWD */
  CONSTRAINT yc;		/* constraints on y                          */
  LENGTH backy, fwdy;		/* back(y), fwd(y) would be if x was (0, 0)  */
  LENGTH be, fe;		/* amount back(x), fwd(x) can be for free    */
  LENGTH beffect, feffect;	/* scratch variables for calculations        */
  LENGTH seffect;		/* scratch variables for calculations        */
  OBJECT link, sg, pg;	/* link to x, its successor and predecessor  */
  OBJECT prec_def, sd;	/* definite object preceding (succeeding) x  */
  int tb, tbf, tf, tbc, tbfc, tfc, mxy, myz;

  Constrained(y, &yc, dim);
  if( constrained(yc) )
  {
    /* find the link of x, and its neighbours and their links */
    link = UpDim(x, dim);
    SetNeighbours(link, ratm, &pg, &prec_def, &sg, &sd, &side);

    /* amount of space available at x without changing the size of y */
    be = pg == nilobj ? 0 : ExtraGap(fwd(prec_def, dim), 0, &gap(pg), BACK);
    fe = sg == nilobj ? 0 : ExtraGap(0, back(sd, dim),      &gap(sg), FWD);

    if( is_indefinite(type(x)) )
    {
      /* insert two lengths and delete one */
      beffect = pg==nilobj ? 0 : MinGap(fwd(prec_def, dim), 0, 0, &gap(pg));
      feffect = sg==nilobj ? 0 : MinGap(0, back(sd,dim), fwd(sd,dim), &gap(sg));
      seffect = pg==nilobj ?
	  sg == nilobj ? 0 : back(sd, dim) :
	  sg == nilobj ? fwd(prec_def, dim) :
	    MinGap(fwd(prec_def, dim), back(sd, dim), fwd(sd, dim), &gap(sg));

      switch( side )
      {
	case BACK:	backy = back(y, dim) + beffect + feffect - seffect;
			fwdy  = fwd(y, dim);
			break;

	case ON:	/* must be first, other cases prohibited */
			backy = 0;
			fwdy = fwd(y, dim) + feffect;
			break;

	case FWD:	backy = back(y, dim);
			fwdy  = fwd(y, dim) + beffect + feffect - seffect;
			break;
      }
    }

    else /* x is definite */

    { beffect = pg == nilobj ? back(x, dim) :
	MinGap(fwd(prec_def, dim), back(x,dim), fwd(x,dim), &gap(pg)) -
	MinGap(fwd(prec_def, dim), 0,           0,          &gap(pg));

      feffect = sg == nilobj ? fwd(x, dim) :
	MinGap(fwd(x, dim), back(sd, dim), fwd(sd, dim), &gap(sg)) -
	MinGap(0,           back(sd, dim), fwd(sd, dim), &gap(sg));

      switch( side )
      {
	case BACK:	backy = back(y, dim) - beffect - feffect;
			fwdy  = fwd(y, dim);
			break;

	case ON:	backy = back(y, dim) - beffect;
			fwdy  = fwd(y, dim)  - feffect;
			break;

	case FWD:	backy = back(y, dim);
			fwdy  = fwd(y, dim) - beffect - feffect;
			break;
      }
    }

    debug5(DSC, DDD, "side: %s, backy: %s, fwdy: %s, be: %s, fe: %s",
		Image(side), EchoLength(backy), EchoLength(fwdy),
		EchoLength(be), EchoLength(fe) );

    if( !FitsConstraint(backy, fwdy, yc) )
      SetConstraint(*xc, -1, -1, -1);
    else switch( side )
    {

      case BACK:
	
	tbc = bc(yc) == MAX_LEN ? MAX_LEN : bc(yc) - backy;
	tbfc = bfc(yc) == MAX_LEN ? MAX_LEN : bfc(yc) - backy - fwdy;
	mxy = min(tbc, tbfc);
	tb  = min(MAX_LEN, be + mxy);
	tbf = min(MAX_LEN, be + fe + mxy);
	tf  = min(MAX_LEN, fe + mxy);
	SetConstraint(*xc, tb, tbf, tf);
	break;


      case ON:
	
	tbc = bc(yc) == MAX_LEN ? MAX_LEN : bc(yc) - backy;
	tbfc = bfc(yc) == MAX_LEN ? MAX_LEN : bfc(yc) - backy - fwdy;
	tfc = fc(yc) == MAX_LEN ? MAX_LEN : fc(yc) - fwdy;
	mxy = min(tbc, tbfc);
	myz = min(tfc, tbfc);
	tb  = min(MAX_LEN, be + mxy);
	tbf = min(MAX_LEN, be + fe + tbfc);
	tf  = min(MAX_LEN, fe + myz);
	SetConstraint(*xc, tb, tbf, tf);
	break;
	

      case FWD:

	tfc = fc(yc) == MAX_LEN ? MAX_LEN : fc(yc) - fwdy;
	tbfc = bfc(yc) == MAX_LEN ? MAX_LEN : bfc(yc) - backy - fwdy;
	mxy = min(tfc, tbfc);
	tb  = min(MAX_LEN, be + mxy);
	tbf = min(MAX_LEN, be + fe + mxy);
	tf  = min(MAX_LEN, fe + mxy);
	SetConstraint(*xc, tb, tbf, tf);
	break;
	
    }
  } /* end if( constrained ) */
  else SetConstraint(*xc, MAX_LEN, MAX_LEN, MAX_LEN);
} /* end CatConstrained */


/*@::Constrained()@***********************************************************/
/*                                                                           */
/*  Constrained(x, xc, dim)                                                  */
/*                                                                           */
/*  Calculate the size constraint of object x, and return it in *xc.         */
/*                                                                           */
/*****************************************************************************/

void Constrained(OBJECT x, CONSTRAINT *xc, int dim)
{ OBJECT y, link, lp, rp, z, tlink, g;  CONSTRAINT yc, hc, vc;
  BOOLEAN ratm;  LENGTH xback, xfwd;  int tb, tf, tbf, tbc, tfc;
  debug2(DSC, DD, "[ Constrained( %s, xc, %s )", EchoObject(x), dimen(dim));
  assert( Up(x) != x, "Constrained: x has no parent!" );

  /* find y, the parent of x */
  link = UpDim(x, dim);  ratm = FALSE;
  for( tlink = NextDown(link);  type(tlink) == LINK;  tlink = NextDown(tlink) )
  { Child(g, tlink);
    if( type(g) == GAP_OBJ && mark(gap(g)) )  ratm = TRUE;
  }
  y = tlink;
  debug1(DSC, DDD, "parent y = %s", Image(type(y)));
  ifdebug(DSC, DDD, DebugObject(y));

  switch( type(y) )
  {
    case GRAPHIC:
    case ONE_COL:
    case ONE_ROW:
    case HCONTRACT:
    case VCONTRACT:
    case HEXPAND:
    case VEXPAND:
    case PADJUST:
    case HADJUST:
    case VADJUST:
    case SPLIT:
    
      Constrained(y, xc, dim);
      break;


    case VSCALE:
    case HSCALE:
    
      if( (dim == COL) != (type(y) == HSCALE) )  Constrained(y, xc, dim);
      else SetConstraint(*xc, MAX_LEN, MAX_LEN, MAX_LEN);
      break;


    case SCALE:

      Constrained(y, &yc, dim);
      InvScaleConstraint(xc,
	dim == COL ? bc(constraint(y)) : fc(constraint(y)), &yc);
      break;


    case ROTATE:
    
      Constrained(y, &hc, COL);  Constrained(y, &vc, ROW);
      RotateConstraint(xc, x, sparec(constraint(y)), &hc, &vc, dim);
      break;


    case WIDE:
    case HIGH:
    
      Constrained(y, xc, dim);
      if( (type(y)==WIDE) == (dim==COL) )  MinConstraint(xc, &constraint(y));
      break;


    case HSHIFT:
    case VSHIFT:

      if( (type(y) == HSHIFT) == (dim == COL) )
      { Constrained(y, &yc, dim);
	tf = FindShift(y, x, dim);
	SetConstraint(*xc,
	  min(bc(yc), bfc(yc)) - tf, bfc(yc), min(fc(yc), bfc(yc)) + tf);
      }
      else Constrained(y, xc, dim);
      break;


    case HEAD:
    
      if( dim == ROW ) SetConstraint(*xc, MAX_LEN, MAX_LEN, MAX_LEN);
      else
      {	CopyConstraint(yc, constraint(y));
	debug1(DSC, DD, "  head: %s; val is:", EchoConstraint(&yc));
	ifdebug(DSC, DD, DebugObject(y));
	goto REST_OF_HEAD;   /* a few lines down */
      }
      break;


    case COL_THR:
    case ROW_THR:

      assert( (type(y)==COL_THR) == (dim==COL), "Constrained: COL_THR!" );
      Constrained(y, &yc, dim);
      tb = bfc(yc) == MAX_LEN ? MAX_LEN : bfc(yc) - fwd(y, dim);
      tb = min(bc(yc), tb);
      tf = bfc(yc) == MAX_LEN ? MAX_LEN : bfc(yc) - back(y, dim);
      tf = min(fc(yc), tf);
      SetConstraint(*xc, tb, bfc(yc), tf);
      break;


    case VCAT:
    case HCAT:
    case ACAT:
    
      if( (type(y)==VCAT) == (dim==ROW) )
      {	CatConstrained(x, xc, ratm, y, dim);
	break;
      }
      Constrained(y, &yc, dim);
      if( !constrained(yc) )  SetConstraint(*xc, MAX_LEN, MAX_LEN, MAX_LEN);
      else
      {
	REST_OF_HEAD:
	/* let lp and rp be the links of the gaps delimiting */
	/* the components joined to x (or parent if no such) */
	for( lp = PrevDown(link);  lp != y;  lp = PrevDown(lp) )
	{ Child(z, lp);
	  if( type(z) == GAP_OBJ && !join(gap(z)) )  break;
	}
	for( rp = NextDown(link);  rp != y;  rp = NextDown(rp) )
	{ Child(z, rp);
	  if( type(z) == GAP_OBJ && !join(gap(z)) )  break;
	}
	if( lp == y && rp == y && !(type(y) == HEAD && seen_nojoin(y)) )
	{
	  /* if whole object is joined, do this */
          tb = bfc(yc) == MAX_LEN ? MAX_LEN : bfc(yc) - fwd(y, dim);
          tb = min(bc(yc), tb);
          tf = bfc(yc) == MAX_LEN ? MAX_LEN : bfc(yc) - back(y, dim);
          tf = min(fc(yc), tf);
          SetConstraint(*xc, tb, bfc(yc), tf);
	}
	else
	{
	  /* if // or || is present, do this */
	  xback = xfwd = 0;
	  for(link = NextDown(lp); link != rp;  link = NextDown(link) )
	  { Child(z, link);
	    if( type(z) == GAP_OBJ || is_index(type(z)) )  continue;
	    xback = max(xback, back(z, dim));  xfwd = max(xfwd, fwd(z, dim));
	  }
	  debug2(DSC, DD, "  lp != rp; xback,xfwd = %s,%s",
			EchoLength(xback), EchoLength(xfwd));
	  tbf = min(bfc(yc), fc(yc));
	  tbc = tbf == MAX_LEN ? MAX_LEN : tbf - xfwd;
	  tfc = tbf == MAX_LEN ? MAX_LEN : tbf - xback;
	  SetConstraint(*xc, tbc, tbf, tfc);
	}
      }
      break;


    default:  Error(15, 1, "Constrained: %s", INTERN, &fpos(y),Image(type(y)));
	      break;
  }
  debug1(DSC, DD, "] Constrained returning %s", EchoConstraint(xc));
} /* end Constrained */


/*@::EchoConstraint(), DebugConstrained()@************************************/
/*                                                                           */
/*  FULL_CHAR *EchoConstraint(c)                                             */
/*                                                                           */
/*  Returns a string showing constraint *c, in centimetres.                  */
/*                                                                           */
/*****************************************************************************/
#if DEBUG_ON

FULL_CHAR *EchoConstraint(CONSTRAINT *c)
{ static char str[2][40];
  static int i = 0;
  i = (i+1) % 2;
  sprintf(str[i], "<");
  switch( BackEnd )
  {
    case POSTSCRIPT:

      if( bc(*c)==MAX_LEN )  sprintf(&str[i][strlen(str[i])], "INF, ");
      else sprintf(&str[i][strlen(str[i])], "%.3fc, ", (float) bc(*c)/CM);
      if( bfc(*c)==MAX_LEN )  sprintf(&str[i][strlen(str[i])], "INF, ");
      else sprintf(&str[i][strlen(str[i])], "%.3fc, ", (float) bfc(*c)/CM);
      if( fc(*c)==MAX_LEN )  sprintf(&str[i][strlen(str[i])], "INF>");
      else sprintf(&str[i][strlen(str[i])], "%.3fc>", (float) fc(*c)/CM);
      break;

    case PLAINTEXT:

      if( bc(*c)==MAX_LEN )  sprintf(&str[i][strlen(str[i])], "INF, ");
      else sprintf(&str[i][strlen(str[i])], "%.1fs, ",
	(float) bc(*c)/PlainCharWidth);
      if( bfc(*c)==MAX_LEN )  sprintf(&str[i][strlen(str[i])], "INF, ");
      else sprintf(&str[i][strlen(str[i])], "%.1fs, ",
	(float) bfc(*c)/PlainCharWidth);
      if( fc(*c)==MAX_LEN )  sprintf(&str[i][strlen(str[i])], "INF>");
      else sprintf(&str[i][strlen(str[i])], "%.1fs>",
	(float) fc(*c)/PlainCharWidth);
      break;

  }
  return AsciiToFull(str[i]);
} /* end EchoConstraint */


/*****************************************************************************/
/*                                                                           */
/*  DebugConstrained(x)                                                      */
/*                                                                           */
/*  Calculate and print the constraints of all closures lying within         */
/*  sized object x.                                                          */
/*                                                                           */
/*****************************************************************************/

void DebugConstrained(OBJECT x)
{ OBJECT y, link;
  CONSTRAINT c;
  debug1(DSC, DDD, "DebugConstrained( %s )", EchoObject(x) );
  switch( type(x) )
  {

    case CROSS:
    case ROTATE:
    case INCGRAPHIC:
    case SINCGRAPHIC:
    case GRAPHIC:
    case WORD:
    case QWORD:
    
      break;


    case CLOSURE:
    
      Constrained(x, &c, COL);
      debug2(DSC, DD, "Constrained( %s, &c, COL ) = %s",
	EchoObject(x), EchoConstraint(&c));
      Constrained(x, &c, ROW);
      debug2(DSC, DD, "Constrained( %s, &c, ROW ) = %s",
	EchoObject(x), EchoConstraint(&c));
      break;


    case SPLIT:
    
      link = DownDim(x, COL);  Child(y, link);
      DebugConstrained(y);
      break;


    case HEAD:
    case ONE_COL:
    case ONE_ROW:
    case HCONTRACT:
    case VCONTRACT:
    case HEXPAND:
    case VEXPAND:
    case PADJUST:
    case HADJUST:
    case VADJUST:
    case HSCALE:
    case VSCALE:
    case SCALE:
    case WIDE:
    case HIGH:
    
      link = Down(x);  Child(y, link);
      DebugConstrained(y);
      break;


    case COL_THR:
    case VCAT:
    case HCAT:
    case ACAT:
    
      for( link = Down(x);  link != x;  link =NextDown(link) )
      {	Child(y, link);
	if( type(y) != GAP_OBJ && !is_index(type(y)) )  DebugConstrained(y);
      }
      break;


    default:
    
      Error(15, 2, "DebugConstrained: %s", INTERN, &fpos(x), Image(type(x)));
      break;

  }
  debug0(DSC, DDD, "DebugConstrained returning.");
} /* end DebugConstrained */
#endif
