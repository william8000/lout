/*@z12.c:Size Finder:MinSize()@***********************************************/
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
/*  FILE:         z12.c                                                      */
/*  MODULE:       Size Finder                                                */
/*  EXTERNS:      MinSize()                                                  */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define IG_LOOKING	0
#define IG_NOFILE	1
#define IG_BADFILE	2
#define IG_BADSIZE	3
#define	IG_OK		4


/*****************************************************************************/
/*                                                                           */
/*  OBJECT MinSize(x, dim, extras)                                           */
/*                                                                           */
/*  Set fwd(x, dim) and back(x, dim) to their minimum possible values.       */
/*  If dim == ROWM, construct an extras list and return it in *extras.       */
/*                                                                           */
/*****************************************************************************/

OBJECT MinSize(OBJECT x, int dim, OBJECT *extras)
{ OBJECT y, z, link, prev, t, g, full_name;
  int b, f, dble_fwd, llx, lly, urx, ury, status;
  float fllx, flly, furx, fury;
  BOOLEAN dble_found, found, will_expand, first_line, cp;
  FILE *fp;  FULL_CHAR buff[MAX_BUFF];

  debug2(DSF, DD, "[ MinSize( %s, %s, extras )", EchoObject(x), dimen(dim));
  ifdebug(DSF, DDD, DebugObject(x));

  switch( type(x) )
  {

    case WORD:
    case QWORD:
    
      if( dim == COLM )  FontWordSize(x);
      break;


    case CROSS:

      /* add index to the cross-ref */
      if( dim == ROWM )
      {	New(z, cross_type(x));	/* CROSS_PREC or CROSS_FOLL */
	debug2(DCR, DD, "  MinSize CROSS: %ld %s", (long) x, EchoObject(x));
	actual(z) = x;
	Link(z, x);		/* new code to avoid disposal */
	Link(*extras, z);
	debug2(DCR, DD, "  MinSize index: %ld %s", (long) z, EchoObject(z));
      }
      back(x, dim) = fwd(x, dim) = 0;
      break;


    case PAGE_LABEL:
    
      if( dim == ROWM )
      { New(z, PAGE_LABEL_IND);
	actual(z) = x;
	Link(z, x);
	Link(*extras, z);
      }
      back(x, dim) = fwd(x, dim) = 0;
      break;


    case NULL_CLOS:
    
      back(x, dim) = fwd(x, dim) = 0;
      break;


    case HEAD:

      if( dim == ROWM )
      {	
	/* replace the galley x by a dummy closure y */
	New(y, NULL_CLOS);
	FposCopy(fpos(y), fpos(x));
	ReplaceNode(y, x);

	if( has_key(actual(x)) )
	{
	  /* galley is sorted, make insinuated cross-reference */
	  New(z, foll_or_prec(x));
	  pinpoint(z) = y;
	  Child(t, Down(x));
	  actual(z) = CrossMake(whereto(x), t, (int) type(z));
	  Link(*extras, z);
	  DisposeObject(x);
	  debug1(DCR, DDD, "  MinSize: %s", EchoObject(z));
	}
	else
	{
	  /* galley is following, make UNATTACHED */
	  New(z, UNATTACHED);  Link(z, x);
	  pinpoint(z) = y;
	  Link(*extras, z);
	  debug1(DCR, DDD, "  MinSize: %s", EchoObject(z));
	}
	x = y;	/* now sizing y, not x */
	back(x, COLM) = fwd(x, COLM) = 0;  /* fix non-zero size @Null bug!! */
      }
      else external_ver(x) = external_hor(x) = FALSE;
      back(x, dim) = fwd(x, dim) = 0;
      break;


    case CLOSURE:

      assert( !has_target(actual(x)), "MinSize: CLOSURE has target!" );
      if( dim == ROWM )
      { if( indefinite(actual(x)) )
	{ New(z, RECEPTIVE);
	  actual(z) = x;
	  Link(*extras, z);
	  debug1(DCR, DDD, "  MinSize: %s", EchoObject(z));
	}
	else if( recursive(actual(x)) )
	{ New(z, RECURSIVE);
	  actual(z) = x;
	  Link(*extras, z);
	  debug1(DCR, DDD, "  MinSize: %s", EchoObject(z));
	}
	else
	{ assert(FALSE, "MinSize: definite non-recursive closure");
	}
      }
      else external_ver(x) = external_hor(x) = FALSE;/* nb must be done here!*/
      back(x, dim) = fwd(x, dim) = 0;
      break;


    case ONE_COL:
    case ONE_ROW:
    case HCONTRACT:
    case VCONTRACT:
    
      Child(y, Down(x));
      y = MinSize(y, dim, extras);
      back(x, dim) = back(y, dim);
      fwd(x, dim)  = fwd(y, dim);
      break;


    case HEXPAND:
    case VEXPAND:

      Child(y, Down(x));
      y = MinSize(y, dim, extras);
      back(x, dim) = back(y, dim);
      fwd(x, dim)  = fwd(y, dim);

      /* insert index into *extras for expanding later */
      if( dim == ROWM )
      {	New(z, EXPAND_IND);
	actual(z) = x;
	Link(*extras, z);
	/* Can't do Link(z, x) because Constrained goes up and finds z */
	debug2(DCR, DD, "  MinSize index: %ld %s", (long) z, EchoObject(z));
      }	
      break;


    case GRAPHIC:
    
      Child(y, LastDown(x));
      y = MinSize(y, dim, extras);
      back(x, dim) = back(y, dim);
      fwd(x, dim)  = fwd(y, dim);
      break;


    case HCOVER:
    case VCOVER:

      /* work out size and set to 0 if parallel */
      Child(y, Down(x));
      y = MinSize(y, dim, extras);
      if( (dim == COLM) == (type(x) == HCOVER) )
	back(x, dim) = fwd(x, dim) = 0;
      else
      {	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
      }

      /* insert index into *extras for revising size later */
      if( dim == ROWM )
      {	New(z, COVER_IND);
	actual(z) = x;
	Link(*extras, z);
	/* Can't do Link(z, x) because Constrained goes up and finds z */
	debug2(DCR, DD, "  MinSize index: %ld %s", (long) z, EchoObject(z));
      }	
      break;


    case HSCALE:
    case VSCALE:

      /* work out size and set to 0 if parallel */
      Child(y, Down(x));
      y = MinSize(y, dim, extras);
      if( (dim == COLM) == (type(x) == HSCALE) )
	back(x, dim) = fwd(x, dim) = 0;
      else
      {	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
      }
      break;


    case ROTATE:
    
      Child(y, Down(x));
      if( dim == COLM )
      {	y = MinSize(y, COLM, extras);
	New(whereto(x), ACAT);
	y = MinSize(y, ROWM, &whereto(x));
	RotateSize(&back(x, COLM), &fwd(x, COLM), &back(x, ROWM), &fwd(x, ROWM),
	  y, sparec(constraint(x)));
      }
      else
      {	TransferLinks(Down(whereto(x)), whereto(x), *extras);
	Dispose(whereto(x));
      }
      break;
	

    case SCALE:

      Child(y, Down(x));
      y = MinSize(y, dim, extras);
      if( dim == COLM )
      { back(x, dim) = (back(y, dim) * bc(constraint(x))) / SF;
        fwd(x, dim)  = (fwd(y, dim)  * bc(constraint(x))) / SF;
	if( bc(constraint(x)) == 0 )  /* Lout-supplied factor required */
        { New(z, SCALE_IND);
	  actual(z) = x;
	  Link(*extras, z);
	  debug1(DSF, DDD, "  MinSize: %s", EchoObject(z));
	  vert_sized(x) = FALSE;
        }	
      }
      else
      { back(x, dim) = (back(y, dim) * fc(constraint(x))) / SF;
        fwd(x, dim)  = (fwd(y, dim)  * fc(constraint(x))) / SF;
	vert_sized(x) = TRUE;
      }
      break;


    case WIDE:

      Child(y, Down(x));
      y = MinSize(y, dim, extras);
      if( dim == COLM )
      { y = BreakObject(y, &constraint(x));
        assert( FitsConstraint(back(y, dim), fwd(y, dim), constraint(x)),
		"MinSize: BreakObject failed to fit!" );
        back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
	EnlargeToConstraint(&back(x, dim), &fwd(x, dim), &constraint(x));
      }
      else
      {	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
      }
      break;


    case HIGH:
    
      Child(y, Down(x));
      y = MinSize(y, dim, extras);
      if( dim == ROWM )
      { if( !FitsConstraint(back(y, dim), fwd(y, dim), constraint(x)) )
        { Error(12, 1, "forced to enlarge %s", WARN, &fpos(x), KW_HIGH);
	  debug0(DSF, DD, "offending object was:");
	  ifdebug(DSF, DD, DebugObject(y));
	  SetConstraint(constraint(x), MAX_LEN, size(y, dim), MAX_LEN);
        }
        back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
	EnlargeToConstraint(&back(x, dim), &fwd(x, dim), &constraint(x));
      }
      else
      {	back(x, dim) = back(y, dim);
	fwd(x, dim)  = fwd(y, dim);
      }
      break;


    case HSHIFT:
    case VSHIFT:

      Child(y, Down(x));
      y = MinSize(y, dim, extras);
      if( (dim == COLM) == (type(x) == HSHIFT) )
      { f = FindShift(x, y, dim);
	back(x, dim) = min(MAX_LEN, max(0, back(y, dim) + f));
	fwd(x, dim)  = min(MAX_LEN, max(0, fwd(y, dim)  - f));
      }
      else
      { back(x, dim) = back(y, dim);
	fwd(x, dim) = fwd(y, dim);
      }
      break;


    case SPLIT:
    
      link = DownDim(x, dim);  Child(y, link);
      y = MinSize(y, dim, extras);
      back(x, dim) = back(y, dim);
      fwd(x, dim)  = fwd(y, dim);
      break;


    case ACAT:
    case HCAT:
    case VCAT:
    
      if( (dim == ROWM) == (type(x) == VCAT) )
      {
	/********************************************************************/
	/*                                                                  */
	/*  Calculate sizes parallel to join direction; loop invariant is:  */
	/*                                                                  */
	/*     If prev == nilobj, there are no definite children equal to   */
	/*        or to the left of Child(link).                            */
	/*     If prev != nilobj, prev is the rightmost definite child to   */
	/*        the left of Child(link), and (b, f) is the total size up  */
	/*        to the mark of prev i.e. not including fwd(prev).         */
	/*     g is the most recent gap, or nilobj if none found yet.       */
	/*     will_expand == TRUE when a gap is found that is likely to    */
	/*        enlarge when ActualGap is called later on.                */
	/*                                                                  */
	/********************************************************************/

	prev = g = nilobj;  will_expand = FALSE;  must_expand(x) = FALSE;
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  if( is_index(type(y)) )
	  { if( dim == ROWM )
	    { link = PrevDown(link);
	      MoveLink(NextDown(link), *extras, PARENT);
	    }
	    continue;
	  }
	  else if( type(y) == type(x) )
	  { link = PrevDown(link);
	    TransferLinks(Down(y), y, NextDown(link));
	    DisposeChild(Up(y));
	    continue;
	  }
	  else if( type(y) == GAP_OBJ )  g = y;
	  else /* calculate size of y and accumulate it */
	  { if( is_word(type(y)) )
	    { if( dim == COLM )
	      {
		/* compress adjacent words if compatible */
		if( prev != nilobj && width(gap(g)) == 0 && type(x) == ACAT &&
		    is_word(type(prev)) && vspace(g) + hspace(g) == 0 &&
		    units(gap(g)) == FIXED_UNIT &&
		    mode(gap(g)) == EDGE_MODE && !mark(gap(g)) &&
		    word_font(prev) == word_font(y) &&
		    word_colour(prev) == word_colour(y) &&
		    word_language(prev) == word_language(y) &&
		    underline(prev) == underline(y) )
		{
		  unsigned typ;
		  debug3(DSF, D, "compressing %s and %s at %s",
		    EchoObject(prev), EchoObject(y), EchoFilePos(&fpos(prev)));
		  if( StringLength(string(prev)) + StringLength(string(y))
		      >= MAX_BUFF )
		    Error(12, 2, "word %s%s is too long", FATAL, &fpos(prev),
		      string(prev), string(y));
		  typ = type(prev) == QWORD || type(y) == QWORD ? QWORD : WORD;
		  y = MakeWordTwo(typ, string(prev), string(y), &fpos(prev));
		  word_font(y) = word_font(prev);
		  word_colour(y) = word_colour(prev);
		  word_language(y) = word_language(prev);
		  word_hyph(y) = word_hyph(prev);
		  underline(y) = underline(prev);
		  FontWordSize(y);
		  Link(Up(prev), y);
		  DisposeChild(Up(prev));
		  DisposeChild(Up(g));
		  DisposeChild(link);
		  prev = y;
		  link = Up(prev);
		  continue;
		}

		FontWordSize(y);
		debug4(DSF, DD, "FontWordSize( %s ) font %d = %s,%s",
		EchoObject(y), word_font(y),
		EchoLength(back(y, COLM)), EchoLength(fwd(y, COLM)));
	      }
	    }
	    else y = MinSize(y, dim, extras);

	    if( is_indefinite(type(y)) )
	    {
	      /* error if preceding gap has mark */
	      if( g != nilobj && mark(gap(g)) )
	      {	Error(12, 3, "^ deleted (it may not precede this object)",
		  WARN, &fpos(y));
		mark(gap(g)) = FALSE;
	      }

	      /* error if next unit is used in preceding gap */
	      if( g != nilobj && units(gap(g)) == NEXT_UNIT )
	      {	Error(12, 4, "gap replaced by 0i (%c unit not allowed here)",
		  WARN, &fpos(y), CH_UNIT_WD);
		units(gap(g)) = FIXED_UNIT;
		width(gap(g)) = 0;
	      }
	    }
	    else
	    {
	      /* calculate running total length */
	      if( prev == nilobj )  b = back(y, dim), f = 0;
	      else
	      {
		assert(g!=nilobj && mode(gap(g))!=NO_MODE, "MinSize: NO_MODE!");
		f += MinGap(fwd(prev, dim), back(y, dim), fwd(y, dim), &gap(g));
		if( units(gap(g)) == FRAME_UNIT && width(gap(g)) > FR )
		    will_expand = TRUE;
		if( mark(gap(g)) )  b += f, f = 0;
	      }
	      prev = y;
	    }
	    debug2(DSF,DD,"  b = %s, f = %s",EchoLength(b),EchoLength(f));
	  }
	} /* end for */

	if( prev == nilobj )  b = f = 0;
	else f += fwd(prev, dim);
	back(x, dim) = min(MAX_LEN, b);
	fwd(x, dim)  = min(MAX_LEN, f);

	if( type(x) == ACAT && will_expand )  fwd(x, COLM) = MAX_LEN;
      }
      else
      {
	/********************************************************************/
	/*                                                                  */
	/*  Calculate sizes perpendicular to join direction                 */
	/*                                                                  */
	/*  Loop invariant:                                                 */
	/*                                                                  */
	/*     if found, (b, f) is the size of x, from the last // or from  */
	/*     the start, up to link exclusive.  Else no children yet.      */
	/*     If dble_found, a previous // exists, and (0, dble_fwd) is    */
	/*     the size of x from the start up to that //.                  */
	/*                                                                  */
	/********************************************************************/

	dble_found = found = FALSE;  dble_fwd = 0;
	for( link = Down(x);  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  debug4(DSF, DD, "  %s in %s, y = %s %s", dimen(dim),
	    Image(type(x)), Image(type(y)), EchoObject(y));
	  if( is_index(type(y)) )
	  { if( dim == ROWM )
	    { link = PrevDown(link);
	      MoveLink(NextDown(link), *extras, PARENT);
	    }
	    continue;
	  }
	  else if( type(y) == type(x) )
	  { link = PrevDown(link);
	    TransferLinks(Down(y), y, NextDown(link));
	    DisposeChild(Up(y));
	    continue;
	  }
	  else if( type(y) == GAP_OBJ )
	  { assert( found, "MinSize/VCAT/perp: !found!" );
	    if( !join(gap(y)) )
	    {
	      /* found // or || operator, so end current group */
	      dble_found = TRUE;
	      dble_fwd = max(dble_fwd, b + f);
	      debug1(DSF, DD, "  endgroup, dble_fwd: %s", EchoLength(dble_fwd));
	      found = FALSE;
	    }
	  }
	  else /* found object */
	  {
	    /* calculate size of subobject y */
	    if( is_word(type(y)) )
	    { if( dim == COLM )  FontWordSize(y);
	    }
	    else y = MinSize(y, dim, extras);
	    if( found )
	    { b = max(b, back(y, dim));
	      f = max(f, fwd(y, dim));
	    }
	    else
	    { b = back(y, dim);
	      f = fwd(y, dim);
	      found = TRUE;
	    }
	    debug2(DSF,DD, "  b: %s, f: %s", EchoLength(b), EchoLength(f));
	  }
	} /* end for */
	assert( found, "MinSize/VCAT/perp: !found (2)!" );

	/* finish off last group */
	if( dble_found )
	{ back(x, dim) = 0;
	  dble_fwd = max(dble_fwd, b + f);
	  fwd(x, dim) = min(MAX_LEN, dble_fwd);
	  debug1(DSF, DD, "  end group, dble_fwd: %s", EchoLength(dble_fwd));
	}
	else
	{ back(x, dim) = b;
	  fwd(x, dim)  = f;
	}
      } /* end else */
      break;


    case COL_THR:
    case ROW_THR:

      assert( (type(x) == COL_THR) == (dim == COLM), "Manifest/COL_THR: dim!" );
      if( thr_state(x) == NOTSIZED )
      {	assert( Down(x) != x, "Manifest/COL_THR: Down(x)!" );
	Child(y, Down(x));
	y = MinSize(y, dim, extras);
	b = back(y, dim);
	f = fwd(y, dim);
	for( link = NextDown(Down(x));  link != x;  link = NextDown(link) )
	{ Child(y, link);
	  assert( type(y) != GAP_OBJ, "Manifest/COL_THR: GAP_OBJ!" );
	  y = MinSize(y, dim, extras);
	  b = max(b, back(y, dim));
	  f = max(f, fwd(y, dim));
	}
	back(x, dim) = b;
	fwd(x, dim)  = f;
	thr_state(x) = SIZED;
      }
      break;


    case INCGRAPHIC:
    case SINCGRAPHIC:

      /* open file, check for initial %!, and hunt for %%BoundingBox line */
      /* according to DSC Version 3.0, the BoundingBox parameters must be */
      /* integers; but we read them as floats and truncate since files    */
      /* with fractional values seem to be common in the real world       */
      if( dim == ROWM )  break;
      status = IG_LOOKING;
      Child(y, Down(x));
      fp = OpenIncGraphicFile(string(y), type(x), &full_name, &fpos(y), &cp);
      /* *** fp = OpenFile(fnum = sparec(constraint(x)), FALSE); */
      if( fp == NULL )  status = IG_NOFILE;
      first_line = TRUE;
      while( status == IG_LOOKING && StringFGets(buff, MAX_BUFF, fp) != NULL )
      {
	if( first_line && !StringBeginsWith(buff, AsciiToFull("%!")) )
	  status = IG_BADFILE;
	else
	{ first_line = FALSE;
	  if( buff[0] == '%'
	      && StringBeginsWith(buff, AsciiToFull("%%BoundingBox:"))
	      && !StringContains(buff, AsciiToFull("(atend)")) )
	  { if( sscanf( (char *) buff, "%%%%BoundingBox: %f %f %f %f",
		&fllx, &flly, &furx, &fury) == 4 )
	    {
	      status = IG_OK;
	      llx = fllx;
	      lly = flly;
	      urx = furx;
	      ury = fury;
	    }
	    else status = IG_BADSIZE;
	  }
	}
      }

      /* report error or calculate true size, depending on status */
      switch( status )
      {
	case IG_NOFILE:

	  Error(12, 5, "%s deleted (cannot open file %s)", WARN, &fpos(x),
	    type(x) == INCGRAPHIC ? KW_INCGRAPHIC : KW_SINCGRAPHIC,
	    string(full_name));
	  sparec(constraint(x)) = FALSE;
	  back(x, COLM) = fwd(x, COLM) = back(x, ROWM) = fwd(x, ROWM) = 0;
	  break;

	case IG_LOOKING:

	  Error(12, 6, "%s given zero size (no BoundingBox line in file %s)",
	    WARN, &fpos(x),
	    type(x) == INCGRAPHIC ? KW_INCGRAPHIC : KW_SINCGRAPHIC,
	    string(full_name));
	  back(y, COLM) = fwd(y, COLM) = back(y, ROWM) = fwd(y, ROWM) = 0;
	  back(x, COLM) = fwd(x, COLM) = back(x, ROWM) = fwd(x, ROWM) = 0;
	  sparec(constraint(x)) = TRUE;
	  fclose(fp);
	  if( cp )  StringRemove(AsciiToFull(LOUT_EPS));
	  break;

	case IG_BADFILE:

	  Error(12, 7, "%s deleted (bad first line in file %s)", WARN,
	    &fpos(x), type(x) == INCGRAPHIC ? KW_INCGRAPHIC : KW_SINCGRAPHIC,
	    string(full_name));
	  sparec(constraint(x)) = FALSE;
	  back(x, COLM) = fwd(x, COLM) = back(x, ROWM) = fwd(x, ROWM) = 0;
	  fclose(fp);
	  if( cp )  StringRemove(AsciiToFull(LOUT_EPS));
	  break;
	
	case IG_BADSIZE:

	  Error(12, 8, "%s given zero size (bad BoundingBox line in file %s)",
	    WARN, &fpos(x),
	    type(x) == INCGRAPHIC ? KW_INCGRAPHIC : KW_SINCGRAPHIC,
	    string(full_name));
	  back(y, COLM) = fwd(y, COLM) = back(y, ROWM) = fwd(y, ROWM) = 0;
	  back(x, COLM) = fwd(x, COLM) = back(x, ROWM) = fwd(x, ROWM) = 0;
	  sparec(constraint(x)) = TRUE;
	  fclose(fp);
	  if( cp )  StringRemove(AsciiToFull(LOUT_EPS));
	  break;

	case IG_OK:

	  Child(y, Down(x));
	  back(y, COLM) = llx;  fwd(y, COLM) = urx;
	  back(y, ROWM) = lly;  fwd(y, ROWM) = ury;
	  b = (urx - llx) * PT;
	  b = max(0, min(b, MAX_LEN));
	  back(x, COLM) = fwd(x, COLM) = b / 2;
	  b = (ury - lly) * PT;
	  b = max(0, min(b, MAX_LEN));
	  back(x, ROWM) = fwd(x, ROWM) = b / 2;
	  sparec(constraint(x)) = TRUE;
	  fclose(fp);
	  if( cp )  StringRemove(AsciiToFull(LOUT_EPS));
	  break;

      }
      DisposeObject(full_name);
      break;


    default:
    
      assert1(FALSE, "MinSize", Image(type(x)));
      break;


  } /* end switch */
  debug1(DSF, DD,  "] MinSize returning, x = %s", EchoObject(x));
  debug3(DSF, DD, "  (%s size is %s, %s)", dimen(dim),
		EchoLength(back(x, dim)), EchoLength(fwd(x, dim)) );
  ifdebug(DSF, DDD, DebugObject(x));

  assert(back(x, dim) >= 0, "MinSize: back(x, dim) < 0!");
  assert(fwd(x, dim) >= 0, "MinSize: fwd(x, dim) < 0!");

  return x;
} /* end MinSize */
