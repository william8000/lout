/*@z08.c:Object Manifest:ReplaceWithSplit()@**********************************/
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
/*  FILE:         z08.c                                                      */
/*  MODULE:       Object Manifest                                            */
/*  EXTERNS:      Manifest()                                                 */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  static ReplaceWithSplit(x, bthr, fthr)                                   */
/*                                                                           */
/*  Replace object x with a SPLIT object, if threads for this object are     */
/*  requested by bthr and/or fthr.                                           */
/*                                                                           */
/*****************************************************************************/

#define ReplaceWithSplit(x, bthr, fthr)					\
   if( bthr[ROW] || bthr[COL] || fthr[ROW] || fthr[COL] )		\
	x = insert_split(x, bthr, fthr)

static OBJECT insert_split(x, bthr, fthr)
OBJECT x;  OBJECT bthr[2], fthr[2];
{ OBJECT res, new_op;  int dim;
  debug1(DOM, DD, "ReplaceWithSplit(%s, -)", EchoObject(x));
  assert( type(x) != SPLIT, "ReplaceWithSplit: type(x) already SPLIT!" );
  res = New(SPLIT);
  FposCopy(fpos(res), fpos(x));
  ReplaceNode(res, x);
  for( dim = COL;  dim <= ROW;  dim++ )
  { if( bthr[dim] || fthr[dim] )
    { new_op = New(dim == COL ? COL_THR : ROW_THR);
      thr_state(new_op) = NOTSIZED;
      fwd(new_op, 1-dim) = 0;	/* will hold max frame_size */
      back(new_op, 1-dim) = 0;	/* will hold max frame_origin */
      FposCopy(fpos(new_op), fpos(x));
      Link(res, new_op);  Link(new_op, x);
      if( bthr[dim] )  Link(bthr[dim], new_op);
      if( fthr[dim] )  Link(fthr[dim], new_op);
    }
    else Link(res, x);
  }

  debug1(DOM, DD, "ReplaceWithSplit returning %s", EchoObject(res));
  return res;
} /* end insert_split */

/*@::ReplaceWithTidy()@*******************************************************/
/*                                                                           */
/*  OBJECT ReplaceWithTidy(x, one_word)                                      */
/*                                                                           */
/*  Replace object x with a tidier version in which juxtapositions are       */
/*  folded.  If this is not possible, return the original object.            */
/*                                                                           */
/*  If one_word is TRUE, the result is to be a single QWORD with inter-      */
/*  word spaces converted to single space characters.  Otherwise an ACAT     */
/*  is the preferred result.                                                 */
/*                                                                           */
/*****************************************************************************/

OBJECT ReplaceWithTidy(x, one_word)
OBJECT x;  BOOLEAN one_word;
{ static FULL_CHAR	buff[MAX_BUFF];		/* the growing current word */
  static int		buff_len;		/* length of current word   */
  static FILE_POS	buff_pos;		/* filepos of current word  */
  static unsigned	buff_typ;		/* WORD or QWORD of current */
  OBJECT                link, y, tmp, res;	/* temporaries              */
  debug2(DOM, DD, "ReplaceWithTidy(%s, %s)", EchoObject(x), bool(one_word));
  switch( type(x) )
  {
    case ACAT:
    
      for( link = Down(x);  link != x;  link = NextDown(link) )
      {	Child(y, link);
	if( type(y) == ACAT )
	{ tmp = Down(y);  TransferLinks(tmp, y, link);
	  DisposeChild(link);  link = PrevDown(tmp);
	}
      }
      res = nil;  buff_len = 0;  buff_typ = WORD;  FposCopy(buff_pos, fpos(x));
      for( link = Down(x); link != x; link = NextDown(link) )
      {	Child(y, link);
	if( is_word(type(y)) )
	{ if( buff_len + StringLength(string(y)) >= MAX_BUFF )
	    Error(8, 1, "word is too long", WARN, &fpos(y));
	  else
	  { if( buff_len == 0 )  FposCopy(buff_pos, fpos(y));
	    StringCopy(&buff[buff_len], string(y));
	    buff_len += StringLength(string(y));
	    if( type(y) == QWORD )  buff_typ = QWORD;
	  }
	}
	else if( type(y) == GAP_OBJ )
	{ if( Down(y) != y || hspace(y) + vspace(y) > 0 )
	  { if( one_word )
	    { if( buff_len + 1 >= MAX_BUFF )
		Error(8, 2, "word is too long", WARN, &fpos(y));
	      else
	      { StringCopy(&buff[buff_len], AsciiToFull(" "));
		buff_len++;
		buff_typ = QWORD;
	      }
	    }
	    else
	    { tmp = MakeWord(buff_typ, buff, &buff_pos);
	      buff_len = 0;  buff_typ = WORD;
	      if( res == nil )
	      { res = New(ACAT);
		FposCopy(fpos(res), fpos(x));
	      }
	      Link(res, tmp);  Link(res, y);
	    }
	  }
	}
	else /* error */
	{ if( res != nil )  DisposeObject(res);
	  debug0(DOM, DD, "ReplaceWithTidy returning unchanged");
	  return x;
	}
      }
      tmp = MakeWord(buff_typ, buff, &buff_pos);
      if( res == nil )  res = tmp;
      else Link(res, tmp);
      ReplaceNode(res, x);  DisposeObject(x);
      debug1(DOM, DD, "ReplaceWithTidy returning %s", EchoObject(res));
      return res;


    case WORD:
    case QWORD:

      debug1(DOM, DD, "ReplaceWithTidy returning %s", EchoObject(x));
      return x;


    default:

      debug0(DOM, DD, "ReplaceWithTidy returning unchanged");
      return x;
  }
} /* end ReplaceWithTidy */


/*@::GetScaleFactor()@********************************************************/
/*                                                                           */
/*  static float GetScaleFactor(x)                                           */
/*                                                                           */
/*  Find a scale factor in object x and return it as a float, after checks.  */
/*                                                                           */
/*****************************************************************************/

static float GetScaleFactor(x)
OBJECT x;
{ float scale_factor;
  if( !is_word(type(x)) )
  { Error(8, 3, "replacing invalid scale factor by 1.0", WARN, &fpos(x));
    scale_factor = 1.0;
  }
  else if( sscanf( (char *) string(x), "%f", &scale_factor) != 1 )
  { Error(8, 4, "replacing invalid scale factor %s by 1.0",
      WARN, &fpos(x), string(x));
    scale_factor = 1.0;
  }
  else if( scale_factor < 0.01 )
  { Error(8, 5, "replacing undersized scale factor %s by 1.0",
      WARN, &fpos(x), string(x));
    scale_factor = 1.0;
  }
  else if( scale_factor > 100 )
  { Error(8, 6, "replacing oversized scale factor %s by 1.0",
      WARN, &fpos(x), string(x));
    scale_factor = 1.0;
  }
  return scale_factor;
} /* GetScaleFactor */


static OBJECT nbt[2] = { nil, nil };		/* constant nil threads      */
static OBJECT nft[2] = { nil, nil };		/* constant nil threads      */
static OBJECT ntarget = nil;			/* constant nil target       */


/*@::ManifestCat@*************************************************************/
/*                                                                           */
/*  OBJECT ManifestCat(x,env,style,bthr, fthr, target, crs, ok, need_expand) */
/*                                                                           */
/*  This procedure implements Manifest (see below) when x is HCAT or VCAT.   */
/*                                                                           */
/*****************************************************************************/

static OBJECT ManifestCat(x,env,style,bthr, fthr, target, crs, ok, need_expand)
OBJECT x, env;  STYLE *style;
OBJECT bthr[2], fthr[2]; OBJECT *target, *crs;
BOOLEAN ok, need_expand;
{ OBJECT bt[2], ft[2], y, link, gaplink, g, first_bt, last_ft, z;
  int par, perp;  unsigned res_inc;  BOOLEAN still_backing;
    
  par = type(x) == HCAT ? ROW : COL;
  perp = 1 - par;
  link = Down(x);
  gaplink = NextDown(link);
  assert( link!=x && gaplink!=x, "Manifest/VCAT: less than two children!" );
  Child(y, link);  Child(g, gaplink);

  /* set bt and ft threads for y */
  bt[perp] = bthr[perp];
  ft[perp] = fthr[perp];
  first_bt = bt[par] = bthr[par] ? New(THREAD) : nil;
  ft[par] = join(gap(g)) ? New(THREAD) : nil;
  still_backing = first_bt != nil;

  /* manifest y and insinuate any cross-references */
  y = Manifest(y, env, style, bt, ft, target, crs, ok, FALSE);
  if( type(x) == VCAT && ok && *crs != nil )
  { debug1(DCR, D, "  insinuating %s", EchoObject(*crs));
    TransferLinks(Down(*crs), *crs, link);
    DisposeObject(*crs);
    *crs = nil;
  }

  /* manifest the remaining children */
  while( g != nil )
  {	
    /* manifest the gap object, store it in gap(g), add perp threads */
    assert( type(g) == GAP_OBJ, "Manifest/VCAT: type(g) != GAP_OBJECT!" );
    assert( Down(g) != g, "Manifest/VCAT: GAP_OBJ has no child!" );
    Child(z, Down(g));
    debug1(DOM, DD, "manifesting gap, style = %s", EchoStyle(style));
    z = Manifest(z, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
    debug1(DOM, DD, "replacing with tidy, style = %s", EchoStyle(style));
    z = ReplaceWithTidy(z, FALSE);
    debug1(DOM, DD, "calling GetGap, style = %s", EchoStyle(style));
    GetGap(z, style, &gap(g), &res_inc);
    if( bt[perp] )  Link(bt[perp], g);
    if( ft[perp] )  Link(ft[perp], g);

    /* find the next child y, and following gap if any */
    link = NextDown(gaplink);
    assert( link != x, "Manifest/VCAT: GAP_OBJ is last child!" );
    Child(y, link);
    gaplink = NextDown(link);
    if( gaplink == x )  g = nil;
    else Child(g, gaplink);

    /* set bt and ft threads for y */
    last_ft = ft[par];
    bt[par] = ft[par] ? New(THREAD) : nil;
    ft[par] = g != nil ? join(gap(g)) ? New(THREAD) : nil
			   : fthr[par]    ? New(THREAD) : nil;

    /* manifest y and insinuate any cross references */
    y = Manifest(y, env, style, bt, ft, target, crs, ok, FALSE);
    if( type(x) == VCAT && ok && *crs != nil )
    { debug1(DCR, D, "  insinuating %s", EchoObject(*crs));
      TransferLinks(Down(*crs), *crs, link);
      DisposeObject(*crs);
      *crs = nil;
    }

    if( bt[par] )	/* then thread lists last_ft and bt[par] must merge */
    { OBJECT llink, rlink, lthread, rthread;  BOOLEAN goes_through;
      assert( Down(bt[par]) != bt[par], "Manifest: bt[par] no children!" );
      assert( last_ft!=nil && Down(last_ft)!=last_ft, "Manifest:last_ft!" );

      /* check whether marks run right through y in par direction */
      goes_through = FALSE;
      if( ft[par] )
      {	assert( Down(ft[par]) != ft[par], "Manifest: ft[par] child!" );
	Child(lthread, LastDown(bt[par]));
	Child(rthread, LastDown(ft[par]));
	goes_through = lthread == rthread;
      }

      /* merge the thread lists */
      llink = Down(last_ft);  rlink = Down(bt[par]);
      while( llink != last_ft && rlink != bt[par] )
      {	Child(lthread, llink);
	Child(rthread, rlink);
	assert( lthread != rthread, "Manifest: lthread == rthread!" );
	MergeNode(lthread, rthread);
	llink = NextDown(llink);
	rlink = NextDown(rlink);
      }

      /* attach leftover back threads to first_bt if required */
      if( rlink != bt[par] )
      { 
	if( still_backing )  TransferLinks(rlink, bt[par], first_bt);
      }
      DisposeObject(bt[par]);

      /* attach leftover forward threads to ft[par] if required */
      if( llink != last_ft )
      {
	if( goes_through )  TransferLinks(llink, last_ft, ft[par]);
      }
      DisposeObject(last_ft);

      if( !goes_through )  still_backing = FALSE;

    }
    else still_backing = FALSE;

  } /* end while */

  /* export par threads */
  if( fthr[par] )  MergeNode(fthr[par], ft[par]);
  if( bthr[par] )  MergeNode(bthr[par], first_bt);
  return x;
} /* end ManifestCat */


/*@::ManifestCase@************************************************************/
/*                                                                           */
/*  OBJECT ManifestCase(x,env,style,bthr,fthr, target, crs, ok, need_expand) */
/*                                                                           */
/*  This procedure implements Manifest (see below) when x is CASE.           */
/*                                                                           */
/*****************************************************************************/

static OBJECT ManifestCase(x,env,style,bthr,fthr,target, crs, ok, need_expand)
OBJECT x, env;  STYLE *style;
OBJECT bthr[2], fthr[2]; OBJECT *target, *crs;
BOOLEAN ok, need_expand;
{ OBJECT y, tag, ylink, yield, ytag, zlink;
  OBJECT res, z, firsttag, firstres;

  /* make sure left parameter (the tag) is in order */
  debug0(DOM, DD, "  manifesting CASE now");
  Child(tag, Down(x));
  debug1(DOM, DD, "  manifesting CASE tag %s now", EchoObject(tag));
  tag = Manifest(tag, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
  tag = ReplaceWithTidy(tag, FALSE);

  /* make sure the right parameter is an ACAT */
  Child(y, LastDown(x));
  if( type(y) == YIELD )
  { z = New(ACAT);
    MoveLink(Up(y), z, PARENT);
    Link(x, z);
    y = z;
  }
  if( type(y) != ACAT )
  { Error(8, 7, "%s deleted (right parameter is malformed)",
      WARN, &fpos(y), KW_CASE);
    y = MakeWord(WORD, STR_EMPTY, &fpos(x));
    ReplaceNode(y, x);  DisposeObject(x);
    x = Manifest(y, env, style, bthr, fthr, target, crs, ok, FALSE);
    return x;
  }

  /* hunt through right parameter for res, the selected case */
  res = nil;  firsttag = nil;
  for( ylink = Down(y);  ylink != y && res == nil;  ylink = NextDown(ylink) )
  { Child(yield, ylink);
    if( type(yield) == GAP_OBJ )  continue;
    if( type(yield) != YIELD )
    { Error(8, 8, "%s expected here", WARN, &fpos(yield), KW_YIELD);
      break;
    }
    Child(ytag, Down(yield));
    ytag = Manifest(ytag, env, style, nbt, nft, &ntarget, crs, FALSE,FALSE);
    ytag = ReplaceWithTidy(ytag, FALSE);
    if( is_word(type(ytag)) )
    { if( firsttag == nil )
      {	firsttag = ytag;
	Child(firstres, LastDown(yield));
      }
      if( (is_word(type(tag)) && StringEqual(string(ytag), string(tag))) ||
	     StringEqual(string(ytag), STR_ELSE)  )
      {	Child(res, LastDown(yield));
	break;
      }
    }
    else if( type(ytag) == ACAT )
    { z = ytag;
      for( zlink = Down(z);  zlink != z;  zlink = NextDown(zlink) )
      {	Child(ytag, zlink);
	if( type(ytag) == GAP_OBJ )  continue;
	if( !is_word(type(ytag)) )
	{ Error(8, 9, "error in left parameter of %s",
	    WARN, &fpos(ytag), KW_YIELD);
	  break;
        }
        if( firsttag == nil )
        { firsttag = ytag;
	  Child(firstres, LastDown(yield));
        }
        if( (is_word(type(tag)) && StringEqual(string(ytag), string(tag)))
	        || StringEqual(string(ytag), STR_ELSE) )
        { Child(res, LastDown(yield));
	  break;
        }
      }
    }
    else Error(8, 10, "error in left parameter of %s",
	   WARN, &fpos(ytag), KW_YIELD);
  }
  if( res == nil )
  { if( firsttag != nil )
    { Error(8, 11, "replacing unknown %s option %s by %s",
	WARN, &fpos(tag), KW_CASE, string(tag), string(firsttag));
      res = firstres;
    }
    else
    { Error(8, 12, "%s deleted (choice %s unknown)",
	WARN, &fpos(tag), KW_CASE, string(tag));
      y = MakeWord(WORD, STR_EMPTY, &fpos(x));
      ReplaceNode(y, x);  DisposeObject(x);
      x = Manifest(y, env, style, bthr, fthr, target, crs, ok, FALSE);
      return x;
    }
  }

  /* now manifest the result and replace x with it */
  DeleteLink(Up(res));
  ReplaceNode(res, x);
  DisposeObject(x);
  x = Manifest(res, env, style, bthr, fthr, target, crs, ok, FALSE);
  return x;
} /* ManifestCase */


/*@::ManifestTg@**************************************************************/
/*                                                                           */
/*  OBJECT ManifestTg(x,env,style,bthr, fthr, target, crs, ok, need_expand)  */
/*                                                                           */
/*  This procedure implements Manifest (see below) when x is TAGGED.         */
/*                                                                           */
/*****************************************************************************/

static OBJECT ManifestTg(x,env,style,bthr,fthr, target, crs, ok, need_expand)
OBJECT x, env;  STYLE *style;
OBJECT bthr[2], fthr[2]; OBJECT *target, *crs;
BOOLEAN ok, need_expand;
{ OBJECT y, tag, z;

  /* make sure first argument is a cross-reference */
  assert( Down(x) != x && NextDown(Down(x)) != x &&
	NextDown(NextDown(Down(x))) == x, "Manifest TAGGED: children!" );
  Child(y, Down(x));
  if( type(y) != CROSS )
  { Error(8, 13, "left parameter of %s is not a cross reference",
      WARN, &fpos(y), KW_TAGGED);
    y = MakeWord(WORD, STR_EMPTY, &fpos(x));
    ReplaceNode(y, x);  DisposeObject(x);
    x = Manifest(y, env, style, bthr, fthr, target, crs, ok, FALSE);
    return x;
  }

  /* make sure the arguments of the cross-reference are OK */
  Child(z, Down(y));
  if( type(z) != CLOSURE )
  { Error(8, 14, "left parameter of %s must be a symbol",
      WARN, &fpos(y), KW_TAGGED);
    y = MakeWord(WORD, STR_EMPTY, &fpos(x));
    ReplaceNode(y, x);  DisposeObject(x);
    x = Manifest(y, env, style, bthr, fthr, target, crs, ok, FALSE);
    return x;
  }
  if( !has_tag(actual(z)) )
  { Error(8, 15, "symbol %s not allowed here (it has no %s)",
      WARN, &fpos(z), SymName(actual(z)), KW_TAG);
    y = MakeWord(WORD, STR_EMPTY, &fpos(x));
    ReplaceNode(y, x);  DisposeObject(x);
    x = Manifest(y, env, style, bthr, fthr, target, crs, ok, FALSE);
    return x;
  }
  Child(z, NextDown(Down(y)));
  z = Manifest(z, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
  z = ReplaceWithTidy(z, FALSE);
  if( is_word(type(z)) && StringEqual(string(z), KW_PRECEDING) )
    cross_type(y) = CROSS_PREC;
  else if( is_word(type(z)) && StringEqual(string(z), KW_FOLLOWING) )
    cross_type(y) = CROSS_FOLL;
  else
  { Error(8, 16, "%s or %s expected in left parameter of %s",
      WARN, &fpos(z), KW_PRECEDING, KW_FOLLOWING, KW_TAGGED);
    y = MakeWord(WORD, STR_EMPTY, &fpos(x));
    ReplaceNode(y, x);  DisposeObject(x);
    x = Manifest(y, env, style, bthr, fthr, target, crs, ok, FALSE);
    return x;
  }

  /* make sure second argument (the new key) is ok */
  Child(tag, LastDown(x));
  tag = Manifest(tag, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
  tag = ReplaceWithTidy(tag, FALSE);
  if( !is_word(type(tag)) )
  { Error(8, 17, "right parameter of %s must be a simple word",
      WARN, &fpos(tag), KW_TAGGED);
    ifdebug(DOM, DD, DebugObject(tag));
    y = MakeWord(WORD, STR_EMPTY, &fpos(x));
    ReplaceNode(y, x);  DisposeObject(x);
    x = Manifest(y, env, style, bthr, fthr, target, crs, ok, FALSE);
    return x;
  }

  /* assemble insinuated cross reference which replaces x */
  ReplaceNode(tag, z);
  DisposeObject(z);
  ReplaceNode(y, x);
  DisposeObject(x);
  x = y;
  ReplaceWithSplit(x, bthr, fthr);
  return x;
} /* end ManifestTg */


/*@::ManifestCl@**************************************************************/
/*                                                                           */
/*  OBJECT ManifestCl(x,env,style,bthr, fthr, target, crs, ok, need_expand)  */
/*                                                                           */
/*  This procedure implements Manifest (see below) when x is CLOSURE.        */
/*                                                                           */
/*****************************************************************************/
#define MAX_DEPTH 500

static OBJECT ManifestCl(x,env,style,bthr, fthr, target, crs, ok, need_expand)
OBJECT x, env;  STYLE *style;
OBJECT bthr[2], fthr[2]; OBJECT *target, *crs;
BOOLEAN ok, need_expand;
{ OBJECT y, link, sym, res_env, hold_env, hold_env2, z, newz, command;
  BOOLEAN symbol_free;  int i;
#ifdef DEBUG_ON
  static OBJECT sym_names[MAX_DEPTH];  static int depth = 0;
#endif

  sym = actual(x);
  StyleCopy(save_style(x), *style);
  debug1(DOM, DD,  "  closure; sym = %s", SymName(sym));
#if DEBUG_ON
  sym_names[depth++] = sym;
  if( depth == MAX_DEPTH )
  { Error(8, 18, "maximum depth of symbol expansion (%d) reached",
      WARN, &fpos(x), MAX_DEPTH);
    Error(8, 19, "the symbols currently being expanded are:", WARN, &fpos(x));
    while( --depth >= 0 )
      Error(8, 20, "at %d: %s", WARN, &fpos(x),depth,SymName(sym_names[depth]));
    Error(8, 21, "exiting now", FATAL, &fpos(x));
  }
#endif

  /* expand parameters where possible, and find if they are all free */
  symbol_free = TRUE;
  debugcond1(DOM, D, indefinite(sym), "  freeing %s", EchoObject(x));
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link);
    assert( type(y) == PAR, "Manifest/CLOSURE: type(y) != PAR!" );
    Child(z, Down(y));

    /* try to evaluate the actual parameter z */
    if( !is_word(type(z)) && !has_par(actual(y)) )
    { if( is_tag(actual(y)) || is_key(actual(y)) || type(z) == NEXT )
      {	z = Manifest(z, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
	z = ReplaceWithTidy(z, FALSE);
      }
      else if( type(z) == CLOSURE && is_par(type(actual(z))) )
      {
        /* see whether z would come to something simple if expanded */
        newz = ParameterCheck(z, env);
        debugcond2(DOM, D, indefinite(sym), "  ParameterCheck(%s, env) = %s",
	  EchoObject(z), EchoObject(newz));
        if( newz != nil )
        { ReplaceNode(newz, z);
	  DisposeObject(z);
	  z = newz;
        }
      }
    }

    /*  now check z to see whether it is either a word or and ACAT of words */
    /* ***
    if( type(z) == ACAT )
    { int i = 0;  OBJECT t, tlink, g;
      tlink = Down(z);
      for( ; tlink != z && symbol_free;  tlink = NextDown(tlink), i++ )
      { Child(t, tlink);
	switch( type(t) )
	{
	  case WORD:
	  case QWORD:	if( i > 20 )  symbol_free = FALSE;
			break;

	  case GAP_OBJ:	if( Down(t) != t )
			{ Child(g, Down(t));
			  if( !is_word(type(g)) )  symbol_free = FALSE;
			}
			break;

	  default:	symbol_free = FALSE;
			break;

	}
      }
    }
    else
    *** */
    
    if( !is_word(type(z)) )
    { symbol_free = FALSE;
    }
  }
  debugcond2(DOM, D, indefinite(sym),"  s_f = %s, x = %s",
    bool(symbol_free), EchoObject(x));

  /* if all parameters are free of symbols, optimize environment */
  if( symbol_free && imports(sym) == nil && enclosing(sym) != StartSym )
  { y = SearchEnv(env, enclosing(sym));
    if( y != nil && type(y) == CLOSURE )
    { debug0(DCR, DD, "calling SetEnv from Manifest (a)");
      env = SetEnv(y, nil);
      hold_env2 = New(ACAT);  Link(hold_env2, env);
    }
    else
    { Error(8, 22, "symbol %s used outside %s", WARN, &fpos(x), SymName(sym),
	SymName(enclosing(sym)));
      hold_env2 = nil;
    }
  }
  else hold_env2 = nil;

  if( has_target(sym) && !need_expand )
  {
    /* convert symbols with targets to unsized galleys */
    OBJECT hd = New(HEAD);
    FposCopy(fpos(hd), fpos(x));
    actual(hd) = sym;
    backward(hd) = TargetSymbol(x, &whereto(hd));
    ready_galls(hd) = nil;
    must_expand(hd) = TRUE;
    sized(hd) = FALSE;
    ReplaceNode(hd, x);
    Link(hd, x);
    AttachEnv(env, x);
    x = hd;
    threaded(x) = bthr[COL] != nil || fthr[COL] != nil;
    ReplaceWithSplit(x, bthr, fthr);
  }
  else if(
	    *target == sym			? (*target = nil, TRUE) :
	    need_expand				? TRUE  :
	    uses_galley(sym) && !recursive(sym) ? TRUE  :
	    !indefinite(sym) && !recursive(sym) ? TRUE  :
	    indefinite(sym)  && *target != nil  ? SearchUses(sym, *target)
						: FALSE
	 )
  {
    /* expand the closure and manifest the result */
    debug1(DOM, DD, "expanding; style: %s", EchoStyle(style));
    debug0(DCE, DD, "  calling ClosureExpand from Manifest/CLOSURE");
    x = ClosureExpand(x, env, TRUE, crs, &res_env);
    hold_env = New(ACAT);  Link(hold_env, res_env);
    debug1(DOM, DD, "recursive call; style: %s", EchoStyle(style));
    if( type(x) == FILTERED )
    { assert( type(sym) == RPAR, "ManifestCl/filtered: type(sym)!" );
      assert( filter(enclosing(sym)) != nil, "ManifestCl filter-encl!" );
      command = New(CLOSURE);
      FposCopy(fpos(command), fpos(x));
      actual(command) = filter(enclosing(sym));
      FilterSetFileNames(x);
      command = Manifest(command,env,style,nbt,nft,&ntarget,crs,FALSE,FALSE);
      command = ReplaceWithTidy(command, TRUE);
      if( !is_word(type(command)) )
	Error(8, 23, "filter parameter of %s symbol is not simple",
	  FATAL, &fpos(command), SymName(enclosing(sym)));
      y = FilterExecute(x, string(command), env);
      DisposeObject(command);
      ReplaceNode(y, x);
      DisposeObject(x);
      x = y;
    }
    x = Manifest(x, res_env, style, bthr, fthr, target, crs, ok, FALSE);
    DisposeObject(hold_env);
  }
  else
  {
    /* indefinite symbol, leave unexpanded */
    AttachEnv(env, x);
    threaded(x) = bthr[COL] != nil || fthr[COL] != nil;
    debug0(DOM, DD,  "  closure; calling ReplaceWithSplit");
    ReplaceWithSplit(x, bthr, fthr);
  }
  if( hold_env2 != nil )  DisposeObject(hold_env2);
#ifdef DEBUG_ON
  depth--;
#endif
  return x;
} /* end ManifestCl */


/*@::Manifest()@**************************************************************/
/*                                                                           */
/*  OBJECT Manifest(x, env, style, bthr, fthr, target, crs, ok, need_expand) */
/*                                                                           */
/*  Manifest object x, interpreted in environment env and style style.       */
/*  The result replaces x, and is returned also.                             */
/*  The manifesting operation converts x from a pure parse tree object       */
/*  containing closures and no threads, to an object ready for sizing,       */
/*  with fonts propagated to the words, fill styles propagated to the        */
/*  ACATs, and line spacings propagated to all interested parties.           */
/*  All non-recursive, non-indefinite closures are expanded.                 */
/*  Threads joining objects on a mark are constructed, and SPLIT objects     */
/*  inserted, so that sizing becomes a trivial operation.                    */
/*                                                                           */
/*  Manifest will construct threads and pass them up as children of bthr[]   */
/*  and fthr[] whenever non-nil values of these variables are passed in:     */
/*                                                                           */
/*      bthr[COL]            protrudes upwards from x                        */
/*      fthr[COL]            protrudes downwards from x                      */
/*      bthr[ROW]            protrudes leftwards from x                      */
/*      fthr[ROW]            protrudes rightwards from x                     */
/*                                                                           */
/*  If *target != nil, Manifest will expand indefinite closures leading to   */
/*  the first @Galley lying within an object of type *target.                */
/*                                                                           */
/*  The env parameter contains the environment in which x is to be           */
/*  evaluated.  Environments are shared, so their correct disposal is not    */
/*  simple.  The rule is this:  the code which creates an environment, or    */
/*  detaches it, is responsible for holding it with a dummy parent until     */
/*  it is no longer required.                                                */
/*                                                                           */
/*  Some objects x are not "real" in the sense that they do not give rise    */
/*  to rectangles in the final printed document.  The left parameter of      */
/*  @Wide and similar operators, and the gap following a concatenation       */
/*  operator, are examples of such non-real objects.  The ok flag is true    */
/*  when x is part of a real object.  This is needed because some things,    */
/*  such as the insinuation of cross references and the breaking of          */
/*  lines @Break ACAT objects, only apply to real objects.                   */
/*                                                                           */
/*  If *crs != nil, it points to a list of indexes to cross-references       */
/*  which are to be insinuated into the manifested form of x if x is real.   */
/*                                                                           */
/*  If need_expand is TRUE it forces closure x to expand.                    */
/*                                                                           */
/*****************************************************************************/

OBJECT Manifest(x, env, style, bthr, fthr, target, crs, ok, need_expand)
OBJECT x, env;  STYLE *style;
OBJECT bthr[2], fthr[2]; OBJECT *target, *crs;
BOOLEAN ok, need_expand;
{ OBJECT bt[2], ft[2], y, link, gaplink, g;
  OBJECT res, res_env, res_env2, hold_env, hold_env2, z, prev;
  int par;  GAP res_gap;  unsigned res_inc;  STYLE new_style;
  BOOLEAN done, multiline;  FULL_CHAR ch;  float scale_factor;

  debug2(DOM, DD,   "[Manifest(%s %s )", Image(type(x)), EchoObject(x));
  debug1(DOM, DD,  "  environment: %s", EchoObject(env));
  debug6(DOM, DD,  "  style: %s;  target: %s;  threads: %s%s%s%s",
	EchoStyle(style), SymName(*target),
	bthr[COL] ? " up"    : "",  fthr[COL] ? " down"  : "",
	bthr[ROW] ? " left"  : "",  fthr[ROW] ? " right" : "");

  switch( type(x) )
  {

    case CLOSURE:
    
      x = ManifestCl(x, env, style, bthr, fthr, target, crs, ok, need_expand);
      break;


    case NULL_CLOS:

      StyleCopy(save_style(x), *style);
      ReplaceWithSplit(x, bthr, fthr);
      break;


    case CROSS:
    
      assert( Down(x) != x && LastDown(x) != Down(x), "Manifest: CROSS child!");
      debug0(DCR, DD, "  calling CrossExpand from Manifest/CROSS");
      Child(y, Down(x));
      if( type(y) == CLOSURE )
      { x = CrossExpand(x, env, style, TRUE, crs, &res_env);
        assert( type(x) == CLOSURE, "Manifest/CROSS: type(x)!" );
        hold_env = New(ACAT);  Link(hold_env, res_env);
        /* expand here (calling Manifest immediately makes unwanted cr) */
        debug0(DCE, DD, "  calling ClosureExpand from Manifest/CROSS");
        x = ClosureExpand(x, res_env, FALSE, crs, &res_env2);
        hold_env2 = New(ACAT);  Link(hold_env2, res_env2);
        x = Manifest(x, res_env2, style, bthr, fthr, target, crs, ok, TRUE);
        DisposeObject(hold_env);
        DisposeObject(hold_env2);
      }
      else
      { y = MakeWord(WORD, STR_EMPTY, &fpos(x));
        ReplaceNode(y, x);
        DisposeObject(x);
        x = Manifest(y, env, style, bthr, fthr, target, crs, ok, FALSE);
      }
      break;


    case WORD:
    case QWORD:
    
      if( !ok || *crs == nil )
      {	word_font(x) = font(*style);
	word_colour(x) = colour(*style);
	word_language(x) = language(*style);
	word_hyph(x) = hyph_style(*style) == HYPH_ON;
	ReplaceWithSplit(x, bthr, fthr);
	break;
      }
      y = New(ACAT);
      FposCopy(fpos(y), fpos(x));
      ReplaceNode(y, x);
      Link(y, x);  x = y;
      /* NB NO BREAK! */


    case ACAT:
    
      StyleCopy(save_style(x), *style);
      assert(Down(x) != x, "Manifest: ACAT!" );
      link = Down(x);  Child(y, link);
      assert( type(y) != GAP_OBJ, "Manifest ACAT: GAP_OBJ is first!" );
      multiline = FALSE;

      /* manifest first child and insert any cross references */
      if( is_word(type(y)) )
      { word_font(y) = font(*style);
	word_colour(y) = colour(*style);
	word_language(y) = language(*style);
	word_hyph(y) = hyph_style(*style) == HYPH_ON;
      }
      else y = Manifest(y, env, style, nbt, nft, target, crs, ok, FALSE);
      if( ok && *crs != nil )
      {	
	debug1(DCR, D, "  insinuating %s", EchoObject(*crs));
	TransferLinks(Down(*crs), *crs, link);
	DisposeObject(*crs);
	*crs = nil;
      }
      prev = y;

      for( gaplink = Down(link);  gaplink != x;  gaplink = NextDown(link) )
      {
	Child(g, gaplink);
	assert( type(g) == GAP_OBJ, "Manifest ACAT: no GAP_OBJ!" );
	link = NextDown(gaplink);
	assert( link != x, "Manifest ACAT: GAP_OBJ is last!" );
	Child(y, link);
	assert( type(y) != GAP_OBJ, "Manifest ACAT: double GAP_OBJ!" );

	/* manifest the next child */
        debug1(DOM, DD, "  in ACAT (3), style = %s", EchoStyle(style));
	if( is_word(type(y)) )
	{ word_font(y) = font(*style);
	  word_colour(y) = colour(*style);
	  word_language(y) = language(*style);
	  word_hyph(y) = hyph_style(*style) == HYPH_ON;
	}
	else y = Manifest(y, env, style, nbt, nft, target, crs, ok, FALSE);

	/* manifest the gap object */
	if( Down(g) != g )
	{
	  /* explicit & operator whose value is the child of g */
	  Child(z, Down(g));
	  z = Manifest(z, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
	  z = ReplaceWithTidy(z, FALSE);
	  GetGap(z, style, &gap(g), &res_inc);
	  vspace(g) = hspace(g) = 0;
	}
	else
	{
	  /* implicit & operator */
	  GapCopy(gap(g), space_gap(*style));
	  width(gap(g)) = width(gap(g)) * (vspace(g) + hspace(g));
	  if( vspace(g) > 0 && is_definite(type(y)) )  multiline = TRUE;
	}
        debug1(DOM, DD, "  in ACAT, gap = %s", EchoLength(width(gap(g))));

	/* compress adjacent juxtaposed words of equal font, etc. */
	if( is_word(type(y)) && width(gap(g)) == 0 && vspace(g)+hspace(g)==0 &&
	    units(gap(g)) == FIXED_UNIT && mode(gap(g)) == EDGE_MODE &&
	    prev != nil && is_word(type(prev)) && !mark(gap(g)) &&
	    word_font(prev) == word_font(y) &&
	    word_colour(prev) == word_colour(y) &&
	    word_language(prev) == word_language(y) &&
	    word_hyph(prev) == word_hyph(y) )
	{ unsigned typ;
	  if( StringLength(string(prev))+StringLength(string(y)) >= MAX_BUFF )
	    Error(8, 24, "word %s%s is too long",
	      FATAL, &fpos(prev), string(prev), string(y));
	  z = y;
	  typ = type(prev) == QWORD || type(y) == QWORD ? QWORD : WORD;
	  y = MakeWordTwo(typ, string(prev), string(y), &fpos(prev));
	  word_font(y) = word_font(prev);
	  word_colour(y) = word_colour(prev);
	  word_language(y) = word_language(prev);
	  word_hyph(y) = word_hyph(prev);
	  MoveLink(link, y, CHILD);
	  DisposeObject(z);
	  DisposeChild(Up(prev));
	  DisposeChild(gaplink);
	}
	prev = y;

	/* insinuate any cross-references */
	if( ok && *crs != nil )
	{
	  debug1(DCR, D, "  insinuating %s", EchoObject(*crs));
	  TransferLinks(Down(*crs), *crs, link);
	  DisposeObject(*crs);
	  *crs = nil;
	}

      }

      /* implement FILL_OFF break option if required */
      if( ok && multiline && fill_style(*style) == FILL_UNDEF )
	Error(8, 25, "missing %s symbol or option", FATAL, &fpos(x), KW_BREAK);
      if( ok && multiline && fill_style(*style) == FILL_OFF )
      {	OBJECT last_acat = x, new_acat;
	x = New(VCAT);
	ReplaceNode(x, last_acat);
	Link(x, last_acat);
	for( link = Down(last_acat); link != last_acat; link = NextDown(link) )
	{ Child(g, link);
	  if( type(g) == GAP_OBJ && mode(gap(g)) != NO_MODE && vspace(g) > 0 )
	  { link = PrevDown(link);
	    MoveLink(NextDown(link), x, PARENT);
	    GapCopy(gap(g), line_gap(*style));
	    width(gap(g)) *= vspace(g);
	    new_acat = New(ACAT);
	    FposCopy(fpos(new_acat), fpos(g));
	    if( hspace(g) > 0 )
	    { z = MakeWord(WORD, STR_EMPTY, &fpos(g));
	      Link(new_acat, z);
	      z = New(GAP_OBJ);
	      hspace(z) = hspace(g);
	      vspace(z) = 0;
	      GapCopy(gap(z), space_gap(*style));
	      width(gap(z)) *= hspace(z);
	      Link(new_acat, z);
	    }
	    TransferLinks(NextDown(link), last_acat, new_acat);
	    StyleCopy(save_style(new_acat), *style);
	    Link(x, new_acat);
	    last_acat = new_acat;
	    link = last_acat;
	  }
	}
      }

      ReplaceWithSplit(x, bthr, fthr);
      break;


    case HCAT:
    case VCAT:

     x = ManifestCat(x, env, style, bthr, fthr, target, crs, ok, need_expand);
     break;


    case WIDE:
    case HIGH:
    
      Child(y, Down(x));
      y = Manifest(y, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
      y = ReplaceWithTidy(y, FALSE);
      GetGap(y, style, &res_gap, &res_inc);
      if( res_inc != GAP_ABS || mode(res_gap) != EDGE_MODE ||
	units(res_gap) != FIXED_UNIT )
      {	Error(8, 26, "replacing invalid left parameter of %s by 2i",
	  WARN, &fpos(y), Image(type(x)) );
	units(res_gap) = FIXED_UNIT;
	width(res_gap) = 2*IN;
      }
      SetConstraint(constraint(x), MAX_LEN, width(res_gap), MAX_LEN);
      DisposeChild(Down(x));
      goto ETC;		/* two cases down from here */


    case HSHIFT:
    case VSHIFT:

      Child(y, Down(x));
      y = Manifest(y, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
      y = ReplaceWithTidy(y, FALSE);
      GetGap(y, style, &shift_gap(x), &res_inc);
      shift_type(x) = res_inc;
      if( mode(shift_gap(x)) != EDGE_MODE || 
	  (units(shift_gap(x))!=FIXED_UNIT && units(shift_gap(x))!=NEXT_UNIT) )
      {	Error(8, 27, "replacing invalid left parameter of %s by +0i",
	  WARN, &fpos(y), Image(type(x)) );
	shift_type(x) = GAP_INC;
	units(shift_gap(x)) = FIXED_UNIT;
	width(shift_gap(x)) = 0;
	mode(shift_gap(x)) = EDGE_MODE;
      }
      DisposeChild(Down(x));
      goto ETC;		/* next case down from here */


    case HCONTRACT:
    case VCONTRACT:
    case HEXPAND:
    case VEXPAND:
    case PADJUST:
    case HADJUST:
    case VADJUST:
    case ONE_COL:
    case ONE_ROW:
    
      ETC:
      par = (type(x)==ONE_COL || type(x)==HEXPAND || type(x) == HCONTRACT ||
	     type(x)==PADJUST || type(x)==HADJUST || type(x)==WIDE ||
	     type(x)==HSHIFT) ? COL : ROW;
      Child(y, Down(x));

      /* manifest the child, propagating perp threads and suppressing pars */
      bt[par] = ft[par] = nil;
      bt[1-par] = bthr[1-par];  ft[1-par] = fthr[1-par];
      y = Manifest(y, env, style, bt, ft, target, crs, ok, FALSE);

      /* replace with split object if par threads needed */
      bt[par] = bthr[par];  ft[par] = fthr[par];
      bt[1-par] = ft[1-par] = nil;
      ReplaceWithSplit(x, bt, ft);
      break;


    case ROTATE:

      Child(y, Down(x));
      y = Manifest(y, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
      y = ReplaceWithTidy(y, FALSE);
      GetGap(y, style, &res_gap, &res_inc);
      if( res_inc != GAP_ABS || mode(res_gap) != EDGE_MODE ||
		units(res_gap) != DEG_UNIT )
      {	Error(8, 28, "replacing invalid left parameter of %s by 0d",
	  WARN, &fpos(y), Image(type(x)) );
	units(res_gap) = DEG_UNIT;
	width(res_gap) = 0;
      }
      sparec(constraint(x)) = width(res_gap);
      DisposeChild(Down(x));
      Child(y, Down(x));
      y = Manifest(y, env, style, nbt, nft, target, crs, ok, FALSE);
      ReplaceWithSplit(x, bthr, fthr);
      break;


    case HSCALE:
    case VSCALE:

      Child(y, Down(x));
      y = Manifest(y, env, style, nbt, nft, target, crs, ok, FALSE);
      ReplaceWithSplit(x, bthr, fthr);
      break;


    case SCALE:

      Child(y, Down(x));
      y = Manifest(y, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
      y = ReplaceWithTidy(y, FALSE);
      if( is_word(type(y)) && StringEqual(string(y), STR_EMPTY) )
      {
	/* missing scale factor, meaning to be inserted automatically */
        bc(constraint(x)) = fc(constraint(x)) = 0;  /* work out later */
      }
      else if( type(y) != ACAT )
      {
	/* presumably one word, common factor for horizontal and vertical */
	scale_factor = GetScaleFactor(y);
        bc(constraint(x)) = fc(constraint(x)) = scale_factor * SF;
      }
      else
      {
	/* get horizontal scale factor */
	Child(z, Down(y));
	scale_factor = GetScaleFactor(z);
        bc(constraint(x)) = scale_factor * SF;

	/* get vertical scale factor */
	Child(z, LastDown(y));
	scale_factor = GetScaleFactor(z);
        fc(constraint(x)) = scale_factor * SF;
      }
      DisposeChild(Down(x));
      Child(y, LastDown(x));
      y = Manifest(y, env, style, nbt, nft, target, crs, ok, FALSE);
      ReplaceWithSplit(x, bthr, fthr);
      break;


    case YIELD:

      Error(8, 29, "%s not expected here", FATAL, &fpos(x), KW_YIELD);
      break;


    case CASE:

      x = ManifestCase(x,env,style, bthr, fthr, target, crs, ok, need_expand);
      break;


    case BACKEND:

      switch( BackEnd )
      {
	case PLAINTEXT:
	
	  res = MakeWord(WORD, AsciiToFull("PlainText"), &fpos(x));
	  break;


	case POSTSCRIPT:
	
	  res = MakeWord(WORD, AsciiToFull("PostScript"), &fpos(x));
	  break;
      }
      ReplaceNode(res, x);
      DisposeObject(x);
      x = Manifest(res, env, style, bthr, fthr, target, crs, ok, FALSE);
      break;


    case XCHAR:

      Child(y, Down(x));
      y = Manifest(y, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
      y = ReplaceWithTidy(y, FALSE);
      if( !is_word(type(y)) )
      {	Error(8, 30, "%s dropped (parameter is not a simple word)",
	  WARN, &fpos(y), KW_XCHAR);
	res = MakeWord(WORD, STR_EMPTY, &fpos(x));
      }
      else if( (word_font(y) = font(*style)) == 0 )
      {	Error(8, 31, "%s dropped (no current font at this point)",
	  WARN, &fpos(y), KW_XCHAR);
	res = MakeWord(WORD, STR_EMPTY, &fpos(x));
      }
      else if( (ch=EvRetrieve(string(y), FontEncoding(word_font(y)))) == '\0' )
      {	type(y) = QWORD;
	Error(8, 32, "%s dropped (character %s unknown in font %s)",
	  WARN, &fpos(y), KW_XCHAR, StringQuotedWord(y),
	  FontFamilyAndFace(word_font(y)));
	res = MakeWord(WORD, STR_EMPTY, &fpos(x));
      }
      else
      {	res = MakeWord(QWORD, STR_SPACE, &fpos(x));
	string(res)[0] = ch;
      }
      ReplaceNode(res, x);
      DisposeObject(x);
      x = Manifest(res, env, style, bthr, fthr, target, crs, ok, FALSE);
      break;


    case CURR_LANG:

      if( language(*style) == 0 )
      { Error(8, 33, "no current language at this point, using %s",
	  WARN, &fpos(x), STR_NONE);
	res = MakeWord(WORD, STR_NONE, &fpos(x));
      }
      else res = MakeWord(WORD, LanguageString(language(*style)), &fpos(x));
      ReplaceNode(res, x);
      DisposeObject(x);
      x = Manifest(res, env, style, bthr, fthr, target, crs, ok, FALSE);
      break;


    case FONT:
    case SPACE:
    case BREAK:
    case COLOUR:
    case LANGUAGE:
    
      assert( Down(x) != x && NextDown(Down(x)) != x, "Manifest: FONT!" );
      StyleCopy(new_style, *style);
      Child(y, Down(x));
      y = Manifest(y, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
      y = ReplaceWithTidy(y, (BOOLEAN) (type(x) == COLOUR) );
      if(      type(x) == FONT   ) FontChange(&new_style, y);
      else if( type(x) == SPACE  ) SpaceChange(&new_style, y);
      else if( type(x) == BREAK  ) BreakChange(&new_style, y);
      else if( type(x) == COLOUR ) ColourChange(&new_style, y);
      else                         LanguageChange(&new_style, y);
      DisposeChild(Down(x));
      Child(y, Down(x));
      y = Manifest(y, env, &new_style, bthr, fthr, target, crs, ok, FALSE);
      DeleteLink(Down(x));
      MergeNode(y, x);  x = y;
      break;


    case NEXT:

      assert( Down(x) != x, "Manifest/NEXT: Down(x) == x!" );
      Child(y, Down(x));
      debug1(DCS, D, "  Manifesting Next( %s, 1 )", EchoObject(y));
      y = Manifest(y, env, style, bthr, fthr, target, crs, FALSE, FALSE);
      debug1(DCS, D, "  calling Next( %s, 1 )", EchoObject(y));
      done = FALSE;
      y = Next(y, 1, &done);
      debug2(DCS, D, "  Next(done = %s) returning %s",
			bool(done), EchoObject(y));
      DeleteLink(Down(x));
      MergeNode(y, x);  x = y;
      break;


    case OPEN:

      debug0(DCR, D, "  [ Manifest/OPEN begins:");
      Child(y, Down(x));
      Child(res, LastDown(x));
      if( type(y) == CLOSURE )
      { AttachEnv(env, y);
	StyleCopy(save_style(y), *style);
	debug0(DCR, DD, "calling SetEnv from Manifest (b)");
	res_env = SetEnv(y, nil);
	hold_env = New(ACAT);  Link(hold_env, res_env);
	res = Manifest(res, res_env, style, bthr, fthr, target, crs, ok, FALSE);
	DisposeObject(hold_env);
      }
      else if( type(y) == CROSS )
      { Child(z, Down(y));
	if( type(z) == CLOSURE )
	{ debug0(DCR, DD, "  calling CrossExpand from Manifest/OPEN");
	  y = CrossExpand(y, env, style, TRUE, crs, &res_env);
	  AttachEnv(res_env, y);
	  debug0(DCR, DD, "calling SetEnv from Manifest (c)");
	  res_env = SetEnv(y, env);
	  hold_env = New(ACAT);  Link(hold_env, res_env);
	  res = Manifest(res, res_env, style, bthr, fthr, target, crs, ok, FALSE);
	  DisposeObject(hold_env);
	}
	else
	{ Error(8, 34, "invalid left parameter of %s", WARN, &fpos(y), KW_OPEN);
	  res = Manifest(res, env, style, bthr, fthr, target, crs, ok, FALSE);
	}
      }
      else
      {	Error(8, 34, "invalid left parameter of %s", WARN, &fpos(y), KW_OPEN);
	res = Manifest(res, env, style, bthr, fthr, target, crs, ok, FALSE);
      }
      ReplaceNode(res, x);
      DisposeObject(x);
      x = res;
      debug0(DCR, D, "  ] Manifest/OPEN ends");
      break;


    case TAGGED:

      x = ManifestTg(x, env, style, bthr, fthr, target, crs, ok, need_expand);
      break;


    case GRAPHIC:

      debug1(DRS, DD, "  graphic style in Manifest = %s", EchoStyle(style));
      Child(y, LastDown(x));
      y = Manifest(y, env, style, nbt, nft, target, crs, ok, FALSE);
      StyleCopy(save_style(x), *style);
      Child(y, Down(x));
      y = Manifest(y, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
      ReplaceWithSplit(x, bthr, fthr);
      break;
	

    case INCGRAPHIC:
    case SINCGRAPHIC:

      StyleCopy(save_style(x), *style);
      Child(y, Down(x));
      y = Manifest(y, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
      y = ReplaceWithTidy(y, FALSE);
      if( !is_word(type(y)) )
      { Error(8, 35, "%s deleted (invalid right parameter)", WARN, &fpos(y),
	  type(x) == INCGRAPHIC ? KW_INCGRAPHIC : KW_SINCGRAPHIC);
	y = MakeWord(WORD, STR_EMPTY, &fpos(x));
	ReplaceNode(y, x);  DisposeObject(x);
	x = Manifest(y, env, style, bthr, fthr, target, crs, ok, FALSE);
	return x;
      }
      ReplaceWithSplit(x, bthr, fthr);
      break;
	

    default:

      Error(8, 36, "Manifest: %s", INTERN, &fpos(x), Image(type(x)));
      break;

  } /* end switch */

  debug2(DOM, DD, "]Manifest returning %s %s", Image(type(x)), EchoObject(x));
  debug1(DOM, DD, "  at exit, style = %s", EchoStyle(style));
  debug1(DOM, DDD, "up:    ", EchoObject(bthr[COL]));
  debug1(DOM, DDD, "down:  ", EchoObject(fthr[COL]));
  debug1(DOM, DDD, "left:  ", EchoObject(bthr[ROW]));
  debug1(DOM, DDD, "right: ", EchoObject(fthr[ROW]));
  return x;
} /* end Manifest */
