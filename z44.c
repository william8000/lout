/*@z44.c:Vertical Hyphenation:VerticalHyphenate()@****************************/
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
/*  FILE:         z44.c                                                      */
/*  MODULE:       Vertical Hyphenation                                       */
/*  EXTERNS:      VerticalHyphenate(), ConvertGalleyList()                   */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  FirstDefiniteCompressed(x, link, y)                                      */
/*  NextDefiniteWithGapCompressed(x, link, y, g)                             */
/*                                                                           */
/*  Like FirstDefinite() and NextDefiniteWithGap(), except that these        */
/*  versions assume that x is of type VCAT, and they compress any VCAT       */
/*  objects found within x as they go.                                       */
/*                                                                           */
/*****************************************************************************/

#define FirstDefiniteCompressed(x, link, y)				\
{ BOOLEAN jn;								\
  FirstDefinite(x, link, y, jn);					\
  while( link != x && type(y) == VCAT )					\
  { TransferLinks(Down(y), y, link);					\
    DisposeChild(link);							\
    FirstDefinite(x, link, y, jn);					\
  }									\
  assert( link==x || is_definite(type(y)), "FirstDefiniteCompressed!");	\
}

#define NextDefiniteWithGapCompressed(x, link, y, g)			\
{ OBJECT start_link = PrevDown(link), ylink, yg, z;			\
  BOOLEAN jn;								\
  NextDefiniteWithGap(x, link, y, g, jn);				\
  while( link != x && type(y) == VCAT )					\
  { FirstDefinite(y, ylink, z, jn);					\
    if( ylink != y && PrevDown(ylink) != y )				\
    { Child(yg, PrevDown(ylink));					\
      assert( type(yg)==GAP_OBJ && mode(gap(yg)) != NO_MODE, "NDWGC!");	\
      MoveLink(PrevDown(ylink), Up(g), PARENT);				\
      MoveLink(Up(g), ylink, PARENT);					\
    }									\
    TransferLinks(Down(y), y, link);					\
    DisposeChild(link);							\
    link = NextDown(start_link);					\
    NextDefiniteWithGap(x, link, y, g, jn);				\
  }									\
  assert( link==x || is_definite(type(y)), "FirstDefiniteCompressed!");	\
  assert( link==x || mode(gap(g)) != NO_MODE,				\
    "FirstDefiniteWithGapCompressed: mode(gap(g))!" );			\
}


/*@@**************************************************************************/
/*                                                                           */
/*  OBJECT FindTarget(index)                                                 */
/*                                                                           */
/*  Work out what the given index is pointing at, or nilobj if nothing.      */
/*                                                                           */
/*****************************************************************************/

static OBJECT FindTarget(OBJECT index)
{ OBJECT res;
  debug1(DVH, DD, "FindTarget(%s)", Image(type(index)));
  switch( type(index) )
  {
    case DEAD:

      res = nilobj;
      break;


    case UNATTACHED:
    case GALL_PREC:
    case GALL_FOLL:

      res = pinpoint(index);
      break;


    case RECEPTIVE:
    case RECEIVING:
    case RECURSIVE:
    case SCALE_IND:
    case EXPAND_IND:

      res = actual(index);
      break;


    case PRECEDES:
    case FOLLOWS:
    case CROSS_TARG:
    case CROSS_PREC:
    case CROSS_FOLL:
    case PAGE_LABEL_IND:

      res = nilobj;  /* somewhat doubtful */
      break;


    case GALL_TARG:

      res = nilobj;  /* somewhat doubtful */
      break;


    default:

      Error(44, 1, "unknown index %s", INTERN, no_fpos, Image(type(index)));
      break;
  }
  debug1(DVH, DD, "FindTarget returning %s", EchoObject(res));
  return res;
} /* end FindTarget */


/*@@**************************************************************************/
/*                                                                           */
/*  OBJECT WhichComponent(target)                                            */
/*                                                                           */
/*  Return the component of the enclosing galley that contains target,       */
/*  or nilobj if some problem.                                               */
/*                                                                           */
/*****************************************************************************/

static OBJECT WhichComponent(OBJECT target)
{ OBJECT prnt;
  debug1(DVH, DD, "WhichComponent(%s)", EchoObject(target));
  while( Up(target) != target )
  { Parent(prnt, Up(target));
    if( type(prnt) == HEAD )
    { debug1(DVH, DD, "WhichComponent returning %s", EchoObject(target));
      return target;
    }
    target = prnt;
  }
  debug0(DVH, DD, "WhichComponent returning nilobj");
  return nilobj;
} /* end WhichComponent */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT EncloseInHcat(nxt, y, replace)                                   */
/*                                                                           */
/*  Enclose object nxt in an HCAT, similar to HCAT y, at position replace.  */
/*  The link to nxt will now link to the new HCAT.                          */
/*                                                                           */
/*****************************************************************************/

static OBJECT EncloseInHcat(OBJECT nxt, OBJECT y, OBJECT replace)
{ OBJECT new_y, new_row_thread, s1, new_s1, s2, new_s2, link, sh, new_sh, tmp;
  assert( Up(nxt) != nxt, "EncloseInHCat: Up(nxt) == nxt!" );
  new_y = New(HCAT);
  MoveLink(Up(nxt), new_y, CHILD);
  assert( Up(nxt) == nxt, "EncloseInHCat: Up(nxt) != nxt!" );
  FposCopy(fpos(new_y), fpos(y));
  back(new_y, COL) = back(y, COL);
  fwd(new_y, COL) = fwd(y, COL);
  back(new_y, ROW) = back(nxt, ROW);
  fwd(new_y, ROW) = fwd(nxt, ROW);
  new_row_thread = New(ROW_THR);
  back(new_row_thread, ROW) = back(new_y, ROW);
  fwd(new_row_thread, ROW) = fwd(new_y, ROW);
  for( link = Down(y);  link != y;  link = NextDown(link) )
  { Child(s1, link);
    if( type(s1) == GAP_OBJ )
    { new_s1 = New(GAP_OBJ);
      FposCopy(fpos(new_s1), fpos(s1));
      GapCopy(gap(new_s1), gap(s1));
      Link(new_y, new_s1);
      continue;
    }
    if( type(s1) == WIDE || type(s1) == ONE_COL )
      Child(s2, Down(s1));
    else s2 = s1;
    assert( type(s2) == SPLIT, "EncloseInHcat: type(s2) != SPLIT!" );
    Child(sh, DownDim(s2, COL));
    new_s2 = New(SPLIT);
    FposCopy(fpos(new_s2), fpos(s2));
    if( s2 != s1 )
    { new_s1 = New(type(s1));
      back(new_s1, COL) = back(s1, COL);
      fwd(new_s1, COL) = fwd(s1, COL);
      back(new_s1, ROW) = back(new_row_thread, COL);
      fwd(new_s1, ROW) = fwd(new_row_thread, COL);
      Link(new_y, new_s1);
      Link(new_s1, new_s2);
    }
    else Link(new_y, new_s2);
    if( sh == replace )
    { 
      /* replace sh by nxt in the copy */
      new_sh = nxt;
      back(new_sh, COL) = back(s2, COL);
      fwd(new_sh, COL) = fwd(s2, COL);
    }
    else
    {
      /* replace sh by an empty object of the same width in the copy */
      new_sh = New(WIDE);
      FposCopy(fpos(new_sh), fpos(sh));
      SetConstraint(constraint(new_sh), back(sh,COL),size(sh,COL),fwd(sh,COL));
      back(new_sh, COL) = back(sh, COL);
      fwd(new_sh, COL) = fwd(sh, COL);
      back(new_sh, ROW) = fwd(new_sh, ROW) = 0;
      tmp = MakeWord(WORD, STR_EMPTY, &fpos(sh));
      back(tmp, COL) = fwd(tmp, COL) = 0;
      back(tmp, ROW) = fwd(tmp, ROW) = 0;
      Link(new_sh, tmp);
    }
    Link(new_s2, new_sh);
    back(new_s2, COL) = back(new_sh, COL);
    fwd(new_s2, COL) = fwd(new_sh, COL);
    Link(new_s2, new_row_thread);
    back(new_s2, ROW) = back(new_row_thread, ROW);
    fwd(new_s2, ROW) = fwd(new_row_thread, ROW);
    Link(new_row_thread, new_sh);
  }
  return new_y;
} /* end EncloseInHcat */


/*@::VerticalHyphenate()@*****************************************************/
/*                                                                           */
/*  BOOLEAN VerticalHyphenate(OBJECT y)                                      */
/*                                                                           */
/*  Attempt to vertically hyphenate galley component y, of type HCAT.        */
/*                                                                           */
/*****************************************************************************/

BOOLEAN VerticalHyphenate(OBJECT y)
{ OBJECT large_comp, index, z, link, g;
  OBJECT row_thread, s1, s2, sh, sv, shp, prev, nxt, large_comp_split;
  LENGTH rump_fwd;
  debug1(DVH, D, "[ VerticalHyphenate(y: %s), y =", EchoLength(size(y, ROW)));
  ifdebug(DVH, D, DebugObject(y));
  debug0(DVH, DD, "galley before vertical hyphenation:");
  ifdebug(DVH, DD, Parent(z, Up(y)); DebugGalley(z, y, 2));

  /* find large_comp, the largest VCAT component, or else return FALSE */
  row_thread = large_comp = nilobj;
  rump_fwd = 0;
  assert( type(y) == HCAT, "VerticalHyphenate: type(y) != HCAT!" );
  for( link = Down(y);  link != y;  link = NextDown(link) )
  { Child(s1, link);
    if( type(s1) == GAP_OBJ )
    { if( !join(gap(s1)) )
      { debug0(DVH, D, "] VerticalHyphenate returning FALSE (not joined)");
	return FALSE;
      }
      continue;
    }

    /* check that s2 is a SPLIT object whose children look right */
    if( type(s1) == WIDE || type(s1) == ONE_COL )
      Child(s2, Down(s1));
    else s2 = s1;
    if( type(s2) != SPLIT )
    { debug0(DVH, D, "] VerticalHyphenate returning FALSE (child not SPLIT)");
      return FALSE;
    }
    Child(sh, DownDim(s2, COL));
    Child(sv, DownDim(s2, ROW));
    if( type(sv) != ROW_THR )
    { debug0(DVH, D, "] VerticalHyphenate returning FALSE (no ROW_THR)");
      return FALSE;
    }
    if( row_thread == nilobj )  row_thread = sv;
    if( sv != row_thread )
    { debug0(DVH, D, "] VerticalHyphenate returning FALSE (different ROW_THR)");
      return FALSE;
    }
    Parent(shp, UpDim(sh, ROW));
    if( shp != row_thread )
    { debug0(DVH, D, "] VerticalHyphenate returning FALSE (sh parent)");
      return FALSE;
    }

    /* Now sh is one of the HCAT components */
    if( type(sh) != VCAT )
    { rump_fwd = max(rump_fwd, fwd(sh, ROW));
    }
    else if( large_comp != nilobj )
    { debug0(DVH, D, "] VerticalHyphenate returning FALSE (two VCATs)");
      return FALSE;
    }
    else
    { large_comp = sh;
      large_comp_split = s2;
    }
  }

  /* if no large_comp, return */
  if( large_comp == nilobj )
  { debug0(DVH, D, "] VerticalHyphenate returning FALSE (no VCAT)");
    return FALSE;
  }

  /* check that large_comp has at least two components */
  FirstDefiniteCompressed(large_comp, link, prev);
  if( link == large_comp )
  { debug0(DVH,D, "] VerticalHyphenate returning FALSE (VCAT: no components)");
    return FALSE;
  }
  NextDefiniteWithGapCompressed(large_comp, link, nxt, g);
  if( link == large_comp )
  { debug0(DVH,D, "] VerticalHyphenate returning FALSE (VCAT: one component)");
    return FALSE;
  }

  /* make sure that first gap does not change when rearranging */
  rump_fwd = max(rump_fwd, fwd(prev, ROW));
  if( MinGap(rump_fwd, back(nxt, ROW), fwd(nxt, ROW), &gap(g)) !=
      MinGap(fwd(prev, ROW), back(nxt, ROW), fwd(nxt, ROW), &gap(g)) )
  { debug0(DVH, D, "] VerticalHyphenate returning FALSE (first gap changes)");
    return FALSE;
  }

  /* check that large_comp has no joins */
  for( link = Down(large_comp);  link != large_comp;  link = NextDown(link) )
  { Child(z, link);
    if( type(z) == GAP_OBJ && mode(gap(z)) != NO_MODE && join(gap(z)) )
    { debug0(DVH, D, "] VerticalHyphenate returning FALSE (VCAT: joined)");
      return FALSE;
    }
  }

  /* enclose all definite components after the first in HCATs */
  for( link = NextDown(Up(prev));  link != large_comp;  link = NextDown(link) )
  { Child(nxt, link);
    if( type(nxt) == GAP_OBJ )  continue;
    if( is_definite(type(nxt)) )
      nxt = EncloseInHcat(nxt, y, large_comp);
  }

  /* move all components after the first to the top level */
  TransferLinks(Up(g), large_comp, NextDown(Up(y)));

  /* change the size of y to its new, smaller value */
  fwd(y, ROW) = fwd(row_thread, ROW) = fwd(large_comp, ROW)
	      = fwd(large_comp_split, ROW) = fwd(prev, ROW);

  /* set link to the link of the first thing before y which is not an index */
  for( link = PrevDown(Up(y));  type(link) == LINK;  link = PrevDown(link) )
  { Child(index, link);
    if( !is_index(type(index)) )  break;
  }

  /* for each index, find where it's pointing and possibly move it */
  while( NextDown(link) != Up(y) )
  { Child(index, NextDown(link));
    assert( is_index(type(index)), "MoveIndexes: is_index!" );
    z = FindTarget(index);
    if( z != nilobj )
    { z = WhichComponent(z);
      if( z != nilobj && z != y )
      { MoveLink(NextDown(link), Up(z), PARENT);
      }
      else link = NextDown(link);
    }
    else link = NextDown(link);
  }

  debug1(DVH, D, "] VerticalHyphenate returning TRUE (y: %s)",
    EchoLength(size(y, ROW)));
  debug0(DVH, DD, "galley after vertical hyphenation:");
  ifdebug(DVH, DD, Parent(z, Up(y)); DebugGalley(z, y, 2));
  return TRUE;
} /* end VerticalHyphenate */


/*****************************************************************************/
/*                                                                           */
/*  static OBJECT BuildMergeTree(int n, OBJECT x, OBJECT *lenv, *lact)       */
/*                                                                           */
/*  Build a balanced tree of n-1 @Merge symbols, whose parameters are the    */
/*  first n children of x.  Return in lenv the environment of the root       */
/*  @Merge symbol, and in *lact the symbol table entry for the parent of     */
/*  this @Merge symbol.                                                      */
/*                                                                           */
/*****************************************************************************/

static OBJECT BuildMergeTree(int n, OBJECT x, OBJECT *lenv, OBJECT *lact)
{ OBJECT res, merge, link, y, l, r, env, act, left_par, right_par;
  debug2(DHY, DD, "BuildMergeTree(%d, %s, -. -)", n, EchoObject(x));

  if( n == 1 )
  { res = New(ENV_OBJ);
    Child(y, Down(x));
    MoveLink(Down(x), res, PARENT);
    assert(type(y)==CLOSURE && has_merge(actual(y)), "BuildMergeTree: has_m!");
    *lact = actual(y);
    *lenv = DetachEnv(y);
    AttachEnv(*lenv, res);
  }
  else
  {
    /* build the two subtrees */
    l = BuildMergeTree(n/2, x, lenv, lact);
    r = BuildMergeTree( n - n/2, x, &env, &act);

    /* set merge to new @Merge closure */
    for( link = Down(act);  link != act;  link = NextDown(link) )
    { Child(y, link);
      if( is_merge(y) )  break;
    }
    assert( y != act, "BuildMergeTree: y!" );
    merge = New(CLOSURE);
    actual(merge) = y;

    /* build left parameter of the new @Merge */
    left_par = New(PAR);
    actual(left_par) = ChildSym(y, LPAR);
    Link(merge, left_par);
    Link(left_par, l);

    /* build right parameter of the new @Merge */
    right_par = New(PAR);
    actual(right_par) = ChildSym(y, RPAR);
    Link(merge, right_par);
    Link(right_par, r);

    res = New(ENV_OBJ);
    Link(res, merge);
    Link(res, env);
  }

  debug2(DHY, DD, "BuildMergeTree returning %s (*lact = %s)",
    EchoObject(res), SymName(*lact));
  return res;
} /* end BuildMergeTree */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT ConvertGalleyList(x)                                              */
/*                                                                           */
/*  Convert a set of galleys x into a single galley containing a balanced    */
/*  tree of @Merge symbols.                                                  */
/*                                                                           */
/*****************************************************************************/

OBJECT ConvertGalleyList(OBJECT x)
{ OBJECT res, y, link, junk1, junk2, obj;  int n;
  debug1(DHY, D, "ConvertGalleyList(%s)", EchoObject(x));
  Child(res, Down(x));
  Child(y, Down(res));
  MoveLink(Down(x), y, CHILD);
  DeleteLink(Down(res));
  MoveLink(Up(x), res, CHILD);
  for( link = Down(x), n = 0;  link != x;  link = NextDown(link), n++ );
  y = BuildMergeTree(n, x, &junk1, &junk2);
  assert( Down(x) == x && Up(x) == x, "ConvertGalleyList: x!" );
  Dispose(x);
  Child(obj, Down(y));
  MoveLink(Down(y), res, PARENT);
  MoveLink(LastDown(y), obj, PARENT);
  assert( Down(y) == y && Up(y) == y, "ConvertGalleyList: y!" );
  Dispose(y);
  debug0(DHY, D, "ConvertGalleyList returning, res =");
  ifdebug(DHY, D, DebugObject(res));
  return res;
} /* end ConvertGalleyList */
