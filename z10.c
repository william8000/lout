/*@z10.c:Cross References:CrossExpand(), CrossSequence()@*********************/
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
/*  FILE:         z10.c                                                      */
/*  MODULE:       Cross References                                           */
/*  EXTERNS:      CrossInit(), CrossMake(), GallTargEval(), CrossExpand(),   */
/*                CrossSequence(), CrossClose()                              */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define	CROSS_LIT	CROSS_TARG
#define	NO_TARGET	0
#define	SEEN_TARGET	1
#define	WRITTEN_TARGET	2

static OBJECT RootCross = nil;			/* header for all crs        */


/*****************************************************************************/
/*                                                                           */
/*  CrossInit(sym)                                                           */
/*                                                                           */
/*  Initialize cross_sym(sym).                                               */
/*                                                                           */
/*****************************************************************************/

CrossInit(sym)
OBJECT sym;
{ OBJECT cs;  int i;
  cs = New(CROSS_SYM);
  target_state(cs)  = NO_TARGET;
  target_seq(cs)    = 0;
  cr_file(cs)       = NO_FILE;
  gall_seq(cs)      = 0;
  gall_tag(cs)      = nil;
  gall_tfile(cs)    = NO_FILE;
  gentag_file(cs)   = NO_FILE;
  symb(cs)          = sym;
  cross_sym(sym)    = cs;
  gentag_fseq(cs)   = NewWord(MAX_FILES, no_fpos);
  for( i = 0;  i < MAX_FILES;  i++ ) string(gentag_fseq(cs))[i] = 0;
  if( RootCross == nil )  RootCross = New(CR_ROOT);
  Link(RootCross, cs);
}


/*****************************************************************************/
/*                                                                           */
/*  OBJECT CrossGenTag(x)                                                    */
/*                                                                           */
/*  Generate a tag suitable for labelling closure x, in such a way that      */
/*  the same tag is likely to be generated on subsequent runs.               */
/*                                                                           */
/*****************************************************************************/

OBJECT CrossGenTag(x)
OBJECT x;
{ unsigned char buff[MAX_LINE], *str1, *str2;
  OBJECT sym, cs, gt, res;  FILE_NUM fnum;
  unsigned char *sgt;
  int seq;
  debug1(DCR, D, "CrossGenTag( %s )", SymName(actual(x)));
  sym = actual(x);
  if( cross_sym(sym) == nil )  CrossInit(sym);
  cs = cross_sym(sym);
  fnum = file_num(fpos(x));
  /* ***
  if( fnum != gentag_file(cs) )
  { gentag_file(cs) = fnum;
    gentag_seq(cs)  = 0;
  }
  *** */
  str1 = FullSymName(sym, ".");
  str2 = FileName(fnum);
  gt = gentag_fseq(cs);
  sgt = string(gt);
  seq = ++(sgt[fnum]);
  if( strlen(str1) + strlen(str2) + 10 >= MAX_LINE )
    Error(FATAL,no_fpos, "automatically generated tag \"%s.%s.%d\" is too long",
	str1, str2, seq);
  sprintf(buff, "\"%s.%s.%d\"", str1, str2, seq);
  res = MakeWord(buff, &fpos(x));
  debug1(DCR, DD, "CrossGenTag returning %s", string(res));
  return res;
} /* end CrossGenTag */


/*@@**************************************************************************/
/*                                                                           */
/*  OBJECT CrossMake(sym, val, ctype)                                        */
/*                                                                           */
/*  Make a cross-reference with the given sym and tag value.                 */
/*  NB The fpos fields are not set, so we cannot EchofilePos from this.      */
/*                                                                           */
/*****************************************************************************/

OBJECT CrossMake(sym, val, ctype)
OBJECT sym, val;  int ctype;
{ OBJECT v1, res;
  debug3(DCR, DD, "CrossMake(%s, %s, %s)", SymName(sym),
		EchoObject(null, val), Image(ctype));
  res = New(CROSS);
  cross_type(res) = ctype;  threaded(res) = FALSE;
  v1 = New(CLOSURE);  actual(v1) = sym;
  Link(res, v1);  Link(res, val);
  debug1(DCR, DD, "CrossMake returning %s", EchoObject(null, res));
  return res;
}


/*****************************************************************************/
/*                                                                           */
/*  OBJECT GallTargEval(sym, dfpos)                                          */
/*                                                                           */
/*  Produce a suitable cross-reference for a galley target.                  */
/*                                                                           */
/*****************************************************************************/

OBJECT GallTargEval(sym, dfpos)
OBJECT sym; FILE_POS *dfpos;
{ OBJECT cs, res;
  unsigned char buff[MAX_LINE], *str;
  debug2(DCR, DD, "GallTargEval( %s,%s )", SymName(sym), EchoFilePos(dfpos));
  if( cross_sym(sym) == nil )  CrossInit(sym);
  cs = cross_sym(sym);
  if( file_num(*dfpos) != gall_tfile(cs) )
  { gall_tfile(cs) = file_num(*dfpos);
    gall_seq(cs)   = 0;
  }
  str = FileName(gall_tfile(cs));
  if( strlen(str) + 6 >= MAX_LINE )
    Error(FATAL, dfpos, "automatically generated tag %s&%d is too long",
	str, ++gall_seq(cs));
  sprintf(buff, "%s&%d", str, ++gall_seq(cs));
  res = CrossMake(sym, MakeWord(buff, dfpos), GALL_TARG);
  debug1(DCR, DD, "GallTargEval returning %s", EchoObject(null, res));
  return res;
} /* end GallTargEval */


/*@@**************************************************************************/
/*                                                                           */
/*  OBJECT CrossExpand(x, env, style, crs_wanted, crs, res_env)              */
/*                                                                           */
/*  Return the value of cross-reference x, with environment *res_env.  If x  */
/*  has a non-literal tag, it must be tracked, so an object is added to *crs */
/*  for this purpose if crs_wanted.  Result replaces x, which is disposed.   */
/*                                                                           */
/*****************************************************************************/
static OBJECT nbt[2] = { nil, nil };
static OBJECT nft[2] = { nil, nil };
static OBJECT ntarget = nil;

OBJECT CrossExpand(x, env, style, crs_wanted, crs, res_env)
OBJECT x, env;  STYLE *style;  BOOLEAN crs_wanted; OBJECT *crs, *res_env;
{ OBJECT sym, res, tag, y, cs, link, db, tmp, index;
  int ctype;  unsigned char buff[MAX_LINE], seq[MAX_LINE], *str;
  FILE_NUM fnum, dfnum;
  long cont, dfpos;
  assert( type(x) == CROSS, "CrossExpand: x!" );
  debug2(DCR, DD, "CrossExpand( %s, %s )",
	EchoObject(null, x), EchoObject(null, *crs));
  assert( NextDown(Down(x)) == LastDown(x), "CrossExpand: #args!" );

  /* manifest and tidy the right parameter */
  Child(tag, LastDown(x));
  tag = Manifest(tag, env, style, nbt, nft, &ntarget, crs, FALSE, FALSE);
  tag = ReplaceWithTidy(tag);

  /* extract sym (the symbol name) and tag (the tag value) from x */
  Child(y, Down(x));
  if( type(y) == CLOSURE )  sym = actual(y);
  ctype = type(y) != CLOSURE ? 1 :
	  type(tag) != WORD ? 2 :
	  string(tag)[0] == '\0' ? 3 :
	  strcmp(string(tag), KW_PRECEDING) == 0 ? CROSS_PREC :
	  strcmp(string(tag), KW_FOLLOWING) == 0 ? CROSS_FOLL : CROSS_LIT;

  res = nil;
  switch( ctype )
  {

    case 1:

      Error(WARN, &fpos(y), "left parameter of %s is not a symbol", KW_CROSS);
      break;


    case 2:

      Error(WARN, &fpos(tag),
	"value of right parameter of %s is not a simple word", KW_CROSS);
      break;


    case 3:
    
      Error(WARN, &fpos(tag),
	"value of right parameter of %s is an empty word", KW_CROSS);
      break;


    case CROSS_LIT:
    
      if( cross_sym(sym) == nil )  CrossInit(sym);
      cs = cross_sym(sym);
      if( sym == MomentSym && strcmp(string(tag), KW_NOW) == 0 )
      {	/* this is a request for the current time */
	res = StartMoment();
      }
      else for( link = NextUp(Up(cs));  link != cs;  link = NextUp(link) )
      {	Parent(db, link);
	assert( type(db) == WORD, "CrossExpand: db!" );
	if( DbRetrieve(db, FALSE, sym, string(tag), seq, &dfnum,&dfpos,&cont) )
	{ res = ReadFromFile(dfnum, dfpos, sym);
	  if( db != OldCrossDb )  AttachEnv(env, res);
	  break;
	}
      }
      break;


    case CROSS_PREC:
    case CROSS_FOLL:
    
      if( cross_sym(sym) == nil )  CrossInit(sym);
      cs = cross_sym(sym);
      assert( cs != nil, "CrossExpand/CROSS_FOLL: cs == nil!" );
      assert( type(cs) == CROSS_SYM, "CrossExpand/CROSS_FOLL: type(cs)!" );
      fnum = file_num(fpos(tag));
      if( fnum != cr_file(cs) )
      {	cr_file(cs) = fnum;
	cr_seq(cs) = 0;
      }
      str = FileName(fnum);
      if( strlen(str) + 5 >= MAX_LINE )
	Error(FATAL, &fpos(x), "automatically generated tag %s_%d is too long",
	  str, ++cr_seq(cs));
      sprintf(buff, "%s_%d", str, ++cr_seq(cs));
      tmp = CrossMake(sym, MakeWord(buff, &fpos(tag)), ctype);
      index = New(ctype);
      actual(index) = tmp;
      Link(index, tmp);
      if( crs_wanted )
      {	if( *crs == nil )  *crs = New(CR_LIST);
	link = Link(*crs, index);
      }
      else Error(FATAL, &fpos(x), "%s or %s tag not allowed here",
	KW_PRECEDING, KW_FOLLOWING);
      if( AllowCrossDb &&
	  DbRetrieve(OldCrossDb, FALSE, sym, buff, seq, &dfnum, &dfpos,&cont) )
	res = ReadFromFile(dfnum, dfpos, nil);
      break;


    default:
    
      Error(INTERN, no_fpos, "CrossExpand switch!");
      break;


  } /* end switch */
  if( res == nil )
  { OBJECT envt;
    if( ctype > 1 )  Error(WARN, &fpos(x), "%s%s%s unknown",
		SymName(sym), KW_CROSS, string(tag));

    /* build dummy result with environment attached */
    /* nb at present we are not adding dummy import closures to this! */
    res = New(CLOSURE);  actual(res) = sym;
    y = res;
    debug1(DCR, DD, "First y = %s", SymName(actual(y)));
    while( enclosing(actual(y)) != StartSym )
    { tmp = New(CLOSURE);
      actual(tmp) = enclosing(actual(y));
      envt = SetEnv(tmp, nil);
      AttachEnv(envt, y);
      y = tmp;
      debug1(DCR, DD, "Later y = %s", SymName(actual(y)));
    }
    envt = New(ENV);  Link(y, envt);
  }

  /* set environment, replace x by res, debug and exit */
  *res_env = DetachEnv(res);
  ReplaceNode(res, x);
  DisposeObject(x);
  debug1(DCR, DD, "CrossExpand returning %s", EchoObject(null, res));
  debug1(DCR, DD, "  *crs = %s", EchoObject(null, *crs));
  debug1(DCR, DD, "  *res_env = %s", EchoObject(null, *res_env));
  return res;
} /* end CrossExpand */


/*@@**************************************************************************/
/*                                                                           */
/*  CrossSequence(x)                                                         */
/*                                                                           */
/*  Object x is an insinuated cross-reference that has just been popped off  */
/*  the top of the root galley.  Resolve it with the sequence of others.     */
/*                                                                           */
/*****************************************************************************/

CrossSequence(x)
OBJECT x;
{ OBJECT sym, tag, val, tmp, cs, par, key, link, y;
  unsigned ctype;  unsigned char buff[MAX_LINE], *str, *seq;
  FILE_NUM dfnum;  int dfpos;

  /* if suppressing cross-referencing, dispose x and quit */
  if( !AllowCrossDb )
  { if( Up(x) == x )  DisposeObject(x);
    debug0(DCR, D, "CrossSequence returning (!AllowCrossDb).");
    return;
  }

  /* get interesting fragments from x */
  assert( type(x) == CROSS, "CrossSequence: type(x)!" );
  ctype = cross_type(x);
  Child(tmp, Down(x));
  assert( type(tmp) == CLOSURE, "CrossSequence: type(tmp)!" );
  sym = actual(tmp);
  if( cross_sym(sym) == nil )  CrossInit(sym);
  cs = cross_sym(sym);
  assert( type(cs) == CROSS_SYM, "CrossSequence: cs!" );

  /* debug output */
  debug2(DCR, D, "CrossSequence %s %s", Image(ctype), SymName(sym));
  debug1(DCR, DD, "  x = %s", EchoObject(null, x));
  ifdebug(DCR, DD, EchoObject(stderr, cs));

  /* delete as much of x as possible */
  Child(tag, NextDown(Down(x)));
  DeleteLink(NextDown(Down(x)));
  if( Up(x) == x )  DisposeObject(x);

  switch( ctype )
  {
    case GALL_FOLL:
    case GALL_PREC:

      /* find key of the galley, if any */
      val = tag;  key = nil;
      for( link = Down(val);  link != val;  link = NextDown(link) )
      {	Child(par, link);
	if( type(par) == PAR && (is_key(actual(par)) || is_tag(actual(par))) )
	{ assert( Down(par) != par, "CrossSequence: PAR child!" );
	  Child(key, Down(par));
	  key = ReplaceWithTidy(key);
	}
      }

      /* write out the galley */
      str = FileName(file_num(fpos(val)));
      if( strlen(str) + strlen(DATA_SUFFIX) >= MAX_LINE )
	Error(FATAL, &fpos(val), "database file name %s%s is too long",
		str, DATA_SUFFIX);
      sprintf(buff, "%s%s", str, DATA_SUFFIX);
      dfnum = FileNum(buff);
      if( dfnum == NO_FILE )
	dfnum = DefineFile(MakeWord(buff, &fpos(val)),
				DATABASE_FILE, SOURCE_PATH);
      AppendToFile(val, dfnum, &dfpos);

      /* determine the sequence number or string of this galley */
      if( key == nil )
      {	sprintf(buff, "%.5d", ++gall_seq(cs));
	seq = buff;
      }
      else if( type(key) != WORD )
      {	Error(WARN, &fpos(key), "%s parameter is not a word", KW_KEY);
	seq = (unsigned char *) "badkey";
      }
      else if(  string(key)[0] == '\0' )
      {	Error(WARN, &fpos(key), "%s parameter is empty word", KW_KEY);
	seq = (unsigned char *) "badkey";
      }
      else seq = string(key);

      /* either write out the index immediately or store it for later */
      if( ctype == GALL_PREC )
      {	if( gall_tag(cs) == nil )
	{ Error(WARN, &fpos(val), "no %s precedes this %s%s%s",
		SymName(sym), SymName(sym), KW_CROSS, KW_PRECEDING);
	  debug0(DCR, DD, "  ... so substituting \"none\"");
	  gall_tag(cs) = MakeWord("none", &fpos(val));
	}
	assert( type(gall_tag(cs)) == WORD && string(gall_tag(cs))[0] != '\0',
			"CrossSequence: gall_tag!" );
	debug3(DCR, D, "  inserting galley (prec) %s&%s %s", SymName(sym),
	  string(gall_tag(cs)), seq);
	DbInsert(NewCrossDb, TRUE, sym, string(gall_tag(cs)), seq,
			dfnum, (long) dfpos);
      }
      else
      {	tmp = MakeWord(seq, &fpos(val));
	gall_rec(tmp) = TRUE;
	file_num(fpos(tmp)) = dfnum;
	gall_pos(tmp) = dfpos;
	Link(cs, tmp);
	debug2(DCR, D, "  saving galley (foll) %s&? %s", SymName(sym), seq);
      }
      DisposeObject(val);
      break;


    case GALL_TARG:

      if( gall_tag(cs) != nil )  DisposeObject(gall_tag(cs));
      if( type(tag) != WORD || string(tag)[0] == '\0' )
      {
	debug2(DCR, DD, "  GALL_TARG %s put none for %s",
	  SymName(sym), EchoObject(null, tag));
	DisposeObject(tag);
	gall_tag(cs) = MakeWord("none", no_fpos);
      }
      else gall_tag(cs) = tag;
      debug2(DCR, D, "  have new %s gall_targ %s", SymName(sym),
	  EchoObject(null, gall_tag(cs)));
      for( link = Down(cs);  link != cs;  link = NextDown(link) )
      {	Child(y, link);
	assert( type(y) == WORD && string(y)[0] != '\0',
				"CrossSequence: GALL_TARG y!" );
	if( gall_rec(y) )
	{
	  debug3(DCR, D, "  inserting galley (foll) %s&%s %s", SymName(sym),
	    string(gall_tag(cs)), string(y));
	  DbInsert(NewCrossDb, TRUE, sym, string(gall_tag(cs)), string(y),
			file_num(fpos(y)), (long) gall_pos(y));
	  link = PrevDown(link);
	  DisposeChild(NextDown(link));
	}
      }
      break;


    case CROSS_PREC:

      if( target_state(cs) == NO_TARGET )
      {	Error(WARN, &fpos(tag), "no invokation of %s precedes this %s%s%s",
		SymName(sym), SymName(sym), KW_CROSS, KW_PRECEDING);
	break;
      }
      if( target_state(cs) == SEEN_TARGET )
      {
	debug2(DCR, D, "  inserting %s cross_targ %s",
	  SymName(sym), target_val(cs));
	AppendToFile(target_val(cs), target_file(cs), &target_pos(cs));
	DisposeObject(target_val(cs));
	target_val(cs) = nil;
	target_state(cs) = WRITTEN_TARGET;
      }
      if( type(tag) != WORD || string(tag)[0] == '\0' )
      {
	debug2(DCR, DD, "  GALL_TARG %s put none for %s", SymName(sym),
		EchoObject(null, tag));
	DisposeObject(tag);
	tag = MakeWord("none", no_fpos);
      }
      debug3(DCR, D, "  inserting cross (prec) %s&%s %s", SymName(sym),
	    string(tag), "0");
      DbInsert(NewCrossDb, FALSE, sym, string(tag), "0", target_file(cs),
			(long) target_pos(cs));
      DisposeObject(tag);
      break;


    case CROSS_FOLL:

      if( type(tag) != WORD )
      {	Error(WARN, &fpos(tag), "tag of %s is not a simple word",
		SymName(symb(cs)));
	debug1(DCR, DD, "  tag = %s", EchoObject(null, tag));
      }
      else if( string(tag)[0] == '\0' )
      {
        debug1(DCR, D, "  ignoring cross (foll) %s (empty tag)", SymName(sym));
      }
      else
      { Link(cs, tag);
	gall_rec(tag) = FALSE;
        debug3(DCR, D, "  storing cross (foll) %s&%s %s", SymName(sym),
	    string(tag), "?");
      }
      break;


    case CROSS_TARG:

      /* get rid of old target, if any, and add new one */
      if( target_state(cs) == SEEN_TARGET )
      {
	debug2(DCR, D, "  disposing unused %s cross_targ %s", SymName(sym),
	  target_val(cs));
	DisposeObject(target_val(cs));
      }
      debug2(DCR, D, "  remembering new %s cross_targ %s", SymName(sym),
	EchoObject(null, tag));
      target_val(cs) = tag;
      assert( Up(tag) == tag, "CrossSeq: Up(tag)!" );
      str = FileName(file_num(fpos(tag)));
      if( strlen(str) + strlen(DATA_SUFFIX) >= MAX_LINE )
	Error(FATAL, &fpos(tag), "database file name %s%s is too long",
		str, DATA_SUFFIX);
      sprintf(buff, "%s%s", str, DATA_SUFFIX);
      target_file(cs) = FileNum(buff);
      if( target_file(cs) == NO_FILE )
	target_file(cs) = DefineFile(MakeWord(buff, &fpos(tag)),
					DATABASE_FILE, SOURCE_PATH);
      target_state(cs) = SEEN_TARGET;

      /* store tag of the galley, if any */
      tag = nil;
      assert( type(target_val(cs)) == CLOSURE, "CrossSequence: target_val!" );
      link = Down(target_val(cs));
      for( ;  link != target_val(cs);  link = NextDown(link) )
      {	Child(par, link);
	if( type(par) == PAR && is_tag(actual(par)) )
	{ assert( Down(par) != par, "CrossSequence: Down(PAR)!" );
	  Child(tag, Down(par));
	  tag = ReplaceWithTidy(tag);
	  if( type(tag) != WORD )
	  { Error(WARN, &fpos(tag), "%s tag is not a simple word",
			SymName(actual(target_val(cs))));
	    debug1(DCR, DD, "  tag = %s", EchoObject(null, tag));
	  }
	  else if( string(tag)[0] == '\0' )
	  {
            debug1(DCR, D, "  ignoring cross (own tag) %s (empty tag)",
		SymName(sym));
	  }
	  else
	  { Link(cs, tag);
	    gall_rec(tag) = FALSE;
            debug3(DCR, D, "  storing cross (own tag) %s&%s %s", SymName(sym),
		string(tag), "?");
	  }
	  break;
	}
      }

      /* if new target is already writable, write it */
      if( Down(cs) != cs )
      {
	debug2(DCR, D, "  writing %s cross_targ %s", SymName(sym),
		EchoObject(null, target_val(cs)));
	AppendToFile(target_val(cs), target_file(cs), &target_pos(cs));
	DisposeObject(target_val(cs));
	for( link = Down(cs);  link != cs;  link = NextDown(link) )
	{ Child(tag, link);
	  assert( type(tag) == WORD && string(tag)[0] != '\0',
			"CrossSeq: non-WORD or empty tag!" );
	  if( !gall_rec(tag) )
	  {
	    debug3(DCR, D, "  inserting cross (foll) %s&%s %s", SymName(sym),
	      string(tag), "0");
	    DbInsert(NewCrossDb, FALSE, sym, string(tag),
			"0", target_file(cs), (long) target_pos(cs));
	    link = PrevDown(link);
	    DisposeChild(NextDown(link));
	  }
	}
	target_state(cs) = WRITTEN_TARGET;
      }
      break;


    default:

      Error(INTERN, &fpos(tag), "CrossSequence: ctype = %s", Image(ctype));
      break;

  } /* end switch */
  debug0(DCR, D, "CrossSequence returning.");
  debug0(DCR, DD, "   cs =");
  ifdebug(DCR, DD, EchoObject(stderr, cs));
} /* end CrossSequence */


/*@@**************************************************************************/
/*                                                                           */
/*  CrossClose()                                                             */
/*                                                                           */
/*  Check for dangling forward references, and convert old cross reference   */
/*  database to new one.                                                     */
/*                                                                           */
/*****************************************************************************/

CrossClose()
{ OBJECT link, cs, ylink, y, sym;  BOOLEAN g;  int len, count;
  FILE_NUM dfnum;  long dfpos, cont;
  unsigned char buff[MAX_LINE], seq[MAX_LINE], tag[MAX_LINE];
  debug0(DCR, D, "CrossClose()");
  ifdebug(DCR, DD, if( RootCross != nil ) EchoObject(stderr, RootCross));

  /* if suppressing cross referencing, return */
  if( !AllowCrossDb )
  { debug0(DCR, D, "CrossClose returning (!AllowCrossDb).");
    return;
  }

  /* check for dangling forward references and dispose cross ref structures */
  if( RootCross != nil )
  { for( link = Down(RootCross);  link != RootCross;  link = NextDown(link) )
    { Child(cs, link);
      assert( type(cs) == CROSS_SYM, "CrossClose: type(cs)!" );
      count = 0;  ylink = Down(cs);
      while( ylink != cs && count <= 5 )
      {	Child(y, ylink);
	Error(WARN, &fpos(y), "no invokation of %s follows this %s%s%s",
		SymName(symb(cs)), SymName(symb(cs)), KW_CROSS, KW_FOLLOWING);
	debug2(DCR, D, "gall_rec(y) = %s, y = %s",
			bool(gall_rec(y)), EchoObject(null, y));
	if( gall_rec(y) ) DbInsert(NewCrossDb, TRUE, symb(cs), "none",
			string(y), file_num(fpos(y)), (long) gall_pos(y));
	count++;  ylink = NextDown(ylink);
      }
      if( ylink != cs )  Error(WARN, no_fpos, "and more undefined %s%s%s",
				SymName(symb(cs)), KW_CROSS, KW_FOLLOWING);
      ifdebug(ANY, D,
	if( target_state(cs) == SEEN_TARGET )  DisposeObject(target_val(cs));
	if( gall_tag(cs) != nil )  DisposeObject(gall_tag(cs));
      );
    }
    ifdebug(ANY, D, DisposeObject(RootCross); );
  }

  /* add to NewCrossDb those entries of OldCrossDb from other source files */
  if( AllowCrossDb )
  cont = 0L;  len = strlen(DATA_SUFFIX);
  while( DbRetrieveNext(OldCrossDb, &g, &sym, tag, seq, &dfnum, &dfpos, &cont) )
  { if( g ) continue;
    strcpy(buff, FileName(dfnum));
    buff[strlen(buff) - len] = '\0';
    if( FileNum(buff) == NO_FILE )
      DbInsert(NewCrossDb, FALSE, sym, tag, seq, dfnum, dfpos);
  }

  /* make NewCrossDb readable, for next run */
  DbConvert(NewCrossDb, TRUE);

  debug0(DCR, D, "CrossClose returning.");
} /* end CrossClose */
