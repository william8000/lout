/*@z33.c:Database Service:OldCrossDb(), NewCrossDb(), SymToNum()@*************/
/*                                                                           */
/*  LOUT: A HIGH-LEVEL LANGUAGE FOR DOCUMENT FORMATTING (VERSION 2.05)       */
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
/*  FILE:         z33.c                                                      */
/*  MODULE:       Database Service                                           */
/*  EXTERNS:      OldCrossDb, NewCrossDb, DbCreate(), DbInsert(),            */
/*                DbConvert(), DbClose(), DbLoad(), DbRetrieve(),            */
/*                DbRetrieveNext()                                           */
/*                                                                           */
/*****************************************************************************/
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  OldCrossDb     Database containing cross references from previous run.   */
/*  NewCrossDb     Writable database of cross references from this run.      */
/*                                                                           */
/*****************************************************************************/

OBJECT OldCrossDb, NewCrossDb;


/*****************************************************************************/
/*                                                                           */
/*  #define SymToNum(db, sym, num, gall)                                     */
/*                                                                           */
/*  Set num to the number used to refer to sym in database db.  If sym is    */
/*  not currently referred to in db, create a new number and record sym.     */
/*  If gall is true, sym is the target of galleys stored in this database.   */
/*  Store in boolean fields db_targ(link) and is_extern_target(sym).         */
/*                                                                           */
/*****************************************************************************/

#define SymToNum(db, sym, num, gall)					\
{ OBJECT link, yy;  int count;						\
  count = 0;								\
  for( link = Down(db);  link != db;  link = NextDown(link) )		\
  { Child(yy, link);							\
    assert(type(yy)==CROSS_SYM || type(yy)==ACAT, "SymToNum: yy!");	\
    if( type(yy) != CROSS_SYM )  continue;				\
    if( symb(yy) == sym )  break;					\
    if( number(link) > count )  count = number(link);			\
  }									\
  if( link == db )							\
  { if( cross_sym(sym) == nil )  CrossInit(sym);			\
    link = Link(db, cross_sym(sym));					\
    number(link) = count + 1;						\
    db_targ(link) = FALSE;						\
  }									\
  num = number(link);							\
  if( gall )  db_targ(link) = is_extern_target(sym) =			\
				uses_extern_target(sym) = TRUE;		\
} /* end SymToNum */


/*@::NumToSym(), DbCreate()@**************************************************/
/*                                                                           */
/*  #define NumToSym(db, num, sym)                                           */
/*                                                                           */
/*  Set sym to the symbol which is referred to in database db by num.        */
/*                                                                           */
/*****************************************************************************/

#define NumToSym(db, num, sym)						\
{ OBJECT link, y;							\
  for( link = Down(db);  link != db;  link = NextDown(link) )		\
  { Child(y, link);							\
    if( type(y) == CROSS_SYM && number(link) == num )  break;		\
  }									\
  if( link == db )  Error(INTERN, &fpos(db), "NumToSym: no sym!");	\
  assert( type(y) == CROSS_SYM, "NumToSym: y!" );			\
  sym = symb(y);							\
} /* end NumToSym */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT DbCreate(x)                                                       */
/*                                                                           */
/*  Create a new writable database with name (i.e. file stem) x and file     */
/*  position fpos for error messages.                                        */
/*                                                                           */
/*****************************************************************************/

OBJECT DbCreate(x)
OBJECT x;
{ OBJECT db = x;
  debug1(DBS, D, "DbCreate(%s)", string(db));
  assert( is_word(type(x)), "DbCreate: !is_word(type(x))" );
  reading(db) = FALSE;  filep(db) = null;
  debug1(DBS, D, "DbCreate returning %s", EchoObject(db));
  return db;
} /* end DbCreate */


/*@::DbInsert()@**************************************************************/
/*                                                                           */
/*  DbInsert(db, gall, sym, tag, seq, dfnum, dfpos)                          */
/*                                                                           */
/*  Insert a new entry into writable database db.  The primary key of the    */
/*  entry has these three parts:                                             */
/*                                                                           */
/*      gall        TRUE if inserting a galley                               */
/*      sym         The symbol which is the target of this entry             */
/*      tag         The tag of this target (must be a non-null string)       */
/*                                                                           */
/*  There is also an auxiliary key, seq, which enforces an ordering on       */
/*  entries with equal primary keys but is not itself ever retrieved.  This  */
/*  ordering is used for sorted galleys.  The value of the entry has the     */
/*  following parts:                                                         */
/*                                                                           */
/*      dfnum       The file containing the object                           */
/*      dfpos       The position of the object in that file                  */
/*                                                                           */
/*****************************************************************************/

DbInsert(db, gall, sym, tag, seq, dfnum, dfpos)
OBJECT db;  BOOLEAN gall;  OBJECT sym;  FULL_CHAR *tag, *seq;
FILE_NUM dfnum;  long dfpos;
{ int symnum;
  FULL_CHAR buff[MAX_LINE];
  assert( is_word(type(db)), "DbInsert: db!" );
  assert( tag[0] != '\0', "DbInsert: null tag!" );
  assert( seq[0] != '\0', "DbInsert: null seq!" );
  ifdebug(DPP, D, ProfileOn("DbInsert"));
  debug6(DBS, D, "DbInsert(%s, %s, %s, %s, %s, %s, dfpos)",
	string(db), bool(gall), SymName(sym), tag, seq,
	dfnum == NO_FILE ? AsciiToFull(".") : FileName(dfnum));
  if( reading(db) )  Error(INTERN, &fpos(db), "insert into reading database!");
  if( filep(db) == null )
  { if( StringLength(string(db)) + StringLength(NEW_INDEX_SUFFIX) >= MAX_LINE )
      Error(FATAL, no_fpos, "database file name %s%s is too long",
	string(db), NEW_INDEX_SUFFIX);
    StringCopy(buff, string(db));
    StringCat(buff, NEW_INDEX_SUFFIX);
    filep(db) = StringFOpen(buff, "w");
    if( filep(db) == null )
      Error(FATAL, &fpos(db), "cannot write to database file %s", buff);
  }
  if( dfnum != NO_FILE )
  { StringCopy(buff, FileName(dfnum));
    StringCopy(&buff[StringLength(buff)-StringLength(DATA_SUFFIX)], STR_EMPTY);
  }
  else StringCopy(buff, AsciiToFull("."));
  SymToNum(db, sym, symnum, gall);
  ifdebug(DBS, D,
  fprintf(stderr, "  -> %s%d&%s\t%s\t%ld\t%s\n", gall ? "&" : "", symnum,
    tag, seq, dfpos, buff);
  );
  fprintf(filep(db), "%s%d&%s\t%s\t%ld\t%s\n", gall ? "&" : "", symnum,
    tag, seq, dfpos, buff);
  debug0(DBS, D, "DbInsert returning.");
  ifdebug(DPP, D, ProfileOff("DbInsert"));
} /* end DbInsert */


/*@::DbConvert(), DbClose()@**************************************************/
/*                                                                           */
/*  DbConvert(db, full_name)                                                 */
/*                                                                           */
/*  Convert database db from writable to readable, then dispose it.          */
/*  full_name is TRUE if symbols are to be known by their full path name.    */
/*                                                                           */
/*****************************************************************************/

DbConvert(db, full_name)
OBJECT db;  BOOLEAN full_name;
{ FULL_CHAR oldname[MAX_LINE+10], newname[MAX_LINE];
  char buff[2*MAX_LINE + 20];
  OBJECT link, y;
  ifdebug(DPP, D, ProfileOn("DbConvert"));
  debug2(DBS, D, "DbConvert( %d %s )", (int) db,string(db));
  if( reading(db) )  Error(INTERN, &fpos(db), "DbConvert: reading database!");
  StringCopy(newname, string(db));
  StringCat(newname, INDEX_SUFFIX);
  StringCopy(oldname, string(db));
  StringCat(oldname, NEW_INDEX_SUFFIX);
  if( filep(db) != null )
  { for( link = Down(db);  link != db;  link = NextDown(link) )
    { Child(y, link);
      assert( type(y) == CROSS_SYM || type(y) == ACAT, "DbConvert: y!" );
      if( type(y) != CROSS_SYM )  continue;
      fprintf(filep(db), "%s %d %s\n",
	db_targ(link) ? "#target" : "#symbol",
	number(link),
	full_name ? FullSymName(symb(y), AsciiToFull(" ")) : SymName(symb(y)));
    }
    fclose(filep(db));
    sprintf(buff, "sort -o %s %s", newname, oldname);
    system(buff);
  }
  else StringUnlink(newname);
  StringUnlink(oldname);
  DeleteNode(db);
  debug0(DBS, D, "DbConvert returning.");
  ifdebug(DPP, D, ProfileOff("DbConvert"));
} /* end DbConvert */


/*****************************************************************************/
/*                                                                           */
/*  DbClose(db)                                                              */
/*                                                                           */
/*  Close readable database db.                                              */
/*                                                                           */
/*****************************************************************************/

DbClose(db)
OBJECT db;
{ if( db != nil && filep(db) != NULL )
  {  fclose(filep(db));
     filep(db) = NULL;
  }
} /* end DbClose */


/*@::DbLoad()@****************************************************************/
/*                                                                           */
/*  OBJECT DbLoad(stem, fpath, create, symbs)                                */
/*                                                                           */
/*  Open for reading the database whose index file name is string(stem).li.  */
/*  This file has not yet been defined; its search path is fpath.  If it     */
/*  will not open and create is true, try creating it from string(stem).ld.  */
/*                                                                           */
/*  symbs is an ACAT of CLOSUREs showing the symbols that the database may   */
/*  contain; or nil if the database may contain any symbol.                  */
/*                                                                           */
/*****************************************************************************/

OBJECT DbLoad(stem, fpath, create, symbs)
OBJECT stem;  int fpath;  BOOLEAN create;  OBJECT symbs;
{ FILE *fp;  OBJECT db, t, res, tag, par, sym, link, y;
  int i, lnum, num, count;  FILE_NUM index_fnum, dfnum;  long dfpos;
  BOOLEAN gall;  FULL_CHAR line[MAX_LINE], sym_name[MAX_LINE];
  ifdebug(DPP, D, ProfileOn("DbLoad"));
  debug3(DBS, D, "DbLoad(%s, %d, %s, -)", string(stem), fpath, bool(create));

  /* open or else create index file fp */
  index_fnum = DefineFile(string(stem), INDEX_SUFFIX, &fpos(stem), INDEX_FILE,
		 fpath);
  fp = OpenFile(index_fnum, create, FALSE);
  if( fp == null && create )
  { db = nil;
    dfnum = DefineFile(string(stem), DATA_SUFFIX, &fpos(stem),
      DATABASE_FILE, DATABASE_PATH);
    dfpos = 0L;  LexPush(dfnum, 0, DATABASE_FILE);  t = LexGetToken();
    while( type(t) == LBR )
    { res = Parse(&t, StartSym, FALSE, FALSE);
      if( t != nil || type(res) != CLOSURE )  Error(FATAL, &fpos(res),
	"syntax error in database file %s", FileName(dfnum));
      assert( symbs != nil, "DbLoad: create && symbs == nil!" );
      if( symbs != nil )
      {	for( link = Down(symbs);  link != symbs;  link = NextDown(link) )
	{ Child(y, link);
	  if( type(y) == CLOSURE && actual(y) == actual(res) )  break;
	}
	if( link == symbs )  Error(FATAL, &fpos(res),
	  "%s found in database but not declared in %s line",
	  SymName(actual(res)), KW_DATABASE);
      }
      for( tag = nil, link = Down(res);  link != res;  link = NextDown(link) )
      {	Child(par, link);
	if( type(par) == PAR && is_tag(actual(par)) && Down(par) != par )
	{ Child(tag, Down(par));
	  break;
	}
      }
      if( tag == nil )
	Error(FATAL, &fpos(res), "database symbol %s has no tag", SymName(res));
      tag = ReplaceWithTidy(tag);
      if( !is_word(type(tag)) )
	Error(FATAL, &fpos(res), "database symbol tag is not a simple word");
      if( StringEqual(string(tag), STR_EMPTY) )
	Error(FATAL, &fpos(res), "database symbol tag is an empty word");
      if( db == nil )
      {	StringCopy(line, FileName(dfnum));
	i = StringLength(line) - StringLength(INDEX_SUFFIX);
	assert( i > 0, "DbLoad: FileName(dfnum) (1)!" );
	StringCopy(&line[i], STR_EMPTY);
	db = DbCreate(MakeWord(WORD, line, &fpos(stem)));
      }
      DbInsert(db, FALSE, actual(res), string(tag), STR_ZERO, NO_FILE, dfpos);
      DisposeObject(res);  dfpos = LexNextTokenPos();  t = LexGetToken();
    }
    if( type(t) != END )
      Error(FATAL, &fpos(t), "%s or end of file expected here", KW_LBR);
    LexPop();
    if( db == nil )
    { StringCopy(line, FileName(dfnum));
      i = StringLength(line) - StringLength(INDEX_SUFFIX);
      assert( i > 0, "DbLoad: FileName(dfnum) (2)!" );
      StringCopy(&line[i], STR_EMPTY);
      db = DbCreate(MakeWord(WORD, line, &fpos(stem)));
    }
    DbConvert(db, FALSE);
    if( (fp = OpenFile(index_fnum, FALSE, FALSE)) == null )
      Error(FATAL, &fpos(db), "cannot open database file %s",
      FileName(index_fnum));
  }

  /* set up database record */
  StringCopy(line, FileName(index_fnum));
  i = StringLength(line) - StringLength(INDEX_SUFFIX);
  assert( i > 0, "DbLoad: FileName(index_fnum)!" );
  StringCopy(&line[i], STR_EMPTY);
  db = MakeWord(WORD, line, &fpos(stem));
  reading(db) = TRUE;  filep(db) = fp;
  if( symbs != nil )
  { assert( type(symbs) = ACAT, "DbLoad: type(symbs)!" );
    Link(db, symbs);
  }
  if( fp == null )
  { debug1(DBS, D, "DbLoad returning (empty) %s", string(db));
    ifdebug(DPP, D, ProfileOff("DbLoad"));
    return db;
  }

  /* read header lines of index file, find its symbols */
  left_pos(db) = 0;  lnum = 0;
  while( StringFGets(line, MAX_LINE, fp) != NULL && line[0] == '#' )
  { lnum++;
    left_pos(db) = (int) ftell(fp);
    gall = StringBeginsWith(line, AsciiToFull("#target "));
    sscanf( (char *) line, gall ? "#target %d" : "#symbol %d", &num);
    for( i = 8;  line[i] != CH_SPACE && line[i] != '\0';  i++ );
    if( symbs == nil )
    {
      /* any symbols are possible, full path names in index file required */
      count = 0;  sym = StartSym;
      while( line[i] != CH_NEWLINE && line[i] != '\0' )
      {	PushScope(sym, FALSE, FALSE);  count++;
	sscanf( (char *) &line[i+1], "%s", sym_name);
	sym = SearchSym(sym_name, StringLength(sym_name));
	i += StringLength(sym_name) + 1;
      }
      for( i = 1;  i <= count;  i++ )  PopScope();
    }
    else
    {
      /* only symbs symbols possible, full path names not required */
      sym = nil;
      sscanf( (char *) &line[i+1], "%s", sym_name);
      for( link = Down(symbs);  link != symbs;  link = NextDown(link) )
      {	Child(y, link);
	assert( type(y) == CLOSURE, "DbLoad: type(y) != CLOSURE!" );
	if( StringEqual(sym_name, SymName(actual(y))) )
	{ sym = actual(y);
	  break;
	}
      }
    }
    if( sym != nil && sym != StartSym )
    { if( cross_sym(sym) == nil )  CrossInit(sym);
      link = Link(db, cross_sym(sym));
      number(link) = num;  db_targ(link) = gall;
      if( gall )  is_extern_target(sym) = uses_extern_target(sym) = TRUE;
    }
    else
    { Error(WARN, &fpos(db), "undefined symbol in database file %s (line %d)",
			FileName(index_fnum), lnum);
      debug1(DBS, D, "DbLoad returning %s (error)", string(db));
      fclose(filep(db));  filep(db) = null;  /* effectively an empty database */
      ifdebug(DPP, D, ProfileOff("DbLoad"));
      return db;
    }
  }
  debug1(DBS, D, "DbLoad returning %s", string(db));
  ifdebug(DPP, D, ProfileOff("DbLoad"));
  return db;
} /* end DbLoad */


/*@::SearchFile()@************************************************************/
/*                                                                           */
/*  static BOOLEAN SearchFile(fp, left, right, str, line)                    */
/*                                                                           */
/*  File fp is a text file.  left is the beginning of a line, right is the   */
/*  end of a line.   Search the file by binary search for a line beginning   */
/*  with str.  If found, return it in line, else return FALSE.               */
/*                                                                           */
/*****************************************************************************/

static BOOLEAN SearchFile(fp, left, right, str, line)
FILE *fp;  int left, right;  FULL_CHAR *str, *line;
{ int l, r, mid, mid_end;  FULL_CHAR buff[MAX_LINE];  BOOLEAN res;
  ifdebug(DPP, D, ProfileOn("SearchFile"));
  debug3(DBS, DD, "SearchFile(fp, %d, %d, %s, line)", left, right, str);

  l = left;  r = right;
  while( l <= r )
  {
    /* loop invt: (l==0 or fp[l-1]==CH_NEWLINE) and (fp[r] == CH_NEWLINE)    */
    /* and first key >= str lies in the range fp[l..r+1]                     */

    /* find line near middle of the range; mid..mid_end brackets it */
    debug2(DBS, DD, "  start loop: l = %d, r = %d", l, r);
    mid = (l + r)/2;
    fseek(fp, (long) mid, 0);
    do { mid++; } while( getc(fp) != CH_NEWLINE );
    if( mid == r + 1 )
    { mid = l;
      fseek(fp, (long) mid, 0);
    }
    StringFGets(line, MAX_LINE, fp);
    mid_end = (int) ftell(fp) - 1;
    debug3(DBS, DD, "  mid: %d, mid_end: %d, line: %s", mid, mid_end, line);
    assert( l <= mid,      "SearchFile: l > mid!"        );
    assert( mid < mid_end, "SearchFile: mid >= mid_end!" );
    assert( mid_end <= r,  "SearchFile: mid_end > r!"    );

    /* compare str with this line and prepare next step */
    debug2(DBS, DD, "  comparing key %s with line %s", str, line);
    if( StringLessEqual(str, line) )  r = mid - 1;
    else l = mid_end + 1;
  } /* end while */

  /* now first key >= str lies in fp[l]; compare it with str */
  if( l < right )
  { fseek(fp, (long) l, 0);
    StringFGets(line, MAX_LINE, fp);
    sscanf( (char *) line, "%s", buff);
    res = StringEqual(str, buff);
  }
  else res = FALSE;
  debug1(DBS, DD, "SearchFile returning %s", bool(res));
  ifdebug(DPP, D, ProfileOff("SearchFile"));
  return res;
} /* end SearchFile */


/*@::DbRetrieve()@************************************************************/
/*                                                                           */
/*  BOOLEAN DbRetrieve(db, gall, sym, tag, seq, dfnum, dfpos, cont)          */
/*                                                                           */
/*  Retrieve the first entry of database db with the given gall, sym and     */
/*  tag.  Set *seq, *dfnum, *dfpos to the associated value.                  */
/*  Set *cont to a private value for passing to DbRetrieveNext.              */
/*                                                                           */
/*****************************************************************************/

BOOLEAN DbRetrieve(db, gall, sym, tag, seq, dfnum, dfpos, cont)
OBJECT db;  BOOLEAN gall;  OBJECT sym;  FULL_CHAR *tag, *seq;
FILE_NUM *dfnum;  long *dfpos;  long *cont;
{ int symnum;  FULL_CHAR line[MAX_LINE], buff[MAX_LINE];  OBJECT y;
  ifdebug(DPP, D, ProfileOn("DbRetrieve"));
  debug4(DBS, D, "DbRetrieve(%s, %s%s&%s)", string(db), gall ? "&" : "",
	SymName(sym), tag);
  if( !reading(db) || filep(db) == null )
  { debug0(DBS, D, "DbRetrieve returning FALSE (empty or not reading)");
    ifdebug(DPP, D, ProfileOff("DbRetrieve"));
    return FALSE;
  }
  SymToNum(db, sym, symnum, FALSE);
  sprintf( (char *) buff, "%s%d&%s", gall ? "&" : "", symnum, tag);
  fseek(filep(db), 0L, 2);
  if( !SearchFile(filep(db), (int) left_pos(db), (int) ftell(filep(db)) - 1,
	buff, line) )
  { debug0(DBS, D, "DbRetrieve returning FALSE (key not present)");
    ifdebug(DPP, D, ProfileOff("DbRetrieve"));
    return FALSE;
  }
  sscanf( (char *) line, "%*s\t%s\t%ld\t%[^\n]", seq, dfpos, buff);
  if( StringEqual(buff, AsciiToFull(".")) )
  { StringCopy(buff, string(db));
  }
  *dfnum = FileNum(buff, DATA_SUFFIX);
  if( *dfnum == NO_FILE )  /* can only occur in cross reference database */
    *dfnum = DefineFile(buff, DATA_SUFFIX, &fpos(db),
      DATABASE_FILE, SOURCE_PATH);
  *cont = ftell(filep(db));
  Child(y, Down(db));
  debug2(DBS, D, "DbRetrieve returning TRUE (in %s at %ld)",
    FileName(*dfnum), *dfpos);
  ifdebug(DPP, D, ProfileOff("DbRetrieve"));
  return TRUE;
} /* end DbRetrieve */


/*@::DbRetrieveNext()@********************************************************/
/*                                                                           */
/*  BOOLEAN DbRetrieveNext(db, gall, sym, tag, seq, dfnum, dfpos, cont)      */
/*                                                                           */
/*  Retrieve the entry of database db pointed to by *cont.                   */
/*  Set *gall, *sym, *tag, *seq, *dfnum, *dfpos to the value of the entry.   */
/*  Reset *cont to the next entry for passing to the next DbRetrieveNext.    */
/*                                                                           */
/*****************************************************************************/

BOOLEAN DbRetrieveNext(db, gall, sym, tag, seq, dfnum, dfpos, cont)
OBJECT db;  BOOLEAN *gall;  OBJECT *sym;  FULL_CHAR *tag, *seq;
FILE_NUM *dfnum;  long *dfpos;  long *cont;
{ FULL_CHAR line[MAX_LINE], fname[MAX_LINE]; int symnum;
  ifdebug(DPP, D, ProfileOn("DbRetrieveNext"));
  debug2(DBS, D, "DbRetrieveNext( %s, %ld )", string(db), *cont);
  if( !reading(db) )  Error(INTERN, &fpos(db), "DbRetrieveNext: writing!");
  if( filep(db) == null )
  { debug0(DBS, D, "DbRetrieveNext returning FALSE (empty database)");
    ifdebug(DPP, D, ProfileOff("DbRetrieveNext"));
    return FALSE;
  }
  fseek(filep(db), *cont == 0L ? (long) left_pos(db) : *cont, 0);
  if( StringFGets(line, MAX_LINE, filep(db)) == NULL )
  { debug0(DBS, D, "DbRetrieveNext returning FALSE (no successor)");
    ifdebug(DPP, D, ProfileOff("DbRetrieveNext"));
    return FALSE;
  }
  *gall = (line[0] == '&' ? 1 : 0);
  sscanf( (char *) &line[*gall], "%d&%s\t%s\t%ld\t%[^\n]",
    &symnum, tag, seq,dfpos,fname);
  if( StringEqual(fname, AsciiToFull(".")) )
  { StringCopy(fname, string(db));
  }
  *dfnum = FileNum(fname, DATA_SUFFIX);
  if( *dfnum == NO_FILE )  /* can only occur in cross reference database */
    *dfnum = DefineFile(fname, DATA_SUFFIX, &fpos(db),
      DATABASE_FILE, SOURCE_PATH);
  NumToSym(db, symnum, *sym);  *cont = ftell(filep(db));
  debug2(DBS, D, "DbRetrieveNext returning TRUE (in %s at %ld)",
    FileName(*dfnum), *dfpos);
  ifdebug(DPP, D, ProfileOff("DbRetrieveNext"));
  return TRUE;
} /* end DbRetrieveNext */
