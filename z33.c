/*@z33.c:Database Service:OldCrossDb(), NewCrossDb(), SymToNum()@*************/
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
/*  FILE:         z33.c                                                      */
/*  MODULE:       Database Service                                           */
/*  EXTERNS:      OldCrossDb, NewCrossDb, DbCreate(), DbInsert(),            */
/*                DbConvert(), DbClose(), DbLoad(), DbRetrieve(),            */
/*                DbRetrieveNext()                                           */
/*                                                                           */
/*****************************************************************************/
#define INIT_DBCHECK_NUM	5
#include "externs"


/*****************************************************************************/
/*                                                                           */
/*  DBCHECK_TABLE                                                            */
/*                                                                           */
/*  A symbol table holding all non-galley cross references, basically        */
/*  implementing a function (sym, tag) -> fpos (if any).                     */
/*                                                                           */
/*     dtab_new(newsize)                New empty table, newsize capacity    */
/*     dtab_insert(x, S)                Insert new (sym, tag) pair x into S  */
/*     dtab_retrieve(sym, tag, S)       Retrieve (sym, tag) pair from S      */
/*     dtab_debug(S, fp)                Debug print of table S to file fp    */
/*                                                                           */
/*****************************************************************************/

typedef struct
{ int dbchecktab_size;				/* size of table             */
  int dbchecktab_count;				/* number of objects held    */
  OBJECT dbchecktab_item[1];
} *DBCHECK_TABLE;

#define	dtab_size(S)	(S)->dbchecktab_size
#define	dtab_count(S)	(S)->dbchecktab_count
#define	dtab_item(S, i)	(S)->dbchecktab_item[i]

#define hash(pos, sym, tag, S)						\
{ FULL_CHAR *p = tag;							\
  pos = (int) sym;							\
  while( *p ) pos += *p++;						\
  pos = pos % dtab_size(S);						\
}

static DBCHECK_TABLE dtab_new(newsize)
int newsize;
{ DBCHECK_TABLE S;  int i;
  S = (DBCHECK_TABLE)
	  malloc(2*sizeof(int) + newsize * sizeof(OBJECT));
  if( S == (DBCHECK_TABLE) NULL )
    Error(33, 1, "run out of memory enlarging dbcheck table", FATAL, no_fpos);
  dtab_size(S) = newsize;
  dtab_count(S) = 0;
  for( i = 0;  i < newsize;  i++ )  dtab_item(S, i) = nil;
  return S;
} /* end dtab_new */

static DBCHECK_TABLE dtab_rehash(S, newsize)
DBCHECK_TABLE S;  int newsize;
{ DBCHECK_TABLE NewS;  int i;
  NewS = dtab_new(newsize);
  for( i = 1;  i <= dtab_size(S);  i++ )
  { if( dtab_item(S, i) != nil )
      dtab_insert(dtab_item(S, i), &NewS);
  }
  free(S);
  return NewS;
} /* end dtab_rehash */

static dtab_insert(x, S)
OBJECT x;  DBCHECK_TABLE *S;
{ int pos, num;  OBJECT z, link, y;
  if( dtab_count(*S) == dtab_size(*S) - 1 )	/* one less since 0 unused */
    *S = dtab_rehash(*S, 2*dtab_size(*S));
  hash(pos, db_checksym(x), string(x), *S);
  if( dtab_item(*S, pos) == nil )  dtab_item(*S, pos) = New(ACAT);
  z = dtab_item(*S, pos);
  for( link = Down(z);  link != z;  link = NextDown(link) )
  { Child(y, link);
    if( db_checksym(x) == db_checksym(y) && StringEqual(string(x), string(y)) )
    { Error(33, 2, "Dbcheck: entry inserted twice", INTERN, &fpos(x));
    }
  }
  Link(dtab_item(*S, pos), x);
} /* end dtab_insert */

static OBJECT dtab_retrieve(sym, tag, S)
OBJECT sym;  FULL_CHAR *tag;  DBCHECK_TABLE S;
{ OBJECT x, link, y;  int pos;
  hash(pos, sym, tag, S);
  x = dtab_item(S, pos);
  if( x == nil )  return nil;
  for( link = Down(x);  link != x;  link = NextDown(link) )
  { Child(y, link);
    if( sym == db_checksym(y) && StringEqual(tag, string(y)) )
      return y;
  }
  return nil;
} /* end dtab_retrieve */

#if DEBUG_ON
static dtab_debug(S, fp)
DBCHECK_TABLE S;  FILE *fp;
{ int i;  OBJECT x, link, y;
  fprintf(fp, "  table size: %d;  current number of items: %d\n",
    dtab_size(S), dtab_count(S));
  for( i = 0;  i < dtab_size(S);  i++ )
  { x = dtab_item(S, i);
    fprintf(fp, "dtab_item(S, %d) =", i);
    if( x == nil )
      fprintf(fp, " <nil>");
    else if( type(x) != ACAT )
      fprintf(fp, " not ACAT!");
    else for( link = Down(x);  link != x;  link = NextDown(link) )
    { Child(y, link);
      fprintf(fp, " %s&&%s",
	is_word(type(y)) ? SymName(db_checksym(y)) : AsciiToFull("?"),
	is_word(type(y)) ? string(y) : AsciiToFull("not-WORD!"));
    }
    fprintf(fp, "\n");
  }
} /* end dtab_debug */
#endif

static DBCHECK_TABLE DbCheckTable;		/* the dbcheck table         */
static BOOLEAN	     DbCheckTableInit = FALSE;	/* TRUE if table inited	     */


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
  if( link == db )							\
    Error(33, 3, "NumToSym: no sym", INTERN, &fpos(db));		\
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
/*  DbInsert(db, gall, sym, tag, tagfpos, seq, dfnum, dfpos)                 */
/*                                                                           */
/*  Insert a new entry into writable database db.  The primary key of the    */
/*  entry has these three parts:                                             */
/*                                                                           */
/*      gall        TRUE if inserting a galley                               */
/*      sym         The symbol which is the target of this entry             */
/*      tag         The tag of this target (must be a non-null string)       */
/*                                                                           */
/*  tagfpos is the file position that the tag originated from.               */
/*  There is also an auxiliary key, seq, which enforces an ordering on       */
/*  entries with equal primary keys but is not itself ever retrieved.  This  */
/*  ordering is used for sorted galleys.  The value of the entry has the     */
/*  following parts:                                                         */
/*                                                                           */
/*      dfnum       The file containing the object                           */
/*      dfpos       The position of the object in that file                  */
/*                                                                           */
/*  If check is TRUE, we need to check whether an entry with this key has    */
/*  been inserted before.  This will never be the case with galley entries.  */
/*                                                                           */
/*****************************************************************************/

DbInsert(db, gall, sym, tag, tagfpos, seq, dfnum, dfpos, check)
OBJECT db;  BOOLEAN gall;  OBJECT sym;  FULL_CHAR *tag;  FILE_POS *tagfpos;
FULL_CHAR *seq; FILE_NUM dfnum;  long dfpos;  BOOLEAN check;
{ int symnum;  OBJECT chk;
  FULL_CHAR buff[MAX_BUFF];
  assert( is_word(type(db)), "DbInsert: db!" );
  assert( tag[0] != '\0', "DbInsert: null tag!" );
  assert( seq[0] != '\0', "DbInsert: null seq!" );
  ifdebug(DPP, D, ProfileOn("DbInsert"));
  debug6(DBS, D, "DbInsert(%s, %s, %s, %s, %s, %s, dfpos)",
	string(db), bool(gall), SymName(sym), tag, seq,
	dfnum == NO_FILE ? AsciiToFull(".") : FileName(dfnum));
  if( reading(db) )
    Error(33, 4, "insert into reading database", INTERN, &fpos(db));

  /* open database index file if not already done */
  if( filep(db) == null )
  { if( StringLength(string(db)) + StringLength(NEW_INDEX_SUFFIX) >= MAX_BUFF )
      Error(33, 5, "database file name %s%s is too long",
	FATAL, no_fpos, string(db), NEW_INDEX_SUFFIX);
    StringCopy(buff, string(db));
    StringCat(buff, NEW_INDEX_SUFFIX);
    filep(db) = StringFOpen(buff, "w");
    if( filep(db) == null )
      Error(33, 6, "cannot write to database file %s", FATAL, &fpos(db), buff);
  }

  /* if required, check that (sym, tag) not already inserted */
  if( check )
  { 
    debug2(DBS, D, "  checking %s&&%s, DbCheckTable =", SymName(sym), tag);
    if( !DbCheckTableInit )
    { DbCheckTable = dtab_new(INIT_DBCHECK_NUM);
      DbCheckTableInit = TRUE;
    }
    ifdebug(DBS, D, dtab_debug(DbCheckTable, stderr));
    chk = dtab_retrieve(sym, tag, DbCheckTable);
    if( chk == nil )
    { chk = MakeWord(WORD, tag, tagfpos);
      db_checksym(chk) = sym;
      dtab_insert(chk, &DbCheckTable);
    }
    else
    { if( file_num(fpos(chk)) > 0 )
        Error(33, 7, "cross reference %s&&%s used previously, at%s",
          WARN, tagfpos, SymName(sym), tag, EchoFilePos(&fpos(chk)));
      else Error(33, 8, "cross reference %s&&%s used previously",
	  WARN, tagfpos, SymName(sym), tag);
    }
  }

  /* work out database index file entry and append it to file */
  if( dfnum != NO_FILE )
  { StringCopy(buff, FileName(dfnum));
    StringCopy(&buff[StringLength(buff)-StringLength(DATA_SUFFIX)], STR_EMPTY);
  }
  else StringCopy(buff, AsciiToFull("."));
  SymToNum(db, sym, symnum, gall);
  ifdebug(DBS, D,
  fprintf(stderr, "  -> %s%d&%s\t%s\t%ld\t%s\n", gall ? "0" : "", symnum,
    tag, seq, dfpos, buff);
  );
  fprintf(filep(db), "%s%d&%s\t%s\t%ld\t%s\n", gall ? "0" : "", symnum,
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
{ FULL_CHAR oldname[MAX_BUFF+10], newname[MAX_BUFF];
  char buff[2*MAX_BUFF + 20];
  OBJECT link, y;
  ifdebug(DPP, D, ProfileOn("DbConvert"));
  debug2(DBS, D, "DbConvert( %d %s )", (int) db,string(db));
  if( reading(db) )
    Error(33, 9, "DbConvert: reading database", INTERN, &fpos(db));
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
	db_targ(link) ? "00target" : "00symbol",
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
  BOOLEAN gall;  FULL_CHAR line[MAX_BUFF], sym_name[MAX_BUFF];
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
      if( t != nil || type(res) != CLOSURE )
	Error(33, 10, "syntax error in database file %s",
	  FATAL, &fpos(res), FileName(dfnum));
      assert( symbs != nil, "DbLoad: create && symbs == nil!" );
      if( symbs != nil )
      {	for( link = Down(symbs);  link != symbs;  link = NextDown(link) )
	{ Child(y, link);
	  if( type(y) == CLOSURE && actual(y) == actual(res) )  break;
	}
	if( link == symbs )
	  Error(33, 11, "%s found in database but not declared in %s line",
	    FATAL, &fpos(res), SymName(actual(res)), KW_DATABASE);
      }
      for( tag = nil, link = Down(res);  link != res;  link = NextDown(link) )
      {	Child(par, link);
	if( type(par) == PAR && is_tag(actual(par)) && Down(par) != par )
	{ Child(tag, Down(par));
	  break;
	}
      }
      if( tag == nil )
	Error(33, 12, "database symbol %s has no tag",
	  FATAL, &fpos(res), SymName(res));
      tag = ReplaceWithTidy(tag, FALSE);
      if( !is_word(type(tag)) )
	Error(33, 13, "database symbol tag is not a simple word",
	  FATAL, &fpos(res));
      if( StringEqual(string(tag), STR_EMPTY) )
	Error(33, 14, "database symbol tag is an empty word", FATAL,&fpos(res));
      if( db == nil )
      {	StringCopy(line, FileName(dfnum));
	i = StringLength(line) - StringLength(INDEX_SUFFIX);
	assert( i > 0, "DbLoad: FileName(dfnum) (1)!" );
	StringCopy(&line[i], STR_EMPTY);
	db = DbCreate(MakeWord(WORD, line, &fpos(stem)));
      }
      DbInsert(db, FALSE, actual(res), string(tag), &fpos(tag), STR_ZERO,
	NO_FILE, dfpos, TRUE);
      DisposeObject(res);  dfpos = LexNextTokenPos();  t = LexGetToken();
    }
    if( type(t) != END )
      Error(33, 15, "%s or end of file expected here", FATAL, &fpos(t), KW_LBR);
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
      Error(33, 16, "cannot open database file %s",
        FATAL, &fpos(db), FileName(index_fnum));
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
  while( StringFGets(line, MAX_BUFF, fp) != NULL && line[0] == '0' && line[1] == '0' )
  { lnum++;
    left_pos(db) = (int) ftell(fp);
    gall = StringBeginsWith(line, AsciiToFull("00target "));
    sscanf( (char *) line, gall ? "00target %d" : "00symbol %d", &num);
    for( i = 9;  line[i] != CH_SPACE && line[i] != '\0';  i++ );
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
    { Error(33, 17, "undefined symbol in database file %s (line %d)",
	WARN, &fpos(db), FileName(index_fnum), lnum);
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
{ int l, r, mid, mid_end;  FULL_CHAR buff[MAX_BUFF];  BOOLEAN res;
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
    StringFGets(line, MAX_BUFF, fp);
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
    StringFGets(line, MAX_BUFF, fp);
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
{ int symnum;  FULL_CHAR line[MAX_BUFF], buff[MAX_BUFF];  OBJECT y;
  ifdebug(DPP, D, ProfileOn("DbRetrieve"));
  debug4(DBS, D, "DbRetrieve(%s, %s%s&%s)", string(db), gall ? "0" : "",
	SymName(sym), tag);
  if( !reading(db) || filep(db) == null )
  { debug0(DBS, D, "DbRetrieve returning FALSE (empty or not reading)");
    ifdebug(DPP, D, ProfileOff("DbRetrieve"));
    return FALSE;
  }
  SymToNum(db, sym, symnum, FALSE);
  sprintf( (char *) buff, "%s%d&%s", gall ? "0" : "", symnum, tag);
  fseek(filep(db), 0L, 2);
  if( !SearchFile(filep(db), (int) left_pos(db), (int) ftell(filep(db)) - 1,
	buff, line) )
  { debug0(DBS, D, "DbRetrieve returning FALSE (key not present)");
    ifdebug(DPP, D, ProfileOff("DbRetrieve"));
    return FALSE;
  }
  sscanf( (char *) line, "%*s\t%[^\t]\t%ld\t%[^\n]", seq, dfpos, buff);
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
{ FULL_CHAR line[MAX_BUFF], fname[MAX_BUFF]; int symnum;
  ifdebug(DPP, D, ProfileOn("DbRetrieveNext"));
  debug2(DBS, D, "DbRetrieveNext( %s, %ld )", string(db), *cont);
  if( !reading(db) )
    Error(33, 18, "DbRetrieveNext: writing", INTERN, &fpos(db));
  if( filep(db) == null )
  { debug0(DBS, D, "DbRetrieveNext returning FALSE (empty database)");
    ifdebug(DPP, D, ProfileOff("DbRetrieveNext"));
    return FALSE;
  }
  fseek(filep(db), *cont == 0L ? (long) left_pos(db) : *cont, 0);
  if( StringFGets(line, MAX_BUFF, filep(db)) == NULL )
  { debug0(DBS, D, "DbRetrieveNext returning FALSE (no successor)");
    ifdebug(DPP, D, ProfileOff("DbRetrieveNext"));
    return FALSE;
  }
  *gall = (line[0] == '0' ? 1 : 0);
  sscanf( (char *) &line[*gall], "%d&%s\t%[^\t]\t%ld\t%[^\n]",
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
