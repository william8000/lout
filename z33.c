/*@z33.c:Database Service:DbCreate(), DbRetrieve()@***************************/
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
/*  FILE:         z33.c                                                      */
/*  MODULE:       Database Service                                           */
/*  EXTERNS:      OldCrossDb, NewCrossDb, DbCreate(), DbInsert(),            */
/*                DbConvert(), DbLoad(), DbRetrieve(), DbRetrieveNext()      */
/*                                                                           */
/*****************************************************************************/
#include "externs"
#define	reading(x)	threaded(x)


/*****************************************************************************/
/*                                                                           */
/*  OldCrossDb     Database containing cross references from previous run.   */
/*  NewCrossDb     Writable database of cross references from this run.      */
/*                                                                           */
/*****************************************************************************/

OBJECT OldCrossDb, NewCrossDb;


/*****************************************************************************/
/*                                                                           */
/*  OBJECT DbCreate(str, fpos)                                               */
/*                                                                           */
/*  Create a new writable database with name (i.e. file stem) str and file   */
/*  position fpos for error messages.                                        */
/*                                                                           */
/*****************************************************************************/

OBJECT DbCreate(str, fposn)
unsigned char *str;  FILE_POS *fposn;
{ OBJECT db;
  debug2(DBS, D, "DbCreate( %s,%s )", str, EchoFilePos(fposn));
  db = MakeWord(str, fposn);
  reading(db) = FALSE;
  filep(db) = null;
  debug1(DBS, D, "DbCreate returning %s", EchoObject(null, db));
  return db;
} /* end DbCreate */


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


/*@@**************************************************************************/
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
/*  entries with equal primary keys.  This is used for sorted galleys.       */
/*  The value of the entry has the following parts:                          */
/*                                                                           */
/*      dfnum       The file containing the object                           */
/*      dfpos       The position of the object in that file                  */
/*                                                                           */
/*****************************************************************************/

DbInsert(db, gall, sym, tag, seq, dfnum, dfpos)
OBJECT db;  BOOLEAN gall;  OBJECT sym;  unsigned char *tag, *seq;
FILE_NUM dfnum;  long dfpos;
{ int symnum;
  unsigned char buff[MAX_LINE];
  assert( type(db) == WORD, "DbInsert: db!" );
  assert( tag[0] != '\0', "DbInsert: null tag!" );
  assert( seq[0] != '\0', "DbInsert: null seq!" );
  ifdebug(DPP, D, ProfileOn("DbInsert"));
  debug6(DBS, D, "DbInsert(%s, %s, %s, %s, %s, %s, dfpos)",
	string(db), bool(gall), SymName(sym), tag, seq,
	dfnum == NO_FILE ? (unsigned char *) "." : FileName(dfnum));
  if( reading(db) )  Error(INTERN, &fpos(db), "insert into reading database!");
  if( filep(db) == null )
  { if( strlen(string(db)) + strlen(NEW_INDEX_SUFFIX) >= MAX_LINE )
      Error(FATAL, no_fpos, "database file name %s%s is too long",
	string(db), NEW_INDEX_SUFFIX);
    sprintf(buff, "%s%s", string(db), NEW_INDEX_SUFFIX);
    filep(db) = fopen(buff, "w");
    if( filep(db) == null )
      Error(FATAL, &fpos(db), "cannot write to database file %s", buff);
  }
  SymToNum(db, sym, symnum, gall);
  fprintf(filep(db), "%s%d&%s\t%s\t%ld\t%s\n", gall ? "&" : "", symnum,
    tag, seq, dfpos, dfnum==NO_FILE ? (unsigned char *) "." : FileName(dfnum));
  debug0(DBS, D, "DbInsert returning.");
  ifdebug(DPP, D, ProfileOff("DbInsert"));
} /* end DbInsert */


/*@@**************************************************************************/
/*                                                                           */
/*  DbConvert(db, full_name)                                                 */
/*                                                                           */
/*  Convert database db from writable to readable, then dispose it.          */
/*  full_name is TRUE if symbols are to be known by their full path name.    */
/*                                                                           */
/*****************************************************************************/

DbConvert(db, full_name)
OBJECT db;  BOOLEAN full_name;
{ unsigned char buff[2*MAX_LINE + 20], oldname[MAX_LINE+10], newname[MAX_LINE];
  OBJECT link, y;
  ifdebug(DPP, D, ProfileOn("DbConvert"));
  debug2(DBS, D, "DbConvert( %d %s )", (int) db,string(db));
  if( reading(db) )  Error(INTERN, &fpos(db), "DbConvert: reading database!");
  sprintf(newname, "%s", string(db));
  sprintf(oldname, "%s%s", string(db), NEW_INDEX_SUFFIX);
  if( filep(db) != null )
  { for( link = Down(db);  link != db;  link = NextDown(link) )
    { Child(y, link);
      assert( type(y) == CROSS_SYM || type(y) == ACAT, "DbConvert: y!" );
      if( type(y) != CROSS_SYM )  continue;
      fprintf(filep(db), "%s %d %s\n", db_targ(link) ? "#target" : "#symbol",
	number(link), full_name ? FullSymName(symb(y)," ") : SymName(symb(y)));
    }
    fclose(filep(db));
    sprintf(buff, "sort -o %s %s", newname, oldname);
    system(buff);
  }
  else unlink(newname);
  unlink(oldname);
  DeleteNode(db);
  debug0(DBS, D, "DbConvert returning.");
  ifdebug(DPP, D, ProfileOff("DbConvert"));
} /* end DbConvert */


/*****************************************************************************/
/*                                                                           */
/*  OBJECT DbLoad(index_fnum, str, fpos, create, symbs)                      */
/*                                                                           */
/*  Open the database whose name (i.e. <stem>.li) is str for reading.        */
/*  This file has already been defined; its internal name is index_fnum.     */
/*  If it won't open and create is true, try to create it from <stem>.ld.    */
/*                                                                           */
/*  symbs is an ACAT of CLOSUREs showing the symbols that the database may   */
/*  contain; or nil if the database may contain any symbol.                  */
/*                                                                           */
/*****************************************************************************/

OBJECT DbLoad(index_fnum, str, fposn, create, symbs)
FILE_NUM index_fnum;  unsigned char *str;  FILE_POS *fposn;
BOOLEAN create;  OBJECT symbs;
{ FILE *fp;  OBJECT db, t, res, tag, par, sym, link, y;
  int i, lnum, num, count;  unsigned char line[MAX_LINE];
  unsigned char indexname[MAX_LINE], dataname[MAX_LINE], sym_name[MAX_LINE];
  FILE_NUM dfnum;  long dfpos;  BOOLEAN gall;
  ifdebug(DPP, D, ProfileOn("DbLoad"));
  debug4(DBS, D, "DbLoad(%s, %s, %s, %s, symbs)", FileName(index_fnum),
    str, EchoFilePos(fposn), bool(create));

  /* open or else create index file fp */
  fp = OpenFile(index_fnum, create);
  if( fp == null && create )
  { db = nil;
    strcpy(dataname, str);
    strcpy(&dataname[strlen(dataname) - strlen(INDEX_SUFFIX)], DATA_SUFFIX);
    dfnum = DefineFile(MakeWord(dataname, fposn), DATABASE_FILE, DATABASE_PATH);
    dfpos = 0L;
    LexPush(dfnum, 0, DATABASE_FILE);
    t = LexGetToken();
    while( type(t) == LBR )
    { res = Parse(&t, StartSym, FALSE, FALSE);
      if( t != nil || type(res) != CLOSURE )
	Error(FATAL, &fpos(res), "syntax error in database file %s", dataname);
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
      tag = nil;
      for( link = Down(res);  link != res;  link = NextDown(link) )
      {	Child(par, link);
	if( type(par) == PAR && is_tag(actual(par)) && Down(par) != par )
	{ Child(tag, Down(par));
	  break;
	}
      }
      if( tag == nil )
	Error(FATAL, &fpos(res), "database symbol %s has no tag", SymName(res));
      tag = ReplaceWithTidy(tag);
      if( type(tag) != WORD )
	Error(FATAL, &fpos(res), "database symbol's tag is not a simple word");
      if( string(tag)[0] == '\0' )
	Error(FATAL, &fpos(res), "database symbol's tag is an empty word");
      if( db == nil )
      {	strcpy(indexname, FileName(dfnum));
	strcpy(&indexname[strlen(indexname)-strlen(DATA_SUFFIX)], INDEX_SUFFIX);
	db = DbCreate(indexname, fposn);
      }
      DbInsert(db, FALSE, actual(res), string(tag), "0", NO_FILE, dfpos);
      DisposeObject(res);
      dfpos = LexNextTokenPos();
      t = LexGetToken();
    }
    if( type(t) != END )
      Error(FATAL, &fpos(t), "%s or end of file expected here", KW_LBR);
    LexPop();
    if( db == nil )
    { strcpy(indexname, FileName(dfnum));
      strcpy(&indexname[strlen(indexname)-strlen(DATA_SUFFIX)], INDEX_SUFFIX);
      db = DbCreate(indexname, fposn);
    }
    DbConvert(db, FALSE);
    fp = OpenFile(index_fnum, FALSE);
    if( fp == null )
      Error(FATAL, &fpos(db), "cannot open database file %s", indexname);
  }
  else strcpy(indexname, str);

  /* set up database record */
  db = MakeWord(FileName(index_fnum), fposn);
  reading(db) = TRUE;
  filep(db) = fp;
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
  left_pos(db) = 0;
  lnum = 0;
  while( fgets(line, MAX_LINE, fp) != NULL && line[0] == '#' )
  { lnum++;
    left_pos(db) = (int) ftell(fp);
    gall = line[1] == 't';
    sscanf(line, gall ? "#target %d" : "#symbol %d", &num);
    for( i = 8;  line[i] != ' ' && line[i] != '\0';  i++ );
    if( symbs == nil )
    {
      /* any symbols are possible, full path names in index file required */
      count = 0;  sym = StartSym;
      while( line[i] != '\n' && line[i] != '\0' )
      {	PushScope(sym, FALSE, FALSE);  count++;
	sscanf(&line[i+1], "%s", sym_name);
	sym = SearchSym(sym_name, strlen(sym_name));
	i += strlen(sym_name) + 1;
      }
      for( i = 1;  i <= count;  i++ )  PopScope();
    }
    else
    {
      /* only symbs symbols possible, full path names not required */
      sym = nil;
      sscanf(&line[i+1], "%s", sym_name);
      for( link = Down(symbs);  link != symbs;  link = NextDown(link) )
      {	Child(y, link);
	assert( type(y) == CLOSURE, "DbLoad: type(y) != CLOSURE!" );
	if( strcmp(sym_name, SymName(actual(y))) == 0 )
	{ sym = actual(y);
	  break;
	}
      }
    }
    if( sym != nil && sym != StartSym )
    { if( cross_sym(sym) == nil )  CrossInit(sym);
      link = Link(db, cross_sym(sym));
      number(link) = num;
      db_targ(link) = gall;
      if( gall )  is_extern_target(sym) = uses_extern_target(sym) = TRUE;
    }
    else
    { Error(WARN, &fpos(db), "undefined symbol in database file %s (line %d)",
			indexname, lnum);
      debug1(DBS, D, "DbLoad returning %s (error)", string(db));
      fclose(filep(db));	/* effectively an empty database */
      filep(db) = null;
      ifdebug(DPP, D, ProfileOff("DbLoad"));
      return db;
    }
  }
  debug1(DBS, D, "DbLoad returning %s", string(db));
  ifdebug(DPP, D, ProfileOff("DbLoad"));
  return db;
} /* end DbLoad */


/*@@**************************************************************************/
/*                                                                           */
/*  static BOOLEAN SearchFile(fp, left, right, str, line)                    */
/*                                                                           */
/*  File fp is a text file.  left is the beginning of a line, right is the   */
/*  end of a line.   Search the file by binary search for a line beginning   */
/*  with str.  If found, return it in line, else return FALSE.               */
/*                                                                           */
/*****************************************************************************/

static BOOLEAN SearchFile(fp, left, right, str, line)
FILE *fp;  int left, right;  unsigned char *str, *line;
{ int l, r, mid, mid_end;  unsigned char buff[MAX_LINE];  BOOLEAN res;
  ifdebug(DPP, D, ProfileOn("SearchFile"));
  debug3(DBS, DD, "SearchFile(fp, %d, %d, %s, line)", left, right, str);

  l = left;  r = right;
  while( l <= r )
  {
    /* loop invt: (l==0 or fp[l-1]=='\n') and (fp[r] == '\n')       */
    /* and first key >= str lies in the range fp[l..r+1]            */

    /* find line near middle of the range; mid..mid_end brackets it */
    debug2(DBS, DD, "  start loop: l = %d, r = %d", l, r);
    mid = (l + r)/2;
    fseek(fp, (long) mid, 0);
    do { mid++; } while( getc(fp) != '\n' );
    if( mid == r + 1 )
    { mid = l;
      fseek(fp, (long) mid, 0);
    }
    fgets(line, MAX_LINE, fp);
    mid_end = (int) ftell(fp) - 1;
    debug3(DBS, DD, "  mid: %d, mid_end: %d, line: %s", mid, mid_end, line);
    assert( l <= mid,      "SearchFile: l > mid!"        );
    assert( mid < mid_end, "SearchFile: mid >= mid_end!" );
    assert( mid_end <= r,  "SearchFile: mid_end > r!"    );

    /* compare str with this line and prepare next step */
    debug2(DBS, DD, "  comparing key %s with line %s", str, line);
    if( strcmp(str, line) <= 0 )  r = mid - 1;
    else l = mid_end + 1;
  } /* end while */

  /* now first key >= str lies in fp[l]; compare it with str */
  if( l < right )
  { fseek(fp, (long) l, 0);
    fgets(line, MAX_LINE, fp);
    sscanf(line, "%s", buff);
    res = strcmp(str, buff) == 0;
  }
  else res = FALSE;
  debug1(DBS, DD, "SearchFile returning %s", bool(res));
  ifdebug(DPP, D, ProfileOff("SearchFile"));
  return res;
} /* end SearchFile */


/*@@**************************************************************************/
/*                                                                           */
/*  BOOLEAN DbRetrieve(db, gall, sym, tag, seq, dfnum, dfpos, cont)          */
/*                                                                           */
/*  Retrieve the first entry of database db with the given gall, sym and     */
/*  tag.  Set *seq, *dfnum, *dfpos to the associated value.                  */
/*  Set *cont to a private value for passing to DbRetrieveNext.              */
/*                                                                           */
/*****************************************************************************/

BOOLEAN DbRetrieve(db, gall, sym, tag, seq, dfnum, dfpos, cont)
OBJECT db;  BOOLEAN gall;  OBJECT sym;  unsigned char *tag, *seq;
FILE_NUM *dfnum;  long *dfpos;  long *cont;
{ int symnum;  unsigned char line[MAX_LINE], buff[MAX_LINE];  OBJECT y;
  ifdebug(DPP, D, ProfileOn("DbRetrieve"));
  debug4(DBS, D, "DbRetrieve(%s, %s%s&%s)", string(db), gall ? "&" : "",
	SymName(sym), tag);
  if( !reading(db) || filep(db) == null )
  { debug0(DBS, D, "DbRetrieve returning FALSE (empty or not reading)");
    ifdebug(DPP, D, ProfileOff("DbRetrieve"));
    return FALSE;
  }
  SymToNum(db, sym, symnum, FALSE);
  sprintf(buff, "%s%d&%s", gall ? "&" : "", symnum, tag);
  fseek(filep(db), 0L, 2);
  if( !SearchFile(filep(db), left_pos(db), (int) ftell(filep(db))-1,buff,line) )
  { debug0(DBS, D, "DbRetrieve returning FALSE (key not present)");
    ifdebug(DPP, D, ProfileOff("DbRetrieve"));
    return FALSE;
  }
  sscanf(line, "%*s\t%s\t%ld\t%[^\n]", seq, dfpos, buff);
  if( strcmp(buff, ".") == 0 )
  { strcpy(buff, string(db));
    strcpy(&buff[strlen(buff) - strlen(INDEX_SUFFIX)], DATA_SUFFIX);
  }
  *dfnum = FileNum(buff);
  if( *dfnum == NO_FILE )  /* can only occur in cross reference database */
    *dfnum = DefineFile(MakeWord(buff, &fpos(db)), DATABASE_FILE, SOURCE_PATH);
  *cont = ftell(filep(db));
  Child(y, Down(db));
  debug2(DBS, D, "DbRetrieve returning TRUE (in %s at %ld)",
    FileName(*dfnum), *dfpos);
  ifdebug(DPP, D, ProfileOff("DbRetrieve"));
  return TRUE;
} /* end DbRetrieve */


/*****************************************************************************/
/*                                                                           */
/*  BOOLEAN DbRetrieveNext(db, gall, sym, tag, seq, dfnum, dfpos, cont)      */
/*                                                                           */
/*  Retrieve the entry of database db pointed to by *cont.                   */
/*  Set *gall, *sym, *tag, *seq, *dfnum, *dfpos to the entry's value.        */
/*  Reset *cont to the next entry for passing to the next DbRetrieveNext.    */
/*                                                                           */
/*****************************************************************************/

BOOLEAN DbRetrieveNext(db, gall, sym, tag, seq, dfnum, dfpos, cont)
OBJECT db;  BOOLEAN *gall;  OBJECT *sym;  unsigned char *tag, *seq;
FILE_NUM *dfnum;  long *dfpos;  long *cont;
{ unsigned char line[MAX_LINE], fname[MAX_LINE]; int symnum;
  ifdebug(DPP, D, ProfileOn("DbRetrieveNext"));
  debug2(DBS, D, "DbRetrieveNext( %s, %ld )", string(db), *cont);
  if( !reading(db) )  Error(INTERN, &fpos(db), "DbRetrieveNext: writing!");
  if( filep(db) == null )
  { debug0(DBS, D, "DbRetrieveNext returning FALSE (empty database)");
    ifdebug(DPP, D, ProfileOff("DbRetrieveNext"));
    return FALSE;
  }
  fseek(filep(db), *cont == 0L ? left_pos(db) : *cont, 0);
  if( fgets(line, MAX_LINE, filep(db)) == NULL )
  { debug0(DBS, D, "DbRetrieveNext returning FALSE (no successor)");
    ifdebug(DPP, D, ProfileOff("DbRetrieveNext"));
    return FALSE;
  }
  *gall = (line[0] == '&' ? 1 : 0);
  sscanf(&line[*gall], "%d&%s\t%s\t%ld\t%[^\n]", &symnum, tag, seq,dfpos,fname);
  if( strcmp(fname, ".") == 0 )
  { strcpy(fname, string(db));
    strcpy(&fname[strlen(fname) - strlen(INDEX_SUFFIX)], DATA_SUFFIX);
  }
  *dfnum = FileNum(fname);
  if( *dfnum == NO_FILE )  /* can only occur in cross reference database */
    *dfnum = DefineFile(MakeWord(fname, &fpos(db)), DATABASE_FILE, SOURCE_PATH);
  NumToSym(db, symnum, *sym);  *cont = ftell(filep(db));
  debug2(DBS, D, "DbRetrieveNext returning TRUE (in %s at %ld)",
    FileName(*dfnum), *dfpos);
  ifdebug(DPP, D, ProfileOff("DbRetrieveNext"));
  return TRUE;
} /* end DbRetrieveNext */
