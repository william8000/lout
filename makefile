###############################################################################
#                                                                             #
#  Make file for installing Basser Lout Version 3.12                          #
#                                                                             #
#  Jeffrey H. Kingston                                                        #
#  6 April 1998                                                               #
#                                                                             #
#     make lout         Compile the Lout source                               #
#     make c2lout       Compile a small auxiliary program called c2lout       #
#     make install      Install the Lout and c2lout binaries and libraries    #
#     make installman   Install the Lout and c2lout manual entries            #
#     make installdoc   Install the Lout documentation                        #
#     make installfr    Install French error messages (optional)              #
#     make installde    Install German error messages (optional)              #
#     make clean        Remove compilation temporaries                        #
#     make uninstall    Undo the effect of make install, installman,          #
#                       installdoc, installfr, and installde                  #
#     make restart      Undo everything except changes to this makefile,      #
#                       ready for a fresh start.                              #
#                                                                             #
#     make setupfiles   Creates setup files by editing include/master and     #
#                       puts them in directory setup.  You should not need    #
#                       to do this because it has already been done.          #
#                                                                             #
#  Most installations of Lout should require only the following steps.  If    #
#  something goes wrong, you can start again with "make restart".  Please     #
#  carry out all the steps, in exactly the order given.  Believe me, it       #
#  will be much faster than doing it any other way.                           #
#                                                                             #
#  (1) Set exactly one of the following macros defined below to 1 and the     #
#      others all to 0, to indicate the operating system under which the      #
#      Lout binary is to run.  At present OSUNIX and OSDOS work but OSMAC     #
#      doesn't work.                                                          #
#                                                                             #
#      OSUNIX    Unix in all its flavours, including Linux.                   #
#      OSDOS     MS-DOS etc. ("rb" and "wb" file access modes where needed)   #
#      OSMAC     Macintosh                                                    #
#                                                                             #
#  (2) This may be the trickiest step of all.  On some systems, for example   #
#      NT under Visual C++, Lout's use of the system ftell() call causes      #
#      a problem.  This problem typically manifests itself on the *second*    #
#      pass over a large document such as the Lout User's Guide: a message    #
#      such as "error in database file" is printed and Lout aborts.  The      #
#      precise error may vary but should say something about a problem with   #
#      a database file.                                                       #
#                                                                             #
#      If this problem occurs you can probably fix it by changing the value   #
#      of DBFIX below to 1.  Many thanks to Valeriy E. Ushakov for this fix.  #
#                                                                             #
#         Systems requiring DBFIX = 0      Systems requiring DBFIX = 1        #
#         -------------------------------------------------------------       #
#         Unix                             NT/Visual C++                      #
#         Cygnus gnuwin32 gcc                                                 #
#                                                                             #
#  (3) Set the USESTAT macro defined below to 1 if the system you are         #
#      compiling onto has the stat() file status system call.  If you are     #
#      unsure, or know it doesn't, set USESTAT to 0.  The stat() call,        #
#      if used, will allow Lout to determine the time of last change          #
#      of database index files and rebuild them automatically if required.    #
#                                                                             #
#  (4) Set the SAFEDFT macro defined below to 1 if you want safe execution    #
#      (i.e. disabling calls to system()) to be the default behaviour.  You   #
#      can always specify safe or unsafe execution by means of the -S and     #
#      -U options to lout when processing a document; SAFEDFT means that      #
#      -S rather than -U is the default behaviour.  Unsafe execution is       #
#      required when formatting C programs and verbatim text, so if in        #
#      doubt, do not change the value of SAFEDFT.                             #
#                                                                             #
#  (5) Set the following four macros defined below to appropriate values:     #
#                                                                             #
#      BINDIR    Directory where Lout's binary goes.  This directory is       #
#                assumed to exist.                                            #
#                                                                             #
#      LIBDIR    Directory where Lout's libraries go.  This directory will    #
#                be created.                                                  #
#                                                                             #
#      DOCDIR    Directory where the documents describing the Lout system     #
#                (written in Lout) go.  This directory will be created.       #
#                                                                             #
#      MANDIR    Directory where the lout and c2lout online manual entries    #
#                (in nroff -man) go.  This directory is assumed to exist.     #
#                                                                             #
#  (6) Set the following two macros defined below to appropriate values.      #
#      I strongly recommend CHARIN=1 and CHAROUT=0 for all sites (English     #
#      and non-English language).  This way we get a truly international      #
#      standard in which everyone has access to accented characters, yet      #
#      Lout's output is in the strict 7-bit ASCII that is recommended in      #
#      the PostScript manual.                                                 #
#                                                                             #
#      CHARIN    This macro determines the assignment of characters in Lout   #
#                source files to character classes by the lexical analyser.   #
#                That is, it determines which characters are letters, which   #
#                is the comment character, etc.  Supported values are:        #
#                                                                             #
#                0  For English language only ASCII installations             #
#                                                                             #
#                1  For installations using the ISO-LATIN-1 character set     #
#                   (adds accented letters to the LETTER character class)     #
#                                                                             #
#                Lout will accept any 8-bit character except '\0'; CHARIN     #
#                does not determine the acceptability of any character, just  #
#                its class.                                                   #
#                                                                             #
#      CHAROUT   This macro determines the format of strings of literal       #
#                characters in the PostScript output.  Currently supported    #
#                values are:                                                  #
#                                                                             #
#                0  Every output character will be printable ASCII            #
#                                                                             #
#                1  Every output character will be printable ISO-LATIN-1      #
#                                                                             #
#                The output will be valid PostScript irrespective of the      #
#                value given to CHAROUT, which may be set independently of    #
#                CHARIN.  It just determines which characters are printed     #
#                as \ddd escape sequences and which are printed as one-byte   #
#                literal characters.                                          #
#                                                                             #
#  (7) Set macro USELOC to one of the following values, NOT TO A LOCALE.      #
#                                                                             #
#      0         Lout's error messages will always appear in English, and no  #
#                source code related to locales will be compiled.             #
#                                                                             #
#      1         Lout's error messages may appear in languages other than     #
#                English, depending on the current locale.  The Lout source   #
#                will be compiled including <locale.h>, <nl_types.h>, and     #
#                calls to setlocale(), catopen(), catgets(), and catclose()   #
#                                                                             #
#      If you choose to set USELOC to 1, you now need to set one or more of   #
#      these macros:                                                          #
#                                                                             #
#      LOC_FR    If you want French language error messages, set this macro   #
#                to your French locale name, i.e. to the value that you       #
#                expect setlocale(LC_MESSAGES, "") to return when you want    #
#                to get French language error messages                        #
#                                                                             #
#      LOC_DE    If you want German language error messages, set this macro   #
#                to your German locale name, i.e. to the value that you       #
#                expect setlocale(LC_MESSAGES, "") to return when you want    #
#                to get German language error messages                        #
#                                                                             #
#      For error messages in other languages, consult ./locale/README.        #
#                                                                             #
#  (8) Set macro COLLATE to either 0 or 1.  If you set it to 1, Lout will     #
#      use the strcoll() routine when sorting things alphabetically (e.g.     #
#      when sorting indexes), otherwise Lout will sort based on the ISO       #
#      codes of the characters.                                               #
#                                                                             #
#  (9) Execute "make c2lout".  This will compile the c2lout program, leaving  #
#      its binary in this directory.  No changes to other directories.        #
#                                                                             #
# (10) If you want to be able to produce compressed PDF files, as opposed to  #
#      uncompressed ones, you need to:                                        #
#                                                                             #
#      (a) obtain the zlib compression library from                           #
#          <http://www.cdrom.com/pub/infozip/zlib/>.                          #
#                                                                             #
#      (b) decompress the zlib source files using gunzip and/or tar and       #
#          then build the library by issuing the "make zlib.a" command        #
#          whilst in the zlib directory. If you want to test the library,     #
#          you should use the "make test" command (which also builds the      #
#          library).                                                          #
#                                                                             #
#      (c) set the PDF_COMPRESSION variable below to 1                        #
#                                                                             #
#      (d) set the ZLIB variable to the path of the libz.a file. For example: #
#          ZLIB = /usr/cs3/vtan/lout/lout.3.11/zlib-1.1.1/libz.a              #
#                                                                             #
#      (e) set the ZLIBPATH variable to the path of the zlib directory with   #
#          -I in front. For example:                                          #
#          ZLIBPATH = -I/usr/cs3/vtan/lout/lout.3.11/zlib-1.1.1/              #
#                                                                             #
#      If you don't want zlib support or cannot obtain it or cannot use it,   #
#      leave the PDF_COMPRESSION, ZLIB, and ZLIBPATH variables as they are.   #
#                                                                             #
# (11) Execute "make lout".  This will compile the Lout source, leaving the   #
#      binary in this directory.  No changes are made in other directories.   #
#                                                                             #
# (12) This makefile assumes that Lout is not installed on your system        #
#      already.  If you do have an earlier version of Lout installed,         #
#      the simplest way to get rid of it is to type "make uninstall" now.     #
#      Of course, this is assuming that the old version was installed in the  #
#      same directories as where you are about to install the new version.    #
#                                                                             #
# (13) Execute "make install".  This will do the following things:            #
#                                                                             #
#      (a) It will copy the lout and c2lout binaries into $(BINDIR);          #
#                                                                             #
#      (b) It will create $(LIBDIR) and copy all the library files into it;   #
#                                                                             #
#      (c) It will perform an initializing "lout -x" run.  This run will      #
#          do the following checks and initializations:                       #
#                                                                             #
#          (i)   It will read all the font files mentioned in file            #
#                $(LIBDIR)/include/fontdefs and check that they               #
#                exist and are in the correct format;                         #
#                                                                             #
#          (ii)  It will read all the hyphenation (.lh) files mentioned       #
#                in file $(LIBDIR)/include/langdefs, check them, and build    #
#                the packed (.lp) versions;                                   #
#                                                                             #
#          (iii) It will read and check the three standard database           #
#                (.ld) files in directory $(LIBDIR)/data, and build           #
#                the corresponding database index (.li) files.                #
#                                                                             #
#      (d) It will change the mode of the files created in (c) to be          #
#          publicly readable, just in case they weren't created that way.     #
#                                                                             #
#      It is good to build the various files during installation because      #
#      later runs will not have write permission in the library directories.  #
#                                                                             #
# (14) Execute "make installman".  This installs the manual entries for       #
#      lout and c2lout into directory $(MANDIR), which is assumed to exist.   #
#      These entries are troff files; plain text versions are also available  #
#      in directory ./man if you need them (install them yourself).           #
#                                                                             #
# (15) Execute "make installdoc".  This creates directory $(DOCDIR) and       #
#      copies the Lout documentation into it.                                 #
#                                                                             #
# (16) If you want French error messages, execute "make installfr" now.       #
#      If you want German error messages, execute "make installde" now.       #
#      These commands compile the error messages files into packed forms      #
#      using the gencat command, and store them in $(LIBDIR)/locale.          #
#                                                                             #
# (17) Execute "make clean".  This cleans up this directory.                  #
#                                                                             #
# (18) If the usual size of a piece of paper at your site is not A4, you      #
#      might like to now change the default value of the @PageType option     #
#      on line 1520 of file $(LIBDIR)/include/dl.  You can find the list of   #
#      known page types in the User's Guide, and also at line 2679 in file    #
#      $(LIBDIR)/include/dl.                                                  #
#                                                                             #
# (19) If the usual language at your site is not English, you might like to   #
#      now change the default value of the @InitialLanguage option on line    #
#      1502 of file $(LIBDIR)/include/dl.  This will mean that by default     #
#      the date and words like Chapter and July will appear in a different    #
#      language, and hyphenation will be carried out according to patterns    #
#      designed for that language.  You can find the list of known languages  #
#      in the User's Guide, or in file $(LIBDIR)/include/langdefs; if yours   #
#      is not on the list, let me know and we can work together to add it.    #
#      This has nothing to do with locales and USELOC.                        #
#                                                                             #
#  Mail jeff@cs.usyd.edu.au if you have any problems.                         #
#  PDF related problems can be mailed to vtan@ugrad.ug.cs.usyd.edu.au         #
#                                                                             #
###############################################################################

OSUNIX  = 1
OSDOS   = 0
OSMAC   = 0

DBFIX   = 0

USESTAT = 1
SAFEDFT = 0

COLLATE	= 1

BINDIR	= /usr/staff/jeff/bin
LIBDIR	= /usr/staff/jeff/lout.lib
DOCDIR	= /usr/staff/jeff/lout.doc
MANDIR	= /usr/staff/jeff/lout.man

LIBFONT = font
LIBMAPS = maps
LIBINCL = include
LIBDATA = data
LIBHYPH = hyph
LIBLOCA = locale

CHARIN	= 1
CHAROUT	= 0

USELOC	= 1
LOC_FR	= fr
LOC_DE	= de

PDF_COMPRESSION	= 0
ZLIB		=
ZLIBPATH	=

CC	= gcc

RCOPY	= cp -r

COPTS	= -ansi -pedantic -Wall

CFLAGS	= -DOS_UNIX=$(OSUNIX)					\
	  -DOS_DOS=$(OSDOS)					\
	  -DOS_MAC=$(OSMAC)					\
	  -DDB_FIX=$(DBFIX)					\
	  -DUSE_STAT=$(USESTAT)					\
	  -DSAFE_DFT=$(SAFEDFT)					\
	  -DCOLLATE=$(COLLATE)					\
	  -DLIB_DIR=\"$(LIBDIR)\"				\
	  -DFONT_DIR=\"$(LIBFONT)\"				\
	  -DMAPS_DIR=\"$(LIBMAPS)\"				\
	  -DINCL_DIR=\"$(LIBINCL)\"				\
	  -DDATA_DIR=\"$(LIBDATA)\"				\
	  -DHYPH_DIR=\"$(LIBHYPH)\"				\
	  -DLOCALE_DIR=\"$(LIBLOCA)\"				\
	  -DCHAR_IN=$(CHARIN)					\
	  -DCHAR_OUT=$(CHAROUT)					\
	  -DLOCALE_ON=$(USELOC)					\
	  -DASSERT_ON=1 $(COPTS)				\
	  -DDEBUG_ON=1  -g					\
	  -DPDF_COMPRESSION=$(PDF_COMPRESSION)			\
	  $(ZLIBPATH)

OBJS	= z01.o z02.o z03.o z04.o z05.o z06.o z07.o z08.o	\
	  z09.o z10.o z11.o z12.o z13.o z14.o z15.o z16.o	\
	  z17.o z18.o z19.o z20.o z21.o z22.o z23.o z24.o	\
	  z25.o z26.o z27.o z28.o z29.o z30.o z31.o z32.o	\
	  z33.o z34.o z35.o z36.o z37.o z38.o z39.o z40.o	\
	  z41.o z42.o z43.o z44.o z45.o z46.o z47.o z48.o

lout:	$(OBJS)
	$(CC) -o lout $(OBJS) $(ZLIB) -lm
	chmod a+x lout

$(OBJS): externs.h

externs.h:

c2lout:	c2lout.c
	$(CC) $(COPTS) -o c2lout c2lout.c
	chmod a+x c2lout

install: lout c2lout
	@echo ""
	@echo "(a) Installing lout and c2lout binaries into BINDIR $(BINDIR)"
	cp lout $(BINDIR)/lout
	chmod 755 $(BINDIR)/lout
	cp c2lout $(BINDIR)/c2lout
	chmod 755 $(BINDIR)/c2lout
	@echo ""
	@echo "(b) Installing library files into LIBDIR $(LIBDIR)"
	mkdir $(LIBDIR)
	chmod 755 $(LIBDIR)
	@echo ""
	mkdir $(LIBDIR)/$(LIBINCL)
	chmod 755 $(LIBDIR)/$(LIBINCL)
	cp include/* $(LIBDIR)/$(LIBINCL)
	cp setup/* $(LIBDIR)/$(LIBINCL)
	chmod 644 $(LIBDIR)/$(LIBINCL)/*
	@echo ""
	mkdir $(LIBDIR)/$(LIBDATA)
	chmod 755 $(LIBDIR)/$(LIBDATA)
	cp data/* $(LIBDIR)/$(LIBDATA)
	chmod 644 $(LIBDIR)/$(LIBDATA)/*
	@echo ""
	mkdir $(LIBDIR)/$(LIBHYPH)
	chmod 755 $(LIBDIR)/$(LIBHYPH)
	cp hyph/* $(LIBDIR)/$(LIBHYPH)
	chmod 644 $(LIBDIR)/$(LIBHYPH)/*
	@echo ""
	mkdir $(LIBDIR)/$(LIBFONT)
	chmod 755 $(LIBDIR)/$(LIBFONT)
	cp font/* $(LIBDIR)/$(LIBFONT)
	chmod 644 $(LIBDIR)/$(LIBFONT)/*
	@echo ""
	mkdir $(LIBDIR)/$(LIBMAPS)
	chmod 755 $(LIBDIR)/$(LIBMAPS)
	cp maps/* $(LIBDIR)/$(LIBMAPS)
	chmod 644 $(LIBDIR)/$(LIBMAPS)/*
	@echo ""
	mkdir $(LIBDIR)/$(LIBLOCA)
	chmod 755 $(LIBDIR)/$(LIBLOCA)
	@echo ""
	@echo "(c) Initializing run (should be silent, no errors expected)"
	$(BINDIR)/lout -x -s $(LIBDIR)/$(LIBINCL)/init
	@echo ""
	@echo "(d) Changing mode of files just created by initializing run"
	chmod 644 $(LIBDIR)/$(LIBDATA)/*
	chmod 644 $(LIBDIR)/$(LIBHYPH)/*

installman:
	@echo ""
	@echo "Installing manual entries into MANDIR $(MANDIR)"
	sed -e "s@<BINDIR>@$(BINDIR)@" -e "s@<LIBDIR>@$(LIBDIR)@"	\
	    -e "s@<DOCDIR>@$(DOCDIR)@" -e "s@<MANDIR>@$(MANDIR)@"	\
	man/lout.1 > $(MANDIR)/lout.1
	chmod 644 $(MANDIR)/lout.1
	cp man/c2lout.1 $(MANDIR)/c2lout.1
	chmod 644 $(MANDIR)/c2lout.1

installdoc:
	@echo ""
	@echo "Creating DOCDIR $(DOCDIR) and copying documentation into it"
	$(RCOPY) doc $(DOCDIR)
	chmod 755 $(DOCDIR)
	chmod 755 $(DOCDIR)/*
	chmod 644 $(DOCDIR)/*/*

installfr:
	@echo ""
	@echo "Putting French error messages into $(LIBDIR)/$(LIBLOCA)/$(LOC_FR)"
	mkdir $(LIBDIR)/$(LIBLOCA)/$(LOC_FR)
	chmod 755 $(LIBDIR)/$(LIBLOCA)/$(LOC_FR)
	mkdir $(LIBDIR)/$(LIBLOCA)/$(LOC_FR)/LC_MESSAGES
	chmod 755 $(LIBDIR)/$(LIBLOCA)/$(LOC_FR)/LC_MESSAGES
	cp locale/msgs.fr $(LIBDIR)/$(LIBLOCA)/$(LOC_FR)/LC_MESSAGES/msgs.$(LOC_FR)
	gencat $(LIBDIR)/$(LIBLOCA)/$(LOC_FR)/LC_MESSAGES/errors.$(LOC_FR)	\
	       $(LIBDIR)/$(LIBLOCA)/$(LOC_FR)/LC_MESSAGES/msgs.$(LOC_FR)
	chmod 644 $(LIBDIR)/$(LIBLOCA)/$(LOC_FR)/LC_MESSAGES/*

installde:
	@echo ""
	@echo "Putting German error messages into $(LIBDIR)/$(LIBLOCA)/$(LOC_DE)"
	mkdir $(LIBDIR)/$(LIBLOCA)/$(LOC_DE)
	chmod 755 $(LIBDIR)/$(LIBLOCA)/$(LOC_DE)
	mkdir $(LIBDIR)/$(LIBLOCA)/$(LOC_DE)/LC_MESSAGES
	chmod 755 $(LIBDIR)/$(LIBLOCA)/$(LOC_DE)/LC_MESSAGES
	cp locale/msgs.de $(LIBDIR)/$(LIBLOCA)/$(LOC_DE)/LC_MESSAGES/msgs.$(LOC_DE)
	gencat $(LIBDIR)/$(LIBLOCA)/$(LOC_DE)/LC_MESSAGES/errors.$(LOC_DE)	\
	       $(LIBDIR)/$(LIBLOCA)/$(LOC_DE)/LC_MESSAGES/msgs.$(LOC_DE)
	chmod 644 $(LIBDIR)/$(LIBLOCA)/$(LOC_DE)/LC_MESSAGES/*

uninstall:
	-rm -f  $(BINDIR)/lout $(BINDIR)/c2lout
	-rm -fr $(LIBDIR)
	-rm -fr $(DOCDIR)
	-rm -f  $(MANDIR)/lout.1 $(MANDIR)/c2lout.1

clean:	
	-rm -f lout c2lout *.o

restart:	clean uninstall


setupfiles:
	@echo ""
	@echo "Creating the basic five document types"
	dosetup doc		doc
	dosetup report		report
	dosetup book		book
	dosetup slides		slides
	dosetup picture		picture

	@echo ""
	@echo "The basic five with C program printing"
	dosetup cdoc		doc cprint
	dosetup creport		report cprint
	dosetup cbook		book cprint
	dosetup cslides		slides cprint
	dosetup cpicture	picture cprint

	@echo ""
	@echo "The basic five with Eiffel program printing"
	dosetup edoc		doc eiffelprint
	dosetup ereport		report eiffelprint
	dosetup ebook		book eiffelprint
	dosetup eslides		slides eiffelprint
	dosetup epicture	picture eiffelprint

	@echo ""
	@echo "The basic five with diagram printing"
	dosetup ddoc		doc diag
	dosetup dreport		report diag
	dosetup dbook		book diag
	dosetup dslides		slides diag
	dosetup dpicture	picture diag

	@echo ""
	@echo "Books with Eiffel and C program printing and diagrams"
	dosetup dedoc		doc eiffelprint diag
	dosetup debook		book eiffelprint diag
	dosetup dcbook		book cprint diag

	@echo ""
	@echo "Ordinary documents and overheads with Blue printing"
	dosetup	bdoc		doc blueprint
	dosetup	bddoc		doc diag blueprint
	dosetup bdslides	slides diag blueprint
	dosetup ebdslides	slides diag blueprint eiffelprint
