###############################################################################
#                                                                             #
#  Make file for installing Basser Lout Version 3.06                          #
#                                                                             #
#  Jeffrey H. Kingston                                                        #
#  25 July 1995                                                               #
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
#  Most installations of Lout should require only the following steps.  If    #
#  something goes wrong, you can start again with "make restart".  Please     #
#  carry out all the steps, in exactly the order given.                       #
#                                                                             #
#  (1) Set exactly one of the following macros defined below to 1 and the     #
#      others all to 0, to indicate the operating system under which the      #
#      Lout binary is to run.  At present OSUNIX certainly works, OSDOS       #
#      almost certainly works, and OSMAC doesn't work.                        #
#                                                                             #
#      OSUNIX    Unix in all its flavours, including Linux.                   #
#      OSDOS     MS-DOS etc. ("rb" and "wb" file access modes where needed)   #
#      OSMAC     Macintosh                                                    #
#                                                                             #
#  (2) Set the following four macros defined below to appropriate values:     #
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
#  (3) Set the following two macros defined below to appropriate values.      #
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
#  (4) Set macro USELOC to one of the following values, NOT TO A LOCALE.      #
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
#  (5) This makefile assumes that Lout is not installed on your system        #
#      already.  If you do have an earlier version of Lout installed,         #
#      the simplest way to get rid of it is to type "make uninstall" now.     #
#      Of course, this is assuming that the old version was installed in the  #
#      same directories as where you are about to install the new version.    #
#                                                                             #
#  (6) Execute "make lout".  This will compile the Lout source, leaving the   #
#      binary in this directory.  No changes are made in other directories.   #
#                                                                             #
#  (7) Execute "make c2lout".  This will compile the c2lout program, leaving  #
#      its binary in this directory.  No changes to other directories.        #
#                                                                             #
#  (8) Execute "make install".  This will do the following things:            #
#                                                                             #
#      (a) Copy the lout and c2lout binaries into $(BINDIR);                  #
#                                                                             #
#      (b) Create $(LIBDIR) and copy all the library files into it;           #
#                                                                             #
#      (c) Perform an initializing "lout -x" run.  This run will do the       #
#          following checks and initializations:                              #
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
#          Note to OS/2 users: this initializing run may fail with an error   #
#          message pointing to file norweg.lh (the Norwegian language         #
#          hyphenation patterns file).  The cause is unknown but may be       #
#          something to do with the relatively large size of this file.  If   #
#          this happens, change line 8 of file ./include/langdefs to          #
#                                                                             #
#              langdef Norwegian Norsk { }                                    #
#                                                                             #
#          to dis-inform Lout about this file, then "make uninstall" and      #
#          "make install" again.  Lout will not be able to hyphenate          #
#          Norwegian language words if you have to do this.                   #
#                                                                             #
#      (d) Change the mode of the files created in (c) to be publicly         #
#          readable, just in case they weren't created that way.              #
#                                                                             #
#      It is good to build the various files during installation because      #
#      later runs will not have write permission in the library directories.  #
#                                                                             #
#  (9) Execute "make installman".  This installs the manual entries for       #
#      lout and c2lout into directory $(MANDIR), which is assumed to exist.   #
#      These entries are troff files; plain text versions are also available  #
#      in directory ./man if you need them (install them yourself).           #
#                                                                             #
# (10) Execute "make installdoc".  This creates directory $(DOCDIR) and       #
#      copies the Lout documentation into it.                                 #
#                                                                             #
# (11) If you want French error messages, execute "make installfr" now.       #
#      If you want German error messages, execute "make installde" now.       #
#      These commands compile the error messages files into packed forms      #
#      using the gencat command, and store them in $(LIBDIR)/locale.          #
#                                                                             #
# (12) Execute "make clean".  This cleans up this directory.                  #
#                                                                             #
# (13) If the usual size of a piece of paper at your site is not A4, you      #
#      might like to now change the default value of the @PageType option     #
#      on line 623 of file $(LIBDIR)/include/dl.  You can find the list of    #
#      known page types in the User's Guide, $(LIBDIR)/doc/user/outfile.ps,   #
#      and also at line 1496 in file $(LIBDIR)/include/dl.                    #
#                                                                             #
# (14) If the usual language at your site is not English, you might like to   #
#      now change the default value of the @InitialLanguage option on line    #
#      609 of file $(LIBDIR)/include/dl.  This will mean that by default the  #
#      date and words like Chapter and July will appear in a different        #
#      language, and hyphenation will be carried out according to patterns    #
#      designed for that language.  You can find the list of known languages  #
#      in the User's Guide, or in file $(LIBDIR)/include/langdefs; if yours   #
#      is not on the list, let me know and we can work together to add it.    #
#      This has nothing to do with locales and USELOC.                        #
#                                                                             #
#  Mail jeff@cs.su.oz.au if you have any problems.                            #
#                                                                             #
###############################################################################

OSUNIX  = 1
OSDOS   = 0
OSMAC   = 0

BINDIR	= /usr/local/bin
LIBDIR	= /usr/local/lib/lout
DOCDIR	= /usr/local/lib/loutdoc
MANDIR	= /usr/local/man/man1

CHARIN	= 1
CHAROUT	= 0

USELOC	= 0
LOC_FR	= fr
LOC_DE	= de

CC	= cc

COPTS	=

CFLAGS	= -DOS_UNIX=$(OSUNIX)					\
	  -DOS_DOS=$(OSDOS)					\
	  -DOS_MAC=$(OSMAC)					\
	  -DFONT_DIR=\"$(LIBDIR)/font\"				\
	  -DEVEC_DIR=\"$(LIBDIR)/evec\"				\
	  -DINCL_DIR=\"$(LIBDIR)/include\"			\
	  -DDATA_DIR=\"$(LIBDIR)/data\"				\
	  -DHYPH_DIR=\"$(LIBDIR)/hyph\"				\
	  -DLOCALE_DIR=\"$(LIBDIR)/locale\"			\
	  -DCHAR_IN=$(CHARIN)					\
	  -DCHAR_OUT=$(CHAROUT)					\
	  -DLOCALE_ON=$(USELOC)					\
	  -DDEBUG_ON=0						\
	  -DASSERT_ON=1 $(COPTS)

OBJS	= z01.o z02.o z03.o z04.o z05.o z06.o z07.o z08.o	\
	  z09.o z10.o z11.o z12.o z13.o z14.o z15.o z16.o	\
	  z17.o z18.o z19.o z20.o z21.o z22.o z23.o z24.o	\
	  z25.o z26.o z27.o z28.o z29.o z30.o z31.o z32.o	\
	  z33.o z34.o z35.o z36.o z37.o z38.o z39.o z40.o	\
	  z41.o z42.o z43.o z44.o z45.o z46.o

lout:	$(OBJS)
	$(CC) -o lout $(OBJS) -lm
	chmod a+x lout

$(OBJS): externs

externs:

c2lout:	c2lout.c
	$(CC) -o c2lout c2lout.c
	chmod a+x c2lout

install: lout c2lout
	@echo ""
	@echo "(a) Installing Lout and c2lout binaries into BINDIR $(BINDIR)"
	cp lout $(BINDIR)/lout
	chmod 755 $(BINDIR)/lout
	cp c2lout $(BINDIR)/c2lout
	chmod 755 $(BINDIR)/c2lout
	@echo ""
	@echo "(b) Installing library files into LIBDIR $(LIBDIR)"
	mkdir $(LIBDIR)
	chmod 755 $(LIBDIR)
	@echo ""
	mkdir $(LIBDIR)/include
	chmod 755 $(LIBDIR)/include
	cp include/* $(LIBDIR)/include
	chmod 644 $(LIBDIR)/include/*
	@echo ""
	mkdir $(LIBDIR)/data
	chmod 755 $(LIBDIR)/data
	cp data/* $(LIBDIR)/data
	chmod 644 $(LIBDIR)/data/*
	@echo ""
	mkdir $(LIBDIR)/hyph
	chmod 755 $(LIBDIR)/hyph
	cp hyph/* $(LIBDIR)/hyph
	chmod 644 $(LIBDIR)/hyph/*
	@echo ""
	mkdir $(LIBDIR)/font
	chmod 755 $(LIBDIR)/font
	cp font/* $(LIBDIR)/font
	chmod 644 $(LIBDIR)/font/*
	@echo ""
	mkdir $(LIBDIR)/evec
	chmod 755 $(LIBDIR)/evec
	cp evec/* $(LIBDIR)/evec
	chmod 644 $(LIBDIR)/evec/*
	@echo ""
	mkdir $(LIBDIR)/locale
	chmod 755 $(LIBDIR)/locale
	@echo ""
	@echo "(c) Initializing run (should be silent, no errors expected)"
	$(BINDIR)/lout -x -s $(LIBDIR)/include/init
	@echo ""
	@echo "(d) Changing mode of files just created by initializing run"
	chmod 644 $(LIBDIR)/data/*
	chmod 644 $(LIBDIR)/hyph/*

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
	cp -r doc $(DOCDIR)
	chmod 755 $(DOCDIR)
	chmod 755 $(DOCDIR)/*
	chmod 644 $(DOCDIR)/*/*

installfr:
	@echo ""
	@echo "Installing French error messages into $(LIBDIR)/locale/$(LOC_FR)"
	mkdir $(LIBDIR)/locale/$(LOC_FR)
	chmod 755 $(LIBDIR)/locale/$(LOC_FR)
	mkdir $(LIBDIR)/locale/$(LOC_FR)/LC_MESSAGES
	chmod 755 $(LIBDIR)/locale/$(LOC_FR)/LC_MESSAGES
	cp locale/msgs.fr $(LIBDIR)/locale/$(LOC_FR)/LC_MESSAGES/msgs.$(LOC_FR)
	gencat $(LIBDIR)/locale/$(LOC_FR)/LC_MESSAGES/errors.$(LOC_FR)	\
	       $(LIBDIR)/locale/$(LOC_FR)/LC_MESSAGES/msgs.$(LOC_FR)
	chmod 644 $(LIBDIR)/locale/$(LOC_FR)/LC_MESSAGES/*

installde:
	@echo ""
	@echo "Installing German error messages into $(LIBDIR)/locale/$(LOC_DE)"
	mkdir $(LIBDIR)/locale/$(LOC_DE)
	chmod 755 $(LIBDIR)/locale/$(LOC_DE)
	mkdir $(LIBDIR)/locale/$(LOC_DE)/LC_MESSAGES
	chmod 755 $(LIBDIR)/locale/$(LOC_DE)/LC_MESSAGES
	cp locale/msgs.de $(LIBDIR)/locale/$(LOC_DE)/LC_MESSAGES/msgs.$(LOC_DE)
	gencat $(LIBDIR)/locale/$(LOC_DE)/LC_MESSAGES/errors.$(LOC_DE)	\
	       $(LIBDIR)/locale/$(LOC_DE)/LC_MESSAGES/msgs.$(LOC_DE)
	chmod 644 $(LIBDIR)/locale/$(LOC_DE)/LC_MESSAGES/*

uninstall:
	-rm -f  $(BINDIR)/lout $(BINDIR)/c2lout
	-rm -fr $(LIBDIR)
	-rm -fr $(DOCDIR)
	-rm -f  $(MANDIR)/lout.1 $(MANDIR)/c2lout.1

clean:	
	-rm -f lout c2lout *.o

restart:	clean uninstall
