###############################################################################
#                                                                             #
#  Make file for installing Basser Lout Version 3.02                          #
#                                                                             #
#  Jeffrey H. Kingston                                                        #
#  11 November 1994                                                           #
#                                                                             #
#     make lout         Compile the Lout source                               #
#     make c2lout       Compile a small auxiliary program called c2lout       #
#     make install      Install the Lout and c2lout binaries and libraries    #
#     make installman   Install the Lout and c2lout manual entries            #
#     make installdoc   Install the Lout documentation                        #
#     make clean        Remove compilation temporaries                        #
#     make uninstall    Undo the effect of make install, make installman,     #
#                       and make installdoc                                   #
#     make restart      Undo everything except changes to this makefile,      #
#                       ready for a fresh start.                              #
#                                                                             #
#  Most installations of Lout should require only the following steps.  If    #
#  something goes wrong, you can start again with "make restart".             #
#                                                                             #
#  (1) Set the following four macros defined below to appropriate values:     #
#                                                                             #
#      BINDIR  Directory where Lout's binary goes (this directory is assumed  #
#              to exist).                                                     #
#                                                                             #
#      LIBDIR  Directory where Lout's libraries go (this directory will be    #
#              created).                                                      #
#                                                                             #
#      DOCDIR  Directory where Lout's documents go; they are reports written  #
#              in Lout about the system (this directory will be created).     #
#                                                                             #
#      MANDIR  Directory where Lout's online manual entry goes; the entry is  #
#              in nroff -man format (this directory is assumed to exist).     #
#                                                                             #
#  (2) Set the following two macros defined below to appropriate values.      #
#      I strongly recommend CHARIN=1 and CHAROUT=0 for all sites (English     #
#      and non-English language).  This way we get a truly international      #
#      standard in which everyone has access to accented characters, yet      #
#      Lout's output is in the strict 7-bit ASCII that is recommended in      #
#      the PostScript manual.                                                 #
#                                                                             #
#      CHARIN  This macro determines the assignment of characters in Lout     #
#              source files to character classes in Lout's lexical analyser.  #
#              That is, it determines which characters are letters, which is  #
#              the comment character, etc.  Currently supported values are:   #
#                                                                             #
#                0  For English language only ASCII installations             #
#                                                                             #
#                1  For installations using the ISO-LATIN-1 character set     #
#                   (adds accented letters to the LETTER character class)     #
#                                                                             #
#              Please note that Lout will accept any 8-bit character except   #
#              '\0'; CHARIN does not determine the acceptability of any       #
#              character, just its class.                                     #
#                                                                             #
#      CHAROUT This macro determines the format of strings of literal         #
#              characters in the PostScript output.  Currently supported      #
#              values are:                                                    #
#                                                                             #
#                0  Every output character will be printable ASCII            #
#                                                                             #
#                1  Every output character will be printable ISO-LATIN-1      #
#                                                                             #
#              The output will be valid PostScript irrespective of the value  #
#              given to CHAROUT, which may be set entirely independently of   #
#              CHARIN.  It just determines which characters will be printed   #
#              as \ddd escape sequences and which will be printed as one-byte #
#              literal characters.                                            #
#                                                                             #
#  (3) Set macro USELOC to one of the following values, NOT TO A LOCALE.      #
#                                                                             #
#      0       This value means to compile without including <locale.h> or    #
#              <nl_types.h>, and without calls to setlocale(), catopen(),     #
#              catgets(), or catclose(), and is appropriate for all sites.    #
#                                                                             #
#      1       This value means to compile with setlocale() etc.  The only    #
#              use made of this is to print error messages in the locale      #
#              language if available, that is, if setlocale(LC_MESSAGES, "")  #
#              returns locale X and $(LIBDIR)/locale/X/LC_MESSAGES/errors.X   #
#              can be opened by catopen().  This is where error messages for  #
#              locale X are stored.  This flag has no effect on the Lout      #
#              input language, and it never will.                             #
#                                                                             #
#      For further information consult file lout/locale/README.               #
#                                                                             #
#  (4) Execute "make lout".  This will compile the Lout source, leaving the   #
#      binary in this directory.  No changes are made in other directories.   #
#                                                                             #
#  (5) Execute "make c2lout".  This will compile the c2lout program, leaving  #
#      its binary in this directory.  No changes to other directories.        #
#                                                                             #
#  (6) Execute "make install".  This will do the following things:            #
#                                                                             #
#      (a) Copy the lout and c2lout binaries into $(BINDIR);                  #
#                                                                             #
#      (b) Create $(LIBDIR) and copy all the library files into it;           #
#                                                                             #
#      (c) Perform an initializing "lout -x" run.  This run will do the       #
#          following checks and initializations:                              #
#                                                                             #
#          (i)   If $(USELOC) is set to 1, it will generate error message     #
#                catalogues for all locales in subdirectories of directory    #
#                $(LIBDIR)/locale, by calling the gencat Unix command;        #
#                                                                             #
#          (ii)  It will read all the font files mentioned in file            #
#                $(LIBDIR)/include/fontdefs and check that they               #
#                exist and are in the correct format;                         #
#                                                                             #
#          (iii) It will read all the hyphenation (.lh) files mentioned       #
#                in file $(LIBDIR)/include/langdefs, check them, and build    #
#                the packed (.lp) versions;                                   #
#                                                                             #
#          (iv)  It will read and check the three standard database           #
#                (.ld) files in directory $(LIBDIR)/data, and build           #
#                the corresponding database index (.li) files.                #
#                                                                             #
#      (d) Change the mode of the files created in (c) to be publicly         #
#          readable, just in case they weren't created that way.              #
#                                                                             #
#      It is good to build the various files during installation because      #
#      later runs will not have write permission in the library directories.  #
#                                                                             #
#  (7) Execute "make installman".  This installs the manual entries for       #
#      lout and c2lout into directory $(MANDIR), which is assumed to exist.   #
#                                                                             #
#  (8) Execute "make installdoc".  This creates directory $(DOCDIR) and       #
#      copies the Lout documentation into it.                                 #
#                                                                             #
#  (9) Execute "make clean".  This cleans up this directory.                  #
#                                                                             #
# (10) If the usual size of a piece of paper at your site is not A4, you      #
#      might like to now change the default value of the @PageType option     #
#      on line 468 of file $(LIBDIR)/include/dl.  You can find the list of    #
#      known page types in the User's Guide, $(LIBDIR)/doc/user/outfile.ps.   #
#                                                                             #
# (11) If the usual language at your site is not English, you might like to   #
#      now change the default value of the @InitialLanguage option on line    #
#      454 of file $(LIBDIR)/include/dl.  This will mean that by default the  #
#      date and words like Chapter and July will appear in a different        #
#      language.  You can find the list of known languages in the User's      #
#      Guide; if yours is not on the list, let me know and we can work        #
#      together to add it.  This has nothing to do with locales and USELOC.   #
#                                                                             #
#  Mail jeff@cs.su.oz.au if you have any problems.                            #
#                                                                             #
###############################################################################

BINDIR	= /usr/local/bin
LIBDIR	= /usr/local/lib/lout
DOCDIR	= /usr/local/lib/loutdoc
MANDIR	= /usr/local/man

CHARIN	= 1
CHAROUT	= 0
USELOC	= 0

CC	= cc

COPTS	=

CFLAGS	= -DFONT_DIR=\"$(LIBDIR)/font\"				\
	  -DEVEC_DIR=\"$(LIBDIR)/evec\"				\
	  -DINCL_DIR=\"$(LIBDIR)/include\"			\
	  -DDATA_DIR=\"$(LIBDIR)/data\"				\
	  -DHYPH_DIR=\"$(LIBDIR)/hyph\"				\
	  -DLOCALE_DIR=\"$(LIBDIR)/locale\"			\
	  -DCHAR_IN=$(CHARIN)					\
	  -DCHAR_OUT=$(CHAROUT)					\
	  -DLOCALE_ON=$(USELOC)					\
	  -DASSERT_ON=1 $(COPTS)

OBJS	= z01.o z02.o z03.o z04.o z05.o z06.o z07.o z08.o	\
	  z09.o z10.o z11.o z12.o z13.o z14.o z15.o z16.o	\
	  z17.o z18.o z19.o z20.o z21.o z22.o z23.o z24.o	\
	  z25.o z26.o z27.o z28.o z29.o z30.o z31.o z32.o	\
	  z33.o z34.o z35.o z36.o z37.o z38.o z39.o z40.o	\
	  z41.o z42.o z43.o

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
	cp -r locale $(LIBDIR)/locale
	chmod 755 $(LIBDIR)/locale/*
	chmod 755 $(LIBDIR)/locale/*/*
	chmod 644 $(LIBDIR)/locale/*/*/*
	@echo ""
	@echo "(c) Initializing run (should be silent, no errors expected)"
	$(BINDIR)/lout -xfr:de -s $(LIBDIR)/include/init
	@echo ""
	@echo "(d) Changing mode of files just created by initializing run"
	chmod 644 $(LIBDIR)/data/*
	chmod 644 $(LIBDIR)/hyph/*
	chmod 644 $(LIBDIR)/locale/*/*/*

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

uninstall:
	-rm -f  $(BINDIR)/lout $(BINDIR)/c2lout
	-rm -fr $(LIBDIR)
	-rm -fr $(DOCDIR)
	-rm -f  $(MANDIR)/lout.1 $(MANDIR)/c2lout.1

clean:	
	-rm -f lout c2lout *.o

restart:	clean uninstall
