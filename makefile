###############################################################################
#                                                                             #
#  Make file for installing Basser Lout Version 2.05                          #
#                                                                             #
#  Jeffrey H. Kingston                                                        #
#  21 June 1993                                                               #
#                                                                             #
#     make lout         Compile the Lout source                               #
#     make install      Install the Lout binary and libraries                 #
#     make installman   Install the Lout manual entry                         #
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
#  (2) Set the following three macros defined below to appropriate values.    #
#      I now strongly recommend CHARIN=1, CHAROUT=0, and CHARFT=1 for all     #
#      sites (English and non-English language).  This way we get a truly     #
#      international standard in which everyone has access to accented        #
#      characters (even English sites need them occasionally for foreign      #
#      words and names), yet Lout's output is in the strict 7-bit ASCII that  #
#      is strongly recommended (I don't know why) in the PostScript manual.   #
#                                                                             #
#      CHARIN  This macro determines the assignment of characters in Lout     #
#              source files to character classes in Lout's lexical analyser.  #
#              That is, it determines which characters are letters, which is  #
#              the comment character, etc.  Currently supported values are:   #
#                                                                             #
#                0  For English language ASCII installations                  #
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
#              CHARIN and CHARFT.  It just determines which characters will   #
#              be printed as \ddd escape sequences and which will be printed  #
#              as one-byte literal characters.                                #
#                                                                             #
#      CHARFT  This macro determines which font/encoding vector combinations  #
#              will be loaded by the standard setup files.  Currently         #
#              supported values are                                           #
#                                                                             #
#                0  For English language ASCII installations                  #
#                                                                             #
#                1  For ISO-LATIN-1 installations (ISO-LATIN-1 encoding       #
#                   augmented with extra codes, e.g. for ligatures)           #
#                                                                             #
#              This macro does not affect the Lout binary in any way; it      #
#              operates by copying file fontdefs$(CHARFT) to the standard     #
#              fontdefs file "ft" during "make install".                      #
#                                                                             #
#  (3) Execute "make lout".  This will compile the Lout source, leaving the   #
#      binary in this directory.  No changes are made in other directories.   #
#                                                                             #
#  (4) Execute "make install".  This will do the following things:            #
#                                                                             #
#      (a)     Copy the binary into BINDIR;                                   #
#                                                                             #
#      (b)     Create LIBDIR and copy all the library files into it;          #
#                                                                             #
#      (c)     Within directory $(LIBDIR)/include, copy fontdefs$(CHARFT)     #
#              to file ft.  See above under CHARFT for explanation.           #
#                                                                             #
#      (d)     Perform a test run on the document kept in ./doc/tr.eq.  This  #
#              is compulsory because it has side effects: the database index  #
#              files loutrefs.li, refstyles.li, and standard.li are created   #
#              in directory $(LIBDIR)/data, and the packed hyphenation        #
#              pattern file lout.hyph.packed is created in directory          #
#              $(LIBDIR)/include.  The test run will produce quite a few      #
#              warning messages about unresolved cross references, but there  #
#              should be no fatal ones.  (These warning messages gradually    #
#              go away on subsequent runs.)                                   #
#                                                                             #
#  (5) Execute "make installman".  This installs the manual entry in MANDIR.  #
#                                                                             #
#  (6) Execute "make installdoc".  This creates $(DOCDIR) and copies the      #
#      technical reports into it.                                             #
#                                                                             #
#  (7) Execute "make clean".  This cleans up this directory.                  #
#                                                                             #
#  (8) If the usual size of a piece of paper at your site is not A4, change   #
#      the default values of the @PageWidth and @PageHeight parameters of     #
#      the DocumentLayout package to the physical width and height of your    #
#      paper.  Find them on lines 160-1 of file $(LIBDIR)/include/dl.  For    #
#      example, 29.70c is 29.7 centimetres, the A4 height.                    #
#                                                                             #
#  (9) If you intend the installation to produce output in a language other   #
#      than English, you need to change all the places where Lout's standard  #
#      packages and databases insert English words automatically (such as     #
#      "Chapter" in a chapter heading, or "July" in a date).  This is how:    #
#                                                                             #
#      (a)     Change the words between braces on lines 214-223 of file       #
#              $(LIBDIR)/include/dl to their equivalents in your language;    #
#                                                                             #
#      (b)     Change the month names and weekday names on lines 276-316 of   #
#              file $(LIBDIR)/data/standard.ld to their equivalents in your   #
#              language, then delete the file $(LIBDIR)/data/standard.li      #
#              that was created by step (4d) above.  This .li file will be    #
#              re-created automatically on the next run, so you must ensure   #
#              that the next run can write into directory $(LIBDIR)/data.     #
#                                                                             #
#      (c)     There are a few English words in $(LIBDIR)/data/refstyles.ld   #
#              also.  Again, delete refstyles.li if you change refstyles.ld.  #
#                                                                             #
#      There are no English literals in the binary.                           #
#                                                                             #
#                                                                             #
#  Mail jeff@cs.su.oz.au if you have any problems.                            #
#                                                                             #
###############################################################################

BINDIR	= /usr/local/bin
LIBDIR	= /usr/local/lib/lout
DOCDIR	= /usr/local/lib/lout.doc
MANDIR	= /usr/local/man/man1

CHARIN	= 1
CHAROUT	= 0
CHARFT	= 1

CFLAGS	= -DFONT_DIR=\"$(LIBDIR)/font\"				\
	  -DEVEC_DIR=\"$(LIBDIR)/evec\"				\
	  -DINCL_DIR=\"$(LIBDIR)/include\"			\
	  -DDATA_DIR=\"$(LIBDIR)/data\"				\
	  -DCHAR_IN=$(CHARIN)					\
	  -DCHAR_OUT=$(CHAROUT)					\
	  -DDEBUG_ON=0						\
	  -DASSERT_ON=1

OBJS	= z01.o z02.o z03.o z04.o z05.o z06.o z07.o z08.o	\
	  z09.o z10.o z11.o z12.o z13.o z14.o z15.o z16.o	\
	  z17.o z18.o z19.o z20.o z21.o z22.o z23.o z24.o	\
	  z25.o z26.o z27.o z28.o z29.o z30.o z31.o z32.o	\
	  z33.o z34.o z35.o z36.o z37.o z38.o z39.o

lout:	$(OBJS)
	$(CC) -o lout $(OBJS) -lm
	chmod a+x lout

$(OBJS): externs

externs:

install: lout
	@echo ""
	@echo "(a) Installing Lout binary into BINDIR $(BINDIR)"
	cp lout $(BINDIR)/lout
	chmod a+x-w $(BINDIR)/lout
	@echo ""
	@echo "(b) Installing library files into LIBDIR $(LIBDIR)"
	mkdir $(LIBDIR)
	chmod 775 $(LIBDIR)
	@echo ""
	mkdir $(LIBDIR)/include
	chmod 775 $(LIBDIR)/include
	cp include/* $(LIBDIR)/include
	chmod a+r-wx $(LIBDIR)/include/*
	@echo ""
	mkdir $(LIBDIR)/data
	chmod 775 $(LIBDIR)/data
	cp data/* $(LIBDIR)/data
	chmod a+r-wx $(LIBDIR)/data/*
	@echo ""
	mkdir $(LIBDIR)/font
	chmod 775 $(LIBDIR)/font
	cp font/* $(LIBDIR)/font
	chmod a+r-wx $(LIBDIR)/font/*
	@echo ""
	mkdir $(LIBDIR)/evec
	chmod 775 $(LIBDIR)/evec
	cp evec/* $(LIBDIR)/evec
	chmod a+r-wx $(LIBDIR)/evec/*
	@echo ""
	@echo "(c) Copying a font definitions file to ft"
	cp $(LIBDIR)/include/fontdefs$(CHARFT) $(LIBDIR)/include/ft
	@echo ""
	@echo "(d) Compulsory test on doc/tr.eq (expect many warning messages)"
	./lout ./doc/tr.eq/setup ./doc/tr.eq/s? > ./doc/tr.eq/op
	rm ./doc/tr.eq/op ./doc/tr.eq/s?.ld lout.li

installman:
	@echo ""
	@echo "Installing manual entry into MANDIR $(MANDIR)"
	sed -e "s@<BINDIR>@$(BINDIR)@" -e "s@<LIBDIR>@$(LIBDIR)@"	\
	    -e "s@<DOCDIR>@$(DOCDIR)@" -e "s@<MANDIR>@$(MANDIR)@"	\
	man/lout.1 > $(MANDIR)/lout.1
	chmod a+r $(MANDIR)/lout.1

installdoc:
	@echo ""
	@echo "Creating DOCDIR $(DOCDIR) and installing documentation into it"
	mkdir $(DOCDIR)
	chmod 775 $(DOCDIR)
	mkdir $(DOCDIR)/tr.lout
	chmod 775 $(DOCDIR)/tr.lout
	cp doc/tr.lout/* $(DOCDIR)/tr.lout
	chmod a+r-wx $(DOCDIR)/tr.lout/*
	mkdir $(DOCDIR)/tr.impl
	chmod 775 $(DOCDIR)/tr.impl
	cp doc/tr.impl/* $(DOCDIR)/tr.impl
	chmod a+r-wx $(DOCDIR)/tr.impl/*
	mkdir $(DOCDIR)/tr.over
	chmod 775 $(DOCDIR)/tr.over
	cp doc/tr.over/* $(DOCDIR)/tr.over
	chmod a+r-wx $(DOCDIR)/tr.over/*
	mkdir $(DOCDIR)/tr.begin
	chmod 775 $(DOCDIR)/tr.begin
	cp doc/tr.begin/* $(DOCDIR)/tr.begin
	chmod a+r-wx $(DOCDIR)/tr.begin/*
	mkdir $(DOCDIR)/tr.eq
	chmod 775 $(DOCDIR)/tr.eq
	cp doc/tr.eq/*   $(DOCDIR)/tr.eq
	chmod a+r-wx $(DOCDIR)/tr.eq/*
	mkdir $(DOCDIR)/tr.fig
	chmod 775 $(DOCDIR)/tr.fig
	cp doc/tr.fig/*  $(DOCDIR)/tr.fig
	chmod a+r-wx $(DOCDIR)/tr.fig/*
	mkdir $(DOCDIR)/tr.tab
	chmod 775 $(DOCDIR)/tr.tab
	cp doc/tr.tab/*  $(DOCDIR)/tr.tab
	chmod a+r-wx $(DOCDIR)/tr.tab/*

uninstall:
	-rm -f  $(BINDIR)/lout
	-rm -fr $(LIBDIR)
	-rm -fr $(DOCDIR)
	-rm -f  $(MANDIR)/lout.1

clean:	
	-rm -f lout *.o

restart:
	-rm -f lout *.o
	-rm -f  $(BINDIR)/lout
	-rm -fr $(LIBDIR)
	-rm -fr $(DOCDIR)
	-rm -f  $(MANDIR)/lout.1
