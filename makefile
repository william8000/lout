
###################################################################
#                                                                 #
#  Make file for installing Lout Version 2.03                     #
#                                                                 #
#  Jeffrey H. Kingston                                            #
#  23 April 1993                                                  #
#                                                                 #
#     make lout        Compile the Lout source                    #
#     make install     Install the Lout binary and libraries and  #
#                      perform a compulsory initial run           #
#     make installman  Install the Lout manual entry              #
#     make installdoc  Install the Lout documentation             #
#     make clean       Remove compilation temporaries             #
#     make clobber     Undo the effect of `make lout'             #
#     make uninstall   Undo the effect of `make install',         #
#                      `make installman', and `make installdoc'   #
#                                                                 #
#  Most installations of Lout should require no more than the     #
#  following steps.                                               #
#                                                                 #
#  (1) Set the following five macros defined below to appropriate #
#      values.  The macros are                                    #
#                                                                 #
#         LATIN        Set this to 1 if the ISO-LATIN-1 character #
#                      set is to be used (i.e. for non-English    #
#                      languages), or 0 for ASCII (i.e. English). #
#                      This has two effects: it changes the       #
#                      lexical analyser in the lout binary so     #
#                      as to recognize ISO-LATIN-1 characters,    #
#                      and it changes the directory that the Lout #
#                      font files are copied from during install  #
#                      so that Latin .AFM files are used.  It     #
#                      has no effect on hyphenation.              #
#                                                                 #
#                      At the time of writing the Latin font      #
#                      directory is empty and the changes to the  #
#                      lexical analyser are not implemented; but  #
#                      someday someone will fill in these gaps.   #
#                                                                 #
#         BINDIR       The directory where the Lout binary goes   #
#                      (this directory is assumed to exist)       #
#                                                                 #
#         LIBDIR       Directory where the Lout libraries go      #
#                      (this directory will be created)           #
#                                                                 #
#         DOCDIR       Directory where the Lout documents go;     #
#                      they are tech. reports written in Lout     #
#                      describing various parts of the system     #
#                      (this directory will be created)           #
#                                                                 #
#         MANDIR       Directory where Lout manual entry goes;    #
#                      it has the form of input to nroff -man     #
#                      (this directory is assumed to exist)       #
#                                                                 #
#  (2) Execute "make lout".  This will compile the Lout source    #
#      code and leave the binary in this directory.  No changes   #
#      will be made in any other directories.                     #
#                                                                 #
#  (3) Execute "make install".  This will (a) copy the Lout       #
#      binary into directory BINDIR; (b) create LIBDIR and copy   #
#      all the library files into it; and finally (c) perform a   #
#      test run on the document stored in ./doc/tr.eq.  This test #
#      is compulsory because it has side effects: the database    #
#      index files loutrefs.li, refstyles.li, and standard.li     #
#      are created in directory $(LIBDIR)/data, and the packed    #
#      hyphenation pattern file lout.hyph.packed is created in    #
#      directory $(LIBDIR)/include.  The test run will produce    #
#      quite a few warning messages about unresolved cross        #
#      references, but there should be no fatal errors.  (These   #
#      warning messages gradually go away on subsequent runs.)    #
#                                                                 #
#  (4) Execute "make installman".  This installs the manual       #
#      entry in directory $(MANDIR).                              #
#                                                                 #
#  (5) Execute "make installdoc".  This creates $(DOCDIR) and     #
#      copies the tech. reports into it.                          #
#                                                                 #
#  (6) Execute "make clobber".  This undoes "make lout".          #
#                                                                 #
#  You probably also want to change the default values of the     #
#  @PageWidth and @PageHeight parameters of the DocumentLayout    #
#  package, if you are using something other than A4 paper.       #
#  These are found on lines 154-5 of file $(LIBDIR)/include/dl.   #
#  Set them to the physical width and height of your paper; for   #
#  example, 29.70c is 29.7 centimetres, the A4 height.            #
#                                                                 #
#  Mail jeff@cs.su.oz.au if you have any problems.                #
#                                                                 #
###################################################################

LATIN	= 0
BINDIR	= /usr/local/bin
LIBDIR	= /usr/local/lib/lout
DOCDIR	= /usr/local/lib/lout.doc
MANDIR	= /usr/local/man/man1

CFLAGS	= -DFONT_DIR=\"$(LIBDIR)/font$(LATIN)\"			\
	  -DINCL_DIR=\"$(LIBDIR)/include\"			\
	  -DDATA_DIR=\"$(LIBDIR)/data\"				\
	  -DLATIN=$(LATIN)					\
	  -DDEBUG_ON=0						\
	  -DASSERT_ON=1

OBJS	= z01.o z02.o z03.o z04.o z05.o z06.o z07.o		\
	  z08.o z09.o z10.o z11.o z12.o z13.o z14.o		\
	  z15.o z16.o z17.o z18.o z19.o z20.o z21.o		\
	  z22.o z23.o z24.o z25.o z26.o z27.o z28.o		\
	  z29.o z30.o z31.o z32.o z33.o z34.o z35.o z36.o

lout:	$(OBJS)
	$(CC) -o lout $(OBJS) -lm
	chmod a+x lout

$(OBJS): externs

externs:

install: lout
	@echo ""
	@echo "Copying Lout binary to BINDIR"
	cp lout $(BINDIR)/lout
	chmod a+x-w $(BINDIR)/lout
	@echo ""
	@echo "Creating LIBDIR and copying library files"
	mkdir $(LIBDIR)
	chmod 775 $(LIBDIR)
	mkdir $(LIBDIR)/include
	chmod 775 $(LIBDIR)/include
	mkdir $(LIBDIR)/data
	chmod 775 $(LIBDIR)/data
	mkdir $(LIBDIR)/font$(LATIN)
	chmod 775 $(LIBDIR)/font$(LATIN)
	cp include/* $(LIBDIR)/include
	chmod a+r-wx $(LIBDIR)/include/*
	cp data/* $(LIBDIR)/data
	chmod a+r-wx $(LIBDIR)/data/*
	cp font$(LATIN)/* $(LIBDIR)/font$(LATIN)
	chmod a+r-wx $(LIBDIR)/font$(LATIN)/*
	@echo ""
	@echo "Compulsory test run on doc/tr.eq (expect many non-fatal error messages)"
	./lout ./doc/tr.eq/setup ./doc/tr.eq/s? > ./doc/tr.eq/op
	rm ./doc/tr.eq/op ./doc/tr.eq/s?.ld lout.li

installman:
	sed -e "s@<BINDIR>@$(BINDIR)@" -e "s@<LIBDIR>@$(LIBDIR)@"	\
	    -e "s@<DOCDIR>@$(DOCDIR)@" -e "s@<MANDIR>@$(MANDIR)@"	\
	man/lout.1 > $(MANDIR)/lout.1
	chmod a+r $(MANDIR)/lout.1

installdoc:
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
	rm -f *.o

clobber: clean
	rm -f lout
