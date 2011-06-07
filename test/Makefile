#=======================================================================
# Makefile for files in /local/build/Par152/test/
#
# [17-Apr-2003]
#=======================================================================
# Program definitions

PROGRAM		= CHANGE-THIS-VALUE
PROGLIB		= CHANGE-THIS-VALUE
VERSION		= 1.0

#=======================================================================
# Installation locations

prefix		= /usr/local
bindir		= $(prefix)/bin
catdir		= $(prefix)/man/cat$(manext)
libdir		= $(libtop)/$(PROGRAM)-$(VERSION)
libtop		= $(prefix)/$(PROGLIB)/
mandir		= $(prefix)/man/man$(manext)
manext		= 1
shrlibdir	= $(shrlibtop)/$(PROGRAM)-$(VERSION)
shrlibtop	= $(prefix)/share/$(PROGLIB)/

#=======================================================================
# Macro definitions

CAT		= cat

CHMOD		= chmod

CMP		= cmp

CP		= /bin/cp -p
CP		= rcp -p

DETEX		= detex

DETEXFLAGS	= -m -n -s

DW		= dw

ISPELL		= ispell

LIBFILES	= /dev/null

LN		= ln

LS		= ls

MKDIR		= mkdir -p

MV		= /bin/mv

RM		= /bin/rm -f

PAR		= ../par	

SED		= sed

SHELL		= /bin/sh

SHRLIBFILES	= /dev/null

SORT		= sort

SPELL		= spell

SPELLFILTER	= $(CAT)
SPELLFILTER	= $(SED) -e 's/[0-9,.:;?&]/ /g'

TESTFILES	= test??.in

#=======================================================================
# Compiler flags and source files

#=======================================================================
# Suffixes and extra rule(s)

#=======================================================================
# Targets:

all:


check:
	@echo ; \
	NFAIL=0 ; \
	NPASS=0 ; \
	NTEST=0 ; \
	for f in $(TESTFILES) ; \
	do \
		NTEST=`expr $$NTEST + 1` ; \
		g=`basename $$f .in` ; \
		$(PAR) `cat $$g.opt` < $$f > $$g.out ; \
		if $(CMP) -s okay/$$g.out $$g.out ; \
		then \
			echo PASS: $$g ; \
			NPASS=`expr $$NPASS + 1` ; \
			$(RM) $$g.out ; \
		else \
			echo FAIL: $$g ; \
			NFAIL=`expr $$NFAIL + 1` ; \
		fi ; \
	done ; \
	echo ; \
	if test $$NTEST -eq $$NPASS ; \
	then \
		echo Success: all $$NTEST tests passed ; \
	else \
		echo Failure: $$NFAIL of $$NTEST tests failed ; \
	fi ; \
	echo

clean: clean-test
	-$(RM) *~
	-$(RM) \#*
	-$(RM) a.out
	-$(RM) core

clean-test:
	-$(RM) *.out

clobber:	distclean

distclean:	mostlyclean

install:	install-exe install-lib install-shrlib install-man install-show

install-exe:	uninstall-exe
	$(CP) $(PROGRAM) $(bindir)/$(PROGRAM)
	$(LN) $(bindir)/$(PROGRAM) $(bindir)/$(PROGRAM)-$(VERSION)
	$(CHMOD) 775 $(bindir)/$(PROGRAM) $(bindir)/$(PROGRAM)-$(VERSION)

install-lib:	uninstall-lib
##	$(MKDIR) $(libtop)
##	@if test -d $(libtop) ; then true ; else echo Cannot create $(libtop) ; exit 1 ; fi
##	$(MKDIR) $(libdir)
##	@if test -d $(libdir) ; then true ; else echo Cannot create $(libdir) ; exit 1 ; fi
##	$(CP) $(LIBFILES) $(libdir)/
##	$(CHMOD) 664 $(libdir)/*

install-man:	uninstall-man
	$(CP) $(PROGRAM).man $(mandir)/$(PROGRAM).$(manext)
	$(CHMOD) 664 $(mandir)/$(PROGRAM).$(manext)

install-show:
	@echo ''
	@echo Installed files...
	@$(LS) -l $(bindir)/$(PROGRAM) $(bindir)/$(PROGRAM)-$(VERSION) \
		$(mandir)/$(PROGRAM).$(manext)
	@if test -d $(libdir) ; then $(LS) -lR $(libdir) ; fi
	@if test -d $(shrlibdir) ; then $(LS) -lR $(shrlibdir) ; fi
	@echo ''

install-shrlib:	uninstall-shrlib
##	$(MKDIR) $(shrlibtop)
##	@if test -d $(shrlibtop) ; then true ; else echo Cannot create $(shrlibtop) ; exit 1 ; fi
##	$(MKDIR) $(shrlibdir)
##	@if test -d $(shrlibdir) ; then true ; else echo Cannot create $(shrlibdir) ; exit 1 ; fi
##	$(CP) $(SHRLIBFILES) $(shrlibdir)/
##	$(CHMOD) 664 $(shrlibdir)/*

maintainer-clean:	distclean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."

mostlyclean:	clean

uninstall:	uninstall-exe uninstall-lib uninstall-shrlib uninstall-man

uninstall-exe:
	-$(RM) $(bindir)/$(PROGRAM)
	-$(RM) $(bindir)/$(PROGRAM)-$(VERSION)

uninstall-lib:
##	-$(RM) -r $(libdir)

uninstall-man:
	-$(RM) $(mandir)/$(PROGRAM).$(manext)
	-$(RM) $(catdir)/$(PROGRAM).$(manext)

uninstall-shrlib:
##	-$(RM) -r $(shrlibdir)

#=======================================================================
# File dependencies

#=======================================================================
