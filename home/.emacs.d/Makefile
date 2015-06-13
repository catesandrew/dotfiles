EMACS = emacs
EMACSFLAGS =
EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)
PKGDIR = $(EMACSBATCH) -l package --eval '(princ (expand-file-name package-user-dir))'

export EMACS

LIB_SRCS = config/init-util.el \
		   config/init-macosx.el \
		   config/init-scratch.el \
		   config/init-buffer.el \
		   config/init-window.el \
		   config/init-markdown.el \
		   config/init-simple.el \
		   config/init-smartparens.el \
		   config/init-hippie-exp.el \
		   config/init-flycheck.el \
		   config/init-lisp.el \
		   config/flycheck-auto-scalastyle.el \
		   config/flycheck-virtualenv.el \
		   config/flycheck-ensime.el \
		   config/init-compile.el \
		   config/init-firestarter.el

SRCS = init.el $(LIB_SRCS)
OBJECTS = $(SRCS:.el=.elc)

.PHONY: all
all: compile

.PHONY: clean-packages
clean-packages:
	rm -rf $(PKGDIR)

.PHONY: compile
compile : $(OBJECTS)

.PHONY: clean
clean :
	rm -f $(OBJECTS)

%.elc : %.el
	$(EMACSBATCH) -f package-initialize -f batch-byte-compile $<
