include Makefile.config

#---------------------------------------
# Directories
#---------------------------------------

SRCDIR = $(shell pwd)
PREFIX = $(CAMLLIB_PREFIX)

FILES = print sette mappe hashhe \
	setteI setteS mappeI mappeS hashheI hashheIB hashheS \
	dMappe dHashhe \
	setList multiSetList \
	ilist fGraph1 fGraph sHGraph symbol union \
	statistic time rational parse

SRC = $(FILES:%=%.mli) $(FILES:%=%.ml)

INT = $(FILES:%=%.cmi)
OBJS = $(FILES:%=%.cmo)
OBJSx = $(FILES:%=%.cmx)

LIB_TOINSTALL = $(INT) $(FILES:%=%.mli) camllib.cma
LIB_TOINSTALLx = $(OBJSx) camllib.a camllib.cmxa

all: $(LIB_TOINSTALL) $(OBJSx) camllib.cmxa

camllib.cma: $(OBJS)
	$(OCAMLC) $(OCAMLFLAGS) -a $^ -o $@

camllib.cmxa: $(OBJSx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -a $^ -o $@

install:
	$(INSTALLd) $(PREFIX)/lib
	for i in $(LIB_TOINSTALL) $(LIB_TOINSTALLx); do \
		if test -f $$i; then $(INSTALL) $$i $(PREFIX)/lib; fi; \
	done

mostlyclean: clean
	/bin/rm Makefile.depend

clean:
	/bin/rm -f *.cm[ioxa] *.o *.a *.cmxa *.html *.ps *.pdf *.dvi *.out
	/bin/rm -f *.aux *.bbl *.blg *.dvi *.pdf *.log *.toc *.idx *.ilg *.ind ocamldoc*.tex ocamldoc.sty
	/bin/rm -fr html

distclean: clean
	(cd $(PREFIX)/lib; rm -f $(LIB_TOINSTALL) $(LIB_TOINSTALLx))

wc: $(SRC)
	wc $^

tar: $(SRC) Makefile Makefile.config.model camllib.tex camllib.pdf README
	@echo "*** Archiving source files in ~/camllib.tgz ***"
	(cd ..; tar zcf $(HOME)/camllib.tgz $(^:%=camllib/%))
	@echo "*** Files archived in ~/camllib.tgz ***"

# TEX rules
.PHONY: camllib.dvi camllib.pdf html depend

camllib.pdf: camllib.dvi
	$(DVIPDF) camllib.dvi camllib.pdf

camllib.dvi: $(INT) $(FILES:%=%.mli) $(FILES:%=%.ml)
	$(OCAMLDOC) -latextitle 1,chapter -latextitle 2,section -latextitle 3,subsection -latextitle 4,subsubsection -latextitle 5,paragraph -noheader -notrailer -latex -o ocamldoc.tex $(FILES:%=%.mli) $(FILES:%=%.ml)
	$(LATEX) camllib
	$(MAKEINDEX) camllib
	$(LATEX) camllib
	$(LATEX) camllib

camllibcode.dvi: $(INT) $(FILES:%=%.mli) $(FILES:%=%.ml)
	$(OCAMLDOC) -latextitle 1,chapter -latextitle 2,section -latextitle 3,subsection -latextitle 4,subsubsection -latextitle 5,paragraph -noheader -notrailer -inv-merge-ml-mli -keep-code -latex -o ocamldoc.tex $(FILES:%=%.mli)  $(FILES:%=%.ml)
	$(LATEX) camllib
	$(MAKEINDEX) camllib
	$(LATEX) camllib
	$(LATEX) camllib

html: $(INT) $(FILES:%=%.mli) $(FILES:%=%.ml)
	mkdir -p html
	$(OCAMLDOC) -html -d html -colorize-code $(FILES:%=%.ml) $(FILES:%=%.mli)

dot: $(INT) $(FILES:%=%.mli)
	$(OCAMLDOC) -dot $(FILES:%=%.mli)
	dot -Tps ocamldoc.out >ocamldoc.ps

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .dvi .tex .ps

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $(SRCDIR)/$<
%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $(SRCDIR)/$<
%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $(SRCDIR)/$<

depend: 
	$(OCAMLDEP) $(SRC) > Makefile.depend

Makefile.depend:
	$(OCAMLDEP) $(SRC) > Makefile.depend

-include Makefile.depend
