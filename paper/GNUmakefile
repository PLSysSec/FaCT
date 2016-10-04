MAIN = main.ltx
PDF = pdf

ifeq ($(PDF),pdf)
FIGEXT    = pdf
OUTEXT    = pdf
else
FIGEXT    = eps
OUTEXT    = dvi
endif

BASE      := $(basename $(MAIN))
SRCEXT    := $(patsubst $(BASE).%,%,$(MAIN))
XFIGS     := $(wildcard *.fig)
DIAFIGS   := $(wildcard *.dia)
SVGFIGS   := $(wildcard *.svg)
TEXFIGS   := $(XFIGS:.fig=.tex) # $(DIAFIGS:.dia=.tex)
BINFIGS   := $(XFIGS:.fig=.$(FIGEXT)) $(DIAFIGS:.dia=.$(FIGEXT)) $(SVGFIGS:.svg=.$(FIGEXT)) $(GENFIGS)
STYLINK	  := $(notdir $(wildcard ../*.sty))
STYFILES  := $(sort $(STYLINK) $(wildcard *.sty))
TEXFILES  := $(sort $(MAIN) $(wildcard *.tex) $(STYFILES) $(TEXFIGS))
BIBFILES  := $(wildcard *.bib)

JGRAPHS   := $(wildcard *.j)
GGRAPHS   := $(wildcard *.gnuplot)
GRAPHS    := $(JGRAPHS.j=.$(FIGEXT)) $(GGRAPHS:.gnuplot=.$(FIGEXT))

ifeq ($(PDF),pdf)
IMAGESRC  := $(wildcard figs/*.gif) $(wildcard figs/*.eps) 
else
IMAGESRC  := $(wildcard figs/*.gif) 
endif
IMAGEGEN  := $(addsuffix .$(FIGEXT), $(basename $(IMAGESRC)))
IMAGES    := $(sort $(IMAGEGEN) $(wildcard figs/*.$(FIGEXT)))
IMAGEDIST := $(filter-out $(IMAGEGEN), $(IMAGES)) \
		$(wildcard figs/*.odp) $(wildcard figs/*.ppt)

ifeq ($(PDF),pdf)
LATEX     = pdflatex -synctex=1 -shell-escape
#\nonstopmode\\input
else
LATEX     = latex
#\\nonstopmode\\input
DVIPS     = dvips -j0 -G0 -t letter -Pdownload35
PS2PDF    = GS_OPTIONS=-sPAPERSIZE=letter ps2pdf -sPAPERSIZE=letter -dEmbedAllFonts=true
#PS2PDF    = ps2pdf
endif
BIBTEX    = bibtex -min-crossrefs=1000

all: $(BASE).pdf

crop: $(BASE).pdf
	pdfcrop --margins 15 $(BASE).pdf

ifeq ($(PDF),pdf)
%.pdf %.tex: %.fig
	fig2dev -L pdftex -p1 $< > $*.pdf
	fig2dev -L pdftex_t -p $*.pdf $< > $*.tex

%.pdf: %.eps
	perl -ne 'exit 1 unless /\n/' $< \
		|| perl -p -i -e 's/\r/\n/g' $<
	epstopdf --outfile=$@ $<

figs/%.pdf: figs/%.gif
	convert $< $@

%.ps: %.pdf
	pdftops -paper A4 $< $@

else
%.ps: %.dvi
	$(DVIPS) $< -o $@

%.eps %.tex: %.fig
	fig2dev -L pstex -p1 $< > $*.eps
	fig2dev -L pstex_t -p $*.eps $< > $*.tex
endif

%.eps: %.dia
	unset DISPLAY; dia -n -e $@ $<
%.tex: %.dia
	unset DISPLAY; dia -n -e $@ $<
%.eps: %.j
	@rm -f $@~
	jgraph $< > $@~
	mv -f $@~ $@
%.eps: %.gnuplot %.data
	gnuplot $*.gnuplot
%.pdf: %.svg
	inkscape -z -A $@ $< && cp $@ .missing/$@ || cp .missing/$@ $@

$(STYLINK):
	rm -f $@
	ln -s ../$@ .

RERUN = egrep -q '(^LaTeX Warning:|\(natbib\)).* Rerun' $*.log
UNDEFINED = egrep -q '^(LaTeX|Package natbib) Warning:.* undefined' $*.log

# So you can "setenv ALWAYS yes" instead of butchering the Makefile...
ifneq ($(ALWAYS),)
ALWAYS = always
endif
$(BASE).$(OUTEXT): %.$(OUTEXT): %.$(SRCEXT) $(TEXFILES) $(BIBFILES) \
				$(BINFIGS) $(GRAPHS) $(IMAGES) $(ALWAYS)
	test ! -s $*.aux || $(BIBTEX) $* || rm -f $*.aux $*.bbl
	$(LATEX) $< || ! rm -f $@
	if $(UNDEFINED); then \
		$(BIBTEX) $* && $(LATEX) $< || ! rm -f $*.bbl $@; \
	fi
	! $(RERUN) || $(LATEX) $< || ! rm -f $@
	! $(RERUN) || $(LATEX) $< || ! rm -f $@
ifeq ($(PDF),pdf)
	touch $*.dvi
	test ! -f .xpdf-running || xpdf -remote $(BASE)-server -reload
else
$(BASE).pdf: %.pdf: %.ps
	$(PS2PDF) $< $@
	test ! -f .xpdf-running || xpdf -remote $(BASE)-server -reload
endif

DIST = GNUmakefile $(filter-out $(TEXFIGS), $(TEXFILES)) \
	$(XFIGS) $(DIAFIGS) $(SVGFIGS) $(GENFIGSRC) \
	$(JGRAPHS) $(GGRAPHS) $(GGRAPHS:.gnuplot=:.data) \
	$(IMAGESRC) $(IMAGEDIST) $(BIBFILES)
dist: $(BASE).tar.gz
$(BASE).tar.gz: $(DIST)
	tar -chzf $@ -C .. \
		$(patsubst %, $(notdir $(PWD))/%, $(DIST))

PREVIEW := $(BASE)
preview: $(PREVIEW).pdf
	if test -f .xpdf-running; then			\
		xpdf -remote $(BASE)-server -quit;	\
		sleep 1;				\
	fi
	touch .xpdf-running
	(xpdf -remote $(BASE)-server $(PREVIEW).pdf; rm -f .xpdf-running) &

show: $(BASE).pdf
	xpdf -fullscreen $(BASE).pdf

osx: $(BASE).pdf
	open $(BASE).pdf

check:
	pdflatex $(BASE).ltx | grep -e "multiply" -e "undefined"

EXTRAIGNORE = '*~' '*.aux' '*.bbl' '*.blg' '*.log' '*.dvi' '*.bak' '*.out'
ignore:
	rm -f .gitignore
	(for file in $(EXTRAIGNORE); do \
		echo "$$file"; \
	done; \
	for file in .xpdf-running \
			$(BASE).tar.gz \
			$(TEXFIGS) $(BINFIGS) $(GRAPHS) $(IMAGEGEN) \
			$(BASE).ps $(BASE).pdf; do \
		echo "/$$file"; \
	done) | sort > .gitignore
	git add .gitignore

clean:
	rm -f $(BASE).ps $(BASE).pdf $(BASE).tar.gz
	rm -f $(TEXFIGS) $(GRAPHS) $(IMAGEGEN)
	rm -f *.dvi *.aux *.log *.bbl *.blg *.lof *.lot *.toc *.bak *.out
	rm -f *~ *.core core

binfigsclean: clean
	rm -f $(BINFIGS)

always:
	@:

.PHONY: install clean binfigsclean all always ignore preview show osx dist

spell:
	make clean
	for i in $(wildcard *.tex); do aspell -p ./aspell.words -c $$i; done

flat:
	./flatten.pl $(MAIN) > $(BASE)-flat.tex

proof:
	./weasel *.tex
	./passive *.tex
