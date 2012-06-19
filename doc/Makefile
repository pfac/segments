PDFTEX	=	pdflatex -halt-on-error
PDFOPN	=	open -a /Applications/Preview.app
BIBTEX	=	bibtex

COMPILE.tex	=	$(PDFTEX) $(*F).tex > /dev/null

COMPILE.bib =	-$(BIBTEX) $(*F) > /dev/null

DIRS	=	. report
DOCS	=	report

%.aux: %.tex
	$(COMPILE.tex)

%.bbl: %.aux
	$(COMPILE.bib)
	$(COMPILE.tex)

%.pdf: %.bbl
	$(COMPILE.tex)

%: %.pdf
	$(PDFOPN) $<

.PHONY: clean clean-pdf clean-bib view check

default:	report.pdf

all: $(DOCS)

check:
	@for d in $(DIRS);
	@do
		@for f in `ls "$d" | grep "tex"`;
		@do
			@hunspell "$f";
		@done;
	@done;

view: all
	open $(DOCS)	

clean:	clean-pdf clean-bib

clean-pdf:
	$(RM) *.{aux,out,log,synctex.gz,pdf}

clean-bib:
	$(RM) *.{bbl,blg}
