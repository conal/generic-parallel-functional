targ = gpf

# .PRECIOUS: %.tex

default: $(targ).pdf

%.pdf: %.tex Makefile
	latexmk -pdf $*.tex

# Was: pdflatex $*.tex

%.tex: %.lhs macros.tex mine.fmt pdfs Makefile
	lhs2TeX -o $*.tex $*.lhs

showpdf=skim

dots = $(wildcard figures/*.dot)
pdfs = $(addsuffix .pdf, $(basename $(dots)))

# Cap the size so that LaTeX doesn't choke.
%.pdf: %.dot # Makefile
	dot -Tpdf -Gmargin=0 -Gsize=10,10 $< -o $@

pdfs: $(pdfs)

see: $(targ).pdf
	${showpdf} $(targ).pdf

SHELL = bash

clean:
	rm -f $(short).{tex,dvi,pdf,aux,bbl,blg,out,log,ptb}

web-paper: web-paper-token

web-paper-token: $(short).pdf $(long).pdf
	scp $(targ).pdf conal@conal.net:/home/conal/domains/conal/htdocs/papers
	touch web-paper-token
