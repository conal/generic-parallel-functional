targ = gpf

.PRECIOUS: %.tex

default: $(targ).pdf

%.pdf: %.tex Makefile
	pdflatex $*.tex

# --poly is default for lhs2TeX

# $(short).lhs includes tcm.lhs, so use this modified rule

%.tex: %.lhs macros.tex mine.fmt Makefile
	lhs2TeX -o $*.tex $*.lhs

showpdf=skim

see: $(targ).pdf
	${showpdf} $(targ).pdf

bib-short: $(targ).tex bib.bib
	bibtex   $(targ)
	pdflatex $(targ)
	pdflatex $(targ)

SHELL = bash

clean:
	rm -f $(short).{tex,dvi,pdf,aux,bbl,blg,out,log,ptb}

web-paper: web-paper-token

web-paper-token: $(short).pdf $(long).pdf
	scp $(targ).pdf conal@conal.net:/home/conal/domains/conal/htdocs/papers
	touch web-paper-token