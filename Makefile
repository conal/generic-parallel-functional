targ = generic-parallel-functional

# .PRECIOUS: %.tex

default: $(targ).pdf

$(targ).pdf: $(targ).tex Makefile
	latexmk -pdf $(targ).tex

# %.pdf: %.tex Makefile
# 	latexmk -pdf $*.tex

# Was: pdflatex $*.tex

%.tex: %.lhs macros.tex formatting.fmt $(pdfs) Makefile
	lhs2TeX -o $*.tex $*.lhs

dots = $(wildcard figures/*.dot)

figures = $(addsuffix .pdf, $(basename $(dots)))

# Cap the size so that LaTeX doesn't choke.
%.pdf: %.dot # Makefile
	dot -Tpdf -Gmargin=0 -Gsize=10,10 $< -o $@

showpdf=skim

see: $(targ).pdf
	${showpdf} $(targ).pdf

SHELL = bash

clean:
	rm -f $(targ).{tex,dvi,pdf,aux,bbl,blg,out,log,ptb}

web-paper: web-paper-token

web-paper-token: $(short).pdf $(long).pdf
	scp $(targ).pdf conal@conal.net:/home/conal/domains/conal/htdocs/papers
	touch web-paper-token
