targ = generic-parallel-functional

.PRECIOUS: %.tex

default: $(targ).pdf

#latex=pdflatex
latex=latexmk -pdf

%.pdf: %.tex bib.bib $(figures) Makefile acmart-tweaked.cls
	$(latex) $*.tex

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

web: web-token

web-token: $(targ).pdf
	scp $(targ).pdf conal@conal.net:/home/conal/domains/conal/htdocs/papers/$(targ)/
	touch web-token
