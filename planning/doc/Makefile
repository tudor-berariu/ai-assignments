.PHONY: build clean view

TEX := $(wildcard ai2014*.tex)
AUX := $(patsubst %.tex,%.aux,$(TEX))
PDF := $(patsubst %.tex,%.pdf,$(TEX))

build:
	pdflatex -shell-escape $(TEX)
	pdflatex -shell-escape $(TEX)
	pdflatex -shell-escape $(TEX)

clean:
	rm -rf *.aux *.bbl *.blg *.xml *.pdf *.dvi *.nav *~ *.bbl *.log *.out *.snm *.toc _region_.tex *.vrb

view:
	evince $(PDF) &
