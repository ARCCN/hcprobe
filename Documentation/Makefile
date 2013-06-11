.DEFAULT: all
.PHONY: all

all: report.pdf manual.pdf manual-rus.tex

clean:
	rm -rf *.log
	rm -rf *.aux
	rm -rf *.out
	rm -rf *.pdf

%.pdf:%.tex
	pdflatex $<
	pdflatex $<
	pdflatex $<
