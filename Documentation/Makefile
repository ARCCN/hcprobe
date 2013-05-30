.DEFAULT: all
.PHONY: all

all: report.pdf manual.pdf

clean:
	rm -rf *.log
	rm -rf *.aux
	rm -rf *.out
	rm -rf *.pdf

%.pdf:%.tex
	pdflatex $<
	pdflatex $<
	pdflatex $<
