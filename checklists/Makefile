.PHONY: all clean

all: strobe_flowchart.png

strobe_flowchart.pdf: strobe_flowchart.tex
	pdflatex -interaction=nonstopmode strobe_flowchart.tex

strobe_flowchart.png: strobe_flowchart.pdf
	pdftoppm -r 300 -png strobe_flowchart.pdf strobe_flowchart
	mv strobe_flowchart-1.png strobe_flowchart.png
	cp strobe_flowchart.png ../figs/strobe_flowchart.png

clean:
	rm -f *.aux *.log *.pdf *.png
