.PHONY: all slides flowchart compiling-flowchart converting-flowchart \
        create-slides clean-strobe-files clean-pass

all: flowchart slides

## ----- STROBE flowchart -----
flowchart: converting-flowchart

compiling-flowchart:
	@echo "Compiling STROBE flowchart..."
	cd checklists && latexmk -pdf strobe_flowchart.tex

converting-flowchart: compiling-flowchart
	@echo "Converting PDF to PNG..."
	cd checklists && pdftoppm -r 300 -png strobe_flowchart.pdf strobe_flowchart
	cd checklists && mv strobe_flowchart-1.png strobe_flowchart.png
	cp checklists/strobe_flowchart.png figs/strobe_flowchart.png

clean-strobe-files:
	rm -f checklists/*.aux checklists/*.log checklists/*.pdf checklists/*.png

## ----- SSM_Epi slides -----
slides: create-slides

create-slides:
	@echo "Creating slides..."
	cd slides && latexmk -pdf presentation.tex
	@echo "Slides created: slides/presentation.pdf"

clean-slide-files:
	@echo "Cleaning up slides auxiliary files..."
	cd slides && rm -f *.aux *.log *.out *.toc *.nav *.snm *.bcf *.bbl *run.xml *.blg *.fdb_latexmk *.fls
	@echo "Slides auxiliary files cleaned up."
