all: Display-items.rtf

clean:
	rm -rf  *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.md figure/ cache/

Display-items.rtf: Display-items.tex
	latex2rtf $^

Display-items.tex: Display-items.Rnw ../Results/A_*.rds ../Scripts/Functions/figurize.R
	Rscript -e "library(knitr)" \
	Rscript -e "knitr::knit('$<.Rnw')"

#cd Documents/DMH/DynDMH/Docs/
#pandoc Display_items.rtf -o Display_itemspandoc.docx
