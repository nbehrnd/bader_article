#!/usr/bin/make

# visual assistant for the edits on the GitHub flavored Markdown file

# default rendering of .html (local update by F5), can include png
phony:
	pandoc pandoc_md.md --from markdown -s -o test.html --mathml

# optionally write a pdf with groff (for being faster than pdfLaTeX
# though for obvious reasons without the illustrations)
p:
	pandoc pandoc_md.md --from markdown -s -o test.pdf --pdf-engine pdfroff
	zathura test.pdf &

# recipes for visually more appealing layout

# pdf via bypass rst
# the display of the mathematical equations relies on Python's `matplotlib`
rst:
	pandoc pandoc_md.md  -s -o ex_rst.rst --number-sections --toc
	rst2pdf -s serif ex_rst.rst --footer=###Page### --smart-quotes=1 \
		--disable-splittables --repeat-table-rows --date-invariant
	rm ex_rst.rst

# export to tex
# requires subsequent manual edit and compilation to yield a pdf
tex:
	pandoc pandoc_md.md --from markdown -s -o ex_pdflatex.tex \
		--number-sections --toc \
		--template ./eisvogel.tex --highlight-style tango

# manually replace `article` of letter format by `scrartcl` for ISO A4 format
# additional edits:
#%\usepackage{lmodern}
#\usepackage{libertine, libertinust1math}
#\usepackage[scaled=0.8]{beramono}
#\usepackage{microtype}
#\usepackage[USenglish]{babel}
#\usepackage{color,xcolor}
#\usepackage[colorlinks=true,linkcolor=true,allcolors=blue]{hyperref}
#
# Pandoc's default setup of hypersetup includes a line `hidelinks`; to request
# sed the substitution allows for the less intrusive blue links to appear.

# export to groff ms
g:
	pandoc pandoc_md.md --from markdown -s -o ex_groff.ms

	# manually continue with this sequence
	#
	# 1. ensure illustrations are present e.g., as a .pdf (else default to .eps)
	#    or run with imagemagick a brief `convert input.png output.pdf`
	# 2. in the .ms, search a comment line like `\" .IMAGE "illustration.png"`
	#    to replace it with the picture environment enclosed by `.PS` and `.PE`
	# 3. include a line like `.PDFPIC -C illustration.pdf` for the centered
	#    inclusion of the pdf picture
	# 4. don't forget to pre-process the .ms with `pic` for the presence of 
	#    illustrations, or/and eqn for mathematics, or/and tbl for tables
	# 5. run `pic ex_groff.ms | groff -e -t -ms -Tpdf -U ex_groff.ms > ex_groff.pdf
	#    So far, the shorter `groff -pet -ms -Tpdf -U ex_groff.ms > ex_groff.pdf`
	#    is not as good as the former approach.
	#    Flag `-U` is required for the unsafe mode because of the inclusion of an
	#    illustration as pdf.  If one uses `PSPIC` in the .ms file instead to add
	#    an .eps instead, `-U` isn't necessary.
	# 6. A `-P-pa4` instructs the post processor the use ISO A4 format instead of
	#    default letter paper in step 5.  Perhaps useful for future work.

# export to pdf with pdfLaTeX managed by pandoc and a style file eisvogel
# found on https://github.com/topics/pandoc-template. 
pdflatex:
	pandoc pandoc_md.md --from markdown -s -o ex_pdflatex.pdf \
		--number-sections --toc \
		--template ./eisvogel.tex --highlight-style tango \
		-V listings-no-page-break
# END
