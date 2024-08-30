#!/usr/bin/make

# visual assistant for the edits on the GitHub flavored Markdown file

# default rendering of .html (local update by F5), can include png
phony:
	pandoc pandoc_md.md --from markdown -s -o test.html --mathml \
		--number-sections --toc \
		--highlight-style tango \
		-c ./html_style.css  # optional change of the layout (wider lines, etc)

# optionally write a pdf with groff (for being faster than pdfLaTeX
# though for obvious reasons without the illustrations, intentionally
# no table of contents)
p:
	pandoc pandoc_md.md --from markdown -s -o test.pdf --pdf-engine pdfroff \
		--number-sections \
		--highlight-style tango
	zathura test.pdf &

# recipes for visually more appealing layout

# pdf via bypass rst
# the display of the mathematical equations relies on Python's `matplotlib`
rst:
	pandoc pandoc_md.md  -s -o ex_rst.rst \
		--number-sections --toc \
		--columns 100  # better than either 80 character default, or a --wrap=none

	# corrections to the rst file eventually used:
	# rst2pdf is unaware of LaTeX amsmath \text{} command: (but rst2pdf does)
	sed -i "s/\\\text{/\\\mathrm{/" ex_rst.rst
	# back links in the same document don't work yet in this approach
	sed -i "s/.. _\`sec:\S*//" ex_rst.rst  # root of back links below a section
	sed -i "s/<#sec:\S*__/A/" ex_rst.rst  # use of the links in the text

	rst2pdf -s serif,tango ex_rst.rst --footer=###Page### --smart-quotes=1 \
		--disable-splittables --repeat-table-rows --date-invariant \
		--default-dpi 300
	rm ex_rst.rst

# export to tex
# requires subsequent manual edit and compilation to yield a pdf
tex:
	pandoc pandoc_md.md --from markdown -s -o ex_pdflatex.tex \
		--number-sections --toc \
		--template ./eisvogel.tex --highlight-style tango

# export to groff ms
g:
	pandoc pandoc_md.md --from markdown -s -o ex_groff.ms \
		--number-sections --toc \
		--template=./groff_template.ms --highlight-style tango

	# corrections, insertions of the illustrations
	sed -i 's/\\" .IMAGE "/.PDFPIC /' ex_groff.ms
	sed -i "s/\.png/.pdf/" ex_groff.ms

	# apparently, explicit image scales in in currently aren't relayed by Pandoc 3.1
	sed -i 's/Inheritance_diagram.pdf" "216p"/Inheritance_diagram.pdf 3i/' ex_groff.ms
	sed -i 's/Dependency_inversion.pdf" "360p"/Dependency_inversion.pdf 4.8i/' ex_groff.ms

	# correction of figure caption declarations
	sed -i "s/.ce 1/.QP/" ex_groff.ms
	sed -i "s/.sp 1//" ex_groff.ms

	# correction of table dimensions
	sed -i "s/lw(20.0n) lw(50.0n)./lw(25n) lw(64.5n)./" ex_groff.ms

	# manual definition of mapsto as \[*mp] glyph
	sed -i "s/^\.1C/.1C\n.if t .ds mp \\\fR|\\\\\\\\h’-0.4m’\\\\\\\[->]\\\fP/" ex_groff.ms
	sed -i "s/\\[u21A6]/*[mp]/g" ex_groff.ms

	groff -e -t -t -ms -Tpdf -U -P-pa4 ex_groff.ms > ex_groff.pdf

# export to pdf with pdfLaTeX managed by pandoc and a style file eisvogel
# found on https://github.com/topics/pandoc-template. 
pdflatex:
	pandoc pandoc_md.md --from markdown -s -o ex_pdflatex.pdf \
		--number-sections --toc \
		--template ./eisvogel.tex --highlight-style tango
# END
