#!/usr/bin/make

# visual assistant for the edits on the GitHub flavored Markdown file

# default rendering of .html (local update by F5), can include png
phony:
	pandoc github_md.md --from gfm -s -o test.html --mathml

# optionally write a pdf with groff (for being faster than pdfLaTeX
# though for obvious reasons without the illustrations)
p:
	pandoc github_md.md --from gfm -s -o test.pdf --pdf-engine pdfroff 
	zathura test.pdf &
