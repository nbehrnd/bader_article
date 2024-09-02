---
title: readme.md
date: 2024-08-05 Mon
edit: 2024-09-03 Tue
---

# purpose

Following a call for help by M. B. Metcalf on
[fortran-lang.org](https://fortran-lang.discourse.group/t/reinhold-bader-1966-2024/8233/5),
this project aims to preserve a draft prepared by the late Reinhold Bader
(1966-2024) about aspects of object oriented programming in Fortran.

Intended for eventual use in Wikipedia (see this page
[here](https://en.wikipedia.org/wiki/User:RBaSc/draft_ftnoo))
it is/was uncertain if Baders' work would remain accessible there, or not.  A
reader primary interested in the topics of OOP and Fortran is suggested to
consult either file
[`ex_pdflatex.pdf`](ex_pdflatex.pdf),
or the primary source file of the draft,
file
[`pandoc_md.md`](pandoc_md.md).

## technical details

The initial format of the draft (mediawiki syntax) eventually was translated
with [pandoc](https://pandoc.org/) (version 3.1) into _Pandoc_ flavored markdown
as a markup language easier to maintain and use.  Reasons for this move include

- the provision of labeled fenced code blocks as a basis for eventual syntax
  highlighting (a feature GitHub flavored markdown equally can provide)
- support of cross-links to earlier sections of the same document (apparently
  GitHub flavored markdown only supports links to external documents)

These outweigh drawbacks like the incorrect/unresolved display of internal
cross-links by GitHub because the platform presumes file extension `.md` refers
to a _GitHub_ flavored markdown file.  (Pandoc has flags to discern these
dialects for file I/O.)

The two illustrations originally fetched (png, given the dimensions they were
used, of low resolution) were redrawn with [inkscape](https://inkscape.org/)
(inheritance diagram), or
[draw.io desktop](https://github.com/jgraph/drawio-desktop/releases) (UML
diagram).  They were exported both into vector formats (.svg, .pdf) and bitmap
(.png with at least 300 dpi given the dimensions of anticipated usage).  Beside
an increase of readability in the pdf to compile, this provided an easier edit
and correction at the source, if needed.

While aiming for a pdf as eventual output (cf. _vide infra_), the (re)build of
the document was monitored by a conversion of the markdown file with Pandoc into
html (default action) or a pdf intentionally without illustrations (with groff).
Like other actions of this project likely to be used multiple times, a
[Makefile](Makefile)
was used to manage these.

Pandoc offers multiple `--pdf-engines` for a _direct_ compilation of a pdf.  In
addition, Pandoc can convert e.g., a markdown file into an other markup language
which itself can be used to compile a pdf using a different engine of its
ecosystem.  Thus, beside the preservation of Bader's draft, a second interest of
the project's owner was to compare a few possible workflows.  This may be
attractive because markdown (used as the container format to edit the draft) is
a lighter markup language than LaTeX for instance to define emphases, or tables
while the visual appearance of the eventual document can be influenced by Pandoc
style templates.

- The workflow via pdfLaTeX eventually uses an edited version of the
  [eisvogel](https://github.com/Wandmalfarbe/pandoc-latex-template) template.
  For greater portability of the project between two operating systems (Windows
  and Linux Debian), the choice of the template's author for
  [`sourcesanspro`](https://ctan.org/pkg/sourcesanspro) was substituted
  by a set of font families arguably more frequently seen
  ([`libertine`](https://ctan.org/pkg/libertine),
  [`libertinust1math`](https://ctan.org/pkg/libertinust1math),
  and
  [`beramono`](https://ctan.org/pkg/bera)) to support the display of text,
  mathematics, and source code (cf.
  [LateX Font Catalogue](https://tug.org/FontCatalogue/mathfonts.html)).
  On a pristine user account within the Windows operating system, this still
  required a considerable amount of permanent memory to resolve the dependencies
  (i.e., many usepackages, including their dependencies) before the pdf intended
  was compiled smoothly.

  Out of the three workflows tested, its result (see file
  [`ex_pdflatex.pdf`](ex_pdflatex.pdf))
  is the most recommended in terms of reliable coverage of the contents and
  visual appearance of the pdf written.

  Other `--pdf-engines` related to TeX (lualatex, xelatex, etc) Pandoc offers to
  use were not tested.  At present, the project's owner can not access to them,
  and does not (yet) have sufficient user experience using them.
- To test the (intermediate) format of
  [restructured text](https://en.wikipedia.org/wiki/ReStructuredText)
  (.rst) was appealing because of its frequent use to document Python projects
  ([Sphinx](https://en.wikipedia.org/wiki/Sphinx_(documentation_generator))).
  Compared to this, [rst2pdf](https://rst2pdf.org/) which is a tool with lesser
  footprint.  Here, mathematical equations are rendered with Python's
  [`mathplotlib`](https://matplotlib.org/)
  as bitmaps which eventually are embedded into the document.  The presence of
  the later thus can facilitate the additional setup to use this approach.

  Compared to the previous workflow with pdfLaTeX, the compilation yielded a
  pdf about twice as large (1.5MB, see file
  [`ex_rst.pdf`](ex_rst.pdf)).
  In one case, while approaching the end of a page, this tool didn't relay the
  illustration across the page break to the next page.  Instead, the picture was
  squeezed into the remainder of the page.  So far, I did not identify a method
  to assist rst2pdf to resolve this "floating problem".
- Tools around (g)roff typically already are on board of a Linux distribution
  (management of man pages, etc), or easily installed if missing.  Compared to
  the workflow with either pdfLaTeX, or rst2pdf & mathplotlib, their footprint
  for permanent and working memory usually is smaller, the pdf is compiled
  faster.  In the present project, the Pandoc written intermediate .ms file
  required a couple of adjustments ahead of the eventual compilation of the pdf
  (cf. the corresponding section in the
  [`Makefile`](Makefile)).
  One of the mathematical characters (mapsto) was too special and had to be
  constructed manually.  Perhaps the setup of of a non-default font to groff
  (see e.g., Peter Schaffter's
  [documentation](https://www.schaffter.ca/mom/momdoc/appendices.html#fonts))
  would remove this hurdle.

  It equally is noteworthy that groff's pre-processor `pic` requires the
  illustrations either in the .eps format (preferred, embedded into the .ms file
  as `PSPIC`), or .pdf format (acceptable as "unsafe" option, embedded into the
  .ms file as `.PDFPIC`).  This wasn't problematic because the illustrations
  were available as vector graphics; the update of the .ms file equally can be
  provided with `sed` launched from the
  [`Makefile`](Makefile).
  (One could embed png into a pdf container with e.g., Imagemagick's
  [convert](https://imagemagick.org/script/convert.php),
  though on expense loosing advantages of an illustration provided in a vector
  format.)  The result (see file
  [`ex_groff.pdf`](ex_groff.pdf))
  was the smallest of all three workflows tested.

## summary

The table below compares the file sizes of the pdf generated by either one of
the three workflows tested (column "original") and eventually stored in the
project archive.
Often, file sizes of pdf can be reduced further by an additional "print to pdf".
As an example, the file sizes of a run like `pdf_rewrite -r input.pdf` with
[pdf_rewriter](https://github.com/nbehrnd/pdf_rewriter) to retain the colors of
the illustrations are compiled in column "rewritten".
Columns "good" and "bad" are an opinionated selection of advantages /
disadvantages of either workflow.

| workflow | original | rewritten | good | bad |
| :--- | ----: | ----: | :-------- | :-------- |
| pdfLaTeX | 795 kB | 412 kB | character set covers the needs, reliable management of illustrations (either png, or pdf) | large footprint to install |
| rst2pdf  | 1.5 MB | 1.0 MB | relates to the Python ecosystem | slow processing, results large pdf |
| groff | 680 kB | 188 kB | light footprint, fast processing | less symbol coverage than LaTeX, illustrations need to be in a pdf/eps container |

## update summary

- 2024-09-03 Tue: a comment by Brad Richardson regarding interfaces
  considered as sufficiently different (section 18.3) is added.
