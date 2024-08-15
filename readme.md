---
title: readme.md
author: Norwid Behrnd, nbehrnd@yahoo.com
date: 2024-08-05 Mon
edit: 2024-08-15 Thu
---

## purpose

Following a call for help by M. B. Metcalf on
[fortran-lang.org](https://fortran-lang.discourse.group/t/reinhold-bader-1966-2024/8233/5),
a draft prepared by the late Reinhold Bader (1966-2024) intended for use in
Wikipedia [here](https://fortran-lang.discourse.group/t/reinhold-bader-1966-2024/8233/5)
was saved in the mediawiki format.  The rescue is complemented by saving two
.png illustrations and a print to pdf of the document fetched from
<https://en.wikipedia.org/wiki/User:RBaSc/draft_ftnoo>

## initial manipulations of the files initially saved

In addition to the initial fetch of the data from Wikipedia,
Pandoc assisted to convert the wikimedia source file into Pandoc
and GitHub flavored markdown, and a complementary pdf written
via Groff.  These are stored in the upstream branch of this project.

- conversion into Pandoc flavored markdown by

```shell
pandoc source.mediawiki --from mediawiki --to markdown -o pandoc_md.md
```

- conversion into GitHub flavored markdown by

```shell
pandoc source.mediawiki --from mediawiki --to gfm -o github_md.md
```

- conversion into a preliminary pdf with groff by without illustrations

```shell
pandoc pandoc_md.md -s -o groff.pdf --pdf-engine=pdfroff
```

## further work in private edit branches

Initially, the container format of the very source file was GitHub
flavored markdown instead of the original mediawiki format.  Since
Bader's draft contains crosslinks to indicate sections within the
same document as the source, Pandoc flavored markdown appears more
suitable for the workflows ahead.

At present, linting is incomplete.  So far, the conversion of the .md file
into a .pdf via restructured text (rst) and `rst2pdf` (see file `ex_rst.pdf`)
represents the content best in terms of completeness.  On the other hand, the
conversions via pdfLaTeX or groff don't squeeze the two illustrations ...

Incremental updates of the markdown file as well as pdf obtained by either
workflow based on this source file will be deposit in the main branch.
