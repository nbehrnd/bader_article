---
title: readme.md
author: Norwid Behrnd, nbehrnd@yahoo.com
date: 2024-08-05 Mon
edit: 2024-08-21 Wed
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

In terms of the content, the workflow via pdfLaTeX is considered
complete (Wednesday, 2024-08-21).  Updates using the other two paths
(groff, and rst2pdf after an intermediate conversion into restructured
text [rst]) may be provided incrementally.
