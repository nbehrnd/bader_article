---
title: readme.md
author: Norwid Behrnd, nbehrnd@yahoo.com
date: 2024-08-05 Mon
edit: 2024-08-07 Wed
---

## purpose

Following a call for help by M. B. Metcalf on
[fortran-lang.org](https://fortran-lang.discourse.group/t/reinhold-bader-1966-2024/8233/5),
a draft prepared by the late Reinhold Bader (1966-2024) intended for use in
Wikipedia [here](https://fortran-lang.discourse.group/t/reinhold-bader-1966-2024/8233/5)
was saved in the mediawiki format.  The rescue is complemented by saving two
.png illustrations and a print to pdf of the document fetched from
<https://en.wikipedia.org/wiki/User:RBaSc/draft_ftnoo>

## manipulations of the files initially saved

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

## further work (branch `edit`)

A couple of edits are necessary to tidy up the GitHub flavored Markdown file
(`github_md.md`) which from now on serves as default container of content.
Future exports to other file formats (e.g., pdf) will start from there.
