---
title: readme.md
author: Norwid Behrnd, nbehrnd@yahoo.com
date: 2024-08-05 Mon
edit:
---

** purpose

Following a call for help by M. B. Metcalf on
[fortran-lang.org](https://fortran-lang.discourse.group/t/reinhold-bader-1966-2024/8233/5),
a draft prepared by Reinhold Bader intended for use in
Wikipedia [here](https://fortran-lang.discourse.group/t/reinhold-bader-1966-2024/8233/5)
was saved in the mediawiki format.  The rescue is complemented by saving two
.png illustrations and a print to pdf.

** manipulations of the files saved

- conversion into Pandoc flavored markdown by

```shell
pandoc source.mediawiki --from mediawiki --to markdown -o pandoc_md.md
```

- conversion into GitHub flavored markdown by

```shell
pandoc source.mediawiki --from mediawiki --to gfm -o github_md.md
```

- convesion into a preliminary pdf with groff by without illustrations

```shell
pandoc pandoc_md.md -s -o groff.pdf --pdf-engine=pdfroff
```
