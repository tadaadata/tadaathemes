
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tadaathemes

<!-- badges: start -->

[![R build
status](https://img.shields.io/github/workflow/status/tadaadata/tadaathemes/R-CMD-check?label=R-CMD-check&logo=github)](https://github.com/tadaadata/tadaathemes/actions)
[![Travis build
status](https://img.shields.io/travis/com/tadaadata/tadaathemes?logo=travis)](https://travis-ci.com/tadaadata/tadaathemes)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tadaathemes)](https://CRAN.R-project.org/package=tadaathemes)
<!-- badges: end -->

The goal of tadaathemes is to provide a bunch of reasonably neat ggplot2
themes.

## Installation

~~You can install the released version of tadaathemes from
[CRAN](https://CRAN.R-project.org) with~~:

``` r
# install.packages("tadaathemes")
```

## Examples

Include a (compact?) gallery of themes (or maybe move them to a
dedicated pkgdown article)

``` r
# library(tadaathemes)
```

## Additional Resources

  - [r-graph-gallery.com](https://www.r-graph-gallery.com/): For
    inspiration.
  - [calligross/ggthemeassist](https://github.com/calligross/ggthemeassist):
    RStudio addin for interactive theme tweaking.
  - [metrumresearchgroup/ggedit](https://github.com/metrumresearchgroup/ggedit):
    Another RStudio addin
  - [wilkelab/cowplot](https://github.com/wilkelab/cowplot): Just
    generally useful.
  - [thomasp85/patchwork](https://github.com/thomasp85/patchwork): For
    plot layouts.

### Themes

  - [hrbrmstr/hrbrthemes](https://github.com/hrbrmstr/hrbrthemes): For
    the *good stuff*.
  - [hrbrmstr/ggexpanse](https://github.com/hrbrmstr/ggexpanse):
    Everyone needs more The Expanse.
  - [cttobin/ggthemr](https://github.com/cttobin/ggthemr):
  - [jrnold/ggthemes](https://github.com/jrnold/ggthemes): A classic
    collection of themes.
  - [delabj/ggCyberPunk](https://github.com/delabj/ggCyberPunk): For the
    cyberpunk aesthetic.
  - [briandconnelly/lato](https://github.com/briandconnelly/lato):
    Simple themes with Lato font.
  - [nanxstats/ggsci](https://github.com/nanxstats/ggsci): For looking
    sciency.
  - [nsgrantham/ggdark](https://github.com/nsgrantham/ggdark): Dark
    version of ggplot2 default themes.
  - [datarootsio/artyfarty](https://github.com/datarootsio/artyfarty):
    Unmaintained, but good for inspiration I guess.
  - [schochastics/Rokemon](https://github.com/schochastics/Rokemon):
    Pokémon themes.
  - [Ryo-N7/tvthemes](https://github.com/Ryo-N7/tvthemes): TV show
    themes.
  - [moldach/vapoRwave](https://github.com/moldach/vapoRwave):
    Everything is better with some ＶＡＰＯＲＷＡＶＥ
  - [xkcd](https://github.com/cran/xkcd): Because of course.

### Colors

  - [EmilHvitfeldt/paletteer](https://github.com/EmilHvitfeldt/paletteer):
    Collection of color palettes.
  - [sjmgarnier/viridisLite](https://github.com/sjmgarnier/viridisLite):
    Viridis is the (at least my) preferred set of color scales for
    continuous variables
  - [coolors](https://coolors.co/): Make your own color palettes easily
    (useful for qualitative palettes etc.).

### Plotting *with fonts* (and text stuff)

  - [wilkelab/ggtext](https://github.com/wilkelab/ggtext): Markdown text
    formatting in ggplot2\! (WIP)
  - [r-lib/ragg](https://github.com/r-lib/ragg/): Alternative output
    device which, among general performance/quality improvements, also
    eases the inclusion of system fonts.
  - [rstudio/thematic](https://github.com/rstudio/thematic/) (WIP):
    Auto-theming depending on context, easy styling using
    [rstudio/bootstraplib](https://github.com/rstudio/bootstraplib) in
    RMarkdown/shiny/RStudio(\!). The latter is also WIP.
  - [yixuan/showtext](https://github.com/yixuan/showtext): For plotting
    with fonts, including the easy inclusion of Google fonts.
  - [wch/extrafont](https://github.com/wch/extrafont): Probably maybe
    required to use fonts for PDF output (using `cairo_pdf`)? I still
    don’t quite understand it, but `extrafont::loadfonts()` tends to do
    more good than harm.

### Reading

  - [Thread (with links) about dark themes and
    readability](https://twitter.com/jburnmurdoch/status/1231235791562694659)
      - <https://uxmovement.com/content/why-you-should-never-use-pure-black-for-text-or-backgrounds/>
      - <https://markboulton.co.uk/journal/five-simple-steps-to-better-typography/>
      - <https://schlosslab.discovery.wisc.edu/wp-content/uploads/2018/09/SchlossGramazioSilvermanParkerWanginPress.pdf>
      - <https://www.nngroup.com/articles/dark-mode/>
