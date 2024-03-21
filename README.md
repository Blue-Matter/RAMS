
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The RAMS App

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The RAMS App is a Shiny application designed for faciliating the Risk
Assessment Methods for Salmon (RAMS) process.

The App can be run locally on a machine with the R software installed. A
live online version is available at
[RAMSalmon.com](https://ramsalmon.com).

The RAMS App is still in development.

## Local Version

To run the RAMS app locally, you need to first install the RAMS package
from GitHub:

``` r
# install.packages('remotes')
remotes::install_github('blue-matter/RAMS')
```

Then load the library and run RAMS:

``` r
library(RAMS)
RAMS()
```
