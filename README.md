# specio <img src='images/logo.svg' align="right" height="138.5" />

<!-- badges: start -->

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/mrc-ide/specio.svg?branch=master)](https://travis-ci.org/mrc-ide/specio)
[![codecov.io](https://codecov.io/github/mrc-ide/specio/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/specio?branch=master)

<!-- badges: end -->

`specio` handles reading and parsing data from Spectrum PJNZ file into an R consumable format for use in the [Estimation and Projection Package (EPP)](https://github.com/mrc-ide/eppasm) for estimating and projecting HIV prevalence.

## Usage

As well as the functions required for EPP `specio` also provides 2 more general functions for extracting population data and PLHIV data from the PJNZ file. These are `extract_population` and `extract_hiv_population`. See [website reference](https://mrc-ide.github.io/specio/reference/index.html) for usage and examples.

## Installation

To install internally released version via drat:
```r
# install.packages("drat") # (if needed)
drat:::add("mrc-ide")
install.packages("specio")
```
