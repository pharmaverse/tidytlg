
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytlg <img src='man/figures/logo.png' align="right" height="200" style="float:right; height:200px;" />

<!-- badges: start -->

<!-- <img src="man/figures/logo.png" align="right" height = "13"/> -->
[![CRAN
status](https://www.r-pkg.org/badges/version/tidytlg)](https://CRAN.R-project.org/package=tidytlg)
<!-- badges: end -->

The goal of tidytlg is to generate table, listings, and graphs (TLG)
using Tidyverse. This can be achieved multiple ways with this package.

- Functional method: build a custom script for each TLG
- Metadata method: build a generic script that utilizes column and table
  metadata to produce each TLG result

## Installation

### Development version

``` r
# install.packages("devtools")
devtools::install_github("pharmaverse/tidytlg")
```

## Functional method example

``` r
library(dplyr)
library(tidytlg)

# Note cdisc_adsl is built into the package for use
ittpop <- cdisc_adsl %>%
  filter(ITTFL == "Y")

# frequency of Intend-to-Treat patients by planned treatment
tbl1 <- freq(ittpop,
             rowvar = "ITTFL",
             statlist = statlist("n"),
             colvar = "TRT01P",
             rowtext = "Analysis Set:  Intend-to-Treat Population",
             subset = ITTFL == "Y")

# N, MEAN (SD), MEDIAN, RANGE, IQ Range of age by planned treatment
tbl2 <- univar(ittpop,
               rowvar = "AGE",
               colvar = "TRT01P",
               row_header = "Age (Years)")

# frequency of Race by planned treatment
tbl3 <- freq(ittpop,
             rowvar =  "RACE",
             statlist = statlist(c("N", "n (x.x%)")),
             colvar = "TRT01P",
             row_header = "Race, n(%)")

# combine results together
tbl <- bind_table(tbl1, tbl2, tbl3)

# conver to hux object -----------------------------------------------------------------
gentlg(huxme       = tbl ,
       orientation = "landscape",
       file        = "DEMO",
       title       = "Custom Method",
       footers     = "Produced with tidytlg",
       colspan     = list(c("", "", "Xanomeline", "Xanomeline")),
       colheader   = c("", "Placebo", "High", "Low"),
       wcol        = .30)
```

## Metadata method example

``` r
library(dplyr)
library(tidytlg)

adsl <- cdisc_adsl

table_metadata <- tibble::tribble(
  ~anbr,~func,    ~df,     ~rowvar,            ~rowtext,   ~row_header,                    ~statlist,         ~subset,
  1,   "freq", "adsl",     "ITTFL", "Analysis set: itt",            NA,                statlist("n"),  "ITTFL == 'Y'",
  2, "univar", "adsl",       "AGE",                  NA, "Age (Years)",                           NA,              NA,
  3,   "freq", "adsl",      "RACE",                  NA,  "Race, n(%)", statlist(c("N", "n (x.x%)")),              NA
) %>%
  mutate(colvar  = "TRT01PN")

tbl <- generate_results(table_metadata, 
                        column_metadata_file = system.file("extdata/column_metadata.xlsx", package = "tidytlg"),
                        tbltype = "type1") 

# conver to hux object -----------------------------------------------------------------
tblid <- "Table01"

gentlg(huxme       = tbl,
       orientation = "landscape",
       file        = tblid,
       title_file = system.file("extdata/titles.xls", package = "tidytlg"),
       wcol        = .30)
```
