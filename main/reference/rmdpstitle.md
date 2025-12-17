# Get Titles and Footnotes for all TLGs or one specific TLG

Get Titles and Footnotes for all TLGs or one specific TLG

## Usage

``` r
rmdpstitle(
  df,
  tblid,
  idvar = "tblid",
  identifier = "identifier",
  text = "text"
)
```

## Arguments

- df:

  dataframe with three variables; table name, row identifier (`TITLE` or
  `FOOTNOTEn`), and title or footnote text to display.

- tblid:

  character vector containing the table id, optional, used to subset
  `df` to a specific table (defaults to `tblid`).

- idvar:

  character vector containing the variable in `df` that contains your
  table id.

- identifier:

  character vector containing the variable name in `df` that contains
  your record identifier (defaults to "identifier").

- text:

  character vector containing the variable name in `df` that contains
  your title and footnote text (defaults to "text").

## Value

list of length two, the first element contains the titles as a `tibble`
and the second contains the footnotes as a list.

## Examples

``` r
tblid <- "TSIDEM01"

titles <- tibble::tribble(
  ~tblid, ~identifier, ~text,
  "TSIDEM01", "TITLE", "Demographics Example",
  "TSIDEM01", "FOOTNOTE1", "Example footnote."
)

title_foot <- rmdpstitle(titles, tblid)

title_foot[[1]]
#> # A tibble: 1 × 1
#>   text                
#>   <chr>               
#> 1 Demographics Example
title_foot[[2]]
#> $`1`
#> [1] "Example footnote."
#> 
```
