# Spanning headers for outputs

This will create the list object to be passed to [`gentlg()`](gentlg.md)
You can create as many spanning headers as you like, just add variables
prefixed with span to the column metadata.

## Usage

``` r
spanning_headers(column_metadata)
```

## Arguments

- column_metadata:

  dataframe containing the column metadata that is passed to
  [`tlgsetup()`](tlgsetup.md) (see [`tlgsetup()`](tlgsetup.md) for
  details)

## Value

List of character vectors containing column headers for an output.

## Examples

``` r
column_metadata <-
  tibble::tribble(
    ~tbltype, ~coldef, ~decode,                ~span1,
    "type1",  "0",     "Placebo",              "",
    "type1",  "54",    "Low Dose",             "Xanomeline",
    "type1",  "81",    "High Dose",            "Xanomeline",
    "type1",  "54+81", "Total Xanomeline",     ""
  )

spanning_headers(column_metadata)
#> $span1
#> [1] ""           ""           "Xanomeline" "Xanomeline" ""          
#> 
```
