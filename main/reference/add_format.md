# Add the formatting variables of `indentme`, `newrows`, `newpage`, and `roworder` to the results dataframe

Add the formatting variables of `indentme`, `newrows`, `newpage`, and
`roworder` to the results dataframe

## Usage

``` r
add_format(df, tableby = NULL, groupby = NULL, .keep = FALSE)
```

## Arguments

- df:

  (required) dataframe of results and must contain the `anbr` variable.

- tableby:

  (optional) character vector containing table by variables.

- groupby:

  (optional) character vector containing group by variables.

- .keep:

  (optional) should `tableby` and `groupby` variables be kept in the
  final dataframe. (default = `FALSE`).

## Value

dataframe with the formatting variables `indentme`, `newrows`,
`newpage`, and `roworder` added.

## Examples

``` r
df <- tibble::tibble(
  row_type =
    c(
      "TABLE_BY_HEADER", "HEADER", "BY_HEADER1", "N", "VALUE",
      "COUNTS", "UNIVAR", "NESTED", "NESTED"
    ),
  nested_level = c(NA, NA, NA, NA, NA, NA, NA, 1, 2),
  group_level = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  label = c(NA, NA, NA, NA, NA, "N", NA, NA, NA),
  by = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  tableby = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  anbr = c(1:9)
)
add_format(df)
#> # A tibble: 9 × 9
#>   row_type        label by    tableby  anbr indentme roworder newrows newpage
#>   <chr>           <chr> <lgl> <lgl>   <int>    <dbl>    <int>   <dbl>   <dbl>
#> 1 TABLE_BY_HEADER NA    NA    NA          1        0        1       1       0
#> 2 HEADER          NA    NA    NA          2        1        1       1       0
#> 3 BY_HEADER1      NA    NA    NA          3        1        1       1       0
#> 4 N               NA    NA    NA          4        2        1       1       0
#> 5 VALUE           NA    NA    NA          5        2        1       1       0
#> 6 COUNTS          N     NA    NA          6        0        1       1       0
#> 7 UNIVAR          NA    NA    NA          7        0        1       1       0
#> 8 NESTED          NA    NA    NA          8        2        1       1       0
#> 9 NESTED          NA    NA    NA          9        3        1       1       0
```
