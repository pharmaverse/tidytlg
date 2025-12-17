# Create a `statlist` interface for a table

The `statlist` is the interface for the presentation of data in a
`tidytlg` table.

## Usage

``` r
statlist(stats, ...)
```

## Arguments

- stats:

  (required) A character vector of statistics to display in the table.

- ...:

  (optional) Additional configuration for stats. See sections below for
  allowable arguments.

## Value

A `statlist` object that can be passed in the `statlist` argument of
`freq`, `nested_freq`, or `univar`.

## `Statlists` for [`freq()`](freq.md) and [`nested_freq()`](nested_freq.md)

[`freq()`](freq.md) `statlists` can be composed of `n` (count), `N`
(denominator), and `x.x` (percentage, formatted with or without a
percent sign). Denominators will include missing values if the
'display_missing' argument is `TRUE`, otherwise they will be excluded.
They can be arranged in the following ways:

- `n`

- `n/N`

- `n (x.x)`

- `n (x.x%)`

- `n/N (x.x)`

- `n/N (x.x%)`

The following other configurations are supported:

- `denoms_by` - Controls what groupings of variables should define the
  denominator. Variables should be passed as a quoted vector

- `distinct` - A boolean value. Should the numerator reflect distinct
  `USUBJIDs` or event counts. Defaults to `TRUE` which captures distinct
  subjects.

- `distinct_by` - A character value used to select the variable that
  should be used to "distinct" the frequency tables. Defaults to
  `USUBJID`.

- `zero_denom` - The string to display when there are no records found
  in an entire denominator group. Defaults to `-`

- `zero_n` - The string to display when there are no records found for a
  numerator. Defaults to `0`.

## `Statlists` for `univar` `statlists`

- `N`

- `SUM`

- `MEAN`

- `GeoMEAN`

- `SD`

- `SE`

- `CV`

- `GSD`

- `GSE`

- `MEANSD`

- `MEANSE`

- `MEDIAN`

- `MIN`

- `MAX`

- `RANGE`

- `Q1`

- `Q3`

- `IQRANGE`

- `MEDRANGE`

- `MEDIQRANGE`

- `MEAN_CI`

- `GeoMEAN_CI`

where `GeoMEAN`: Geometric Mean, `CV`: Coefficient of Variation, `GSD`:
Geometric standard deviation, `GSE`: Geometric standard error,
`MEAN_CI`: Mean (95% C.I.), `GeoMEAN_CI`: Geometric Mean (95% C.I.). In
calculating geometric statistics, if there are zero values in the
inputs, zero values will be excluded before calculating geometric
statistics.

## Examples

``` r
freq(
  mtcars,
  colvar = "gear",
  rowvar = "cyl",
  rowbyvar = "am",
  statlist = statlist("n/N (x.x)",
    distinct = FALSE,
    denoms_by = c("gear", "am"),
    zero_denom = "_0_"
  )
)
#> Column Variables:  gear 
#> Row By Variables Variables:  am 
#> Row Variable:  cyl 
#> Statistic Formatting:  n/N (x.x) FALSE c("gear", "am") _0_ 
#> Value Cutoffs:  pct  by variable  pct 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  gear am 
#> # A tibble: 8 × 7
#>   label `3`            `4`          `5`          row_type   group_level am   
#> * <chr> <chr>          <chr>        <chr>        <chr>            <dbl> <fct>
#> 1 0     ""             ""           ""           BY_HEADER1           0 0    
#> 2 4     "1/15 (6.7)"   "2/4 (50.0)" "_0_"        VALUE                0 0    
#> 3 6     "2/15 (13.3)"  "2/4 (50.0)" "_0_"        VALUE                0 0    
#> 4 8     "12/15 (80.0)" "0/4"        "_0_"        VALUE                0 0    
#> 5 1     ""             ""           ""           BY_HEADER1           0 1    
#> 6 4     "_0_"          "6/8 (75.0)" "2/5 (40.0)" VALUE                0 1    
#> 7 6     "_0_"          "2/8 (25.0)" "1/5 (20.0)" VALUE                0 1    
#> 8 8     "_0_"          "0/8"        "2/5 (40.0)" VALUE                0 1    
```
