# Add indentation variable to the results dataframe

Add the `indentme` variable to your results data. This drives the number
of indents for the row label text (e.g. 0, 1, 2, etc.).

## Usage

``` r
add_indent(df)
```

## Arguments

- df:

  dataframe of results that contains `row_type` and `label` and the
  optional `nested_level` and `group_level` variables.

## Value

dataframe with the `indentme` variable added.

## Details

The `group_level` variable, which is added to the results dataframe by
[`freq()`](freq.md) and [`univar()`](univar.md) calls, is needed to
define indentation when by variables are used for summary.

The `nested_level` variable, which is added to the results dataframe by
[`nested_freq()`](nested_freq.md), is needed to define indentation for
each level of nesting.

Both of these are added to the default indentation which is driven by
`row_type`.

|                  |                     |
|------------------|---------------------|
| row_type         | default indentation |
| TABLE_BY_HEADER  | 0                   |
| BY_HEADER\[1-9\] | 0                   |
| HEADER           | 0                   |
| N                | 1                   |
| VALUE            | 2                   |
| NESTED           | 0                   |

## Examples

``` r
df <- tibble::tibble(
  row_type = c(
    "TABLE_BY_HEADER", "HEADER",
    "BY_HEADER1", "N", "VALUE", "COUNTS", "UNIVAR", "NESTED", "NESTED"
  ),
  nested_level = c(NA, NA, NA, NA, NA, NA, NA, 1, 2),
  group_level = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  label = c(NA, NA, NA, NA, NA, "N", NA, NA, NA),
  by = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  tableby = c(NA, NA, NA, NA, NA, NA, NA, NA, NA)
)
add_indent(df)
#> # A tibble: 9 × 5
#>   row_type        label by    tableby indentme
#>   <chr>           <chr> <lgl> <lgl>      <dbl>
#> 1 TABLE_BY_HEADER NA    NA    NA             0
#> 2 HEADER          NA    NA    NA             1
#> 3 BY_HEADER1      NA    NA    NA             1
#> 4 N               NA    NA    NA             2
#> 5 VALUE           NA    NA    NA             3
#> 6 COUNTS          N     NA    NA             0
#> 7 UNIVAR          NA    NA    NA             0
#> 8 NESTED          NA    NA    NA             2
#> 9 NESTED          NA    NA    NA             3
```
