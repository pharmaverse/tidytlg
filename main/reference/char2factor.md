# Convert character variable to a factor based off it's numeric variable counterpart.

Convert character variable to a factor based off it's numeric variable
counterpart.

## Usage

``` r
char2factor(df, c_var, n_var)
```

## Arguments

- df:

  data frame.

- c_var:

  character variable within the data frame.

- n_var:

  numeric variable counter part within the data frame to control the
  levels.

## Value

A factor.

## Examples

``` r
df <- tibble::tribble(
  ~TRT01P, ~TRT01PN,
  "Placebo", 1,
  "Low Dose", 2,
  "High Dose", 3
)

# alphabetical order
dplyr::arrange(df, TRT01P)
#> # A tibble: 3 × 2
#>   TRT01P    TRT01PN
#>   <chr>       <dbl>
#> 1 High Dose       3
#> 2 Low Dose        2
#> 3 Placebo         1

# change to factor with char2factor
df$TRT01P <- char2factor(df, "TRT01P", "TRT01PN")

# factor order
dplyr::arrange(df, TRT01P)
#> # A tibble: 3 × 2
#>   TRT01P    TRT01PN
#>   <fct>       <dbl>
#> 1 Placebo         1
#> 2 Low Dose        2
#> 3 High Dose       3
```
