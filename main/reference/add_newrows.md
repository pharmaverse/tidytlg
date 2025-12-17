# Add the `newrows` variable to the results dataframe.

The `newrows` variable is used by [`gentlg()`](gentlg.md) to define when
to add a blank row to the output. Data will be grouped by `anbr` and the
variables passed into the `tableby` and `groupby` parameters. `newrows`
will be set to 1 for the first record in each group, except for the
first row in the data. The first row will always be set to 0.

## Usage

``` r
add_newrows(df, tableby = NULL, groupby = NULL)
```

## Arguments

- df:

  dataframe of results. must contain the `anbr` variable that is added
  by [`add_format()`](add_format.md).

- tableby:

  character vector containing table by variables used to generate the
  results.

- groupby:

  character vector containing group by variables used to generate the
  results.

## Value

dataframe with the variable `newrows` and `roworder` added. `newrows` is
used by `gentlg` to insert line breaks.

## Examples

``` r
# Example showing how newrows is set to one for each new anbr except
# the first
tbl <-
  structure(
    list(
      rowvar = c("RANDFL", "AGE", "AGE", "AGE", "AGE", "AGE"),
      anbr = c(1L, 2L, 2L, 2L, 2L, 2L),
      label = c(
        "Analysis set: Subjects Randomized", "Age (Years)", "N",
        "Mean (SD)", "Range", "IQ Range"
      ),
      row_type = c("COUNT", "UNIVAR", "UNIVAR", "UNIVAR", "UNIVAR", "UNIVAR")
    ),
    row.names = c(NA, -6L),
    class = c("tbl_df", "tbl", "data.frame")
  )

add_newrows(tbl)
#> # A tibble: 6 × 6
#>   rowvar  anbr label                             row_type roworder newrows
#>   <chr>  <int> <chr>                             <chr>       <int>   <dbl>
#> 1 RANDFL     1 Analysis set: Subjects Randomized COUNT           1       0
#> 2 AGE        2 Age (Years)                       UNIVAR          1       1
#> 3 AGE        2 N                                 UNIVAR          2       0
#> 4 AGE        2 Mean (SD)                         UNIVAR          3       0
#> 5 AGE        2 Range                             UNIVAR          4       0
#> 6 AGE        2 IQ Range                          UNIVAR          5       0

# Example of use when you have results summarized by one or more variables
tbl2 <- tibble::tribble(
  ~anbr, ~SEX, ~label, ~row_type,
  "01", "F", "Sex : F", "TABLE_BY_HEADER",
  "01", "F", "<65", "VALUE",
  "01", "F", "65-80", "VALUE",
  "01", "F", ">80", "VALUE",
  "01", "M", "Sex : M", "TABLE_BY_HEADER",
  "01", "M", "<65", "VALUE",
  "01", "M", "65-80", "VALUE",
  "01", "M", ">80", "VALUE"
)

add_newrows(tbl2, tableby = "SEX")
#> # A tibble: 8 × 6
#>   anbr  SEX   label   row_type        roworder newrows
#>   <chr> <chr> <chr>   <chr>              <int>   <dbl>
#> 1 01    F     Sex : F TABLE_BY_HEADER        1       0
#> 2 01    F     <65     VALUE                  2       0
#> 3 01    F     65-80   VALUE                  3       0
#> 4 01    F     >80     VALUE                  4       0
#> 5 01    M     Sex : M TABLE_BY_HEADER        1       1
#> 6 01    M     <65     VALUE                  2       0
#> 7 01    M     65-80   VALUE                  3       0
#> 8 01    M     >80     VALUE                  4       0

tbl3 <- tibble::tribble(
  ~anbr, ~SEX, ~ETHNIC, ~label, ~row_type,
  "01", "F", NA, "Sex : F", "TABLE_BY_HEADER",
  "01", "F", "HISPANIC OR LATINO", "HISPANIC OR LATINO", "BY_HEADER1",
  "01", "F", "HISPANIC OR LATINO", "<65", "VALUE",
  "01", "F", "HISPANIC OR LATINO", ">80", "VALUE",
  "01", "F", "HISPANIC OR LATINO", "65-80", "VALUE",
  "01", "F", "NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "BY_HEADER1",
  "01", "F", "NOT HISPANIC OR LATINO", "<65", "VALUE",
  "01", "F", "NOT HISPANIC OR LATINO", "65-80", "VALUE",
  "01", "F", "NOT HISPANIC OR LATINO", ">80", "VALUE",
  "01", "M", NA, "Sex : M", "TABLE_BY_HEADER",
  "01", "M", "HISPANIC OR LATINO", "HISPANIC OR LATINO", "BY_HEADER1",
  "01", "M", "HISPANIC OR LATINO", "<65", "VALUE",
  "01", "M", "HISPANIC OR LATINO", "65-80", "VALUE",
  "01", "M", "HISPANIC OR LATINO", ">80", "VALUE",
  "01", "M", "NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "BY_HEADER1",
  "01", "M", "NOT HISPANIC OR LATINO", "<65", "VALUE",
  "01", "M", "NOT HISPANIC OR LATINO", "65-80", "VALUE",
  "01", "M", "NOT HISPANIC OR LATINO", ">80", "VALUE"
)

add_newrows(tbl3, tableby = "SEX", groupby = "ETHNIC")
#> # A tibble: 18 × 7
#>    anbr  SEX   ETHNIC                 label            row_type roworder newrows
#>    <chr> <chr> <chr>                  <chr>            <chr>       <int>   <dbl>
#>  1 01    F     NA                     Sex : F          TABLE_B…        1       0
#>  2 01    F     HISPANIC OR LATINO     HISPANIC OR LAT… BY_HEAD…        1       1
#>  3 01    F     HISPANIC OR LATINO     <65              VALUE           2       0
#>  4 01    F     HISPANIC OR LATINO     >80              VALUE           3       0
#>  5 01    F     HISPANIC OR LATINO     65-80            VALUE           4       0
#>  6 01    F     NOT HISPANIC OR LATINO NOT HISPANIC OR… BY_HEAD…        1       1
#>  7 01    F     NOT HISPANIC OR LATINO <65              VALUE           2       0
#>  8 01    F     NOT HISPANIC OR LATINO 65-80            VALUE           3       0
#>  9 01    F     NOT HISPANIC OR LATINO >80              VALUE           4       0
#> 10 01    M     NA                     Sex : M          TABLE_B…        1       1
#> 11 01    M     HISPANIC OR LATINO     HISPANIC OR LAT… BY_HEAD…        1       1
#> 12 01    M     HISPANIC OR LATINO     <65              VALUE           2       0
#> 13 01    M     HISPANIC OR LATINO     65-80            VALUE           3       0
#> 14 01    M     HISPANIC OR LATINO     >80              VALUE           4       0
#> 15 01    M     NOT HISPANIC OR LATINO NOT HISPANIC OR… BY_HEAD…        1       1
#> 16 01    M     NOT HISPANIC OR LATINO <65              VALUE           2       0
#> 17 01    M     NOT HISPANIC OR LATINO 65-80            VALUE           3       0
#> 18 01    M     NOT HISPANIC OR LATINO >80              VALUE           4       0
```
