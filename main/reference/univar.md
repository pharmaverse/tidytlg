# Descriptive statistics

Univariate statistics for a variables by treatment and/or group.

## Usage

``` r
univar(
  df,
  colvar = NULL,
  tablebyvar = NULL,
  rowvar = NULL,
  rowbyvar = NULL,
  statlist = getOption("tidytlg.univar.statlist.default"),
  decimal = 1,
  precisionby = NULL,
  precisionon = NULL,
  wide = FALSE,
  alpha = 0.05,
  rowtext = NULL,
  row_header = NULL,
  .keep = TRUE,
  .ord = FALSE,
  ...
)
```

## Arguments

- df:

  (required) dataframe containing records to summarize by treatment.

- colvar:

  (required) character vector of the treatment variable within the
  dataframe.

- tablebyvar:

  (optional) repeat entire table by variable within `df`.

- rowvar:

  (required) character vector of variable to summarize within the
  dataframe.

- rowbyvar:

  (optional) repeat `rowvar` by variable within `df`.

- statlist:

  (optional) `statlist` object of stats to keep (default =
  `statlist(c("N", "MEANSD", "MEDIAN", "RANGE", "IQRANGE"))`).

- decimal:

  (optional) decimal precision root level, when using `presisionby` this
  will be used as the base decimal cap (default = 1).

- precisionby:

  (optional) vector of by variable(s) to use when calculating parameter
  based precision.

- precisionon:

  (optional) variable to use when calculating parameter based precision.
  If `precisionby` is specified but not `precisionon` this will default
  to `rowvar`.

- wide:

  (optional) logical indicating to convert labels to column and columns
  to labels (default = `FALSE`).

- alpha:

  (optional) alpha level for 2-sided confidence interval (default =
  0.05).

- rowtext:

  (optional) A text string to replace the `label` value on the table.
  Useful for tables with a single row.

- row_header:

  (optional) A row to add as a header for the table.

- .keep:

  (optional) Should the `rowbyvar` and `tablebyvar` be output in the
  table. If `FALSE`, `rowbyvar` will still be output in the `label`
  column. (default = `TRUE`).

- .ord:

  Should the ordering columns be output with the table? This is useful
  if a table needs to be merged or reordered in any way after build.

- ...:

  (optional) Named arguments to be included as columns on the table.

## Value

dataframe of results

## Examples

``` r
adsl <-
  structure(
    list(
      USUBJID = c(
        "DEMO-101", "DEMO-102", "DEMO-103", "DEMO-104",
        "DEMO-105", "DEMO-106"
      ),
      AGE = c(59, 51, 57, 65, 21, 80),
      SEX = c("F", "M", "F", "M", "F", "M"),
      WEIGHTBL = c(83.6, 75, 84, 90, 65, 70),
      colnbr = structure(
        c(1L, 3L, 2L, 2L, 3L, 1L),
        .Label = c("Placebo", "Low", "High"),
        class = "factor"
      )
    ),
    row.names = c(NA, 6L),
    class = "data.frame"
  )

# N, Mean(SD), Median, Range, IQ Range for a rowvar by colvar
univar(adsl,
  colvar = "colnbr",
  rowvar = "AGE"
)
#> Column Variables:  colnbr 
#> Row Variable:  AGE 
#> Statistic Formatting:  N MEANSD MEDIAN RANGE IQRANGE 
#> Statistic Presentation 
#> Decimal Precision:  1 
#> Alpha for CI Intervals:  0.05 
#> Parameter Based PrecisionL  None 
#> # A tibble: 5 × 6
#>   label     Placebo        Low            High           row_type group_level
#> * <chr>     <chr>          <chr>          <chr>          <chr>          <dbl>
#> 1 N         2              2              2              N                  0
#> 2 Mean (SD) 69.50 (14.849) 61.00 (5.657)  36.00 (21.213) VALUE              0
#> 3 Median    69.50          61.00          36.00          VALUE              0
#> 4 Range     (59.0; 80.0)   (57.0; 65.0)   (21.0; 51.0)   VALUE              0
#> 5 IQ range  (59.00; 80.00) (57.00; 65.00) (21.00; 51.00) VALUE              0

# N and Mean for a rowvar by colvar
univar(adsl,
  colvar   = "colnbr",
  rowvar   = "AGE",
  statlist = statlist(c("N", "MEAN"))
)
#> Column Variables:  colnbr 
#> Row Variable:  AGE 
#> Statistic Formatting:  N MEAN 
#> Statistic Presentation 
#> Decimal Precision:  1 
#> Alpha for CI Intervals:  0.05 
#> Parameter Based PrecisionL  None 
#> # A tibble: 2 × 6
#>   label Placebo Low   High  row_type group_level
#> * <chr> <chr>   <chr> <chr> <chr>          <dbl>
#> 1 N     2       2     2     N                  0
#> 2 Mean  69.50   61.00 36.00 VALUE              0

# N and Mean for a rowvar by colvar and a by variable
univar(adsl,
  colvar   = "colnbr",
  rowvar   = "AGE",
  rowbyvar = "SEX",
  statlist = statlist(c("N", "MEAN"))
)
#> Column Variables:  colnbr 
#> Row By Variables Variables:  SEX 
#> Row Variable:  AGE 
#> Statistic Formatting:  N MEAN 
#> Statistic Presentation 
#> Decimal Precision:  1 
#> Alpha for CI Intervals:  0.05 
#> Parameter Based PrecisionL  None 
#> # A tibble: 6 × 7
#>   SEX   label Placebo Low     High    row_type   group_level
#> * <chr> <chr> <chr>   <chr>   <chr>   <chr>            <dbl>
#> 1 F     F     ""      ""      ""      BY_HEADER1           0
#> 2 F     N     "1"     "1"     "1"     N                    0
#> 3 F     Mean  "59.00" "57.00" "21.00" VALUE                0
#> 4 M     M     ""      ""      ""      BY_HEADER1           0
#> 5 M     N     "1"     "1"     "1"     N                    0
#> 6 M     Mean  "80.00" "65.00" "51.00" VALUE                0

# Below illustrates how make the same calls to univar() as above, using table
# and column metadata # along with generate_results().

column_metadata <- tibble::tribble(
  ~tbltype, ~coldef, ~decode,
  "type1", "0", "Placebo",
  "type1", "54", "Low",
  "type1", "81", "High"
)

# N, Mean(SD), Median, Range, IQ Range for a rowvar by colvar
table_metadata <- tibble::tribble(
  ~anbr, ~func, ~df, ~rowvar, ~tbltype, ~colvar,
  "1", "univar", "cdisc_adae", "AGE", "type1", "TRTA"
)

generate_results(table_metadata,
  column_metadata = column_metadata,
  tbltype = "type1"
)
#> # A tibble: 5 × 12
#>   label col1  col2  col3  row_type func  tbltype  anbr indentme roworder newrows
#> * <chr> <chr> <chr> <chr> <chr>    <chr> <chr>   <dbl>    <dbl>    <int>   <dbl>
#> 1 N     0     0     0     N        univ… type1       1        1        1       0
#> 2 Mean… -     -     -     VALUE    univ… type1       1        2        2       0
#> 3 Medi… -     -     -     VALUE    univ… type1       1        2        3       0
#> 4 Range (-; … (-; … (-; … VALUE    univ… type1       1        2        4       0
#> 5 IQ r… (-; … (-; … (-; … VALUE    univ… type1       1        2        5       0
#> # ℹ 1 more variable: newpage <dbl>


# N and Mean for a rowvar by colvar
table_metadata <- tibble::tribble(
  ~anbr, ~func, ~df, ~rowvar, ~tbltype, ~colvar, ~statlist,
  "1", "univar", "cdisc_adae", "AGE", "type1", "TRTA",
  statlist(c("N", "MEAN"))
)

generate_results(table_metadata,
  column_metadata = column_metadata,
  tbltype = "type1"
)
#> # A tibble: 2 × 12
#>   label col1  col2  col3  row_type func  tbltype  anbr indentme roworder newrows
#> * <chr> <chr> <chr> <chr> <chr>    <chr> <chr>   <dbl>    <dbl>    <int>   <dbl>
#> 1 N     0     0     0     N        univ… type1       1        1        1       0
#> 2 Mean  -     -     -     VALUE    univ… type1       1        2        2       0
#> # ℹ 1 more variable: newpage <dbl>


# N and Mean for a rowvar by colvar and a by variable
table_metadata <- tibble::tribble(
  ~anbr, ~func, ~df, ~rowvar, ~tbltype, ~colvar, ~statlist, ~by,
  "1", "univar", "cdisc_adae", "AGE", "type1", "TRTA",
  statlist(c("N", "MEAN")), "SEX"
)

generate_results(table_metadata,
  column_metadata = column_metadata,
  tbltype = "type1"
)
#> # A tibble: 2 × 13
#>   label col1  col2  col3  row_type func   tbltype by     anbr indentme roworder
#> * <chr> <chr> <chr> <chr> <chr>    <chr>  <chr>   <chr> <dbl>    <dbl>    <int>
#> 1 N     0     0     0     N        univar type1   SEX       1        1        1
#> 2 Mean  -     -     -     VALUE    univar type1   SEX       1        2        2
#> # ℹ 2 more variables: newrows <dbl>, newpage <dbl>
```
