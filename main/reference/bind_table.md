# Bind a set of `tidytlg` tables together with formatting variables

`bind_table` combines analysis results with formatting variables
(`indentme`, `newrows`, `newpage`) based on `by` variables
(`tablebyvar`, `rowbyvar`), such that appropriate formatting
(indentation, line break, page break) can be applied while creating the
output. It can also attach the column metadata attribute, which will be
automatically used in `gentlg` for creating output.

## Usage

``` r
bind_table(
  ...,
  colvar = NULL,
  tablebyvar = NULL,
  rowbyvar = NULL,
  prefix = NULL,
  add_count = FALSE,
  add_format = TRUE,
  column_metadata_file = NULL,
  column_metadata = NULL,
  tbltype = NULL
)
```

## Arguments

- ...:

  (required) a set of `tidytlg` tables to bind together

- colvar:

  (required) treatment variable within `df` to use to summarize.
  Required if `add_count` is TRUE.

- tablebyvar:

  (optional) repeat entire table by variable within `df`.

- rowbyvar:

  (optional) any `rowbyvar` values used to create the table.

- prefix:

  (optional) text to prefix the values of `tablebyvar` with.

- add_count:

  (optional) Should a count be included in the `tablebyvar`? (default =
  `TRUE`)

- add_format:

  (optional) Should format be added to the output table? This is done
  using the `add_format` function. (default = `TRUE`)

- column_metadata_file:

  (optional) An excel file for `column_metadata`. Does not change the
  behavior of the function binds the column metadata for `gentlg`. If a
  `column_metadata` dataframe is passed in too, this is ignored.

- column_metadata:

  (optional) A dataframe containing the column metadata. This will be
  used in place of `column_metadata_file.`

- tbltype:

  (optional) A value used to subset the `column_metadata_file.`

## Value

The `tidytlg` tables bound together reflecting the `tablebyvars` used.

## Examples

``` r
library(magrittr)
#> 
#> Attaching package: ‘magrittr’
#> The following objects are masked from ‘package:testthat’:
#> 
#>     equals, is_less_than, not

# bind tables together
t1 <- cdisc_adsl %>%
  freq(
    colvar = "TRT01PN",
    rowvar = "ITTFL",
    statlist = statlist("n"),
    subset = ITTFL == "Y",
    rowtext = "Analysis set: ITT"
  )

t2 <- cdisc_adsl %>%
  univar(
    colvar = "TRT01PN",
    rowvar = "AGE",
    decimal = 0,
    row_header = "Age, years"
  )

bind_table(t1, t2)
#> # A tibble: 7 × 10
#>   label       `0`   `54`  `81`  row_type  anbr indentme roworder newrows newpage
#>   <chr>       <chr> <chr> <chr> <chr>    <dbl>    <dbl>    <int>   <dbl>   <dbl>
#> 1 Analysis s… "5"   "5"   "5"   HEADER       1        0        1       0       0
#> 2 Age, years  ""    ""    ""    HEADER       2        0        1       1       0
#> 3 N           "5"   "5"   "5"   N            2        1        2       0       0
#> 4 Mean (SD)   "69.… "75.… "72.… VALUE        2        2        3       0       0
#> 5 Median      "64.… "74.… "75.… VALUE        2        2        4       0       0
#> 6 Range       "(52… "(68… "(57… VALUE        2        2        5       0       0
#> 7 IQ range    "(63… "(71… "(71… VALUE        2        2        6       0       0

# bind tables together w/by groups
t1 <- cdisc_adsl %>%
  freq(
    colvar = "TRT01PN",
    rowvar = "ITTFL",
    rowbyvar = "SEX",
    statlist = statlist("n"),
    subset = ITTFL == "Y",
    rowtext = "Analysis set: ITT"
  )

t2 <- cdisc_adsl %>%
  univar(
    colvar = "TRT01PN",
    rowvar = "AGE",
    rowbyvar = "SEX",
    decimal = 0,
    row_header = "Age, years"
  )

bind_table(t1, t2, rowbyvar = "SEX")
#> # A tibble: 18 × 10
#>    label      `0`   `54`  `81`  row_type  anbr indentme roworder newrows newpage
#>    <chr>      <chr> <chr> <chr> <chr>    <dbl>    <dbl>    <int>   <dbl>   <dbl>
#>  1 F          ""    ""    ""    BY_HEAD…     1        0        1       0       0
#>  2 Analysis … "2"   "1"   "3"   VALUE        1        1        2       0       0
#>  3 M          ""    ""    ""    BY_HEAD…     1        0        1       1       0
#>  4 Analysis … "3"   "4"   "2"   VALUE        1        1        2       0       0
#>  5 F          ""    ""    ""    BY_HEAD…     2        0        1       1       0
#>  6 Age, years ""    ""    ""    HEADER       2        0        2       0       0
#>  7 N          "2"   "1"   "3"   N            2        1        3       0       0
#>  8 Mean (SD)  "74.… "81.… "77.… VALUE        2        2        4       0       0
#>  9 Median     "74.… "81.… "77.… VALUE        2        2        5       0       0
#> 10 Range      "(63… "(81… "(75… VALUE        2        2        6       0       0
#> 11 IQ range   "(63… "(81… "(75… VALUE        2        2        7       0       0
#> 12 M          ""    ""    ""    BY_HEAD…     2        0        1       1       0
#> 13 Age, years ""    ""    ""    HEADER       2        0        2       0       0
#> 14 N          "3"   "4"   "2"   N            2        1        3       0       0
#> 15 Mean (SD)  "66.… "74.… "64.… VALUE        2        2        4       0       0
#> 16 Median     "64.… "72.… "64.… VALUE        2        2        5       0       0
#> 17 Range      "(52… "(68… "(57… VALUE        2        2        6       0       0
#> 18 IQ range   "(52… "(69… "(57… VALUE        2        2        7       0       0

# bind tables together w/table by groups
t1 <- cdisc_adsl %>%
  freq(
    colvar = "TRT01PN",
    rowvar = "ITTFL",
    tablebyvar = "SEX",
    statlist = statlist("n"),
    subset = ITTFL == "Y",
    rowtext = "Analysis set: ITT"
  )

t2 <- cdisc_adsl %>%
  univar(
    colvar = "TRT01PN",
    rowvar = "AGE",
    tablebyvar = "SEX",
    decimal = 0,
    row_header = "Age, years"
  )

bind_table(t1, t2, tablebyvar = "SEX")
#> # A tibble: 16 × 10
#>    label      `0`   `54`  `81`  row_type  anbr indentme roworder newrows newpage
#>    <chr>      <chr> <chr> <chr> <chr>    <dbl>    <dbl>    <int>   <dbl>   <dbl>
#>  1 F           NA    NA    NA   TABLE_B…     0        0        1       1       0
#>  2 Analysis … "2"   "1"   "3"   VALUE        1        2        1       1       0
#>  3 Age, years ""    ""    ""    HEADER       2        1        1       1       0
#>  4 N          "2"   "1"   "3"   N            2        2        2       0       0
#>  5 Mean (SD)  "74.… "81.… "77.… VALUE        2        3        3       0       0
#>  6 Median     "74.… "81.… "77.… VALUE        2        3        4       0       0
#>  7 Range      "(63… "(81… "(75… VALUE        2        3        5       0       0
#>  8 IQ range   "(63… "(81… "(75… VALUE        2        3        6       0       0
#>  9 M           NA    NA    NA   TABLE_B…     0        0        1       0       1
#> 10 Analysis … "3"   "4"   "2"   VALUE        1        2        1       1       0
#> 11 Age, years ""    ""    ""    HEADER       2        1        1       1       0
#> 12 N          "3"   "4"   "2"   N            2        2        2       0       0
#> 13 Mean (SD)  "66.… "74.… "64.… VALUE        2        3        3       0       0
#> 14 Median     "64.… "72.… "64.… VALUE        2        3        4       0       0
#> 15 Range      "(52… "(68… "(57… VALUE        2        3        5       0       0
#> 16 IQ range   "(52… "(69… "(57… VALUE        2        3        6       0       0

# w/prefix
bind_table(t1, t2, tablebyvar = "SEX", prefix = "Gender: ")
#> # A tibble: 16 × 10
#>    label      `0`   `54`  `81`  row_type  anbr indentme roworder newrows newpage
#>    <chr>      <chr> <chr> <chr> <chr>    <dbl>    <dbl>    <int>   <dbl>   <dbl>
#>  1 Gender: F   NA    NA    NA   TABLE_B…     0        0        1       1       0
#>  2 Analysis … "2"   "1"   "3"   VALUE        1        2        1       1       0
#>  3 Age, years ""    ""    ""    HEADER       2        1        1       1       0
#>  4 N          "2"   "1"   "3"   N            2        2        2       0       0
#>  5 Mean (SD)  "74.… "81.… "77.… VALUE        2        3        3       0       0
#>  6 Median     "74.… "81.… "77.… VALUE        2        3        4       0       0
#>  7 Range      "(63… "(81… "(75… VALUE        2        3        5       0       0
#>  8 IQ range   "(63… "(81… "(75… VALUE        2        3        6       0       0
#>  9 Gender: M   NA    NA    NA   TABLE_B…     0        0        1       0       1
#> 10 Analysis … "3"   "4"   "2"   VALUE        1        2        1       1       0
#> 11 Age, years ""    ""    ""    HEADER       2        1        1       1       0
#> 12 N          "3"   "4"   "2"   N            2        2        2       0       0
#> 13 Mean (SD)  "66.… "74.… "64.… VALUE        2        3        3       0       0
#> 14 Median     "64.… "72.… "64.… VALUE        2        3        4       0       0
#> 15 Range      "(52… "(68… "(57… VALUE        2        3        5       0       0
#> 16 IQ range   "(52… "(69… "(57… VALUE        2        3        6       0       0

# w/counts
bind_table(t1, t2, tablebyvar = "SEX", add_count = TRUE, colvar = "TRT01PN")
#> # A tibble: 16 × 10
#>    label      `0`   `54`  `81`  row_type  anbr indentme roworder newrows newpage
#>    <chr>      <chr> <chr> <chr> <chr>    <dbl>    <dbl>    <int>   <dbl>   <dbl>
#>  1 F          "2"   "1"   "3"   TABLE_B…    NA        0        1       1       0
#>  2 Analysis … "2"   "1"   "3"   VALUE        1        2        1       1       0
#>  3 Age, years ""    ""    ""    HEADER       2        1        1       1       0
#>  4 N          "2"   "1"   "3"   N            2        2        2       0       0
#>  5 Mean (SD)  "74.… "81.… "77.… VALUE        2        3        3       0       0
#>  6 Median     "74.… "81.… "77.… VALUE        2        3        4       0       0
#>  7 Range      "(63… "(81… "(75… VALUE        2        3        5       0       0
#>  8 IQ range   "(63… "(81… "(75… VALUE        2        3        6       0       0
#>  9 M          "3"   "4"   "2"   TABLE_B…    NA        0        1       0       1
#> 10 Analysis … "3"   "4"   "2"   VALUE        1        2        1       1       0
#> 11 Age, years ""    ""    ""    HEADER       2        1        1       1       0
#> 12 N          "3"   "4"   "2"   N            2        2        2       0       0
#> 13 Mean (SD)  "66.… "74.… "64.… VALUE        2        3        3       0       0
#> 14 Median     "64.… "72.… "64.… VALUE        2        3        4       0       0
#> 15 Range      "(52… "(68… "(57… VALUE        2        3        5       0       0
#> 16 IQ range   "(52… "(69… "(57… VALUE        2        3        6       0       0
```
