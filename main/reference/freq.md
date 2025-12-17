# Frequency counts and percentages

Frequency counts and percentages for a variable by treatment and/or
group.

## Usage

``` r
freq(
  df,
  denom_df = df,
  colvar = NULL,
  tablebyvar = NULL,
  rowvar = NULL,
  rowbyvar = NULL,
  statlist = getOption("tidytlg.freq.statlist.default"),
  decimal = 1,
  nested = FALSE,
  cutoff = NULL,
  cutoff_stat = "pct",
  subset = TRUE,
  descending_by = NULL,
  display_missing = FALSE,
  rowtext = NULL,
  row_header = NULL,
  .keep = TRUE,
  .ord = FALSE,
  pad = TRUE,
  ...
)
```

## Arguments

- df:

  (required) dataframe containing records to summarize by treatment.

- denom_df:

  (optional) dataframe used for population based denominators (default =
  `df`).

- colvar:

  (required) treatment variable within `df` to use to summarize

- tablebyvar:

  (optional) repeat entire table by variable within `df`

- rowvar:

  (required) character vector of variables to summarize within the
  dataframe.

- rowbyvar:

  (optional) repeat `rowvar` by variable within `df`

- statlist:

  (optional) `statlist` object of stats to keep of length 1 or 2
  specifying list of statistics and format desired (e.g
  `statlist(c("N", "n (x.x\%)"))`) (default = `statlist(c("n (x.x)"))`).

- decimal:

  (optional) decimal precision root level default (default = 1).

- nested:

  (optional) INTERNAL USE ONLY. The default should not be changed.
  Switch on when this function is called by
  [`nested_freq()`](nested_freq.md) so we will not include the by
  variables as part of the group denominators (default = `FALSE`).

- cutoff:

  (optional) percentage cutoff threshold. This can be passed as a
  numeric cutoff, in that case any rows with greater than or equal to
  that cutoff will be preserved, others will be dropped. To specify a
  single column to define the cutoff logic, pass a character value of
  the form `<colName> >= <value>` and only that column will be used.

- cutoff_stat:

  (optional) The value to cutoff by, `n` or `pct.` (default = `'pct'`).
  Can be done with multiple columns by adding `&` or `|` ex.
  `col1 >= val1 & col2 >= val2`.

- subset:

  (optional) An R expression that will be passed to a
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  function to subset the `data.frame`. This is performed on the
  numerator before any other derivations. Denominators must be
  preprocessed and passed through using `denom_df`.

- descending_by:

  (optional) The column or columns to sort descending counts. Can also
  provide a named list to do ascending order ex.
  `c("VarName1" = "asc", "VarName2" = "desc")` would sort by `VarName1`
  in ascending order and `VarName2` in descending order. In case of a
  tie in count or `descending_by` not provided, the columns will be
  sorted alphabetically.

- display_missing:

  (optional) Should the "missing" values be displayed? If missing values
  are displayed, denominators will include missing values. (default =
  `FALSE`).

- rowtext:

  (optional) A character vector used to rename the `label` column. If
  named, names will give the new level and values will be the replaced
  value. If unnamed, and the table has only one row, the `rowtext` will
  rename the label of the row. If the `rowtext` is unnamed, the table
  has no rows, and there is a subset, the table will be populated with
  zeros and the label will be the only row.

- row_header:

  (optional) A character vector to be added to the table.

- .keep:

  (optional) Should the `rowbyvar` and `tablebyvar` be output in the
  table. If `FALSE`, `rowbyvar` will still be output in the `label`
  column. (Default = `TRUE`).

- .ord:

  Should the ordering columns be output with the table? This is useful
  if a table needs to be merged or reordered in any way after build.

- pad:

  (optional) A boolean that controls if levels with zero records should
  be included in the final table. (default = `TRUE`).

- ...:

  (optional) Named arguments to be included as columns on the table.

## Value

A dataframe of results

## Sorting a 'freq' table

By default, a frequency table is sorted based on the factor level of the
`rowvar` variable. If the `rowvar` variable isn't a factor, it will be
sorted alphabetically. This behavior can be modified in two ways, the
first is the [`char2factor()`](char2factor.md) function that offers a
interface for discretization a variable based on a numeric variable,
like `VISITN`. The second is based on the `descending_by` argument which
will sort based on counts on a variable.

## Examples

``` r
adsl <- data.frame(
  USUBJID = c("DEMO-101", "DEMO-102", "DEMO-103"),
  RACE = c("WHITE", "BLACK", "ASIAN"),
  SEX = c("F", "M", "F"),
  colnbr = factor(c("Placebo", "Low", "High"))
)

# Unique subject count of a single variable
freq(adsl,
  colvar = "colnbr",
  rowvar = "RACE",
  statlist = statlist("n")
)
#> Column Variables:  colnbr 
#> Row Variable:  RACE 
#> Statistic Formatting:  n 
#> Value Cutoffs:  pct  by variable  pct 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  colnbr 
#> # A tibble: 3 × 6
#>   label High  Low   Placebo row_type group_level
#> * <chr> <chr> <chr> <chr>   <chr>          <dbl>
#> 1 ASIAN 1     0     0       VALUE              0
#> 2 BLACK 0     1     0       VALUE              0
#> 3 WHITE 0     0     1       VALUE              0

# Unique subject count and percent of a single variable
freq(adsl,
  colvar = "colnbr",
  rowvar = "RACE",
  statlist = statlist(c("N", "n (x.x%)"))
)
#> Column Variables:  colnbr 
#> Row Variable:  RACE 
#> Statistic Formatting:  c("N", "n (x.x%)") 
#> Value Cutoffs:  pct  by variable  pct 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  colnbr 
#> # A tibble: 4 × 6
#>   label High       Low        Placebo    row_type group_level
#> * <chr> <chr>      <chr>      <chr>      <chr>          <dbl>
#> 1 N     1          1          1          N                  0
#> 2 ASIAN 1 (100.0%) 0          0          VALUE              0
#> 3 BLACK 0          1 (100.0%) 0          VALUE              0
#> 4 WHITE 0          0          1 (100.0%) VALUE              0

# Unique subject count of a variable by another variable
freq(adsl,
  colvar = "colnbr",
  rowvar = "RACE",
  rowbyvar = "SEX",
  statlist = statlist("n")
)
#> Column Variables:  colnbr 
#> Row By Variables Variables:  SEX 
#> Row Variable:  RACE 
#> Statistic Formatting:  n 
#> Value Cutoffs:  pct  by variable  pct 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  SEX colnbr 
#> # A tibble: 8 × 7
#>   label High  Low   Placebo row_type   group_level SEX  
#> * <chr> <chr> <chr> <chr>   <chr>            <dbl> <fct>
#> 1 F     ""    ""    ""      BY_HEADER1           0 F    
#> 2 ASIAN "1"   "-"   "0"     VALUE                0 F    
#> 3 BLACK "0"   "-"   "0"     VALUE                0 F    
#> 4 WHITE "0"   "-"   "1"     VALUE                0 F    
#> 5 M     ""    ""    ""      BY_HEADER1           0 M    
#> 6 ASIAN "-"   "0"   "-"     VALUE                0 M    
#> 7 BLACK "-"   "1"   "-"     VALUE                0 M    
#> 8 WHITE "-"   "0"   "-"     VALUE                0 M    

# Unique subject count of a variable by another variable using colvar and
# group to define the denominator
freq(adsl,
  colvar = "colnbr",
  rowvar = "RACE",
  rowbyvar = "SEX",
  statlist = statlist("n (x.x%)", denoms_by = c("colnbr", "SEX"))
)
#> Column Variables:  colnbr 
#> Row By Variables Variables:  SEX 
#> Row Variable:  RACE 
#> Statistic Formatting:  n (x.x%) c("colnbr", "SEX") 
#> Value Cutoffs:  pct  by variable  pct 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  colnbr SEX 
#> # A tibble: 8 × 7
#>   label High         Low          Placebo      row_type   group_level SEX  
#> * <chr> <chr>        <chr>        <chr>        <chr>            <dbl> <fct>
#> 1 F     ""           ""           ""           BY_HEADER1           0 F    
#> 2 ASIAN "1 (100.0%)" "-"          "0"          VALUE                0 F    
#> 3 BLACK "0"          "-"          "0"          VALUE                0 F    
#> 4 WHITE "0"          "-"          "1 (100.0%)" VALUE                0 F    
#> 5 M     ""           ""           ""           BY_HEADER1           0 M    
#> 6 ASIAN "-"          "0"          "-"          VALUE                0 M    
#> 7 BLACK "-"          "1 (100.0%)" "-"          VALUE                0 M    
#> 8 WHITE "-"          "0"          "-"          VALUE                0 M    

# Cut records where count meets threshold for any column
freq(cdisc_adsl,
  rowvar = "ETHNIC",
  colvar = "TRT01P",
  statlist = statlist("n (x.x%)"),
  cutoff = "5",
  cutoff_stat = "n"
)
#> Column Variables:  TRT01P 
#> Row Variable:  ETHNIC 
#> Statistic Formatting:  n (x.x%) 
#> Value Cutoffs:  5  by variable  n 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  TRT01P 
#> # A tibble: 1 × 6
#>   label            Placebo `Xanomeline High Dose` `Xanomeline Low Dose` row_type
#> * <chr>            <chr>   <chr>                  <chr>                 <chr>   
#> 1 NOT HISPANIC OR… 3 (60.… 5 (100.0%)             5 (100.0%)            VALUE   
#> # ℹ 1 more variable: group_level <dbl>

# Cut records where count meets threshold for a specific column
freq(cdisc_adsl,
  rowvar = "ETHNIC",
  colvar = "TRT01P",
  statlist = statlist("n (x.x%)"),
  cutoff = "Placebo >= 3",
  cutoff_stat = "n"
)
#> Column Variables:  TRT01P 
#> Row Variable:  ETHNIC 
#> Statistic Formatting:  n (x.x%) 
#> Value Cutoffs:  Placebo >= 3  by variable  n 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  TRT01P 
#> # A tibble: 1 × 6
#>   label            Placebo `Xanomeline High Dose` `Xanomeline Low Dose` row_type
#> * <chr>            <chr>   <chr>                  <chr>                 <chr>   
#> 1 NOT HISPANIC OR… 3 (60.… 5 (100.0%)             5 (100.0%)            VALUE   
#> # ℹ 1 more variable: group_level <dbl>

# Below illustrates how to make the same calls to freq() as above, using
# table and column metadata.

# Unique subject count of a single variable
table_metadata <- tibble::tribble(
  ~anbr, ~func, ~df, ~rowvar, ~statlist, ~colvar,
  1, "freq", "cdisc_adsl", "ETHNIC", statlist("n"), "TRT01PN"
)

generate_results(table_metadata,
  column_metadata = column_metadata,
  tbltype = "type1"
)
#> # A tibble: 2 × 11
#>   label col1  col2  col3  row_type func   anbr indentme roworder newrows newpage
#> * <chr> <chr> <chr> <chr> <chr>    <chr> <dbl>    <dbl>    <int>   <dbl>   <dbl>
#> 1 HISP… 2     0     0     VALUE    freq      1        1        1       0       0
#> 2 NOT … 3     5     5     VALUE    freq      1        1        2       0       0

# Unique subject count and percent of a single variable
table_metadata <- tibble::tribble(
  ~anbr, ~func, ~df, ~rowvar, ~statlist, ~colvar,
  "1", "freq", "cdisc_adsl", "ETHNIC", statlist(c("N", "n (x.x%)")), "TRT01PN"
)

generate_results(table_metadata,
  column_metadata = column_metadata,
  tbltype = "type1"
)
#> # A tibble: 3 × 11
#>   label col1  col2  col3  row_type func   anbr indentme roworder newrows newpage
#> * <chr> <chr> <chr> <chr> <chr>    <chr> <dbl>    <dbl>    <int>   <dbl>   <dbl>
#> 1 N     5     5     5     N        freq      1        1        1       0       0
#> 2 HISP… 2 (4… 0     0     VALUE    freq      1        2        2       0       0
#> 3 NOT … 3 (6… 5 (1… 5 (1… VALUE    freq      1        2        3       0       0

# Cut records where count meets threshold for any column
table_metadata <- tibble::tibble(
  anbr = "1", func = "freq", df = "cdisc_adsl", rowvar = "ETHNIC",
  statlist = statlist("n (x.x%)"), colvar = "TRT01PN", cutoff = 5,
  cutoff_stat = "n"
)

generate_results(table_metadata,
  column_metadata = column_metadata,
  tbltype = "type1"
)
#> # A tibble: 1 × 11
#>   label col1  col2  col3  row_type func   anbr indentme roworder newrows newpage
#> * <chr> <chr> <chr> <chr> <chr>    <chr> <dbl>    <dbl>    <int>   <dbl>   <dbl>
#> 1 NOT … 3 (6… 5 (1… 5 (1… VALUE    freq      1        1        1       0       0

# Cut records where count meets threshold for a specific column
table_metadata <- tibble::tibble(
  anbr = 1, func = "freq", df = "cdisc_adsl", rowvar = "ETHNIC",
  statlist = statlist("n (x.x%)"), colvar = "TRT01PN",
  cutoff = "col1 >= 3", cutoff_stat = "n"
)

generate_results(table_metadata,
  column_metadata = column_metadata,
  tbltype = "type1"
)
#> # A tibble: 1 × 11
#>   label col1  col2  col3  row_type func   anbr indentme roworder newrows newpage
#> * <chr> <chr> <chr> <chr> <chr>    <chr> <dbl>    <dbl>    <int>   <dbl>   <dbl>
#> 1 NOT … 3 (6… 5 (1… 5 (1… VALUE    freq      1        1        1       0       0
```
