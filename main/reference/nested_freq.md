# Generate nested count/percent for two or three levels

This will call [`freq()`](freq.md) multiple times and combine the levels
together. This is useful for adverse event and concomitant medications.

## Usage

``` r
nested_freq(
  df,
  denom_df = df,
  colvar = NULL,
  tablebyvar = NULL,
  rowvar = NULL,
  rowbyvar = NULL,
  statlist = getOption("tidytlg.nested_freq.statlist.default"),
  decimal = 1,
  cutoff = NULL,
  cutoff_stat = "pct",
  subset = TRUE,
  descending_by = NULL,
  display_missing = FALSE,
  rowtext = NULL,
  row_header = NULL,
  .keep = TRUE,
  .ord = FALSE,
  ...
)
```

## Arguments

- df:

  (required) dataframe containing the two levels to summarize

- denom_df:

  (optional) dataframe containing records to use as the denominator
  (default = `df`)

- colvar:

  (required) treatment variable within `df` to use to summarize

- tablebyvar:

  (optional) repeat entire table by variable within `df.`

- rowvar:

  (required) nested levels separated by a star, for example
  `AEBODSYS*AEDECOD`, this can handle up to three levels.

- rowbyvar:

  (optional) repeat `rowvar` by variable within `df`

- statlist:

  (optional) count/percent type to return `(default = "n (x.x)")`

- decimal:

  (optional) decimal precision root level `(default = 1)`

- cutoff:

  (optional) numeric value used to cut the data to a percentage
  threshold, if any column meets the threshold the entire record is
  kept.

- cutoff_stat:

  (optional) The value to cutoff by, n or pct. (default = 'pct')

- subset:

  (optional) An R expression that will be passed to a
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  function to subset the data.frame

- descending_by:

  (optional) The column or columns to sort descending values by. Can
  also provide a named list to do ascending order. ex.
  `c("VarName1" = "asc", "VarName2" = "desc")` would sort by `VarName1`
  in ascending order and `VarName2` in descending order. If not
  provided, the columns will be sorted alphabetically.

- display_missing:

  (optional) Should the "missing" values be displayed? (default =
  `FALSE`)

- rowtext:

  (optional) A character vector used to rename the `label` column. If
  named, names will give the new level and values will be the replaced
  value. If unnamed, and the table has only one row, the `rowtext` will
  rename the `label` of the row.

- row_header:

  (optional) A character vector to be added to the table.

- .keep:

  (optional) Should the `rowbyvar` and `tablebyvar` be output in the
  table. If `FALSE`, `rowbyvar` will still be output in the `label`
  column. `(default = TRUE)`.

- .ord:

  Should the ordering columns be output with the table? This is useful
  if a table needs to be merged or reordered in any way after build.

- ...:

  (optional) Named arguments to be included as columns on the table.

## Value

A dataframe of nested results by `colvar` and optional `tablebyvar`.
There are a few additional variable sets added to support multiple
requirements.  
  
The level variables (`level1_`, `level2_`, `level3_`) will carry down
the counts for each level to every record. This allows for easy sorting
of nested groups.  
  
The header variables (`header1`, `header2`, `header3`) will flag the
header for each level to ensure each level header is sorted to the top
of the level.  
  
The n variables ("n\_*") provide a numeric variable containing frequency
for each `colvar`. This can be used to sort and filter records.  
  
The pct variables ("pct\_*") provide a numeric variable containing
percentages for each `colvar`. This can be used to sort and filter
records.  
  

## Examples

``` r
adae <- data.frame(
  SITEID = c("100", "100", "100", "200", "200", "200"),
  USUBJID = c(
    "Demo1-101", "Demo1-102", "Demo1-103",
    "Demo1-104", "Demo1-105", "Demo1-106"
  ),
  AEBODSYS = c(
    "Cardiac disorders", "Cardiac disorders",
    "Respiratory, thoracic and mediastinal disorders",
    "Infections and infestations",
    "Skin and subcutaneous tissue disorders",
    "Infections and infestations"
  ),
  AEDECOD = c(
    "Arrhythmia supraventricular", "Cardiac failure",
    "Chronic obstructive pulmonary disease", "Pneumonia",
    "Pustular psoriasis", "Upper respiratory tract infection"
  ),
  colnbr = structure(
    c(1L, 2L, 3L, 1L, 2L, 3L),
    .Label = c("Active", "Placebo", "Comparator"),
    class = "factor"
  )
)

# Frequency and percent for two levels of nesting
nested_freq(adae,
  colvar = "colnbr",
  rowvar = "AEBODSYS*AEDECOD",
  statlist = statlist("n (x.x%)")
)
#> Column Variables:  colnbr 
#> Row Variable:  AEBODSYS*AEDECOD 
#> Statistic Formatting:  n (x.x%) 
#> Value Cutoffs:  pct  by variable  pct 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  colnbr 
#> # A tibble: 10 × 8
#>    label    Active Placebo Comparator row_type nested_level group_level AEBODSYS
#>  * <chr>    <chr>  <chr>   <chr>      <chr>           <dbl>       <dbl> <fct>   
#>  1 Cardiac… 1 (50… 1 (50.… 0          NESTED              0           0 Cardiac…
#>  2 Arrhyth… 1 (50… 0       0          NESTED              1           0 Cardiac…
#>  3 Cardiac… 0      1 (50.… 0          NESTED              1           0 Cardiac…
#>  4 Infecti… 1 (50… 0       1 (50.0%)  NESTED              0           0 Infecti…
#>  5 Pneumon… 1 (50… 0       0          NESTED              1           0 Infecti…
#>  6 Upper r… 0      0       1 (50.0%)  NESTED              1           0 Infecti…
#>  7 Respira… 0      0       1 (50.0%)  NESTED              0           0 Respira…
#>  8 Chronic… 0      0       1 (50.0%)  NESTED              1           0 Respira…
#>  9 Skin an… 0      1 (50.… 0          NESTED              0           0 Skin an…
#> 10 Pustula… 0      1 (50.… 0          NESTED              1           0 Skin an…

# Frequency and percent for three levels of nesting (for illustrative
# purpose)
nested_freq(adae,
  colvar = "colnbr",
  rowvar = "SITEID*AEBODSYS*AEDECOD",
  statlist = statlist("n (x.x%)")
)
#> Column Variables:  colnbr 
#> Row Variable:  SITEID*AEBODSYS*AEDECOD 
#> Statistic Formatting:  n (x.x%) 
#> Value Cutoffs:  pct  by variable  pct 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  colnbr 
#> # A tibble: 12 × 9
#>    label      Active Placebo Comparator row_type nested_level group_level SITEID
#>  * <chr>      <chr>  <chr>   <chr>      <chr>           <dbl>       <dbl> <fct> 
#>  1 100        1 (50… 1 (50.… 1 (50.0%)  NESTED              0           1 100   
#>  2 Cardiac d… 1 (50… 1 (50.… 0          NESTED              1           1 100   
#>  3 Arrhythmi… 1 (50… 0       0          NESTED              2           1 100   
#>  4 Cardiac f… 0      1 (50.… 0          NESTED              2           1 100   
#>  5 Respirato… 0      0       1 (50.0%)  NESTED              1           1 100   
#>  6 Chronic o… 0      0       1 (50.0%)  NESTED              2           1 100   
#>  7 200        1 (50… 1 (50.… 1 (50.0%)  NESTED              0           1 200   
#>  8 Infection… 1 (50… 0       1 (50.0%)  NESTED              1           1 200   
#>  9 Pneumonia  1 (50… 0       0          NESTED              2           1 200   
#> 10 Upper res… 0      0       1 (50.0%)  NESTED              2           1 200   
#> 11 Skin and … 0      1 (50.… 0          NESTED              1           1 200   
#> 12 Pustular … 0      1 (50.… 0          NESTED              2           1 200   
#> # ℹ 1 more variable: AEBODSYS <fct>

# Cut records where pct meets threshold for a any column
nested_freq(cdisc_adae,
  colvar = "TRTA",
  rowvar = "AEBODSYS*AEDECOD",
  statlist = statlist("n (x.x%)", distinct = TRUE),
  cutoff = 2,
  cutoff_stat = "n"
)
#> Column Variables:  TRTA 
#> Row Variable:  AEBODSYS*AEDECOD 
#> Statistic Formatting:  n (x.x%) TRUE 
#> Value Cutoffs:  2  by variable  n 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  TRTA 
#> # A tibble: 7 × 8
#>   label            Placebo `Xanomeline High Dose` `Xanomeline Low Dose` row_type
#> * <chr>            <chr>   <chr>                  <chr>                 <chr>   
#> 1 GENERAL DISORDE… 2 (40.… 5 (100.0%)             3 (75.0%)             NESTED  
#> 2 APPLICATION SIT… 1 (20.… 4 (80.0%)              1 (25.0%)             NESTED  
#> 3 APPLICATION SIT… 1 (20.… 5 (100.0%)             2 (50.0%)             NESTED  
#> 4 FATIGUE          0       2 (40.0%)              1 (25.0%)             NESTED  
#> 5 SKIN AND SUBCUT… 2 (40.… 2 (40.0%)              3 (75.0%)             NESTED  
#> 6 ERYTHEMA         1 (20.… 0                      3 (75.0%)             NESTED  
#> 7 PRURITUS         1 (20.… 0                      2 (50.0%)             NESTED  
#> # ℹ 3 more variables: nested_level <dbl>, group_level <dbl>, AEBODSYS <fct>

# Cut records where pct meets threshold for a specific column
nested_freq(cdisc_adae,
  rowvar = "AEBODSYS*AEDECOD",
  colvar = "TRTAN",
  statlist = statlist("n (x.x%)", distinct = TRUE),
  cutoff = "54 >= 2",
  cutoff_stat = "n"
)
#> Column Variables:  TRTAN 
#> Row Variable:  AEBODSYS*AEDECOD 
#> Statistic Formatting:  n (x.x%) TRUE 
#> Value Cutoffs:  54 >= 2  by variable  n 
#> Descending By:  Factored Sorting 
#> Denominators By Variables:  TRTAN 
#> # A tibble: 5 × 8
#>   label             `0`   `54`  `81`  row_type nested_level group_level AEBODSYS
#> * <chr>             <chr> <chr> <chr> <chr>           <dbl>       <dbl> <fct>   
#> 1 GENERAL DISORDER… 2 (4… 3 (7… 5 (1… NESTED              0           0 GENERAL…
#> 2 APPLICATION SITE… 1 (2… 2 (5… 5 (1… NESTED              1           0 GENERAL…
#> 3 SKIN AND SUBCUTA… 2 (4… 3 (7… 2 (4… NESTED              0           0 SKIN AN…
#> 4 ERYTHEMA          1 (2… 3 (7… 0     NESTED              1           0 SKIN AN…
#> 5 PRURITUS          1 (2… 2 (5… 0     NESTED              1           0 SKIN AN…

# Frequency and percent for two levels of nesting and sort by descending
# active
nested_freq(adae,
  colvar = "colnbr",
  rowvar = "AEBODSYS*AEDECOD",
  statlist = statlist("n (x.x%)"),
  descending = "Active"
)
#> Column Variables:  colnbr 
#> Row Variable:  AEBODSYS*AEDECOD 
#> Statistic Formatting:  n (x.x%) 
#> Value Cutoffs:  pct  by variable  pct 
#> Descending By:  Active 
#> Denominators By Variables:  colnbr 
#> # A tibble: 10 × 8
#>    label    Active Placebo Comparator row_type nested_level group_level AEBODSYS
#>  * <chr>    <chr>  <chr>   <chr>      <chr>           <dbl>       <dbl> <fct>   
#>  1 Cardiac… 1 (50… 1 (50.… 0          NESTED              0           0 Cardiac…
#>  2 Arrhyth… 1 (50… 0       0          NESTED              1           0 Cardiac…
#>  3 Cardiac… 0      1 (50.… 0          NESTED              1           0 Cardiac…
#>  4 Infecti… 1 (50… 0       1 (50.0%)  NESTED              0           0 Infecti…
#>  5 Pneumon… 1 (50… 0       0          NESTED              1           0 Infecti…
#>  6 Upper r… 0      0       1 (50.0%)  NESTED              1           0 Infecti…
#>  7 Respira… 0      0       1 (50.0%)  NESTED              0           0 Respira…
#>  8 Chronic… 0      0       1 (50.0%)  NESTED              1           0 Respira…
#>  9 Skin an… 0      1 (50.… 0          NESTED              0           0 Skin an…
#> 10 Pustula… 0      1 (50.… 0          NESTED              1           0 Skin an…

# Below illustrates how make the same calls to nested_freq() as above, using
# table and # column metadata along with generate_results().

column_metadata <- tibble::tribble(
  ~tbltype, ~coldef, ~decode,
  "type1", "1", "Placebo",
  "type1", "2", "Low",
  "type1", "3", "High"
)

# Frequency and percent for two levels of nesting
table_metadata <- tibble::tribble(
  ~anbr, ~func, ~df, ~rowvar, ~tbltype, ~colvar, ~statlist,
  "1", "nested_freq", "cdisc_adae", "AEBODSYS*AEDECOD", "type1", "TRTP",
  statlist("n (x.x%)")
)
# generate_results(table_metadata,
# column_metadata_file = tidytlg_metadata(path)


# Frequency and percent for three levels of nesting (for illustrative purpose)
table_metadata <- tibble::tribble(
  ~anbr, ~func, ~df, ~rowvar, ~tbltype, ~colvar,
  ~statlist,
  "1", "nested_freq", "cdisc_adae", "SITEID*AEBODSYS*AEDECOD", "type1",
  "TRTP", statlist("n (x.x%)")
)
# Commented out because it takes too long
# generate_results(table_metadata, column_metadata)

# Cut records where pct meets threshold for a any column
column_metadata <- tibble::tribble(
  ~tbltype, ~coldef, ~decode,
  "type2", "1", "Placebo",
  "type2", "2", "Active"
)
table_metadata <- tibble::tibble(
  anbr = "1", func = "nested_freq", df = "cdisc_adae",
  rowvar = "AEBODSYS*AEDECOD",
  tbltype = "type2", colvar = "TRTP", statlist = statlist("n (x.x%)"),
  dotdotdot = "cutoff = 5"
)
# generate_results(table_metadata,
# column_metadata_file = tidytlg_metadata(path)

# Cut records where pct meets threshold for a specific column
table_metadata <- tibble::tibble(
  anbr = "1", func = "nested_freq", df = "cdisc_adae",
  rowvar = "AEBODSYS*AEDECOD",
  tbltype = "type2", colvar = "TRTP", statlist = statlist("n (x.x%)"),
  dotdotdot = "cutoff = 'col1 >= 5'"
)
# generate_results(table_metadata,
# column_metadata_file = tidytlg_metadata(path)

# Frequency and percent for two levels of nesting and sort by descending col1
table_metadata <- tibble::tibble(
  anbr = "1", func = "nested_freq", df = "cdisc_adae",
  rowvar = "AEBODSYS*AEDECOD",
  tbltype = "type2", colvar = "TRTP", statlist = statlist("n (x.x%)"),
  dotdotdot = "descending = 'col1'"
)
# generate_results(table_metadata,
# column_metadata_file = tidytlg_metadata(path)
```
