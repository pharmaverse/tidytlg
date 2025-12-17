# Setup data to support the specified column type

`tlgsetup` is useful for preprocessing total columns and columns
composed of other columns. `tlgsetup` is called internally by
[`generate_results()`](generate_results.md) and can be run manually for
custom tables.

## Usage

``` r
tlgsetup(
  df,
  var,
  column_metadata_file = NULL,
  column_metadata = NULL,
  tbltype = NULL
)
```

## Arguments

- df:

  dataframe of records for analysis.

- var:

  character vector that identifies the numeric column/treatment
  variable.

- column_metadata_file:

  A file containing the column metadata. Read in with
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html).
  If a `column_metadata` dataframe is passed in too, this is ignored.

- column_metadata:

  A dataframe containing the column metadata. This will be used in place
  of `column_metadata_file`.

- tbltype:

  A value used to subset the `column_metadata`, both this and the file
  requirements are needed to bind the data to the table.

## Value

dataframe with observations added to support the column type as well as
the factor variable `colnbr` which is used as our new column summary
variable. Regardless of if a `coldef` exists in data, the column will
exist in the table.

## Examples

``` r
df <-
  tibble::tribble(
    ~TRT01AN, ~USUBJID,
    0, "A",
    54, "B",
    81, "C"
  )

tlgsetup(df, "TRT01AN", column_metadata = column_metadata)
#> # A tibble: 16 × 4
#>    tbltype colnbr TRT01AN USUBJID
#>    <chr>   <fct>  <chr>   <chr>  
#>  1 type1   col1   0       A      
#>  2 type1   col2   54      B      
#>  3 type1   col3   81      C      
#>  4 type2   col4   0       A      
#>  5 type2   col5   54      B      
#>  6 type2   col6   81      C      
#>  7 type2   col7   54      B      
#>  8 type2   col7   81      C      
#>  9 type3   col8   0       A      
#> 10 type3   col9   54      B      
#> 11 type3   col10  81      C      
#> 12 type3   col11  54      B      
#> 13 type3   col11  81      C      
#> 14 type3   col12  0       A      
#> 15 type3   col12  54      B      
#> 16 type3   col12  81      C      

# Using a dataframe of column metadata
column_metadata <-
  tibble::tribble(
    ~tbltype, ~coldef, ~decode,                ~span1,
    "type1",  "0",     "Placebo",              "",
    "type1",  "54",    "Low Dose",             "Xanomeline",
    "type1",  "81",    "High Dose",            "Xanomeline",
    "type1",  "54+81", "Total Xanomeline",     ""
  )

tlgsetup(df, "TRT01AN", column_metadata = column_metadata)
#> # A tibble: 5 × 4
#>   tbltype colnbr TRT01AN USUBJID
#>   <chr>   <fct>  <chr>   <chr>  
#> 1 type1   col1   0       A      
#> 2 type1   col2   54      B      
#> 3 type1   col3   81      C      
#> 4 type1   col4   54      B      
#> 5 type1   col4   81      C      
```
