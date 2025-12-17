# Column Metadata & How tidytlg Sets Up Your Data Using \`tlgsetup\`

``` r
library(dplyr)
library(tidytlg)
```

The `tlgsetup` helper function was created to support generating
different column structures for our outputs. This allows us to specify
column types once that can be used across multiple outputs. This
metadata can be hardcoded as well (please see the code below), or pulled
from another source like a dataframe, file or database.

Here we see an example of the column metadata which defines our columns,
how to combine columns, column labels and spanning headers across
columns.

``` r
column_metadata <-
  tibble::tribble(
    ~tbltype, ~coldef, ~decode,                ~span1,
    "type1",  "0",     "Placebo",              "",
    "type1",  "54",    "Low Dose",             "Xanomeline",
    "type1",  "81",    "High Dose",            "Xanomeline",
    "type1",  "54+81", "Total Xanomeline",     ""
  )
column_metadata
#> # A tibble: 4 × 4
#>   tbltype coldef decode           span1       
#>   <chr>   <chr>  <chr>            <chr>       
#> 1 type1   0      Placebo          ""          
#> 2 type1   54     Low Dose         "Xanomeline"
#> 3 type1   81     High Dose        "Xanomeline"
#> 4 type1   54+81  Total Xanomeline ""
```

In this example, we would summarize `Placebo`, `Xanomeline Low Dose`,
and `Xanomeline High Dose` which are already available in the data. The
helper function will add the observations to the data for the
`Total Xanomeline` column based off of `coldef`. So for our `adsl` data,
we will add a spanning header for `Xanomeline` and a total `Xanomeline`
column which is a combination of the `Xanomeline Low Dose` and
`Xanomeline High Dose`.

In addition, the helper function will add the factor variable `colnbr`
which is used as our new column summary variable. Note that our column
summary variable has been converted to a factor. This is required and
allows us to define column order as well as label in one variable.

Let’s read in `adsl` and check the dimensions.

``` r
data("cdisc_adsl")
adsl <- cdisc_adsl %>%
  filter(ITTFL == "Y") %>%
  select(USUBJID, TRT01PN, TRT01P, ITTFL, SEX, RACE, AGE)
glimpse(adsl)
#> Rows: 15
#> Columns: 7
#> $ USUBJID <chr> "01-701-1015", "01-701-1023", "01-701-1047", "01-701-1118", "0…
#> $ TRT01PN <dbl> 0, 0, 0, 0, 0, 54, 54, 54, 54, 54, 81, 81, 81, 81, 81
#> $ TRT01P  <chr> "Placebo", "Placebo", "Placebo", "Placebo", "Placebo", "Xanome…
#> $ ITTFL   <chr> "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y…
#> $ SEX     <chr> "F", "M", "F", "M", "M", "M", "M", "F", "M", "M", "M", "F", "F…
#> $ RACE    <chr> "WHITE", "WHITE", "WHITE", "WHITE", "WHITE", "WHITE", "WHITE",…
#> $ AGE     <dbl> 63, 64, 85, 52, 84, 74, 68, 81, 84, 71, 71, 77, 81, 75, 57
```

    #> [1] "Dimensions prior to the tlgsetup call are 15 rows and 7 columns."

Now let’s pass the `adsl` data through our `tlgsetup` function to add
observations to support the `type1` column structure and check out the
dimensions.

``` r
setup_table <- tlgsetup(adsl,
  var = "TRT01PN",
  column_metadata = column_metadata
)
glimpse(setup_table)
#> Rows: 25
#> Columns: 9
#> $ tbltype <chr> "type1", "type1", "type1", "type1", "type1", "type1", "type1",…
#> $ colnbr  <fct> col1, col1, col1, col1, col1, col2, col2, col2, col2, col2, co…
#> $ USUBJID <chr> "01-701-1015", "01-701-1023", "01-701-1047", "01-701-1118", "0…
#> $ TRT01PN <chr> "0", "0", "0", "0", "0", "54", "54", "54", "54", "54", "81", "…
#> $ TRT01P  <chr> "Placebo", "Placebo", "Placebo", "Placebo", "Placebo", "Xanome…
#> $ ITTFL   <chr> "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y…
#> $ SEX     <chr> "F", "M", "F", "M", "M", "M", "M", "F", "M", "M", "M", "F", "F…
#> $ RACE    <chr> "WHITE", "WHITE", "WHITE", "WHITE", "WHITE", "WHITE", "WHITE",…
#> $ AGE     <dbl> 63, 64, 85, 52, 84, 74, 68, 81, 84, 71, 71, 77, 81, 75, 57, 74…
```

    #> [1] "Dimensions after to the rmtsetup call are 25 rows and 9 columns."

Here we see we have added two new variables, `colnbr` and `tbltype`,
which will now be used as our treatment variable when generating our
results.

If we take a look at the observation counts for `colnbr`, we see the 168
records added to the data support `Total Xanomeline` as we expected!

``` r
setup_table %>%
  group_by(colnbr) %>%
  count()
#> # A tibble: 4 × 2
#> # Groups:   colnbr [4]
#>   colnbr     n
#>   <fct>  <int>
#> 1 col1       5
#> 2 col2       5
#> 3 col3       5
#> 4 col4      10
```
