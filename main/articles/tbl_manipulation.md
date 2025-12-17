# Manipulating \`tbl\` data frame

## Introduction

The `tbl` data frame is the main input to the `gentlg` function for
creating the RTF or HTML outputs. This vignette will show you the `tbl`
structure and how the `tbl` variables are in action for rendering the
RTF or HTML outputs.

### Basic `tbl` data frame

The basic variables of `tbl` includes `label`, `col1`, `col2`, `...`,
`coln`, where

- `label`: row text displayed on the first column of the table
- `col1`: statistic results displayed on the second column of the table
- `col2`: statistic results displayed on the third column of the table.

The example below shows you a very basic `tbl` data frame, and how this
`tbl` is transformed through the `gentlg` function call to create the
HTML output.

``` r
library(dplyr)
library(tidytlg)

tbl <- tibble::tribble(
  ~label, ~col1, ~col2, ~col3,
  "Analysis Set: ITT", "86", "84", "84",
  "Age (Years)", NA, NA, NA,
  "N", "86", "84", "84"
)

knitr::kable(tbl)
```

| label             | col1 | col2 | col3 |
|:------------------|:-----|:-----|:-----|
| Analysis Set: ITT | 86   | 84   | 84   |
| Age (Years)       | NA   | NA   | NA   |
| N                 | 86   | 84   | 84   |

``` r

# render tbl
gentlg(
  huxme = tbl,
  format = "HTML",
  orientation = "landscape",
  opath = ".",
  file = "DEMO1",
  title = "Basic tbl without formatting",
  colheader = c("", "Placebo", "Active 1", "Active 2"),
  print.hux = FALSE,
  wcol = .30
)
#> [[1]]
#>    <div style='border-top :1pt solid; border-bottom :1pt solid; '>             
#>    <div style = "text-indent: -36px; padding-left: 36px;"> DEMO1:              
#>    &emsp; Basic tbl without formatting</div>                                   
#>                                <div              <div              <div        
#>                           style='border-b   style='border-b   style='border-b  
#>                              ottom:1pt         ottom:1pt         ottom:1pt     
#>                           solid'> Placebo   solid'> Active    solid'> Active   
#>                                                    1                 2         
#>    Analysis Set: ITT            86                84                84         
#>    Age (Years)                                                                 
#>    N                            86                84                84         
#>      <div style='border-bottom:1pt solid'> [demo1.html][] 17DEC2025,           
#>                                                                17:27           
#> 
#> Column names: label, col1, col2, col3
```

## Formatting table

The above example does not have any formatting in actions on the table.
To enable formatting, additional variables will need to be created in
the `tbl` data frame.

### Add new row

For inserting a blank line prior to the second row (i.e. `Age (Years)`),
we will need to add the `newrows` variable with `value = 1`.

``` r
tbl <- tbl %>%
  mutate(newrows = case_when(
    label == "Age (Years)" ~ 1,
    TRUE ~ 0
  ))

knitr::kable(tbl)
```

| label             | col1 | col2 | col3 | newrows |
|:------------------|:-----|:-----|:-----|--------:|
| Analysis Set: ITT | 86   | 84   | 84   |       0 |
| Age (Years)       | NA   | NA   | NA   |       1 |
| N                 | 86   | 84   | 84   |       0 |

``` r

# render tbl
gentlg(
  huxme = tbl,
  format = "HTML",
  orientation = "landscape",
  opath = ".",
  file = "DEMO2",
  title = "Adding the variable of newrows",
  colheader = c("", "Placebo", "Active 1", "Active 2"),
  print.hux = FALSE,
  wcol = .30
)
#> [[1]]
#>    <div style='border-top :1pt solid; border-bottom :1pt solid; '>             
#>    <div style = "text-indent: -36px; padding-left: 36px;"> DEMO2:              
#>    &emsp; Adding the variable of newrows</div>                                 
#>                                <div              <div              <div        
#>                           style='border-b   style='border-b   style='border-b  
#>                              ottom:1pt         ottom:1pt         ottom:1pt     
#>                           solid'> Placebo   solid'> Active    solid'> Active   
#>                                                    1                 2         
#>    Analysis Set: ITT            86                84                84         
#>                                                                                
#>    Age (Years)                                                                 
#>    N                            86                84                84         
#>      <div style='border-bottom:1pt solid'> [demo2.html][] 17DEC2025,           
#>                                                                17:27           
#> 
#> Column names: label, col1, col2, col3
```

### Add indentation

For adding one indentation to the `N` row, we will need to add the
`indentme` variable with `value = 1`, which indicates one indentation
(two will result in two indentation, and so on).

``` r
tbl <- tbl %>%
  mutate(indentme = case_when(
    label == "N" ~ 1,
    TRUE ~ 0
  ))

knitr::kable(tbl)
```

| label             | col1 | col2 | col3 | newrows | indentme |
|:------------------|:-----|:-----|:-----|--------:|---------:|
| Analysis Set: ITT | 86   | 84   | 84   |       0 |        0 |
| Age (Years)       | NA   | NA   | NA   |       1 |        0 |
| N                 | 86   | 84   | 84   |       0 |        1 |

``` r

# render tbl
gentlg(
  huxme = tbl,
  format = "HTML",
  orientation = "landscape",
  opath = ".",
  file = "DEMO3",
  title = "Adding the variable of indentme",
  colheader = c("", "Placebo", "Active 1", "Active 2"),
  print.hux = FALSE,
  wcol = .30
)
#> [[1]]
#>    <div style='border-top :1pt solid; border-bottom :1pt solid; '>             
#>    <div style = "text-indent: -36px; padding-left: 36px;"> DEMO3:              
#>    &emsp; Adding the variable of indentme</div>                                
#>                                <div              <div              <div        
#>                           style='border-b   style='border-b   style='border-b  
#>                              ottom:1pt         ottom:1pt         ottom:1pt     
#>                           solid'> Placebo   solid'> Active    solid'> Active   
#>                                                    1                 2         
#>    <div                         86                84                84         
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    17.76px'> Analysis                                                          
#>    Set: ITT                                                                    
#>    <div                                                                        
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    17.76px'>                                                                   
#>    <div                                                                        
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    17.76px'> Age                                                               
#>    (Years)                                                                     
#>    <div                         86                84                84         
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    35.52px'> N                                                                 
#>      <div style='border-bottom:1pt solid'> [demo3.html][] 17DEC2025,           
#>                                                                17:27           
#> 
#> Column names: label, col1, col2, col3
```

### Enable bold font

To enable bold font on the row of `Age (Years)`, the `boldme` variable
with `value = 1` will need to be added.

``` r
tbl <- tbl %>%
  mutate(boldme = case_when(
    label == "Age (Years)" ~ 1,
    TRUE ~ 0
  ))

knitr::kable(tbl)
```

| label             | col1 | col2 | col3 | newrows | indentme | boldme |
|:------------------|:-----|:-----|:-----|--------:|---------:|-------:|
| Analysis Set: ITT | 86   | 84   | 84   |       0 |        0 |      0 |
| Age (Years)       | NA   | NA   | NA   |       1 |        0 |      1 |
| N                 | 86   | 84   | 84   |       0 |        1 |      0 |

``` r

# render tbl
gentlg(
  huxme = tbl,
  format = "HTML",
  orientation = "landscape",
  opath = ".",
  file = "DEMO4",
  title = "Adding the variable of boldme",
  colheader = c("", "Placebo", "Active 1", "Active 2"),
  print.hux = FALSE,
  wcol = .30
)
#> [[1]]
#>    <div style='border-top :1pt solid; border-bottom :1pt solid; '>             
#>    <div style = "text-indent: -36px; padding-left: 36px;"> DEMO4:              
#>    &emsp; Adding the variable of boldme</div>                                  
#>                                <div              <div              <div        
#>                           style='border-b   style='border-b   style='border-b  
#>                              ottom:1pt         ottom:1pt         ottom:1pt     
#>                           solid'> Placebo   solid'> Active    solid'> Active   
#>                                                    1                 2         
#>    <div                         86                84                84         
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    17.76px'> Analysis                                                          
#>    Set: ITT                                                                    
#>    <div                                                                        
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    17.76px'>                                                                   
#>    <div                                                                        
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    17.76px'> Age                                                               
#>    (Years)                                                                     
#>    <div                         86                84                84         
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    35.52px'> N                                                                 
#>      <div style='border-bottom:1pt solid'> [demo4.html][] 17DEC2025,           
#>                                                                17:27           
#> 
#> Column names: label, col1, col2, col3
```

There is another formatting variable called `newpage`, where assigning
`newpage = 1` will start a new page in the output.

## Formatting functions

Besides manually adding the formatting variables to `tbl`, we have
developed several formatting functions to facilitate the formatting
process:

- `add_newrows`: add `newrows` variable to the `tbl` based on `row_type`
- `add_indent`: add `indentme` variable to the `tbl` based on `row_type`
- `add_newpage`: add `newpage` variable to the `tbl` based on
  `row_type`.

The `row_type` variable is created in the `tbl` by calling the `freq`,
`nested_freq`, and `univar` functions, which is used by the above
functions for setting up the formatting variables. The `tbl` example
below is obtained by calling the `univar` function for summarizing the
age statistics in the `CDISC` `ADSL` dataset. After calling the `univar`
function, we create the `anbr` (analysis number) variable as the
identifier of this `tbl` chunk.

``` r
tbl <- cdisc_adsl %>%
  univar(
    colvar = "TRT01PN",
    rowvar = "AGE",
    statlist = statlist(c("N", "MEANSD")),
    decimal = 0,
    row_header = "Age (Years)"
  ) %>%
  mutate(anbr = "01")

knitr::kable(tbl)
```

| label       | 0            | 54          | 81          | row_type | group_level | anbr |
|:------------|:-------------|:------------|:------------|:---------|------------:|:-----|
| Age (Years) |              |             |             | HEADER   |           0 | 01   |
| N           | 5            | 5           | 5           | N        |           0 | 01   |
| Mean (SD)   | 69.6 (14.40) | 75.6 (6.73) | 72.2 (9.23) | VALUE    |           0 | 01   |

The `add_format` function, which incorporates all 3 formatting functions
above (`add_indent`, `add_newrows`, `add_newpage`), can then be applied
to the `tbl` for creating the formatting variables.

``` r
tbl <- cdisc_adsl %>%
  univar(
    colvar = "TRT01PN",
    rowvar = "AGE",
    statlist = statlist(c("N", "MEANSD")),
    decimal = 0,
    row_header = "Age (Years)"
  ) %>%
  mutate(anbr = "01") %>%
  add_format()

knitr::kable(tbl)
```

| label | 0 | 54 | 81 | row_type | anbr | indentme | roworder | newrows | newpage |
|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|
| Age (Years) |  |  |  | HEADER | 01 | 0 | 1 | 0 | 0 |
| N | 5 | 5 | 5 | N | 01 | 1 | 2 | 0 | 0 |
| Mean (SD) | 69.6 (14.40) | 75.6 (6.73) | 72.2 (9.23) | VALUE | 01 | 2 | 3 | 0 | 0 |

``` r

# render tbl
gentlg(
  huxme = tbl,
  tlf = "Table",
  format = "HTML",
  orientation = "landscape",
  opath = ".",
  file = "DEMO5",
  title = "Using row_type to set up indentation",
  colheader = c("", "Placebo", "Active 1", "Active 2"),
  print.hux = FALSE,
  wcol = .30
)
#> [[1]]
#>    <div style='border-top :1pt solid; border-bottom :1pt solid; '>             
#>    <div style = "text-indent: -36px; padding-left: 36px;"> DEMO5:              
#>    &emsp; Using row_type to set up indentation</div>                           
#>                                <div              <div              <div        
#>                           style='border-b   style='border-b   style='border-b  
#>                              ottom:1pt         ottom:1pt         ottom:1pt     
#>                           solid'> Placebo   solid'> Active    solid'> Active   
#>                                                    1                 2         
#>    <div                                                                        
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    17.76px'> Age                                                               
#>    (Years)                                                                     
#>    <div                          5                 5                 5         
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    35.52px'> N                                                                 
#>    <div                    69.6 (14.40)       75.6 (6.73)       72.2 (9.23)    
#>    style='text-indent:                                                         
#>    -17.76px;                                                                   
#>    padding-left:                                                               
#>    53.28px'> Mean (SD)                                                         
#>      <div style='border-bottom:1pt solid'> [demo5.html][] 17DEC2025,           
#>                                                                17:27           
#> 
#> Column names: label, 0, 54, 81
```

The default indentation for each `row_type` is shown below.

| `row_type`       | default indentation |
|------------------|:-------------------:|
| TABLE_BY_HEADER  |          0          |
| BY_HEADER\[1-9\] |          0          |
| HEADER           |          0          |
| N                |          1          |
| VALUE            |          2          |
| NESTED           |          0          |
