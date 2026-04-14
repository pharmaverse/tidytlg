# Inserts empty rows into a data frame

Inserts empty rows into a data frame

## Usage

``` r
insert_empty_rows(huxme, newrows = huxme$newrows)
```

## Arguments

- huxme:

  `data.frame` The input data frame.

- newrows:

  `integer` A numeric vector specifying where the new rows should be
  inserted.

## Value

A data frame with added new empty rows.

## Details

`gentlg` allows for formatting the input table based on formatting
columns (see [`gentlg()`](gentlg.md)). One of the formatting mechanisms
is empty row insertion. This function inserts the empty rows based on
the `newrows` column in the data frame. The new rows are inserted before
the rows with value `1` in the `newrows` column.

## Examples

``` r
df <- iris[1:10, ]
df <- as.data.frame(apply(df, 2, as.character))
df$newrows <- c(0, 1, 0, 1, 1, 0, 0, 0, 0, 0)
insert_empty_rows(df)
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species newrows
#> 1           5.1         3.5          1.4         0.2  setosa       0
#> 2                                                                  0
#> 3           4.9         3.0          1.4         0.2  setosa       0
#> 4           4.7         3.2          1.3         0.2  setosa       0
#> 5                                                                  0
#> 6           4.6         3.1          1.5         0.2  setosa       0
#> 7                                                                  0
#> 8           5.0         3.6          1.4         0.2  setosa       0
#> 9           5.4         3.9          1.7         0.4  setosa       0
#> 10          4.6         3.4          1.4         0.3  setosa       0
#> 11          5.0         3.4          1.5         0.2  setosa       0
#> 12          4.4         2.9          1.4         0.2  setosa       0
#> 13          4.9         3.1          1.5         0.1  setosa       0
```
