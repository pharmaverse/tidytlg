# Inserts empty rows into a data frame

Inserts empty rows into a data frame

## Usage

``` r
insert_empty_rows(huxme, newrows = huxme$newrows)
```

## Arguments

- huxme:

  `data.frame` The input data frame.

## Value

A data frame with added new empty rows.

## Details

`gentlg` allows for formatting the input table based on formatting
columns (see [`gentlg()`](gentlg.md)). One of the formatting mechanisms
is empty row insertion. This function inserts the empty rows based on
the `newrows` column in the data frame. The new rows are inserted before
the rows with value `1` in the `newrows` column.
