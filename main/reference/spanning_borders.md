# Adds borders under cells in a row, excluding the first column.

Adds borders under cells that are not empty in a given row, omitting the
first column of the row. The borders do not touch each other - they are
separate.

## Usage

``` r
spanning_borders(row, cols = c(-1))
```

## Arguments

- row:

  `numeric` the row of the table

- cols:

  `numeric` the columns of the row to consider

## See also

Other border_functions: [`col_borders()`](col_borders.md),
[`no_borders()`](no_borders.md), [`row_border()`](row_border.md),
[`single_border()`](single_border.md)
