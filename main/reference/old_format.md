# Adds bottom borders according to the old formatting

This function is vectorized over its arguments.

## Usage

``` r
old_format(ht, colspan, colheader, tlf)
```

## Arguments

- ht:

  the `hux` object passed to [`gentlg()`](gentlg.md)

- colspan:

  `colspan` argument to [`gentlg()`](gentlg.md)

- colheader:

  `colheader` argument to [`gentlg()`](gentlg.md)

- tlf:

  `character` type of the output

## Value

a bottom border matrix for use with
[`add_bottom_borders()`](add_bottom_borders.md) or `NULL` if `ht` is
`NULL`
