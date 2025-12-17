# Adds bottom borders to a `huxtable`

Adds bottom borders to a `huxtable`

## Usage

``` r
add_bottom_borders(ht, border_matrix = no_borders(ht), transform_fns = list())
```

## Arguments

- ht:

  `huxtable` A `huxtable` object

- border_matrix:

  (optional) `matrix` A matrix indicating where to add the bottom
  borders. If `NULL`, then no borders are added.

- transform_fns:

  (optional) `list` of `function` A list of functions applied to the
  `border_matrix`. The functions have to accept two arguments:

  1.  The `huxtable`.

  2.  The `border_matrix` with dimensions matching `huxtable`.

  The functions in the list are applied sequentially to `border_matrix`.

## Value

A `huxtable` with added borders.

## Details

Adds bottom borders to a `huxtable` based on a matrix indicating where
the borders should be put.

This function is responsible for adding bottom borders to a `huxtable`
object. It supports borders spanning multiple columns and borders that
are under neighbouring, single cells (or merged cells), but separate
(see examples).

This feature has limitations. Mainly, it does not support both versions
of the borders (continuous and separate) on the same line. In such a
case, the borders in the resulting RTF look misaligned.

## `border_matrix` details

You mark where the bottom borders should go in the table by passing a
matrix. The matrix has to have the same number of columns as the passed
`huxtable` and the number of rows lower by one than the passed
`huxtable`. Each cell in `border_matrix` corresponds to a cell in
`huxtable` (starting from the first row).

Internally, the function adds the first row of 0s to `border_matrix`
before the execution. At that point, `border_matrix`'s dimensions match
`ht`'s dimensions.

Table:

|       |       |
|-------|-------|
| foo   | bar   |
| `baz` | `bim` |

A border matrix:

|     |     |
|-----|-----|
| 1   | 1   |
| 0   | 0   |

The above border matrix puts a bottom border across the entire first row
and no borders in the second row.

A border matrix:

|     |     |
|-----|-----|
| 1   | 2   |
| 0   | 0   |

The above border matrix puts one border under the first cell in the
first row; and another border (separate from the first one) under the
second cell in the first row. The second row stays without any borders.

## Functions transforming the border matrix

The below functions can be passed to [`gentlg()`](gentlg.md)'s
`border_fns` argument to modify how `gentlg` renders the borders under
the cells.

Border functions:

- [`no_borders()`](no_borders.md)

- [`spanning_borders()`](spanning_borders.md)

- [`col_borders()`](col_borders.md)

- [`single_border()`](single_border.md)

- [`row_border()`](row_border.md)

`border_fns` will accept your own, custom functions as long as they
adhere to the format. All the functions passed to `border_fns` need to
accept two arguments:

- the first - the printed `huxtable` object,

- the second - a border matrix.

They also must return a matrix interpreted the same way as
`border_matrix` passed to `add_bottom_borders` or
[`gentlg()`](gentlg.md).

## Examples

``` r
border_matrix <- matrix(c(1, 1, 2, 0, 1, 1, 0, 0, 0), nrow = 3, ncol = 3)
ht <- huxtable::as_huxtable(
  data.frame(a = c(1, 2, 3), b = c("a", "b", "c"), c = c(TRUE, FALSE, TRUE))
)
# By default adds no borders
add_bottom_borders(ht, border_matrix)
#>                                  a   b                c      
#>                                  1   a                TRUE   
#>                   ──────────────────                         
#>                                  2   b                FALSE  
#>                   ───────────────────────────────────        
#>                     \brdrb\brdrs 3   \brdrb\brdrs c   TRUE   
#> 
#> Column names: a, b, c
# Adds spanning borders under cells with text in the second row
add_bottom_borders(ht, transform_fns = list(spanning_borders(2)))
#>                       a   b                c                   
#>                       1   a                TRUE                
#>                       2   \brdrb\brdrs b   \brdrb\brdrs FALSE  
#>                       3   c                TRUE                
#> 
#> Column names: a, b, c
# Adds spanning borders under cells with text in the second row and a border
# under a cell in row 3 and column 3
add_bottom_borders(ht, transform_fns = list(spanning_borders(2), single_border(3, 3)))
#>                       a   b                c                   
#>                       1   a                TRUE                
#>                       2   \brdrb\brdrs b   \brdrb\brdrs FALSE  
#>                       3   c                TRUE                
#>                                          ──────────────────────
#> 
#> Column names: a, b, c

final <- data.frame(
  label = c(
    "Overall", "Safety Analysis Set",
    "Any Adverse event{\\super a}", "- Serious Adverse Event"
  ),
  Drug_A = c("", "40", "10 (25%)", "0"),
  Drug_B = c("", "40", "10 (25%)", "0"),
  anbr = c(1, 2, 3, 4),
  roworder = c(1, 1, 1, 1),
  boldme = c(1, 0, 0, 0),
  newrows = c(0, 0, 1, 0),
  indentme = c(0, 0, 0, 1),
  newpage = c(0, 0, 0, 0)
)
# Add spanning bottom borders under the cells in the first row
gentlg(
  huxme = final,
  wcol = c(0.70, 0.15, 0.15),
  file = "TSFAEX",
  colheader = c("", "Drug A", "Drug B"),
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\super a} Subjects are counted once for any given event."
  ),
  border_fns = list(no_borders, spanning_borders(1))
)
#> Warning: path[1]="-": No such file or directory

# Tables with no bottom borders
gentlg(
  huxme = final,
  wcol = c(0.70, 0.15, 0.15),
  file = "TSFAEX",
  colheader = c("", "Drug A", "Drug B"),
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\super a} Subjects are counted once for any given event."
  ),
  border_fns = list(no_borders)
)
#> Warning: path[1]="-": No such file or directory

# Tables with a border under cell in the 3nd row and 3rd column,
# and borders under cells in the first row
gentlg(
  huxme = final,
  wcol = c(0.70, 0.15, 0.15),
  file = "TSFAEX",
  colheader = c("", "Drug A", "Drug B"),
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\super a} Subjects are counted once for any given event."
  ),
  border_fns = list(no_borders, spanning_borders(1), single_border(3, 3))
)
#> Warning: path[1]="-": No such file or directory

# We discourage, but you can pass the border matrix directly
mat <- matrix(rep(0, 8 * 3), ncol = 3, nrow = 8)
mat[3, 3] <- 1
gentlg(
  huxme = final,
  wcol = c(0.70, 0.15, 0.15),
  file = "TSFAEX",
  colheader = c("", "Drug A", "Drug B"),
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\super a} Subjects are counted once for any given event."
  ),
  bottom_borders = mat, # The same as a single border under 3nd row and 3rd column
  border_fns = list()
)
#> Warning: path[1]="-": No such file or directory

# clean up.
file.remove("tsfaex.rtf")
#> [1] TRUE
```
