# Replace leading whitespaces with left indentation RTF markup

Replace leading whitespaces with left indentation RTF markup

## Usage

``` r
replace_lead_whitespaces_ind(x)
```

## Arguments

- x:

  `character(1)` a string to replace the leading whitespaces.

## Value

`character(1)` RTF markup with leading whitespaces replaced.

## Details

The following function receives a string 'x' and returns a modified
string where every leading whitespace is replaced with 90 twips (0.0625
inches) left indentation RTF markup. If the input does not start with a
whitespace, the string is returned as is.

## Examples

``` r
tidytlg:::replace_lead_whitespaces_ind("    this is x")
#> [1] "\\intbl\\li360\\fi0 this is x"
# [1] "\intbl\li360\fi0 this is x"
tidytlg:::replace_lead_whitespaces_ind("this is x")
#> [1] "this is x"
# [1] "this is x"
```
