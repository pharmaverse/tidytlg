# Replace NA with ""

Used to swap in "" for by variables so the headers sort correctly to the
top

## Usage

``` r
replace_na_with_blank(x)
```

## Arguments

- x:

  variable to check for NA and replace with "".

## Value

x with NAs replaced with "". Factors will add "" as the first level.

## Examples

``` r
replace_na_with_blank(c("a", "b", NA))
#> [1] "a" "b" "" 

replace_na_with_blank(factor(c("a", "b", NA), levels = c("a", "b")))
#> [1] a b  
#> Levels:  a b
```
