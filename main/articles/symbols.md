# Tips & Tricks: Including Symbols, Superscripts, Subscripts & Line Breaks

## Symbols

Symbols can be added to the label column via `UNICODE`. Here is a quick
reference to commonly used symbols.

| Symbol | Textual Description      | Unicode |
|--------|--------------------------|---------|
| ←      | Left arrow               | \u2190  |
| →      | Right arrow              | \u2192  |
| ≤      | Less-than or equal to    | \u2264  |
| ≥      | Greater-than or equal to | \u2265  |
| ≠      | Not equal to             | \u2260  |
| ±      | Plus-minus sign          | \u00b1  |
| α      | Alpha                    | \u03b1  |
| β      | Beta                     | \u03b2  |
| μ      | Mu                       | \u03bc  |
| «      | Non-breaking space       | \u00ab  |

Here is an example call to [`tidytlg::gentlg()`](../reference/gentlg.md)
that will add the symbols to the label column.

``` r
df <- tibble::tibble(
  label = c("\u2264", "\u2265"),
  col1 = c("100", "200")
)

tidytlg::gentlg(df,
  file = "demo"
)
```

## Superscripts and Subscripts

Superscripts and Subscripts can be added to the label column via
`UNICODE`.

``` r
df <- tibble::tibble(
  label = c(
    "This is a superscript a{\\super a}",
    "This is a subscript b{\\sub b}"
  ),
  col1 = c("100", "200")
)

tidytlg::gentlg(df,
  file = "demo"
)
```

Superscripts and Subscripts can be added to the footnotes via `UNICODE`
as well.

``` r
df <- tibble::tibble(
  label = c(
    "This is a superscript a{\\super a}",
    "This is a subscript b{\\sub b}"
  ),
  col1 = c("100", "200")
)

tidytlg::gentlg(df,
  file = "demo",
  footers = "This is a footnote superscript{\\super a}"
)
```

## Inline RTF Line Breaks

Sometimes you need add a line break into your RTF. Inserting `'\\\\\\n'`
into your string will add your line break for you.

``` r
df <- tibble::tibble(
  label = c("Bodysystem \\\n Preferred Term"),
  col1 = c("100")
)

tidytlg::gentlg(df,
  file = "demo"
)
```

If you need a line break followed by a tab, just add in `'\\\\li180'`.

``` r
df <- tibble::tibble(
  label = c("Bodysystem\\\n\\li180Preferred Term"),
  col1 = c("100")
)

tidytlg::gentlg(df,
  file = "demo"
)
```
