# Removes no-op padding from an RTF markup

Removes no-op padding from an RTF markup

## Usage

``` r
remove_zero_padding(rtf)
```

## Arguments

- rtf:

  `character(1)` RTF markup.

## Value

`character(1)` RTF markup with removed zero padding

## Details

Removes no-op padding, which is padding that has a thickness of zero
from an RTF markup.
