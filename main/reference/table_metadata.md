# Metadata describing the data, functions and arguments needed to produce your results.

Metadata describing the data, functions and arguments needed to produce
your results.

## Usage

``` r
table_metadata
```

## Format

A data frame with one row per function call and 16 variables:

- `func`:

  name of the function you wish to call

- `df`:

  data frame to pass to the function call

- `subset`:

  filter `df` records, this is passed directly to filter, ex.
  `"AESER == 'Y'"`

- `rowvar`:

  variable being summarized that will pass to the function call

- `rowtext`:

  row label text to display in the table

- `row_header`:

  header text to display above row summary

- `statlist`:

  list of statistics in the analysis, see individual functions for what
  is available per function (e.g. `"N, n (x.x)"`)

- `colvar`:

  variable used to determine the columns of the table

- `decimal`:

  decimal precision

- `rowbyvar`:

  repeat `rowvar` summary by this variable/s, comma separated for
  multiple (e.g. `"ETHNIC, AGEGR1"`)

- `tablebyvar`:

  repeat the entire table summary by this variable/s, comma separated
  for multiple (e.g. `"ETHNIC, AGEGR1"`)

- `denom_df`:

  used to set denominators if `df` does not contain all required records
