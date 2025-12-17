# Metadata describing table column layouts

This is used by `tlgsetup` to prepare you input data to support the
desired column layout.

## Usage

``` r
column_metadata
```

## Format

A data frame with one row per column for each table type and six
variables:

- `tbltype`:

  identifier used to group a table column layout

- `coldef`:

  distinct variable values used, typically numeric and typically a
  treatment or main effect variable, think `TRT01PN`

- `decode`:

  decode of `coldef` that will display as a column header in the table

- `span1`:

  spanning header to display across multiple columns

- `span2`:

  spanning header to display across multiple columns, second level

- `span3`:

  spanning header to display across multiple columns, third level
