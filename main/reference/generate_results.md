# Generate Results using Table and Column Metadata

Generate Results using Table and Column Metadata

## Usage

``` r
generate_results(
  table_metadata,
  column_metadata_file = NULL,
  column_metadata = NULL,
  env = parent.frame(),
  tbltype = NULL,
  add_count = FALSE
)
```

## Arguments

- table_metadata:

  a data frame containing table metadata (see ?table_metadata for
  details)

- column_metadata_file:

  An excel file with the data for `column_metadata`. The file is read in
  with
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html).
  Should not be used with `column_metadata` argument. Results in a data
  frame containing the column metadata that is passed to `tlgsetup` (see
  [`tlgsetup()`](tlgsetup.md) for details). If a `column_metadata` data
  frame is passed in too, this is ignored.

- column_metadata:

  A data frame containing the column metadata. This will be used in
  place of `column_metadata_file`.

- env:

  environment to find data frame specified in the table metadata
  (defaults to parent environment).

- tbltype:

  If used, this will be used to subset the `column_metadata` based on
  the `tbltype` column.

- add_count:

  Passed to [`bind_table()`](bind_table.md) should counts be added for
  `tablebyvars`?

## Value

dataframe of results
