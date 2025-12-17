# Output a `tidytlg` table

Generate and output a `huxtable` with desired properties During this
function call, the `huxtable` can be written to an RTF or displayed in
HTML. `gentlg` is vectorized, see parameter descriptions to learn for
which arguments.

## Usage

``` r
gentlg(
  huxme = NULL,
  tlf = "Table",
  format = "rtf",
  colspan = NULL,
  idvars = NULL,
  plotnames = NULL,
  plotwidth = NULL,
  plotheight = NULL,
  wcol = 0.45,
  orientation = "portrait",
  opath = ".",
  title_file = NULL,
  file = NULL,
  title = NULL,
  footers = NULL,
  print.hux = TRUE,
  watermark = NULL,
  colheader = NULL,
  pagenum = FALSE,
  bottom_borders = "old_format",
  border_fns = list(),
  alignments = list()
)
```

## Arguments

- huxme:

  (optional) For tables and listings, A list of input dataframes
  containing all columns of interest. For graphs, either `NULL` or a
  list of `ggplot` objects. Vectorized.

- tlf:

  (optional) String, representing the output choice. Choices are
  `"Table"` `"Listing"` `"Figure"`. Abbreviations are allowed e.g. `"T"`
  for Table. Strings can be either upper- or lowercase. Vectorized.
  (Default = `"Table"`)

- format:

  (optional) String, representing the output format. Choices are `"rtf"`
  and `"html"`. Strings can be either upper- or lowercase.(Default =
  `"rtf"`)

- colspan:

  (optional) A list of character vectors representing the spanning
  headers to be used for the table or listing. The first vector
  represents the top spanning header, etc. Each vector should have a
  length equal to the number of columns in the output data frame. A
  spanning header is identified through the use of the same column name
  in adjacent elements. Vectorized.

- idvars:

  (optional) Character vector defining the columns of a listing where
  repeated values should be removed recursively. If `NULL` then all
  column names are used in the algorithm. If `NA`, then the listing
  remains as is.

- plotnames:

  (optional) Character vector containing the names of the PNG files,
  with their extension to be incorporated for figure outputs. The PNG
  files need to be located in the path defined by the parameter `opath`.

- plotwidth:

  (optional) Numerical value that indicates the plot width in cm for
  figure outputs. (Default = 6)

- plotheight:

  (optional) Numerical value that indicates the plot height in cm for
  figure outputs. (Default = 5)

- wcol:

  (optional) Can be a single numerical value that represents the width
  of the first column or a vector, specifying the lengths of all columns
  in the final table or listing.  
  When a single numerical value is used, this will be taken as the
  column width for the first column. The other columns will be equally
  spaced across the remainder of the available space. Alternatively, a
  vector can be used to represent the widths of all columns in the final
  output. The order of the arguments needs to correspond to the order of
  the columns in the `huxme` dataset, that are not part of the
  formatting algorithms (e.g. `anbr`, `roworder`, `newpage`, `newrow`,
  `indentme`, `boldme`, `by_value`, `by_order`). The sum of the widths
  in the vector needs to be less or equal to one. When `format="HTML"`
  `wcol` can take only one value, the width of the first column.
  (Default = 0.45).

- orientation:

  (optional) String: "portrait" or "landscape". (Default = "portrait")

- opath:

  (optional) File path pointing to the output files (including PNG files
  for graphs). (Default = ".").

- title_file:

  An Excel file that will be read in with
  [`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html)
  to be used as the `title` and `footers` argument. The use of `title`
  or `footers` will override the values passed by this argument. The
  file should be either an `xls` or `xlsx` file with the columns
  `TABLE ID`, `IDENTIFIER`, and `TEXT`. The file will be read in, subset
  to where the `tblid` matches the `tlf` argument, and identifiers with
  'title' or 'footnote' will be used to populate the table.

- file:

  (required) String. Output identifier. File name will be adjusted to be
  lowercase and have `-` and `_` removed, this will not affect table
  title.

- title:

  (required) String. Title of the output. Vectorized.

- footers:

  (optional) Character vector, containing strings of footnotes to be
  included. Vectorized.

- print.hux:

  (optional) Logical, indicating whether the output should be printed to
  RTF `('format' = "rtf")` or displayed as HTML `('format' = "HTML")`.
  (Default = `TRUE`). Note that RTF is written using `quick_rtf_jnj()`
  function and that the HTML is displayed via the
  [`huxtable::print_html`](https://hughjonesd.github.io/huxtable/reference/to_html.html)
  function.

- watermark:

  (optional) String containing the desired watermark for RTF outputs.
  Vectorized.

- colheader:

  (optional) Character vector that contains the column labels for a
  table or listing. Default uses the column labels of `huxme`.
  Vectorized.

- pagenum:

  (optional) Logical. When true page numbers are added on the right side
  of the footer section in the format page `x/y`. Vectorized. (Default =
  `FALSE`).

- bottom_borders:

  (optional) Matrix or `"old_format"`. A matrix indicating where to add
  the bottom borders. Vectorized. See
  [`add_bottom_borders()`](add_bottom_borders.md) for more information.
  If `"old_format"`, then borders are added to the `colspan` and
  `colheader` rows. (Default = "old_format").

- border_fns:

  (optional) List. A list of functions that transform the matrix passed
  to `bottom_borders`. Vectorized. See
  [`add_bottom_borders()`](add_bottom_borders.md) for more information.

- alignments:

  (optional) List of named lists. Vectorized. (Default =
  [`list()`](https://rdrr.io/r/base/list.html)) Used to specify
  individual column or cell alignments. Each named list contains `row`,
  `col`, and `value`, which are passed to
  [`huxtable::set_align()`](https://hughjonesd.github.io/huxtable/reference/align.html)
  to set the alignments.

## Value

A list of formatted `huxtables` with desired properties for output to an
RTF or HTML.

## `Huxme` Details

For tables and listings, formatting of the output can be dictated
through the formatting columns (`newrows`, `indentme`, `boldme`,
`newpage`), present in the input dataframe. The final `huxtable` will
display all columns of the input dataframe, except any recognized
formatting or sorting columns. For tables, the algorithm uses the column
`label` as first column. The remaining columns are treated as summary
columns. For graphs, you can pass a `ggplot` object directly into
`huxme` and `gentlg` will save a PNG with with
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
and output an RTF.

## References

<https://github.com/hughjonesd/huxtable>

## Author

Steven Haesendonckx <shaesen2@its.jnj.com>

Pelagia Alexandra Papadopoulou <ppapadop@its.jnj.com>

## Examples

``` r
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

# Produce output in rtf format
gentlg(
  huxme = final,
  wcol = c(0.70, 0.15, 0.15),
  file = "TSFAEX",
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\super a} Subjects are counted once for any given event."
  )
)
#> Warning: Column header not used; 9 column header provided, but data contain 3 columns
#> Warning: path[1]="-": No such file or directory

# Pass in column headers instead of using variable name
gentlg(
  huxme = final,
  wcol = c(0.70, 0.15, 0.15),
  file = "TSFAEX",
  colheader = c("", "Drug A", "Drug B"),
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\super a} Subjects are counted once for any given event."
  )
)
#> Warning: path[1]="-": No such file or directory

# Add spanning bottom borders under the cells in the second row
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
  border_fns = list(spanning_borders(2))
)
#> Warning: path[1]="-": No such file or directory

# Use a watermark
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
  watermark = "Confidential"
)
#> Warning: path[1]="-": No such file or directory

# Set alignments
gentlg(
  huxme = final,
  file = "TSFAEX",
  alignments = list(
    # Align the second column to the left
    list(row = 1:7, col = 2, value = "left"),

    # Align cell "Drug: B" to the right
    list(row = 2, col = 3, value = "right")
  )
)
#> Warning: Column header not used; 9 column header provided, but data contain 3 columns
#> Warning: path[1]="-": No such file or directory

# Produce output in HTML format
hux <- gentlg(
  huxme = final,
  file = "TSFAEX",
  colheader = c("", "Drug A", "Drug B"),
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\super a} Subjects are counted once for any given event."
  ),
  watermark = "Confidential",
  format = "HTML",
  print.hux = FALSE
)
#> Warning: path[1]="-": No such file or directory

# Export to HTML page
huxtable::quick_html(hux, file = "TSFAEX.html", open = FALSE)

# clean up.
file.remove("TSFAEX.html", "tsfaex.rtf")
#> [1] TRUE TRUE
```
