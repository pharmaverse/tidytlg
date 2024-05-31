#' @title Adds bottom borders to a huxtable
#'
#' @details
#' Adds bottom borders to a huxtable based on
#' a matrix indicating where the borders should be put.
#'
#' @details
#' This function is responsible for adding bottom borders to a `huxtable` object.
#' It supports borders spanning multiple columns and borders that are under neighbouring,
#' single cells (or merged cells), but separate (see examples).
#'
#' This feature has limitations. Mainly, it does not support both versions of the borders
#' (continuous and separate) on the same line. In such a case, the borders in the resulting
#' RTF look misaligned.
#'
#' @section `border_matrix` details:
#' You mark where the bottom borders should go in the table by passing a matrix.
#' The matrix has to have the same number of columns as the passed `huxtable`
#' and the number of rows lower by one than the passed `huxtable`. Each cell
#' in `border_matrix` corresponds to a cell in `huxtable` (starting from the first row).
#'
#' Internally, the function adds the first row of 0s to `border_matrix` before the execution.
#' At that point, `border_matrix`'s dimensions match `ht`'s dimensions.
#'
#' Table:
#' | foo | bar |
#' | --- | --- |
#' | baz | bim |
#'
#' A border matrix:
#' | 1 | 1 |
#' | - | - |
#' | 0 | 0 |
#'
#' The above border matrix puts a bottom border across the entire first row
#' and no borders in the second row.
#'
#' A border matrix:
#' | 1 | 2 |
#' | - | - |
#' | 0 | 0 |
#'
#' The above border matrix puts one border under the first cell in the first row;
#' and another border (separate from the first one) under the second cell
#' in the first row. The second row stays without any borders.
#'
#' @section
#' Functions transforming the border matrix:
#'
#' The below functions can be passed to [gentlg()]'s
#' `border_fns` argument to modify how `gentlg` renders
#' the borders under the cells.
#'
#' Border functions:
#' * [no_borders()]
#' * [spanning_borders()]
#' * [col_borders()]
#' * [single_border()]
#' * [row_border()]
#'
#' `border_fns` will accept your own, custom functions as long as
#' they adhere to the format.
#' All the functions passed to `border_fns` need to accept two arguments:
#' * the first - the printed huxtable object,
#' * the second - a border matrix.
#'
#' They also must return a matrix interpreted the same way as `border_matrix`
#' passed to `add_bottom_borders` or [gentlg()].
#'
#' @param ht `huxtable` A huxtable object
#' @param border_matrix (optional) `matrix` A matrix indicating where to add the bottom
#' borders. If `NULL`, then no borders are added.
#' @param transform_fns (optional) `list` of `function` A list of functions applied to the
#' `border_matrix`. The functions have to accept two arguments:
#' 1. The `huxtable`.
#' 1. The `border_matrix` with dimentions matching `huxtable`.
#'
#' The functions in the list are applied sequentially to `border_matrix`.
#'
#' @return A huxtable with added borders.
#'
#' @examples
#' border_matrix <- matrix(c(1, 1, 2, 0, 1, 1, 0, 0, 0), nrow = 3, ncol = 3)
#' ht <- huxtable::as_huxtable(
#'   data.frame(a = c(1, 2, 3), b = c("a", "b", "c"), c = c(TRUE, FALSE, TRUE))
#' )
#' # By default adds no borders
#' add_bottom_borders(ht, border_matrix)
#' # Adds spanning borders under cells with text in the second row
#' add_bottom_borders(ht, transform_fns = list(spanning_borders(2)))
#' # Adds spanning borders under cells with text in the second row and a border
#' # under a cell in row 3 and column 3
#' add_bottom_borders(ht, transform_fns = list(spanning_borders(2), single_border(3, 3)))
#'
#' final <- data.frame(
#'   label = c(
#'     "Overall", "Safety Analysis Set",
#'     "Any Adverse event{\\super a}", "- Serious Adverse Event"
#'   ),
#'   Drug_A = c("", "40", "10 (25%)", "0"),
#'   Drug_B = c("", "40", "10 (25%)", "0"),
#'   anbr = c(1, 2, 3, 4),
#'   roworder = c(1, 1, 1, 1),
#'   boldme = c(1, 0, 0, 0),
#'   newrows = c(0, 0, 1, 0),
#'   indentme = c(0, 0, 0, 1),
#'   newpage = c(0, 0, 0, 0)
#' )
#' # Add spanning bottom borders under the cells in the first row
#' gentlg(
#'   huxme = final,
#'   wcol = c(0.70, 0.15, 0.15),
#'   file = "TSFAEX",
#'   colheader = c("", "Drug A", "Drug B"),
#'   title = "This is Amazing Demonstration 1",
#'   footers = c(
#'     "Note: For demonstrative purposes only",
#'     "{\\super a} Subjects are counted once for any given event."
#'   ),
#'   border_fns = list(no_borders, spanning_borders(1))
#' )
#'
#' # Tables with no bottom borders
#' gentlg(
#'   huxme = final,
#'   wcol = c(0.70, 0.15, 0.15),
#'   file = "TSFAEX",
#'   colheader = c("", "Drug A", "Drug B"),
#'   title = "This is Amazing Demonstration 1",
#'   footers = c(
#'     "Note: For demonstrative purposes only",
#'     "{\\super a} Subjects are counted once for any given event."
#'   ),
#'   border_fns = list(no_borders)
#' )
#'
#' # Tables with a border under cell in the 3nd row and 3rd column,
#' # and borders under cells in the first row
#' gentlg(
#'   huxme = final,
#'   wcol = c(0.70, 0.15, 0.15),
#'   file = "TSFAEX",
#'   colheader = c("", "Drug A", "Drug B"),
#'   title = "This is Amazing Demonstration 1",
#'   footers = c(
#'     "Note: For demonstrative purposes only",
#'     "{\\super a} Subjects are counted once for any given event."
#'   ),
#'   border_fns = list(no_borders, spanning_borders(1), single_border(3, 3))
#' )
#'
#' # We discourage, but you can pass the border matrix directly
#' mat <- matrix(rep(0, 8 * 3), ncol = 3, nrow = 8)
#' mat[3, 3] <- 1
#' gentlg(
#'   huxme = final,
#'   wcol = c(0.70, 0.15, 0.15),
#'   file = "TSFAEX",
#'   colheader = c("", "Drug A", "Drug B"),
#'   title = "This is Amazing Demonstration 1",
#'   footers = c(
#'     "Note: For demonstrative purposes only",
#'     "{\\super a} Subjects are counted once for any given event."
#'   ),
#'   bottom_borders = mat, # The same as a single border under 3nd row and 3rd column
#'   border_fns = list()
#' )
#'
#' # clean up.
#' file.remove("tsfaex.rtf")
#'
#' @export
add_bottom_borders <- function(ht, border_matrix = no_borders(ht), transform_fns = list()) {
  if (is.null(border_matrix) && is.null(transform_fns)) {
    return(ht)
  } else if (is.null(border_matrix)) {
    border_matrix <- no_borders(ht)
  }
  for (transform_fn in transform_fns) {
    border_matrix <- transform_fn(ht, border_matrix)
  }
  border_matrix <- rbind(rep(0, ncol(border_matrix)), border_matrix)

  for (row in seq_len(nrow(border_matrix))) {
    # The below enforces an "all or nothing" behaviour on using
    # one of the border styles, because mixing them in the
    # scope of a row gives bad results - the borders
    # are misaligned and of slightly different width
    if (should_use_internal_borders(border_matrix[row, ])) {
      ht <- huxtable::map_contents(
        ht = ht,
        row = row,
        fn = function(ht, rows, cols, current) {
          res <- current
          res[border_matrix[rows, ] != 0] <- sprintf("%s %s", r"{\brdrb\brdrs}", res[border_matrix[rows, ] != 0])
          res
        }
      )
      huxtable::right_padding(ht)[row, border_matrix[row, ] != 0] <- 3
      # This is in fact interpreted as left padding by Word...
      huxtable::top_padding(ht)[row, border_matrix[row, ] != 0] <- 3
      huxtable::top_padding(ht)[row, 1] <- 0
    } else {
      huxtable::bottom_border(ht)[row, border_matrix[row, ] != 0] <- huxtable::brdr(thickness = 0.9, style = "solid")
    }
  }
  ht
}

should_use_internal_borders <- function(row) {
  col_seq <- seq_len(length(row))
  for (col in col_seq[-1]) {
    if (row[col] != 0 && row[col - 1] != 0 && row[col - 1] != row[col]) {
      return(TRUE)
    }
  }
  FALSE
}

#' Removes all borders from the table
#'
#' @param ht `huxtable` object.
#' @param matrix `matrix` of bottom borders. Ignored. Included for the sake
#' of compatibility with the interface of all border mutating functions.
#' @export
#' @family border_functions
no_borders <- function(ht, matrix = NULL) {
  if (is.null(ht)) {
    return(NULL)
  }
  ht_dims <- dim(ht)
  matrix(rep(0, (ht_dims[1] - 1) * ht_dims[2]), nrow = ht_dims[1] - 1, ncol = ht_dims[2])
}

#' Adds borders under cells in a row, excluding the first column.
#'
#' Adds borders under cells that are not empty in a given row,
#' omitting the first column of the row.
#' The borders do not touch each other - they are separate.
#'
#' @param row `numeric` the row of the table
#' @param cols `numeric` the columns of the row to consider
#' @export
#' @family border_functions
spanning_borders <- function(row, cols = c(-1)) {
  function(ht, matrix) {
    last_num <- matrix[row][1]
    r <- trimws(gsub("\\\\keepn\\\\trhdr", "", ht[row + 1, ]))
    for (col in seq_len(length(r))[cols]) {
      if (r[col] != "") {
        if (col == 1 || (r[col - 1] != r[col])) {
          last_num <- last_num + 1
        }
        matrix[row, col] <- last_num
      }
    }
    matrix
  }
}

#' Adds borders under cells in a column
#'
#' @param col `numeric` the column of the table
#' @param rows `numeric` the range of rows to include
#' @export
#' @family border_functions
col_borders <- function(col, rows) {
  rows <- rows + 1
  function(ht, matrix) {
    for (row in rows) {
      matrix[row, col] <- 1
    }
    matrix
  }
}

#' Adds a border under a cell
#'
#' @param row `numeric` the row of the cell
#' @param col `numeric` the column of the cell
#' @export
#' @family border_functions
single_border <- function(row, col) {
  function(ht, matrix) {
    border <- 1
    if (col > 1) {
      border <- border + matrix[row, col - 1]
    }
    if (col < ncol(matrix)) {
      border <- border + matrix[row, col + 1]
    }
    matrix[row, col] <- border
    matrix
  }
}

#' Adds a continuous bottom border under a row
#'
#' @param row `numeric` the row of the table
#' @export
#'
#' @family border_functions
row_border <- function(row) {
  function(ht, matrix) {
    matrix[row, ] <- 1
    matrix
  }
}

#' Adds bottom borders according to the old formatting
#'
#' This function is vectorized over its arguments.
#'
#' @param ht the hux object passed to [gentlg()]
#' @param colspan `colspan` argument to [gentlg()]
#' @param colheader `colheader` argument to [gentlg()]
#' @param tlf `character` type of the output
#' @return a bottom border matrix for use with [add_bottom_borders()] or `NULL`
#' if `ht` is `NULL`
#'
#' @keywords internal
old_format <- function(ht, colspan, colheader, tlf) {
  if (is.null(ht)) {
    return(NULL)
  }

  border_matrix <- no_borders(ht)

  # Attach borders to colspan and colheader rows
  colspan_length <- length(colspan)
  if (is_listing(tlf)) {
    for (row in 1:(1 + colspan_length)) {
      border_matrix <- spanning_borders(
        row,
        cols = seq_len(ncol(border_matrix))
      )(ht, border_matrix)
    }
  } else {
    for (row in 1:(1 + colspan_length)) {
      border_matrix <- spanning_borders(
        row,
        cols = c(-1)
      )(ht, border_matrix)
    }
  }

  border_matrix
}
