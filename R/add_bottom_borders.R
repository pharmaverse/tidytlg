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
#' @details `border_matrix` details
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
#' @param ht `huxtable` A huxtable object
#' @param border_matrix (optional) `matrix` A matrix indicating where to add the bottom
#' borders.
#' @param transform_fns (optional) `list` of `function` A list of functions applied to the
#' `border_matrix`. The functions have to accept two arguments:
#' 1. The `huxtable`.
#' 1. The `border_matrix` with dimentions matching `huxtable`.
#'
#' The function have to return a matrix with the same dimensions as `huxtable`
#' indicating where to put the borders. The functions in the list are applied
#' sequentially to `border_matrix`.
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
#' @export
add_bottom_borders <- function(ht, border_matrix = no_borders(ht), transform_fns = list()) {
  if (is.null(border_matrix) && is.null(transform_fns)) {
    return(ht)
  } else if (is.null(border_matrix)) {
    border_matrix <- no_borders(ht)
  }
  border_matrix <- rbind(rep(0, ncol(border_matrix)), border_matrix)
  for (transform_fn in transform_fns) {
    border_matrix <- transform_fn(ht, border_matrix)
  }

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
      if (border_matrix[row, ncol(border_matrix)]) {
        ht <- huxtable::set_right_padding(ht, row, ncol(border_matrix), 0)
      }
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

no_borders <- function(ht) {
  if (is.null(ht)) {
    return(NULL)
  }
  ht_dims <- dim(ht)
  matrix(rep(0, (ht_dims[1] - 1) * ht_dims[2]), nrow = ht_dims[1] - 1, ncol = ht_dims[2])
}

#' Adds borders under cells in a row
#'
#' Adds borders under cells that are not empty.
#' The borders do not touch each other - they are separate.
#'
#' @param row `numeric` the row of the table
#' @export
spanning_borders <- function(row) {
  function(ht, matrix) {
    last_num <- matrix[row][1]
    for (col in seq_len(ncol(ht))[-1]) {
      if (ht[row, col] != "") {
        if (ht[row, col - 1] != ht[row, col]) {
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
#' Adds borders under cells that are not empty in a column
#'
#' @param col `numeric` the column of the table
#' @param rows `numeric` the range of rows to include
#' @export
col_borders <- function(col, rows) {
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
row_border <- function(row) {
  function(ht, matrix) {
    matrix[row, ] <- 1
    matrix
  }
}
