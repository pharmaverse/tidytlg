#' Adds bottom borders to a huxtable
#'
#' Adds bottom borders to a huxtable based on
#' a matrix indicating where the borders should be put.
#'
#' @param ht `huxtable` A huxtable object
#' @param border_matrix `matrix` A matrix indicating where to add the bottom
#'   borders
#'
#' @return A huxtable with added borders
#'
#' @example
#' border_matrix <- matrix(c(1, 1, 2, 0, 1, 1, 0, 0, 0), nrow = 3, ncol = 3)
#' ht <- huxtable::as_huxtable(
#'   data.frame(a = c(1, 2, 3), b = c("a", "b", "c"), c = c(TRUE, FALSE, TRUE))
#' )
#' ht <- add_bottom_borders(ht, border_matrix)
#' rtf <- huxtable::print_rtf(ht)
add_bottom_borders <- function(ht, border_matrix = no_borders(ht)) {
  if (is.null(border_matrix)) {
    return(ht)
  }
  internal_bottom_border <- r"{\brdrb\brdrs}"
  continuous_bottom_border <- huxtable::brdr(thickness = 0.9, style = "solid")
  for (row in seq_len(nrow(border_matrix))) {
    for (col in seq_len(ncol(border_matrix))) {
      if (border_matrix[row, col] == 0) {
        next
      }
      if (
        (col == ncol(border_matrix)) ||
          (border_matrix[row, col] == border_matrix[row, col + 1]) ||
          (border_matrix[row, col + 1] == 0)
      ) {
        ht <- huxtable::set_bottom_border(ht, row, col, continuous_bottom_border)
      } else {
        ht <- huxtable::set_contents(ht, row, col, sprintf("%s %s", internal_bottom_border, ht[row, col]))
      }
    }
  }
  ht
}

no_borders <- function(ht) {
  if (is.null(ht)) {
    return(NULL)
  }
  ht_dims <- dim(ht)
  matrix(rep(0, ht_dims[1] * ht_dims[2]), nrow = ht_dims[1], ncol = ht_dims[2])
}
