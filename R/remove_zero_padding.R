#' Removes noop padding from an RTF markup
#'
#' @details
#' Removes noop padding, which is padding that has a thickness of zero
#' from an RTF markup.
#'
#' @param rtf `character(1)` RTF markup
#'
#' @return `character(1)` RTF markup with removed zero padding
#' @keywords internal
remove_zero_padding <- function(rtf) {
  noop_padding <- function(direction) {
    sprintf(r"{\\clpadf%s3\\clpad%s0 }", direction, direction)
  }
  for (direction in c("l", "r", "b", "t")) {
    rtf <- stringr::str_replace_all(rtf, noop_padding(direction), "")
  }
  rtf
}
