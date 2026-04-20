#' Replace leading whitespaces with left indentation RTF markup
#'
#' @details
#' The following function receives a string 'x' and returns a modified string
#' where every leading whitespace is replaced with 90 twips (0.0625 inches) left indentation RTF markup.
#' If the input does not start with a whitespace, the string is returned as is.
#'
#' @param x `character(1)` a string to replace the leading whitespaces.
#'
#' @examples
#' tidytlg:::replace_lead_whitespaces_ind("    this is x")
#' # [1] "\intbl\li360\fi0 this is x"
#' tidytlg:::replace_lead_whitespaces_ind("this is x")
#' # [1] "this is x"
#'
#' @return `character(1)` RTF markup with leading whitespaces replaced.
#' @keywords internal
replace_lead_whitespaces_ind <- function(x) {
  # get number of leading whitespaces
  num_whitespaces <- attr(regexpr("^\\s*", x), "match.length")
  # 2 whitespaces represent an indentation of 0.125 inches = 180 twips
  # e.g. 4 whitespaces represent a left-indentation of 0.25 inches = 360 twips
  num_twips <- num_whitespaces * 90
  if (num_twips > 0) {
    raw_rtf_markup <- paste0("\\intbl\\li", num_twips, "\\fi0")
    x <- paste0(raw_rtf_markup, " ", trimws(x, "left"))
  }
  x
}
