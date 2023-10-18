#' Create a statlist interface for a table
#'
#' The statlist is the interface for the presentation of data in a tidytlg
#' table.
#'
#' @param stats (required) A character vector of statistics to display in the
#'   table.
#' @param ... (optional) Additional configuration for stats. See sections below
#'   for allowable arguments.
#'
#'
#' @section Statlists for `freq()` and `nested_freq()`:
#' `freq()` statlists can be composed of n(count), N(denominator), and
#' x.x(percentage, formatted with or without a percent sign). Denominators will
#' include missing values if the 'display_missing' argument is TRUE, otherwise
#' they will be excluded. They can be arranged
#' in the following ways:
#' \itemize{
#'   \item{n}
#'   \item{n/N}
#'   \item{n (x.x)}
#'   \item{n (x.x%)}
#'   \item{n/N (x.x)}
#'   \item{n/N (x.x%)}
#' }
#'
#' The following other configurations are supported:
#' \itemize{
#'   \item{denoms_by - Controls what groupings of variables should define the
#'     denominator. Variables should be passed as a quoted vector}
#'   \item{distinct - A boolean value. Should the numerator reflect distinct
#'     USUBJIDs or event counts. Defaults to TRUE which captures distinct
#'     subjects.}
#'    \item{distinct_by - A character value used to select the variable that
#'    should be used to "distinct" the freq tables. Defaults to USUBJID.}
#'   \item{zero_denom - The string to display when there are no records found
#'     in an entire denominator group. Defaults to "-"}

#'   \item{zero_n - The string to display when there are no records found for
#'     a numerator. Defaults to "0".}
#' }
#'
#' @section Statlists for univar statlists:
#' \itemize{
#'   \item{N}
#'   \item{SUM}
#'   \item{MEAN}
#'   \item{GeoMEAN}
#'   \item{SD}
#'   \item{SE}
#'   \item{CV}
#'   \item{GSD}
#'   \item{GSE}
#'   \item{MEANSD}
#'   \item{MEANSE}
#'   \item{MEDIAN}
#'   \item{MIN}
#'   \item{MAX}
#'   \item{RANGE}
#'   \item{Q1}
#'   \item{Q3}
#'   \item{IQRANGE}
#'   \item{MEDRANGE}
#'   \item{MEDIQRANGE}
#'   \item{MEAN_CI}
#'   \item{GeoMEAN_CI}
#' }
#'
#'   where GeoMEAN: Geometric Mean, CV: Coefficient of Variation, GSD: Geometric
#'   Std. Dev., GSE: Geometric Std. Error, MEAN_CI: Mean (95% C.I.), GeoMEAN_CI:
#'   Geometric Mean (95% C.I.). In calculating geometric statistics, if there
#'   are zero values in the inputs, zero values will be excluded before
#'   calculating geometric statistics.
#'
#' @return A statlist object that can be passed in the 'statlist' argument of
#'   `freq`, `nested_freq`, or `univar`.
#'
#' @export
#'
#' @examples
#'
#' freq(
#'   mtcars,
#'   colvar = "gear",
#'   rowvar = "cyl",
#'   rowbyvar = "am",
#'   statlist = statlist("n/N (x.x)",
#'                       distinct = FALSE,
#'                       denoms_by = c("gear", "am"),
#'                       zero_denom = "_0_")
#'   )
statlist <- function(stats, ...) {

  # Validate Here
  st <- list()
  st$stats <- stats

  structure(append(st, list(...)), class = c("statlist", "list"))

}
