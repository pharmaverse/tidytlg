#' Spanning headers for outputs
#'
#' This will create the list object to be passed to `gentlg()` You can create as
#' many spanning headers as you like, just add variables prefixed with span to
#' the column metadata.
#'
#' @param column_metadata dataframe containing the column metadata that is
#'   passed to `tlgsetup()` (see `tlgsetup()` for details)
#'
#' @return List of character vectors containing column headers for an output.
#' @export
#'
#' @examples
#' column_metadata <-
#'   tibble::tribble(
#'     ~tbltype, ~coldef, ~decode,                ~span1,
#'     "type1",  "0",     "Placebo",              "",
#'     "type1",  "54",    "Low Dose",             "Xanomeline",
#'     "type1",  "81",    "High Dose",            "Xanomeline",
#'     "type1",  "54+81", "Total Xanomeline",     ""
#'   )
#'
#' spanning_headers(column_metadata)

spanning_headers <- function(column_metadata) {

  span_element <- function(x) (
    c("", stringr::str_replace_na(x, ""))
  )

  not_empty <- function(x) {
    !all(is.na(x) | is.null(x) | x == "")
    }

  span <- column_metadata %>%
    dplyr::select_if(not_empty) %>%
    dplyr::select(dplyr::starts_with("span")) %>%
    lapply(span_element)

  if (length(span) == 0) (span <- NULL)

  span
}
