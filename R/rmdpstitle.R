#' Get Titles and Footnotes for all TLGs or one specific TLG
#'
#' @param df dataframe with three variables; table name, row identifier (TITLE
#'   or FOOTNOTEn), and title/footnote text to display
#' @param tblid  character vector containing the table id, optional, used to
#'   subset df to a specific table (defaults to tblid)
#' @param idvar  character vector containing the variable in df that contains
#'   your table id
#' @param identifier  character vector containing the variable name in df that
#'   contains your record identifier (defaults to "identifier")
#' @param text  character vector containing the variable name in df that
#'   contains your title and footnote text (defaults to "text")
#'
#' @return list of length two, the first element contains the titles as a tibble
#'   and the second contains the footnotes as a list
#' @export
#'
#' @examples
#' tblid <- "TSIDEM01"
#'
#' titles <-tibble::tribble(
#'   ~tblid, ~identifier,                  ~text,
#'   "TSIDEM01",     "TITLE", "Demographics Example",
#'   "TSIDEM01", "FOOTNOTE1",    "Example footnote."
#' )
#'
#' title_foot <- rmdpstitle(titles, tblid)
#'
#' title_foot[[1]]
#' title_foot[[2]]

rmdpstitle <-
  function(df,
           tblid,
           idvar = "tblid",
           identifier = "identifier",
           text = "text") {

  # function argument checks
    if (rlang::is_missing(df))
      usethis::ui_stop("in rmdpstitle, df is not specified")
    if (rlang::is_missing(tblid))
      usethis::ui_stop("in rmdpstitle, tblid is not specified")
    if (!(identifier %in% names(df)))
      usethis::ui_stop("in rmdpstitle, {identifier} is not in {substitute(df)}")
    if (!(text %in% names(df)))
      usethis::ui_stop("in rmdpstitle, {text} is not in {substitute(df)}")

  # split titles and footnotes into separate objects
  ttl <- df %>%
    dplyr::filter(.data[[identifier]] == "TITLE")

  foot <- df %>%
    dplyr::filter(.data[[identifier]] != "TITLE")

  if (!(tblid %in% unique(df[[idvar]])))
    usethis::ui_stop("in rmdpstitle, {tblid} is not in
                     {substitute(df)}${substitute(idvar)}")

  ttl <- ttl %>%
    dplyr::filter_at(idvar, ~ . == tblid) %>%
    dplyr::select(.data[[text]])

  foot <- foot %>%
    dplyr::filter_at(idvar, ~ . == tblid) %>%
    dplyr::select(.data[[text]])

  # return NULL for titles and/or footnotes if none are found
  if (nrow(ttl) == 0) {
    ttl <- NULL
    }
  if (nrow(foot) == 0) {
    foot <- NULL
  } else {
      foot <- split(foot$text, seq(nrow(foot)))
      }

  return(list(ttl, foot))

}
