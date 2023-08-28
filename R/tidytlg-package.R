#' tidytlg: A package for producing tables, listings, and graphs (TLGs) using
#' tidyverse packages.
#'
#' The tidytlg package provide a set of function allowing you to produce TLGs
#' using table metadata.
#'
#'
#' @section tidytlg Options:
#'
#' \itemize{ \item{tidytlg.row_type.levels - The values of row_type and their
#' respective levels. Default: c("TABLE_BY_HEADER", "BY_HEADER1", "BY_HEADER2",
#' "BY_HEADER3", "BY_HEADER4", "BY_HEADER5", "HEADER", "N", "VALUE", "NESTED")}
#' \item{tidytlg.fontsize.title - Font size for titles in points. Default: 10}
#' \item{tidytlg.fontsize.table.footnote - Font size for footnotes in points.
#' For tables. Default: 9} \item{tidytlg.fontsize.listing.footnote - Font size
#' for footnotes in points. For listings. Default: 8}
#' \item{tidytlg.fontsize.graph.footnote - Font size for footnotes in points.
#' For graphs. Default: 8} \item{tidytlg.fontsize.table - Font size for tables
#' in points. Default: 9} \item{tidytlg.fontsize.graph - Font size for graphs in
#' points. Default: 10} \item{tidytlg.fontsize.listing - Font size for listings
#' in poins. Default: 8} \item{tidytlg.right.padding - Amount of right padding
#' for cells in points. Default: 1} \item{tidytlg.left.padding - Amount of left
#' padding for cells in points. Default: 1} \item{tidytlg.stat_labels - Labels
#' to be used for each summary statistic in the output table.  Will need to have
#' all available statistics if this option is changed!}
#' \item{tidytlg.precision.extra - Value to be added on for each summary
#' statistic when calculating precision.  This will be used to make different
#' statistics have different precision as needed}
#' \item{tidytlg.nested_freq.statlist.default - Default statlist object for
#' nested_freq tables. Default: statlist("n (x.x)")}
#' \item{tidytlg.univar.statlist.default - Default statlist object for univar
#' tables. Default: statlist(c("N", "MEANSD", "MEDIAN", "RANGE", "IQRANGE"))}
#' \item{tidytlg.freq.statlist.default - Default statlist object for freq
#' tables. Default: statlist("n (x.x)")} \item{tidytlg.stat_labels - A
#' data.frame controlling how the stats are labeled in a univar table. See
#' 'Default Stat Labels' section for defaults.} \item{tidytlg.denoms.message - A
#' boolean, should a message print detailing what the denominators are?}
#' }
#'
#' @section Default Stat Labels:
#'
#' | **stat** | **label** |
#' | --- | --- |
#' | N | N |
#' | SUM | Sum |
#' | MEAN | Mean |
#' | GeoMEAN | Geometric Mean |
#' | SD | Std. Dev. |
#' | SE | Std. Error |
#' | GSD | Geometric Std. Dev. |
#' | GSE | Geometric Std. Error |
#' | CV | CV |
#' | MEANSD | Mean (SD) |
#' | MEANSE | Mean (SE) |
#' | MEDIAN | Median |
#' | MIN | Minimum |
#' | MAX | Maximum |
#' | RANGE | Range |
#' | Q1 | First quartile |
#' | Q3 | Third quartile |
#' | IQRANGE | IQ range |
#' | MEDRANGE | Median (Range) |
#' | MEDIQRANGE | Median (Q1; Q3) |
#' | MEAN_CI | Mean (95% C.I.) |
#' | GeoMEAN_CI | Geometric Mean (95% C.I.) |
#'
#' @section Default Precision Extra:
#'
#' | **stat** | **extra** |
#' | --- | --- |
#' |N| 0 |
#' |SUM| 0 |
#' |MEAN| 1 |
#' |GeoMEAN| 1 |
#' |SD| 2 |
#' |SE| 2 |
#' |GSD| 2 |
#' |GSE| 2 |
#' |CV| 1 |
#' |MEDIAN| 1 |
#' |MIN| 0 |
#' |MAX| 0 |
#' |Q1| 1 |
#' |Q3| 1 |
#' |LCL_MEAN| 2 |
#' |UCL_MEAN| 2 |
#' |LCL_GeoMEAN| 2 |
#' |UCL_GeoMEAN| 2 |
#'
#' @section Updating Options:
#' \itemize{
#'   \item{For a single session, an option can be changed by
#'   `option(<optionToChange> = <NewValue>)`}
#'   \item{To change an option for a single projects across sessions in that
#'   projects, place the options update in the `.Rprofile` in that project
#'   directory.}
#'   \item{To change an option for a user across all sessions, place the options
#'   update in the `.Rprofile` file in the users home directory.}
#'   \item{To change an option for all users in an R environment, place the
#'   options update in the `.Rprofile.site` file in the R home directory.}
#' }
#'
#' See [Managing R with .Rprofile, .Renviron, Rprofile.site, Renviron.site, rsession.conf, and repos.conf](https://support.posit.co/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf)
#'
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @docType package
#' @name tidytlg
#' @keywords internal
"_PACKAGE"

globalVariables(c("anbr", "label", "rowvar", "rowcondition", "row_type",
                  "rowtext", "tableby", "by", "func", "colvar", "header_order",
                  "df", "filter_to_df", "statlist", "denom_df", "precision",
                  "tbltype", "dotdotdot", ":="))

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.tidytlg <- list(
    tidytlg.row_type.levels =
      c("TABLE_BY_HEADER",
        "BY_HEADER1", "BY_HEADER2", "BY_HEADER3", "BY_HEADER4", "BY_HEADER5",
        "HEADER",
        "N", "VALUE", "NESTED"),
    tidytlg.fontsize.title = 10,
    tidytlg.fontsize.table.footnote = 9,
    tidytlg.fontsize.listing.footnote = 8,
    tidytlg.fontsize.graph.footnote = 8,
    tidytlg.fontsize.table = 9,
    tidytlg.fontsize.graph = 10,
    tidytlg.fontsize.listing = 8,
    tidytlg.right.padding = 1,
    tidytlg.left.padding = 1,
    tidytlg.stat_labels = tibble::tribble(
      ~stat,           ~label,
      "N",             "N",
      "SUM",           "Sum",
      "MEAN",          "Mean",
      "GeoMEAN",       "Geometric Mean",
      "SD",            "Std. Dev.",
      "SE",            "Std. Error",
      "GSD",           "Geometric Std. Dev.",
      "GSE",           "Geometric Std. Error",
      "CV",            "CV (%)",
      "MEANSD",        "Mean (SD)",
      "MEANSE",        "Mean (SE)",
      "MEDIAN",        "Median",
      "MIN",           "Minimum",
      "MAX",           "Maximum",
      "RANGE",         "Range",
      "Q1",            "First quartile",
      "Q3",            "Third quartile",
      "IQRANGE",       "IQ range",
      "MEDRANGE",      "Median (Range)",
      "MEDIQRANGE",    "Median (Q1; Q3)",
      "MEAN_CI",       "Mean (95% C.I.)",
      "GeoMEAN_CI",    "Geometric Mean (95% C.I.)"
    ),
    tidytlg.precision.extra = tibble::tribble(
      ~stat,           ~extra,
      "N",             0,
      "SUM",           0,
      "MEAN",          1,
      "GeoMEAN",       1,
      "SD",            2,
      "SE",            2,
      "GSD",           2,
      "GSE",           2,
      "CV",            1,
      "MEDIAN",        1,
      "MIN",           0,
      "MAX",           0,
      "Q1",            1,
      "Q3",            1,
      "LCL_MEAN",      2,
      "UCL_MEAN",      2,
      "LCL_GeoMEAN",   2,
      "UCL_GeoMEAN",   2
    ),
    tidytlg.univar.statlist.default =
      statlist(c("N", "MEANSD", "MEDIAN", "RANGE", "IQRANGE")),
    tidytlg.freq.statlist.default = statlist(c("n (x.x)")),
    tidytlg.nested_freq.statlist.default = statlist(c("n (x.x)")),
    tidytlg.denoms.message = FALSE,
    # Do not document. This is just used for testing
    tidytlg.add_datetime = TRUE
  )

  toset <- !(names(op.tidytlg) %in% names(op))
  if (any(toset)) options(op.tidytlg[toset])

  invisible()
}
