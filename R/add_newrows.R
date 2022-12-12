#' Add the newrows variable to the results dataframe.
#'
#' The newrows variable is used by `gentlg()` to define when to add a blank row
#' to the output. Data will be grouped by anbr and the variables passed into
#' the tableby and groupby parameters.`newrows` will be set to 1 for the first
#' record in each group, except for the first row in the data.
#' The first row will always be set to 0.
#'
#' @param df dataframe of results.  must contain the anbr variable that is
#' added by add_format()
#' @param tableby character vector containing table by variables used to
#' generate the results
#' @param groupby character vector containing group by variables used to
#' generate the results
#'
#' @return dataframe with the variable newrows and roworder added.
#' newrows is used by gentlg to insert line breaks.
#' @export
#'
#' @examples
#' # Example showing how newrows is set to one for each new anbr except
#' # the first
#' tbl <-
#'   structure(
#'     list(rowvar = c("RANDFL", "AGE", "AGE", "AGE", "AGE", "AGE"),
#'      anbr   = c(1L, 2L, 2L, 2L, 2L, 2L),
#'      label  = c("Analysis set: Subjects Randomized", "Age (Years)", "N",
#'      "Mean (SD)", "Range", "IQ Range"),
#'      row_type = c("COUNT", "UNIVAR", "UNIVAR", "UNIVAR", "UNIVAR", "UNIVAR")
#'       ),
#'     row.names = c(NA,-6L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   )
#'
#' add_newrows(tbl)
#'
#' # Example of use when you have results summarized by one or more variables
#' tbl2 <- tibble::tribble(
#'   ~anbr, ~SEX,    ~label,         ~row_type,
#'   "01",  "F", "Sex : F", "TABLE_BY_HEADER",
#'   "01",  "F",     "<65",           "VALUE",
#'   "01",  "F",   "65-80",           "VALUE",
#'   "01",  "F",     ">80",           "VALUE",
#'   "01",  "M", "Sex : M", "TABLE_BY_HEADER",
#'   "01",  "M",     "<65",           "VALUE",
#'   "01",  "M",   "65-80",           "VALUE",
#'   "01",  "M",     ">80",           "VALUE"
#' )
#'
#' add_newrows(tbl2, tableby = "SEX")
#'
#' tbl3 <- tibble::tribble(
#' ~anbr, ~SEX,           ~ETHNIC,                  ~label,         ~row_type,
#'  "01",  "F",                NA,                "Sex : F", "TABLE_BY_HEADER",
#'  "01",  "F", "HISPANIC OR LATINO", "HISPANIC OR LATINO",      "BY_HEADER1",
#'  "01",  "F", "HISPANIC OR LATINO",               "<65",           "VALUE",
#'  "01",  "F", "HISPANIC OR LATINO",               ">80",           "VALUE",
#'  "01",  "F", "HISPANIC OR LATINO",             "65-80",           "VALUE",
#'  "01", "F", "NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "BY_HEADER1",
#'  "01", "F", "NOT HISPANIC OR LATINO",                    "<65",      "VALUE",
#'  "01", "F", "NOT HISPANIC OR LATINO",                  "65-80",      "VALUE",
#'  "01", "F", "NOT HISPANIC OR LATINO",                    ">80",      "VALUE",
#'  "01", "M",                       NA,           "Sex : M", "TABLE_BY_HEADER",
#'  "01", "M",    "HISPANIC OR LATINO",   "HISPANIC OR LATINO",    "BY_HEADER1",
#'  "01", "M",    "HISPANIC OR LATINO",                  "<65",         "VALUE",
#'  "01", "M",    "HISPANIC OR LATINO",                "65-80",         "VALUE",
#'  "01", "M",     "HISPANIC OR LATINO",               ">80",           "VALUE",
#'  "01", "M", "NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "BY_HEADER1",
#'  "01", "M", "NOT HISPANIC OR LATINO",              "<65",           "VALUE",
#'  "01",  "M", "NOT HISPANIC OR LATINO",            "65-80",           "VALUE",
#'  "01",  "M", "NOT HISPANIC OR LATINO",              ">80",           "VALUE"
#' )
#'
#' add_newrows(tbl3, tableby = "SEX", groupby = "ETHNIC")

add_newrows <- function(df, tableby = NULL, groupby = NULL) {

  dfn <- df %>%
    dplyr::group_by_at(c(tableby, groupby, "anbr")) %>%
    dplyr::mutate(roworder = dplyr::row_number(),
                  newrows =
                    dplyr::case_when(
                      .data$roworder == 1 ~ 1,
                      row_type ==  "TABLE_BY_HEADER" ~ 1,
                      TRUE ~ 0
                    )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(newrows = ifelse(dplyr::row_number() == 1, 0, .data$newrows),
                  newrows = ifelse(lag(newrows, na = 0) == 1 &
                                     row_type == "BY_HEADER2", 0, newrows))

  if ("nested_level" %in% names(dfn)) {
    dfn %>%
      dplyr::mutate(newrows = case_when(row_type == "NESTED" &
                                          nested_level == 0 ~ 1,
                                        TRUE ~ newrows))
  } else {
    dfn
  }
}
