#' Add the formatting variables of `indentme`, `newrows`, `newpage`, and `roworder` to
#' the results dataframe
#'
#' @param df (required) dataframe of results and must contain the `anbr` variable
#' @param tableby (optional) character vector containing table by variables
#' @param groupby (optional) character vector containing group by variables
#' @param .keep (optional) should `tableby` and `groupby` variables be kept in the
#'   final dataframe.  (default = FALSE)
#'
#' @return dataframe with the formatting variables indentme, newrows, newpage, and roworder added
#' @importFrom purrr walk
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' df <- tibble::tibble(row_type =
#'       c("TABLE_BY_HEADER", "HEADER", "BY_HEADER1", "N", "VALUE",
#'                                    "COUNTS", "UNIVAR", "NESTED", "NESTED"),
#'         nested_level =  c(NA, NA, NA, NA, NA, NA, NA, 1, 2),
#'         group_level =  c(0, 0, 0, 0, 0, 0, 0, 0, 0),
#'         label        =  c(NA, NA, NA, NA, NA, "N",NA, NA, NA),
#'         by           =  c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
#'         tableby      =  c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
#'         anbr      =  c(1:9))
#' add_format(df)
add_format <- function(df, tableby = NULL, groupby = NULL, .keep = FALSE) {

  # check all the arguments being passed in except ...
  arglist <- list()
  args_to_chk <- names(formals())[names(formals()) != "..."]
  walk(args_to_chk, .f = function(x) arglist[[x]] <<- eval(sym(x)))
  check_add_format(arglist)

  df2 <- df %>%
    add_indent() %>%
    add_newrows(tableby = tableby, groupby = groupby) %>%
    add_newpage()

  if (is.null(c(tableby, groupby)) | .keep == TRUE) {
    df2
  } else {
    df2 %>%
      select(-all_of(c(tableby, groupby)))
  }
}

#Function that sets when a new page is to be added
# A new page is added when row_type is TABLE_BY_HEADER and not the first row
# but when first row instead adds a new row
#' add_newpage
#'
#' @param df dataframe to add newpage col to
#'
#' @return df with the newpage cols row added and newrows updated
#' @noRd
add_newpage <- function(df) {
  if (any(df[["row_type"]] == "TABLE_BY_HEADER")) {
    df %>%
      dplyr::mutate(newpage = case_when(row_type == "TABLE_BY_HEADER" &
                                          dplyr::row_number() > 1 ~ 1,
                                 TRUE ~ 0),
                    newrows = case_when(row_type == "TABLE_BY_HEADER" &
                                          dplyr::row_number() == 1 ~ 1,
                                        row_type == "TABLE_BY_HEADER" &
                                          dplyr::row_number() > 1 ~ 0,
                                 TRUE ~ newrows))
  } else {
    df %>%
      mutate(newpage = 0)
  }
}
