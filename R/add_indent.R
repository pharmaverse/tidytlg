#' Add indentation variable to the results dataframe
#'
#' Add the `indentme` variable to your results data. This drives the number of
#' indents for the row label text (e.g. 0, 1, 2, etc.).
#'
#' @details
#'
#' The `group_level` variable, which is added to the results dataframe by `freq()`
#' and `univar()` calls, is needed to define indentation when by variables are
#' used for summary.
#'
#' The `nested_level` variable, which is added to the results dataframe by
#' `nested_freq()`, is needed to define indentation for each level of nesting.
#'
#' Both of these are added to the default indentation which is driven by
#' `row_type`.
#'
#' | row_type          | default indentation |
#' | ----------------- |:-------------------:|
#' | TABLE_BY_HEADER   | 0                   |
#' | BY_HEADER\[1-9\]  | 0                   |
#' | HEADER            | 0                   |
#' | N                 | 1                   |
#' | VALUE             | 2                   |
#' | NESTED            | 0                   |
#'
#' @md
#'
#' @param df dataframe of results that contains `row_type` and `label`
#' and the optional `nested_level` and `group_level` variables.
#'
#' @return dataframe with the `indentme` variable added.
#' @export
#'
#' @examples
#' df <- tibble::tibble(row_type     =  c("TABLE_BY_HEADER", "HEADER",
#'        "BY_HEADER1", "N", "VALUE", "COUNTS", "UNIVAR", "NESTED", "NESTED"),
#'                      nested_level =  c(NA, NA, NA, NA, NA, NA, NA, 1, 2),
#'                      group_level =  c(0, 0, 0, 0, 0, 0, 0, 0, 0),
#'                      label        =  c(NA, NA, NA, NA, NA, "N",NA, NA, NA),
#'                      by           =  c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
#'                      tableby      =  c(NA, NA, NA, NA, NA, NA, NA, NA, NA))
#' add_indent(df)

add_indent <- function(df) {

  # set default list of variables to be removed
  remove_vars <- c("nested_level", "group_level", "value_add")

  # check if variables are already present in df, if not, add and set defaults
  if (!("nested_level" %in% names(df))) {
    df <- df %>%
      dplyr::mutate(nested_level = 0)
  }
  if (!("group_level" %in% names(df))) {
    df <- df %>%
      dplyr::mutate(group_level = 0)
  }

  # define amount to indent VALUE rows based on if a N row exists in the group
  # based on anbr
  if (!("value_add" %in% names(df))) {
    if ("anbr" %in% names(df)) {
      df <- df %>%
        group_by(anbr) %>%
        dplyr::mutate(value_add = 1 + max(as.numeric(row_type == "N")))
    } else {
      df <- df %>%
        dplyr::mutate(value_add = 1 + max(as.numeric(row_type == "N")))
    }
  }
  # if any table by headers, add one indentation to everything
  add_tableby_to_default <- any(df[["row_type"]] == "TABLE_BY_HEADER")

  df %>%
    dplyr::mutate(indentme =
            dplyr::case_when(
              row_type == "TABLE_BY_HEADER" ~ 0,
                grepl("BY_HEADER[0-9]", row_type) ~ 0 + add_tableby_to_default
                  + group_level,
              row_type == "HEADER" ~ 0 + add_tableby_to_default + group_level,
              row_type == "N" ~ 1 + add_tableby_to_default + group_level,
              row_type == "VALUE" ~ value_add + add_tableby_to_default
                + group_level,
              row_type == "NESTED" ~ nested_level + add_tableby_to_default
                + group_level,
              TRUE ~ 0
                    )) %>%
    select(-any_of(remove_vars))
}
