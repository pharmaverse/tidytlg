#' Setup data to support the specified column type
#'
#' tlgsetup is useful for pre-processing total columns and columns composed of
#' other columns. tlgsetup is called internally by `generate_results()` and can
#' be run manually for custom tables.
#'
#' @param df dataframe of records for analysis
#' @param var character vector that identifies the numeric column/treatment
#'   variable
#' @param column_metadata_file A file containing the column metadata. Read in
#'   with `readxl::read_excel()`. If a `column_metadata` dataframe is passed in
#'   too, this is ignored.
#' @param column_metadata A dataframe containing the column metadata. This will
#'   be used in place of `column_metadata_file`.
#' @param tbltype A value used to subset the `column_metadata`, both this and the
#'   file requirements are needed to bind the data to the table.
#'
#' @return dataframe with observations added to support the column type as well
#'   as the factor variable `colnbr` which is used as our new column summary
#'   variable. Regardless of if a `coldef` exists in data, the column will exist in
#'   the table.
#' @export
#'
#' @examples
#' df <-
#'   tibble::tribble(
#'   ~TRT01AN, ~USUBJID,
#'   0,        "A",
#'  54,       "B",
#'  81,       "C"
#' )
#'
#' tlgsetup(df, "TRT01AN", column_metadata = column_metadata)
#'
#' # Using a dataframe of column metadata
#' column_metadata <-
#'   tibble::tribble(
#'     ~tbltype, ~coldef, ~decode,                ~span1,
#'     "type1",  "0",     "Placebo",              "",
#'     "type1",  "54",    "Low Dose",             "Xanomeline",
#'     "type1",  "81",    "High Dose",            "Xanomeline",
#'     "type1",  "54+81", "Total Xanomeline",     ""
#'   )
#'
#' tlgsetup(df, "TRT01AN", column_metadata = column_metadata)
tlgsetup <-
  function(df,
           var,
           column_metadata_file = NULL,
           column_metadata = NULL,
           tbltype = NULL) {

  arglist <- list()
  args_to_chk <- names(formals())[names(formals()) != "..."]
  purrr::walk(
    args_to_chk,
    .f = function(x) {
      arglist[[x]] <<- eval(rlang::sym(x))
    }
  )
  check_tlgsetup(arglist)

  if ("colnbr" %in% names(df)) {
    message("tidytlg seems to have been run more than once. Remove 'colnbr'
            column and rerun if chnages to column_metadata.")
    return(df)
  }

  if (is.null(c(column_metadata_file, column_metadata))) {
    return(df)
  } else if (is.null(column_metadata)) {
    column_metadata <- readxl::read_excel(column_metadata_file, sheet = 1)
  }

  if (!is.null(tbltype)) {
    column_metadata <- column_metadata %>%
# Per JCEV-16: Any non breaking space(utf8 160) should be collapsed into space
# (utf8 32)
      mutate(
        tbltype = gsub("\u00A0", " ", tbltype)
      ) %>%
      filter(tbltype == !!tbltype)
  }

  if (rlang::is_missing(df))
    usethis::ui_stop("df is not specified for tlgsetup")
  if (rlang::is_missing(var))
    usethis::ui_stop("var is not specified for tlgsetup")
  if (rlang::is_missing(column_metadata))
    usethis::ui_stop("column_metadata is not specified for tlgsetup")

  if (inherits(df, "survfit")) {
    return(df)
  } else {
    # add rows to df based off column metadata
    split <- column_metadata %>%
      dplyr::mutate(colnbr = forcats::fct_reorder(paste0("col",
                              dplyr::row_number()), dplyr::row_number())) %>%
      tidyr::separate_rows(coldef, sep = "\\+")

    if (!is.factor(df[[var]]))
      df[[var]] <- as.character(df[[var]])


    # temp fix to align attributes and prevent warning
    attr(split$coldef, "label") <- attr(df[[var]], "label")

    split %>%
      inner_join(df, by = c("coldef" = var), keep = TRUE) %>%
      select(- coldef, - decode, -dplyr::starts_with("span")) %>%
      mutate(
        colnbr = fct_expand(colnbr, unique(as.character(split$colnbr)))
      )

  }
}
