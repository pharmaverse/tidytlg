#' Generate nested count/percent for two or three levels
#'
#' This will call `freq()` multiple times and combine the levels together. This
#' is useful for adverse event and concomitant mediations.
#'
#' @param df (required) dataframe containing the two levels to summarize
#' @param denom_df (optional) dataframe containing records to use as the
#'   denominator (default = df)
#' @param colvar (required) treatment variable within df to use to summarize
#' @param tablebyvar (optional) repeat entire table by variable within df.
#' @param rowvar (required) nested levels separated by a star, for example
#'   AEBODSYS*AEDECOD,  this can handle up to three levels.
#' @param rowbyvar (optional) repeat `rowvar` by variable within df
#' @param statlist (optional) count/percent type to return (default = "n (x.x)")
#' @param decimal (optional) decimal precision root level (default = 1)
#' @param cutoff (optional) numeric value used to cut the data to a percentage
#'   threshold, if any column meets the threshold the entire record is kept.
#' @param cutoff_stat (optional) The value to cutoff by, n or pct. (default =
#'   'pct')
#' @param subset (optional) An R expression that will be passed to a
#'   `dplyr::filter()` function to subset the data.frame
#' @param descending_by (optional) The column or columns to sort descending
#'   values by. Can also provide a named list to do ascending order. ex.
#'   c("VarName1" = "asc", "VarName2" = "desc") would sort by VarName1 in
#'   ascending order and VarName2 in descending order. If not provided, the
#'   columns will be sorted alphabetically.
#' @param display_missing (optional) Should the "missing" values be displayed?
#'   (default = FALSE)
#' @param rowtext (optional) A character vector used to rename the `label`
#'   column. If named, names will give the new level and values will be the
#'   replaced value. If unnamed, and the table has only one row, the `rowtext`
#'   will rename the `label` of the row.
#' @param row_header (optional) A character vector to be added to the table.
#' @param .keep (optional) Should the `rowbyvar` and `tablebyvar` be output in the
#'   table.  If FALSE, `rowbyvar` will still be output in the `label` column.
#'   (default = TRUE)
#' @param .ord Should the ordering columns be output with the table? This is
#'   useful if a table needs to be merged or reordered in any way after build.
#' @param ... (optional) Named arguments to be included as columns on the table.
#'
#' @return A dataframe of nested results by `colvar` and optional `tablebyvar`.
#'   There are a few additional variable sets added to support multiple
#'   requirements. \cr  \cr The level variables (`level1_`, `level2_`,
#'   `level3_`) will carry down the counts for each level to every record.  This
#'   allows for easy sorting of nested groups. \cr  \cr The header variables
#'   (`header1`, `header2`, `header3`) will flag the header for each level to
#'   ensure each level header is sorted to the top of the level. \cr  \cr The n
#'   variables ("n_*") provide a numeric variable containing frequency for each
#'   `colvar`.  This can be used to sort and filter records. \cr  \cr The pct
#'   variables ("pct_*") provide a numeric variable containing percentages for
#'   each `colvar`.  This can be used to sort and filter records.\cr  \cr
#'
#' @export
#'
#' @examples
#' adae <- data.frame(
#'       SITEID = c("100", "100", "100","200", "200", "200"),
#'       USUBJID = c("Demo1-101", "Demo1-102", "Demo1-103",
#'                   "Demo1-104", "Demo1-105", "Demo1-106"),
#'       AEBODSYS = c("Cardiac disorders", "Cardiac disorders",
#'                    "Respiratory, thoracic and mediastinal disorders",
#'                    "Infections and infestations",
#'                    "Skin and subcutaneous tissue disorders",
#'                    "Infections and infestations"),
#'       AEDECOD = c("Arrhythmia supraventricular", "Cardiac failure",
#'                   "Chronic obstructive pulmonary disease", "Pneumonia",
#'                   "Pustular psoriasis", "Upper respiratory tract infection"),
#'       colnbr = structure(
#'         c(1L, 2L, 3L, 1L,  2L, 3L),
#'         .Label = c("Active", "Placebo", "Comparator"),
#'         class = "factor"
#'       )
#'     )
#'
#' # Frequency and percent for two levels of nesting
#' nested_freq(adae
#'            ,colvar = "colnbr"
#'            ,rowvar = "AEBODSYS*AEDECOD"
#'            ,statlist = statlist("n (x.x%)"))
#'
#' # Frequency and percent for three levels of nesting (for illustrative
#' # purpose)
#' nested_freq(adae
#'            ,colvar = "colnbr"
#'           ,rowvar = "SITEID*AEBODSYS*AEDECOD"
#'            ,statlist = statlist("n (x.x%)"))
#'
#' # Cut records where pct meets threshold for a any column
#' nested_freq(cdisc_adae
#'             ,colvar = "TRTA"
#'             ,rowvar = "AEBODSYS*AEDECOD"
#'             ,statlist = statlist("n (x.x%)", distinct = TRUE)
#'             ,cutoff = 2
#'             ,cutoff_stat = "n")
#'
#' # Cut records where pct meets threshold for a specific column
#' nested_freq(cdisc_adae
#'             ,rowvar = "AEBODSYS*AEDECOD"
#'             ,colvar = "TRTAN"
#'             ,statlist = statlist("n (x.x%)", distinct = TRUE)
#'             ,cutoff = "54 >= 2"
#'             ,cutoff_stat = "n")
#'
#' # Frequency and percent for two levels of nesting and sort by descending
#' # active
#' nested_freq(adae
#'             ,colvar = "colnbr"
#'             ,rowvar = "AEBODSYS*AEDECOD"
#'             ,statlist = statlist("n (x.x%)")
#'             ,descending = "Active")
#'
#'# Below illustrates how make the same calls to nested_freq() as above, using
#'# table and # column metadata along with generate_results().
#'
#'column_metadata <- tibble::tribble(
#'  ~tbltype, ~coldef,   ~decode,
#'  "type1",     "1", "Placebo",
#'  "type1",     "2",     "Low",
#'  "type1",     "3",    "High"
#')
#'
#'# Frequency and percent for two levels of nesting
#'table_metadata <- tibble::tribble(
#'  ~anbr,         ~func,    ~df,       ~rowvar, ~tbltype,  ~colvar, ~statlist,
#'  "1", "nested_freq", "cdisc_adae", "AEBODSYS*AEDECOD",  "type1",  "TRTP",
#'  statlist("n (x.x%)")
#')
#'#generate_results(table_metadata,
#'#column_metadata_file = tidytlg_metadata(path)
#'
#'
#'# Frequency and percent for three levels of nesting (for illustrative purpose)
#'table_metadata <- tibble::tribble(
#'  ~anbr,         ~func,    ~df,                 ~rowvar, ~tbltype,  ~colvar,
#'  ~statlist,
#'  "1", "nested_freq", "cdisc_adae", "SITEID*AEBODSYS*AEDECOD","type1",
#'  "TRTP", statlist("n (x.x%)")
#')
#' # Commented out because it takes too long
#' # generate_results(table_metadata, column_metadata)
#'
#' #Cut records where pct meets threshold for a any column
#'column_metadata <- tibble::tribble(
#'  ~tbltype, ~coldef,   ~decode,
#'  "type2",     "1", "Placebo",
#'  "type2",     "2",  "Active"
#')
#'table_metadata <- tibble::tibble(
#'  anbr = "1", func = "nested_freq", df= "cdisc_adae",
#'  rowvar = "AEBODSYS*AEDECOD",
#'  tbltype = "type2", colvar = "TRTP", statlist = statlist("n (x.x%)"),
#'  dotdotdot = "cutoff = 5"
#')
#' #generate_results(table_metadata,
#' # column_metadata_file = tidytlg_metadata(path)
#'
#'# Cut records where pct meets threshold for a specific column
#'table_metadata <- tibble::tibble(
#'  anbr = "1", func = "nested_freq", df= "cdisc_adae",
#'  rowvar = "AEBODSYS*AEDECOD",
#'  tbltype = "type2", colvar = "TRTP", statlist = statlist("n (x.x%)"),
#'  dotdotdot = "cutoff = 'col1 >= 5'"
#')
#' #generate_results(table_metadata,
#' #column_metadata_file = tidytlg_metadata(path)
#'
#'# Frequency and percent for two levels of nesting and sort by descending col1
#'table_metadata <- tibble::tibble(
#'  anbr = "1", func = "nested_freq", df= "cdisc_adae",
#'  rowvar = "AEBODSYS*AEDECOD",
#'  tbltype = "type2", colvar = "TRTP", statlist = statlist("n (x.x%)"),
#'  dotdotdot = "descending = 'col1'"
#')
#' #generate_results(table_metadata,
#' #column_metadata_file = tidytlg_metadata(path)
nested_freq <- function(df,
                        denom_df = df,
                        colvar = NULL,
                        tablebyvar = NULL,
                        rowvar = NULL,
                        rowbyvar = NULL,
                  statlist = getOption("tidytlg.nested_freq.statlist.default"),
                        decimal = 1,
                        cutoff = NULL,
                        cutoff_stat = "pct",
                        subset = TRUE,
                        descending_by = NULL,
                        display_missing = FALSE,
                        rowtext = NULL,
                        row_header = NULL,
                        .keep = TRUE,
                        .ord = FALSE,
                        ...) {

  subset <- rlang::enexprs(subset)
  if (length(subset) > 0 && is.character(subset[[1]]))
    subset <- as.list(parse(text = subset))

  # check all the arguments being passed in except ...
  arglist <- list()
  args_to_chk <- names(formals())[names(formals()) != "..."]
  purrr::walk(
    args_to_chk,
    .f = function(x) {
      arglist[[x]] <<- eval(rlang::sym(x))
    }
  )
  check_nested_freq(arglist)

  level <- stringr::str_split(rowvar, "\\s*\\*\\s*", simplify = TRUE)[1, ]

  attr(df, "nested") <- TRUE
  attr(df, "nested_level") <- level
  attr(df, "rowbyvar") <- rowbyvar

# If the statlist has a denoms_by config, use that. Otherwise default to colvar,
# tablebyvar, and rowbyvar
  denoms_by <- statlist[["denoms_by"]] %||% c(colvar, tablebyvar)

  if (getOption("tidytlg.denoms.message"))
    message("denominator set using ", paste0(denoms_by, collapse = ", "),
    ". You can overwrite using the denom_by argument in statlist.\n", sep = "")
  colvar_cols <- levels(as.factor(df[[colvar]]))

  freqs <- df %>%
    # get necessary records
    filter(!!!subset) %>%
    # derive nested counts
    derive_nested(
      denom_df = denom_df,
      colvar = colvar,
      level = level,
      rowbyvar = rowbyvar,
      tablebyvar = tablebyvar,
      statlist = statlist,
      denoms_by = denoms_by,
      display_missing = display_missing
    ) %>%
    # remove any results that do not meet table criteria
drop_nested(level, cutoff, colvar, level, tablebyvar, cutoff_stat, rowbyvar) %>%
    round_freq(decimal) %>%
    arrange_freq(statlist) %>%
    row_header_merge(
      colvar = colvar,
      rowvar = tail(level, 1),
      rowbyvar = NULL,
      tablebyvar = tablebyvar,
      row_header = row_header
    ) %>%
    # pivot table to final orientation
    pivot_freq(
      colvar,
      tail(level, 1),
      c(rowbyvar, head(level, -1)),
      tablebyvar,
      statlist,
      rowtext = rowtext,
      row_header = row_header,
      display_missing = FALSE,
      nested = TRUE,
      .keep = .keep,
      .ord = .ord,
      ... = ...
    ) %>%
    sort_freq(level, descending_by, tablebyvar, rowbyvar, colvar_cols, colvar)

  attr(freqs, "colvar") <- colvar
  attr(freqs, "tablebyvar") <- tablebyvar
  attr(freqs, "rowvar") <- rowvar
  attr(freqs, "statlist") <- statlist
  attr(freqs, "cutoff") <- cutoff
  attr(freqs, "cutoff_stat") <- cutoff_stat
  attr(freqs, "descending_by") <- descending_by
  attr(freqs, "rowtext") <- rowtext
  attr(freqs, "row_header") <- row_header
  attr(freqs, "subset") <- subset
  attr(freqs, "denoms_by") <- denoms_by

  structure(freqs,
            class = c("tidytlg.nested_freq", "tidytlg.freq", class(freqs)))

}

#' Derive nested frequency
#'
#' @return A long dataframe with n, denom, N, and pct for each combination
#' @noRd
derive_nested <- function(df, denom_df, colvar, level, rowbyvar, tablebyvar,
                          statlist, descending_by, denoms_by, display_missing) {


  # Get long dataframes for the "top" level
  level1_df <- df %>%
    derive_freq(denom_df = denom_df,
         colvar = colvar,
         rowbyvar = rowbyvar,
         tablebyvar = tablebyvar,
         rowvar = level[1],
         statlist = statlist,
         nested = TRUE,
         rowtext = NULL,
         has_subset = FALSE,
         denoms_by = denoms_by,
         display_missing = display_missing) %>%
    mutate(
      nested_level = 0)



  # Get derivations for second level
  level2_df <- df %>%
    derive_freq(denom_df = denom_df,
         colvar = colvar,
         rowbyvar = c(level[1], rowbyvar),
         tablebyvar = tablebyvar,
         rowvar = level[2],
         statlist = statlist,
         nested = TRUE,
         rowtext = NULL,
         has_subset = FALSE,
         denoms_by = denoms_by,
         display_missing = display_missing) %>%
    mutate(
      nested_level = 1)

  if (length(level) == 3) {

   # If there are three levels, make a third level
    level3_df <- df %>%
      derive_freq(denom_df = denom_df,
                  colvar = colvar,
                  rowbyvar = c(level[1], level[2], rowbyvar),
                  tablebyvar = tablebyvar,
                  rowvar = level[3],
                  statlist = statlist,
                  nested = TRUE,
                  rowtext = NULL,
                  has_subset = FALSE,
                  denoms_by = denoms_by,
                  display_missing = display_missing) %>%
      mutate(
        nested_level = 2)

# In the top level dataframe, make a new variable named level[2], and level[3],
# that are the values of whatever is in level[1]. When the merge happens the
# higher levels will have values to help row bind.
    level1_df <- level1_df %>%
      mutate(
        !!level[2] := !!sym(level[1]),
        !!level[3] := !!sym(level[1])
      )
    level2_df <- level2_df %>%
      mutate(
        !!level[3] := !!sym(level[2])
      )
  } else {
    level3_df <- NULL

    level1_df <- level1_df %>%
      mutate(
        !!level[2] := !!sym(level[1])
      )
  }

  # Bind the other data.frames together
  bind_rows(level1_df, level2_df, level3_df) %>%
  # The complete is different for nested because the levels should only include
  # The combinations present in the data.
  complete(nesting(!!!syms(c(level, rowbyvar))), !!!syms(c(tablebyvar, colvar)),
             fill = list(n = 0, N = 0, denom = 0, pct = 0))


}

#' Drop values for a nested frequency
#'
#' Calls drop_freq but also additional logic if the lowest rowvar doesn't have
#' any values to remove all upper values.
#'
#' @return Long dataframe with values dropped
#' @noRd
drop_nested <-
  function(df,
           level,
           cutoff,
           colvar,
           rowvar,
           tablebyvar,
           cutoff_stat,
           rowbyvar) {

  # Use the normal steps for dropping values
  drop_freq(df = df,
            cutoff = cutoff,
            # tail(level, 1) is always the lowest level
            rowvar = tail(level, 1),
            colvar = colvar,
            cutoff_stat = cutoff_stat,
            # Treat the top levels as a tablebyvar so values that span higher
            # level are calculated properly.
            drop_by = c(tablebyvar, head(level, -1), rowbyvar)) %>%
    # nest the table by any tablebyvars and the top levels
    nest(nest_treatment = -any_of(c(tablebyvar, head(level, -1)))) %>%
# Update the nested tables to filter out any where n is 0 for all treatment
# groups
    mutate(
      nest_treatment = map(.data[["nest_treatment"]],
                           ~ group_by(.x, !!!syms(tail(level, 1))) %>%
                             filter(any(n > 0)), .preserve = FALSE)
    ) %>%
    rowwise() %>%
    # If that removed all of the treatments, drop it
    filter(nrow(nest_treatment) > 0) %>%
    unnest(nest_treatment) %>%
    # In certain cases the top level rowvar variables will persist even if there
    # are no subordinate values below them. This nests the top levels(level1, or
    # level1 and level2 if a three variable nest) and drops the ones that don't
    # have subordinate items.
    # -head(level, 1) returns everything but the top levels for nesting
    nest(nested_level = -any_of(c(head(level, 1), tablebyvar))) %>%
    rowwise() %>%
  # If there are only 'length(unique(df[[colvar]]))' number
  #of entries in the nested df, we know that the entries that are there are the
  # top level and contain subordinate items, so remove them
    filter(nrow(nested_level) > length(unique(df[[colvar]]))) %>%
    unnest(cols = nested_level)
}
