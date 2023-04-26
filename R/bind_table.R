#' Bind a set of tidytlg tables together with formatting variables
#'
#' bind_table combines analysis results with formatting variables (indentme, newrows, newpage)
#' based on by variables (tablebyvar, rowbyvar), such that appropriate formatting (indentation,
#' line break, page break) can be applied in creating the output. It can also attach the column
#' metadata attribute, which will be automatically used in `gentlg` for creating output.
#'
#' @param ... (required) a set of tidytlg tables to bind together
#' @param colvar (required) treatment variable within df to use to summarize.
#'   Required if `add_count` is TRUE.
#' @param tablebyvar (optional) repeat entire table by variable within df
#' @param rowbyvar (optional) any rowbyvar values used to create the table
#' @param prefix (optional) text to prefix the values of tablebyvar with
#' @param add_count (optional) Should a count be included in the tablebyvar?
#'   (default = TRUE)
#' @param add_format (optional) Should format be added to the output table?
#'   This is done using the add_format function. (default = TRUE)
#' @param column_metadata_file (optional) An excel file for column_metadata.
#'   Does not change the behavior of the function binds the column metadata
#'   for `gentlg`. If a column_metadata dataframe is passed in too,
#'   this is ignored.
#' @param column_metadata (optional) A dataframe containing the column metadata.
#'   This will be used in place of column_metadata_file.
#' @param tbltype (optional) A value used to subset the column_metadata_file.
#'
#' @return The tidytlg tables bound together reflecting the tablebyvars used
#' @export
#'
#' @examples
#' library(magrittr)
#'
#' # bind tables together
#' t1 <- cdisc_adsl %>%
#'   freq(colvar = "TRT01PN",
#'        rowvar = "ITTFL",
#'        statlist = statlist("n"),
#'        subset = ITTFL == "Y",
#'        rowtext = "Analysis set: ITT")
#'
#' t2 <- cdisc_adsl %>%
#'   univar(colvar = "TRT01PN",
#'          rowvar = "AGE",
#'          decimal = 0,
#'          row_header = "Age, years")
#'
#' bind_table(t1, t2)
#'
#' # bind tables together w/by groups
#' t1 <- cdisc_adsl %>%
#'   freq(colvar = "TRT01PN",
#'        rowvar = "ITTFL",
#'        rowbyvar = "SEX",
#'        statlist = statlist("n"),
#'        subset = ITTFL == "Y",
#'        rowtext = "Analysis set: ITT")
#'
#' t2 <- cdisc_adsl %>%
#'   univar(colvar = "TRT01PN",
#'          rowvar = "AGE",
#'          rowbyvar = "SEX",
#'          decimal = 0,
#'          row_header = "Age, years")
#'
#' bind_table(t1, t2, rowbyvar = "SEX")
#'
#' # bind tables together w/table by groups
#' t1 <- cdisc_adsl %>%
#'   freq(colvar = "TRT01PN",
#'        rowvar = "ITTFL",
#'        tablebyvar = "SEX",
#'        statlist = statlist("n"),
#'        subset = ITTFL == "Y",
#'        rowtext = "Analysis set: ITT")
#'
#' t2 <- cdisc_adsl %>%
#'   univar(colvar = "TRT01PN",
#'          rowvar = "AGE",
#'          tablebyvar = "SEX",
#'          decimal = 0,
#'          row_header = "Age, years")
#'
#' bind_table(t1, t2, tablebyvar = "SEX")
#'
#' # w/prefix
#' bind_table(t1, t2, tablebyvar = "SEX", prefix = "Gender: ")
#'
#' # w/counts
#' bind_table(t1, t2, tablebyvar = "SEX", add_count = TRUE, colvar = "TRT01PN")
bind_table <- function(...,
                       colvar = NULL,
                       tablebyvar = NULL,
                       rowbyvar = NULL,
                       prefix = NULL,
                       add_count = FALSE,
                       add_format = TRUE,
                       column_metadata_file = NULL,
                       column_metadata = NULL,
                       tbltype = NULL) {

  # Logic to unnest list if passed in generate_results
  dfs_ <- list(...)
  if (length(dfs_) == 1 && all(class(dfs_[[1]]) == "list"))
    dfs_ <- dfs_[[1]]

  # check all the arguments being passed in except ...
  arglist <- list()
  args_to_chk <- names(formals())[names(formals()) != "..."]
  purrr::walk(args_to_chk, .f = function(x) {
    arglist[[x]] <<- eval(rlang::sym(x))
  }
    )
  check_bind_table(dfs_, arglist)

  # set up the environment for the iteration of anbr to happen in
  env <- new.env()
  if (is.null(tablebyvar)) {
    res <- map_dfr(dfs_, ~add_anbr(.x, env = env)) %>%
      {if (add_format) add_format(., tableby = tablebyvar, groupby = rowbyvar)
        else .
        }
  } else {

    dfs <- purrr::map_dfr(dfs_, ~add_rowtext_by(.x, tablebyvar = tablebyvar,
                                                env = env))

    if (add_count) {
      first_freq <- min(which(purrr::map_chr(dfs_, first_class) ==
                                "tidytlg.freq"))
      denoms_ <- attr(dfs_[[first_freq]], "denom")

      res <- dfs %>%
        nest(data_nest = -all_of(tablebyvar))

      if (is.null(colvar)) stop("bind_table is missing colvar")
      for (i in seq_len(nrow(res))) {
        cur_denoms_ <- get_tby_denoms(denoms_, tablebyvar,
                                      res[i, tablebyvar][[1]], colvar)
        res[i, "data_nest"] <- res[i, "data_nest"] %>%
          extract2(1) %>%
          extract2(1) %>%
          add_row(!!!as.list(cur_denoms_),
                  label = paste0(prefix, res[i, tablebyvar][[1]]),
                  row_type = "TABLE_BY_HEADER",
                  .before = 1) %>%
          list() %>%
          list()
      }
      res <- res %>%
        unnest(data_nest) %>%
        ungroup() %>%
        {if (add_format) add_format(., tableby = tablebyvar, groupby = rowbyvar)
          else .
          }
    } else {

      res <- dfs %>%
        nest(data_nest = -all_of(tablebyvar)) %>%
        rowwise() %>%
        mutate(data_nest = list(
          data_nest %>%
            add_row(label = paste0(prefix, !!sym(tablebyvar)),
                    row_type = "TABLE_BY_HEADER", anbr = 0,
                    .before = 1))) %>%
        unnest(data_nest) %>%
        ungroup() %>%
        {if (add_format) add_format(., tableby = tablebyvar, groupby = rowbyvar)
          else .
          }
    }

  }

  if (!is.null(c(column_metadata_file, column_metadata)) && !is.null(tbltype)) {
    if (is.null(column_metadata)) {
      column_metadata <- readxl::read_excel(column_metadata_file, sheet = 1)
    }

    attr(res, "column_metadata") <- column_metadata %>%
      filter(tbltype == !!tbltype)
  }

  res
}

#' add_rowtext_by
#'
#' Adds in new rows with `label` equal to `rowtext` for each `tablebyvar` group
#'
#' @param df dataframe
#' @param tablebyvar df field that breaks apart table
#' @param env environment
#'
#' @return df with rowtext row header added
#' @noRd
add_rowtext_by <- function(df, tablebyvar, env) {

  if (any(df[[tablebyvar]] == "")) {
    rowtext <- df[df[[tablebyvar]] == "", "label"][[1]]
    df <- df %>%
      nest(data_nest = -all_of(tablebyvar)) %>%
      rowwise() %>%
      filter(!(!!sym(tablebyvar) == "")) %>%
      mutate(data_nest = list(data_nest %>%
                                add_row(label = rowtext,
                                        row_type = "HEADER",
                                        .before = 1))) %>%
      unnest(data_nest)
  }

  df <- df %>%
    add_anbr(env = env)

  if ("anbr" %in% names(df))
    df[is.na(df[["anbr"]]), "anbr"] <- unique(df[is.na(df[["anbr"]]), "anbr"])

  df
}

#' add_anbr
#'
#' Adds or updates anbr counter field in `df` which comes from `anbr_counter`
#' variable in `env`
#'
#' @param df dataframe
#' @param env environment
#'
#' @return df with rowby row header added
#' @noRd
add_anbr <- function(df, env = parent.frame()) {
  # check if the counter variable exists in the specified env
  if (!exists("anbr_counter", envir = env)) {
    # set up anbr counter to be used later
    assign("anbr_counter", 0, envir = env)
  }

  # check if anbr has been added or if it's not a valid numeric
  if ("anbr" %in% names(df) &&
      !all(is.na(suppressWarnings(as.numeric(df[["anbr"]]))))) {
    # update counter to be the max anbr in the input df for future layers
    anbr_values <- df[["anbr"]]
    assign("anbr_counter",
           max(c(suppressWarnings(as.numeric(df[["anbr"]])),
                 get("anbr_counter", envir = env) + 1), na.rm = TRUE),
           envir = env)
    # return df
    df %>%
      select(-"anbr") %>%
      mutate(anbr = suppressWarnings(as.numeric(anbr_values)))
  } else {
    if ("anbr" %in% names(df)) {
      df <- df %>%
        select(-"anbr")
    }
    # get the value from the parent env
    anbr_value <- get("anbr_counter", envir = env) + 1
    # increment the anbr_counter
    assign("anbr_counter", anbr_value, envir = env)
    # return df with anbr added
    df %>%
      mutate(anbr = anbr_value)
  }
}

#' get_tby_denoms
#'
#' Filters _denoms for current tablebyvar in `cur_tby` var
#'
#' @param denoms_ denominator
#' @param tablebyvar repeat entire table by variable within df
#' @param cur_tby current by
#' @param colvar treatment variable within df to use to summarize
#'
#' @return `denoms_` filtered for tablebyvar
#' @noRd
get_tby_denoms <- function(denoms_, tablebyvar, cur_tby, colvar) {
  tmp <- denoms_ %>%
    filter(!!sym(paste0("denom_", tablebyvar)) == as.character(cur_tby))

  cur_denoms <- tmp %>%
    extract2("denom") %>%
    as.character()

  names(cur_denoms) <- tmp[[paste0("denom_", colvar)]]

  cur_denoms
}
