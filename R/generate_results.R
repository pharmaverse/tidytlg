#' Generate Results using Table and Column Metadata
#'
#' @param table_metadata dataframe containing table metadata (see
#'   ?table_metadata for details)
#' @param column_metadata_file An excel file with the data for column_metadata.
#'   The file is read in with `readxl::read_excel()`. Should not be used with
#'   `column_metadata` argument. Results in a dataframe containing the column
#'   metadata that is passed to tlgsetup (see `tlgsetup()` for details). If a
#'   column_metadata dataframe is passed in too, this is ignored.
#' @param column_metadata A dataframe containing the column metadata. This will
#'   be used in place of `column_metadata_file`.
#' @param env environment to find dataframe specified in the table metadata
#'   (defaults to parent environment)
#' @param tbltype If used, this will be used to subset the `column_metadata` based
#'   on the `tbltype` column.
#' @param add_count Passed to `bind_table()` should counts be added for
#'   `tablebyvars`?
#'
#' @return dataframe of results
#' @export
generate_results <-
  function(table_metadata,
           column_metadata_file = NULL,
           column_metadata = NULL,
           env = parent.frame(),
           tbltype = NULL,
           add_count = FALSE) {

  if (is.null(table_metadata)) {
    stop("Argument table_metadata to function generate_results is required
         but no value has been supplied",
         call. = FALSE)
  }

  if (!("anbr" %in% names(table_metadata)))
    table_metadata$anbr <- 1:nrow(table_metadata)

  # check incoming metadata
  arglist <- list()
  args_to_chk <- names(formals())[names(formals()) != "..."]
  purrr::walk(
    args_to_chk,
    .f = function(x) {
      arglist[[x]] <<- eval(rlang::sym(x))
    }
  )
  check_generate_results(arglist)

  if (is.null(column_metadata) && !is.null(column_metadata_file)) {
    column_metadata <- read_excel(column_metadata_file, sheet = 1)  %>%
      mutate_if(is.character, function(x) {
        x %>% stringr::str_replace_all("\\\\U2190", "\U2190")
        }) %>%
      mutate_if(is.character, function(x) {
        x %>% stringr::str_replace_all("\\\\U2192", "\U2192")
        }) %>%
      mutate_if(is.character, function(x) {
        x %>% stringr::str_replace_all("\\\\U2264", "\U2264")
        }) %>%
      mutate_if(is.character, function(x) {
        x %>% stringr::str_replace_all("\\\\U2265", "\U2265")
        }) %>%
      mutate_if(is.character, function(x) {
        x %>% stringr::str_replace_all("\\\\U2260", "\U2260")
        }) %>%
      mutate_if(is.character, function(x) {
        x %>% stringr::str_replace_all("\\\\U00b1", "\U00b1")
        }) %>%
      mutate_if(is.character, function(x) {
        x %>% stringr::str_replace_all("\\\\U03b1", "\U03b1")
        }) %>%
      mutate_if(is.character, function(x) {
        x %>% stringr::str_replace_all("\\\\U03b2", "\U03b2")
        }) %>%
      mutate_if(is.character, function(x) {
        x %>% stringr::str_replace_all("\\\\U03bc", "\U03bc")
        }) %>%
      mutate_if(is.character, function(x) {
        x %>% stringr::str_replace_all("\\\\U00ab", "\U00ab")
        })
  } else if (is.null(column_metadata_file) &&
             is.null(column_metadata)) {
    stop(
      "Either column_metadata_file or column_metadata must be supplied
      to generate_results"
    )
  }

  chunks <- split(table_metadata, as.numeric(rownames(table_metadata)))

# pass each record to the funtion and return one set of results for each list
# element
  result_list <- purrr::map(seq_len(length(chunks)), function(i) {
    do_tidytlg(chunks[[i]],
               column_metadata = column_metadata,
               column_metadata_file = column_metadata_file,
               tbltype = tbltype,
               env = env)
    })

  tablebys <- unique(unlist(purrr::map(chunks, ~extract2(., "tablebyvar"))))

  res <- bind_table(result_list,
                    tablebyvar = tablebys,
                    add_count = add_count,
                    colvar = "colnbr",
                    add_format = FALSE) %>%
    add_format(tableby = tablebys)

  attr(res, "column_metadata") <- column_metadata %>%
# Per JCEV-16: Any non breaking space(utf8 160) should be collapsed into space
# (utf8 32)
    mutate(
      tbltype = gsub("\u00A0", " ", tbltype)
    ) %>%
    filter(tbltype == !!tbltype)

  structure(
    res,
    class = c("tidytlg.metadata_table", class(res))
  )
}

do_tidytlg <-
  function(x,
           column_metadata,
           column_metadata_file,
           tbltype,
           env = parent.frame()) {

  # The dfs are characters, so this takes them and turns them into the actual
  # data.frames
  x <- as.list(x)
  x <- x[!purrr::map_lgl(x, is.na)]
  if ("statlist" %in% names(x))
    st <- if (class(x$statlist)[1] == "list") x$statlist[[1]] else x$statlist
  x <- purrr::map(x[names(x) != "statlist"], unlist)
  if (exists("st")) x$statlist <- st
  x$df <- get(x$df, envir = env) %>%
    tlgsetup(var = x$colvar,
             column_metadata = column_metadata,
             column_metadata_file = column_metadata_file,
             tbltype = tbltype)
  if (!is.null(x$denom_df) && exists(x$denom_df, envir = env))
    x$denom_df <- get(x$denom_df, envir = env) %>%
    tlgsetup(var = x$colvar,
             column_metadata = column_metadata,
             column_metadata_file = column_metadata_file,
             tbltype = tbltype)

  x$colvar <- "colnbr"
  # If a value is a function argument, pass it into the function. Otherwise
  # append it to the output table
  do.call(x$func, x)
}
