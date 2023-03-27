#' check_req_arg
#'
#' @param arg required argument to check
#'
#' @return TRUE/FALSE based on if argument is NULL
#' @noRd
check_req_arg <- function(arg) {
  arg <- arg[[1]]
  !is.null(arg)
}

#' check_is_statlist
#'
#' @param arg required argument to check
#'
#' @return TRUE/FALSE if is statlist
#' @noRd
check_is_statlist <- function(arg) {
  return(class(arg)[1] == "statlist")
}

#' check_is_nested_rowvar
#'
#' @param arg required argument to check
#'
#' @return TRUE/FALSE if is rowvar is correct
#' @noRd
check_is_nested_rowvar <- function(arg) {
  if (!stringr::str_detect(arg, "[a-zA-Z0-9]+\\*[a-zA-Z0-9]+")) {
    return(FALSE)
  }
  if (stringr::str_detect(arg,
                "[a-zA-Z0-9]+\\*[a-zA-Z0-9]+\\*[a-zA-Z0-9]+\\*[a-zA-Z0-9]+")) {
    return(FALSE)
  }
  return(stringr::str_detect(arg, "^[a-zA-Z0-9]+\\*([a-zA-Z0-9]+\\**)+$"))
}


#' check_is_data_frame
#'
#' @param arg argument to check
#'
#' @return TRUE/FALSE based on if argument is a dataframe
#' @noRd
check_is_data_frame <- function(arg) {
  arg <- arg[[1]]
  is.data.frame(arg)
}

#' check_is_numeric
#'
#' @param arg argument to check
#'
#' @return TRUE/FALSE based on if argument is a numeric
#' @noRd
check_is_numeric <- function(arg) {
  arg <- arg[[1]]
  is.numeric(arg)
}

#' check_is_boolean
#'
#' @param arg argument to check
#'
#' @return TRUE/FALSE based on if argument is a boolean
#' @noRd
check_is_boolean <- function(arg) {
  arg <- arg[[1]]
  is.logical(arg)
}

#' check_is_character
#'
#' @param arg argument to check
#'
#' @return TRUE/FALSE based on if argument is a character
#' @noRd
check_is_character <- function(arg) {
  arg <- arg[[1]]
  is.character(arg)
}


#' check_arg_exists
#'
#' @param arg required argument to check
#'
#' @return TRUE/FALSE based on if argument exists in df
#' @noRd
check_arg_exists <- function(arg, df) {
  arg <- arg[[1]]
  if (length(arg) == 1) {
    if (arg %>% stringr::str_detect("\\*")) {
      arg <- arg %>%
        stringr::str_split("\\*") %>%
        base::unlist()
    }
  }
  if (!all(arg %in% names(df))) {
    return(FALSE)
  }
  return(TRUE)
}
#' check_arg_exists
#'
#' @param arg required argument to check
#'
#' @return TRUE/FALSE based on if argument exists in df
#' @noRd
check_file <- function(arg) {
  arg <- arg[[1]]
  if (is.null(arg)) {
    return(TRUE)
  }
  return(file.exists(arg))
}


#' check_words
#'
#' @param ... permissible words in the list
#' @param values the list to check for specific words
#'
#' @noRd
check_words <- function(..., values) {
  accepted_words <- unlist(c(...))
  expr <- expr(function(values) values %in% !!accepted_words)
  make_function(body = expr, env = parent.frame())()
}

make_function <- function(args = pairlist(), body, env = parent.frame()) {
  eval(call("function", args, body), env)
}


#' check_statlist_univar
#'
#' @param vec statlist argument to univar
#'
#' @return TRUE/FALSE based on if stat in accepted values
#' @noRd
check_statlist_univar <- function(statlist_values) {
  accepted_stats <- c(
    "N", "SUM", "MEAN", "GeoMEAN", "SD", "SE", "CV", "GSD", "GSE",
    "MEANSD", "MEANSE", "MEDIAN", "MIN", "MAX", "RANGE", "Q1", "Q3",
    "IQRANGE", "MEDRANGE", "MEDIQRANGE", "MEAN_CI", "GeoMEAN_CI"
  )
  statlist_values[!(statlist_values %in% accepted_stats)]
}

#' check_statlist_freq
#'
#' @param vec statlist argument to freq and nested_freq
#'
#' @return TRUE/FALSE based on if stat in accepted values
#' @noRd
check_statlist_freq <- function(statlist_values) {
  accepted_stats <- c(
    "n", "n (x.x)", "n (x.x%)", "n/N", "n/N (x.x)",
    "n/N (x.x%)", "N"
  )
  statlist_values[!(statlist_values %in% accepted_stats)]
}

#' check_cutoff_stat
#'
#' @param cutoff_stat argument from freq/nested_freq
#'
#' @return TRUE/FALSE based on if stat in accepted values
#' @noRd
check_cutoff_stat <- function(func, cutoff_stat) {
  if (cutoff_stat %in% c("pct", "n")) {
    return()
  }
  stop("Cutoff stat ", cutoff_stat,
       " for function ", func, "\n\n", call. = FALSE)
}




#' check_arg_tlf
#'
#' @param arg tlg argument from gentlg
#'
#' @return TRUE/FALSE based on if arg in accepted values
#' @noRd
check_arg_tlf <- function(arg) {
  func <- check_words("t", "l", "f", "g")
  do.call(func, list(substr(tolower(arg), 1, 1)))
}

#' check_arg_format
#'
#' @param arg tlg argument from gentlg
#'
#' @return TRUE/FALSE based on if arg in accepted values
#' @noRd
check_arg_format <- function(arg) {
  func <- check_words("rtf", "html")
  do.call(func, list(tolower(arg)))
}

#' check_names_column_metadata
#'
#' @param column_metadata column metadata from generate_results()
#'
#' @return TRUE/FALSE based on if names are in accepted values
#' @noRd
check_names_column_metadata <- function(column_metadata) {
  all(c("coldef", "decode") %in% names(column_metadata))
}

#' check_any_na
#'
#' @param vec vector to check
#'
#' @return TRUE/FALSE based on if NA values are found
#' @noRd
check_any_na <- function(vec) {
  !any(is.na(vec))
}

#' check_all_na
#'
#' @param vec vector to check
#'
#' @return TRUE/FALSE based on if vector is all NA values
#' @noRd
check_all_na <- function(vec) {
  !all(is.na(vec))
}

#' check_geo_est
#'
#' @param rowvar row variable to check for geometric estimability
#'
#' @return TRUE/FALSE based on if geometric stats are estimable
#' @noRd
check_geo_est <- function(rowvar, df) {
  !any(df[[rowvar[[1]]]] < 0)
}

#' check_decimal
#'
#' @param decimal
#'
#' @return TRUE/FALSE if number is greater than or equal to 0
#' @noRd
check_decimal <- function(decimal) {
  0 <= decimal
}


#' check_nested_vars
#'
#' @param nested_vars argument that has nested vars to check
#'
#' @return TRUE/FALSE if argument is structured correctly
#' @noRd
check_nested_vars <- function(nested_vars) {
  stringr::str_detect(nested_vars, "^([:alnum:]+\\*)([:alnum:]+\\*)?[:alnum:]+$")
}

#' check_in_df
#'
#' @param func argument base function name
#' @param arglist  argument to check
#' @param df_field which dataframe to test
#' @param args_req_check required argument to check
#'
#' @return NULL
#' Throws error if arg does not exist in df
#' @noRd
check_in_df <- function(func,
                        arglist,
                        df_field = "df",
                        args_req_check =
                          c("colvar", "rowvar",
                            "rowbyvar", "tablebyvar",
                            "precisionby", "precisionon",
                            "groupby", "tableby", "idvars", "var")) {
  args_req_check <- args_req_check[args_req_check %in% names(arglist)]

  tests <- tidyr::tibble(args = "arglist", el = args_req_check)
  tests <- tests %>% dplyr::mutate(
    test = "check_arg_exists",
    level = "stop", msg = paste0(
      el, " '", arglist[el],
      "' does not exist in ", df_field, " or is all NA for function: ", func
    ),
    req_df = TRUE
  )
  check_args(func, arglist, tests, field = df_field)
}

#' check_nested_rowvar
#'
#' @param func argument base function name
#' @param arglist required argument to check
#' @param df_field required df to check
#'
#' @return NULL
#' Throws error if arg does not exist in df
#' @noRd
check_nested_rowvar <- function(func, arglist, df_field = "df") {
  tests <- tidyr::tibble(args = "arglist", el = "rowvar")
  tests <- tests %>% dplyr::mutate(
    test = "check_is_nested_rowvar",
    level = "stop",
    msg = "Argument rowvar to function nested_freq is incorrectly formatted.",
    req_df = FALSE
  )
  check_args(func, arglist, tests, field = df_field)
}

#' check_in_df_bind_table
#'
#' @param func argument base function name
#' @param dfs list of dfs to check
#' @param arglist required argument to check
#'
#' @return NULL
#' Throws error if arg does not exist in df
#' @noRd
check_in_df_bind_table <- function(func, dfs, arglist) {
  args_req_check <- c("rowbyvar", "tablebyvar")
  args_req_check <- args_req_check[args_req_check %in% names(arglist)]

  count <- 1
  for (i in dfs) {
    arglist_tmp <- append(arglist, list(i))
    names(arglist_tmp)[length(arglist_tmp)] <- "df"
    tests <- tidyr::tibble(args = "arglist", el = args_req_check)
    tests <- tests %>% dplyr::mutate(
      test = "check_arg_exists",
      level = "stop", msg = paste0(
        el, " '", arglist[el],
        "' does not exist in df ", count, " or is all NA for function: ", func
      ),
      req_df = TRUE
    )
    check_args(func, arglist_tmp, tests)
    count <- count + 1
  }
}

#' check_req_fields
#'
#' @param func argument base function name
#' @param arglist required argument to check
#' @param req_fields fields in df that are required
#'
#' @return NULL
#' Throws error if arg does not exist in df
#' @noRd
check_req_fields <- function(func, arglist, req_fields) {
  df <- arglist[["df"]]
  missing <- req_fields[!(req_fields %in% names(df))]
  if (length(missing) == 0) {
    return()
  }

  stop("Reqiured fields ", paste0(missing, colapse = ", "),
       " for function ", func, "\n\n", call. = FALSE)
}

#' check_cutoff_format
#'
#' @param func that is being checked
#' @param cutoff to check
#'
#' @return TRUE/FALSE if is cutoff format is correct
#' @noRd
check_cutoff_format <- function(func, cutoff) {
  if (is.null(cutoff)) {
    return()
  }
  if (stringr::str_detect(cutoff, "^([a-zA-Z0-9]+|[a-zA-Z0-9]+ >= [a-zA-Z0-9]+( (&|\\|) [a-zA-Z0-9]+ >= [a-zA-Z0-9]+)*)$")) {
    return()
  }
  stop("cutoff for function ", func,
       " is incorrectly formated\n\n", call. = FALSE)
}


#' check_req_fields
#'
#' @param func argument base function name
#' @param arglist required argument to check
#' @param req_fields fields in df that are required
#'
#' @return NULL
#' Throws error if arg does not exist in df
#' @noRd
check_req_fields <- function(func, arglist, req_fields) {
  df <- arglist[["df"]]
  missing <- req_fields[!(req_fields %in% names(df))]
  if (length(missing) == 0) {
    return()
  }

  stop("Reqiured fields ", paste0(missing, colapse = ", "),
       " for function ", func, "\n\n", call. = FALSE)
}

#' check_req_vars
#'
#' @param func argument base function name
#' @param arglist required argument to check
#' @param lst_req required arguments for function
#'
#' @return NULL
#' Throws error if required arg is missing
#' @noRd
check_req_vars <- function(func, arglist, lst_req) {
  tests <- tidyr::tibble(args = "arglist", el = lst_req)
  tests <- tests %>% dplyr::mutate(
    test = "check_req_arg",
    level = "stop", msg = paste0(
      "Argument ",
      el, " to function ", func,
      " is required but no value has been supplied"
    ),
    req_df = FALSE
  )
  check_args(func, arglist, tests)
}

#' check_file_exists
#'
#' @param func argument base function name
#' @param arglist required argument to check
#' @param file required arguments for function
#'
#' @return NULL
#' Throws error file missing
#' @noRd
check_file_exists <- function(func, arglist, file) {
  tests <- tidyr::tibble(args = "arglist", el = file)
  tests <- tests %>% dplyr::mutate(
    test = "check_file",
    level = "stop", msg = paste0(
      "Argument ",
      file, " to function ", func,
      " file does not exist."
    ),
    req_df = FALSE
  )
  check_args(func, arglist, tests)
}


#' check_var_types
#'
#' @param func argument base function name
#' @param arglist required argument to check
#' @param lst_types required arguments types
#'
#' @return NULL
#' Throws error if arg is incorrect types
#' @noRd
check_var_types <- function(func, arglist, lst_types) {
  tests_for_types <- list(
    "data.frame" = "check_is_data_frame",
    "numeric" = "check_is_numeric",
    "character" = "check_is_character",
    "boolean" = "check_is_boolean"
  )
  tests <- tidyr::tibble(args = "arglist", el = names(lst_types))
  tests <- tests %>% dplyr::mutate(
    test = tests_for_types[lst_types],
    level = "stop", msg = paste0(
      "Argument ",
      el, " to function ", func,
      " should be type ", lst_types
    ),
    req_df = FALSE
  )
  check_args(func, arglist, tests)
}


#' check_var_duplicates
#'
#' @param func argument base function name
#' @param arglist required argument to check
#'
#' @return NULL
#' Throws error if arg reference the same value
#' @noRd
check_var_duplicates <- function(func, arglist) {
  args_req_check <- c("colvar", "rowvar", "rowbyvar",
                               "tablebyvar", "groupby", "tableby")
  args_req_check <- arglist[names(arglist) %in% args_req_check]

  if (length(args_req_check) < 2) {
    return()
  }

  # Breaks apart nested rowvar
  if (!is.null(arglist[["rowvar"]])) {
    if (arglist[["rowvar"]] %>% stringr::str_detect("\\*")) {
      row_var_list <- arglist[["rowvar"]] %>%
        stringr::str_split("\\*") %>%
        base::unlist()
      names(row_var_list) <- paste0("nested rowvar ", seq_along(row_var_list))
      args_req_check <- args_req_check[- (which(names(args_req_check) == "rowvar"))]
      args_req_check <- append(args_req_check, row_var_list)
    }
  }
  df_dupes <- as.data.frame(args_req_check)
  lst_dupes <- unlist(args_req_check)

  df_dupes <- df_dupes %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      CHECK = length(args_req_check) == length(unique(dplyr::c_across(cols = everything())))
      )

  if (df_dupes$CHECK[1]) {
    return()
  }

  dup_count <- df_dupes %>%
    t() %>%
    base::as.data.frame() %>%
    dplyr::count(V1) %>%
    dplyr::filter(n > 1)
  first <- TRUE
  for (i in dup_count$V1) {
    if (first) {
      msg <- paste0(paste0(names(lst_dupes[lst_dupes == i]), collapse = ", "),
                    " all have value ", i, " for function ", func)
      first <- FALSE
    } else {
      msg <- paste0(msg, "\n", paste0(names(lst_dupes[lst_dupes == i]),
                collapse = ", "), " all have value ", i, " for function ", func)
    }
  }

  stop(paste0(msg, "\n\n"), call. = FALSE)
}

#' check_statlist
#'
#' @param func argument base function name
#' @param astatlist statlist to test
#'
#' @return NULL
#' Throws error if statlist contains incorrect stat or stats
#' @noRd
check_statlist <- function(func, astatlist) {
  if (is.null(astatlist)) {
    msg <- paste0(
      "statlist for function ", func,
      " is empty\nIf you would like to see a full list of included stats ",
      "type `?", func, "` in the console"
    )
  } else {
    if (func == "univar") {
      errs <- check_statlist_univar(astatlist)
    } else {
      errs <- check_statlist_freq(astatlist)
    }

    if (length(errs) == 0) {
      return()
    }

    msg <- paste0(
      "The following values are included in statlist",
      " to function ", func, " but are not allowed: ",
      paste0(errs, collapse = ", "),
      " if you would like to see a full list of included stats ",
      "type `?", func, "` in the console"
    )
  }
  stop(paste0(msg, "\n\n"), call. = FALSE)
}


#' check_statlist_N
#' Throws error if N is used with no other args used by freq functions
#' @param func argument base function name
#' @param astatlist statlist to test
#'
#' @return NULL
#' @noRd
check_statlist_N <- function(func, astatlist) {
  if (!("N" %in% astatlist)) {
    return()
  }
  if ("N" %in% astatlist && length(astatlist) != 1) {
    return()
  }

  msg <- paste0("Stat N must be used with another stat ex. statlist(c('N','n (x.x%)')) for function ", func)

  stop(msg, call. = FALSE)
}

#' check_freq_cutoff
#' @param df freq dataframe
#'
#' @return NULL
#' Throws error if cutoff results in empty df, called by freq
#' @noRd
check_freq_cutoff <- function(df) {
  if (nrow(df) == 0) {
    stop("cutoff resulted in an empty table for function freq\n\n",
         call. = FALSE)
  } else {
    return()
  }
}

#' check_geo_stats
#' @param arglist arguments to check
#'
#' @return NULL
#' Throws error if geo stats can not be calculated and are present
#' @noRd
check_geo_stats <- function(arglist) {
  # add optional checks for special cases
  if (!any(arglist[["statlist"]][["stats"]] %in%
           c("GeoMEAN", "GSD", "GSE", "GeoMEAN_CI"))) {
    return()
  }

  tests <- tibble(
    args = "arglist", el = "rowvar", test = "check_geo_est",
    level = "stop",
    msg = (paste0(
      el, " argument to ", "univar",
      " contain negative values, geometric stats are not estimable"
    )), req_df = TRUE
  )

  # run tier two tests
  check_args("univar", arglist, tests)
}

#' check_alpha
#' @param alpha argument for univar
#'
#' @return NULL
#' Throws error if alpha is not between 0 and 1
#' @noRd
check_alpha <- function(alpha) {
  if (0 <= alpha && alpha <= 1.0) {
    return()
  }
  stop("alpha must be a number in between 0 and 1 for function univar\n\n",
       call. = FALSE)
}



#' check_opath
#'
#' @param opath argument to check if they exist
#'
#' @return NULL
#' Throws error if opath does not exist
#' @noRd
check_opath <- function(opath) {
  if (file.exists(opath)) {
    return()
  }
  stop(paste0("opath '", opath, "' does not exist for function gentlg", "\n\n"),
       call. = FALSE)
}

#' check_wcol
#' @param huxme dataframe to check
#' @param wcol argument to check if they exist
#'
#' @return NULL
#' Throws error if `wcol` is not the correct length
#' @noRd
check_wcol <- function(huxme, wcol) {
  lst_to_remove <- c(
    "anbr", "roworder", "boldme", "indentme", "newrows", "newpage",
    "rowvar", "row_type", "nested_level", "group_level"
  )
  huxme_output <- huxme[- (which(names(huxme) %in% lst_to_remove))]
  huxme_output <- huxme_output %>% dplyr::select(!c(any_of("func"),
                                                    ends_with("_ord")))

  if (length(wcol) == 1 || length(wcol) == length(huxme_output)) {
    return()
  }
  stop(paste0("wcol's length must be 1 or the length of final output", "\n\n"),
       call. = FALSE)
}

#' check_plotnames
#' Throws error if `plotnames` do not exist
#' @param plotnames plotname to check
#'
#' @return NULL
#' @noRd
check_plotnames <- function(plotnames) {
  if (is.null(plotnames)) {
    return()
  }
  if (file.exists(plotnames)) {
    return()
  }
  stop(paste0("plotnames '", plotnames,
              "' does not exist for function gentlg", "\n\n"), call. = FALSE)
}

#' check_orientation
#'
#' @param orientation argument to check if they exist
#'
#' @return NULL
#' Throws error if orientation is incorrect
#' @noRd
check_orientation <- function(orientation) {
  if (orientation %in% c("landscape", "portrait")) {
    return()
  }
  stop(paste0("orientation '", orientation,
              "' is not either 'landscape' or 'portrait' for function gentlg",
              "\n\n"), call. = FALSE)
}

#' check_dfs_formats
#'
#' @param dfs to check for format issues
#'
#' @return NULL
#' Throws error if any df has a missing required field
#' @noRd
check_dfs_formats <- function(dfs) {
  index_missing <- c()
  count <- 1
  for (i in dfs) {
    if (!all((c("row_type", "group_level") %in% names(i)))) {
      index_missing <- c(index_missing, count)
    }
    count <- count + 1
  }
  if (length(index_missing) == 0) {
    return()
  }
  stop(paste0("dfs ", paste0(index_missing, collapse = ", "),
              " are missing the required fields for function bind_table",
              "\n\n"), call. = FALSE)
}

#' check_dfs
#'
#' @param dfs to check if provided
#'
#' @return NULL
#' Throws error if any df has a missing required field
#' @noRd
check_dfs <- function(dfs) {
  if (length(dfs) >= 1) {
    return()
  }
  stop("dfs should be provided for function bind_table\n\n", call. = FALSE)
}

#' check_column_metadata_file
#'
#' @param column_metadata_file meta data file to check
#'
#' @return NULL
#' Throws error if file does not exist then throws another error if file exists but has incorrect version
#' @noRd
check_column_metadata_file <- function(column_metadata_file) {
  if (!file.exists(column_metadata_file)) {
    stop(paste0(
      "column_metadata_file '", column_metadata_file,
      "' does not exist for function generate_results", "\n\n"
    ), call. = FALSE)
  }
  check_column_metadata(readxl::read_excel(column_metadata_file))
}

#' check_column_metadata
#'
#' @param column_metadata to check for format issues
#'
#' @return NULL
#' Throws error if any column_metadata has a missing required field
#' @noRd
check_column_metadata <- function(column_metadata) {
  if (is.null(column_metadata)) {
    stop(paste0(
      "column_metadata is missing for function generate_results",
      "\n\n"
    ), call. = FALSE)
  }

  if (!all(c("tbltype", "coldef", "decode") %in% names(column_metadata))) {
    cheked_fields <- c("tbltype", "coldef", "decode", "span1")
    needed_fields <- cheked_fields[!cheked_fields %in% names(column_metadata)]
    stop(paste0(
      "column_metadata requries fields '", paste0(needed_fields, collapse = "',
                                                  '"),
      "' for function generate_results",
      "\n\n"
    ), call. = FALSE)
  }
}

#' check_table_metadata
#'
#' @param table_metadata to check for format issues
#' @param env to check for values
#'
#' @return NULL
#' Throws error if any column_metadata has a missing required field
#' @noRd
check_table_metadata <- function(table_metadata, env) {
  if (is.null(table_metadata)) {
    stop(paste0(
      "column_metadata is missing for function generate_results",
      "\n\n"
    ), call. = FALSE)
  }

  if (!all(c("func", "df", "colvar", "rowvar") %in% names(table_metadata))) {
    check_fields <- c("func", "df", "colvar", "rowvar")
    needed_fields <- check_fields[!check_fields %in% names(table_metadata)]
    stop(paste0(
      "table_metadata requries fields '", paste0(needed_fields, collapse = "',
                                                 '"),
      "' for function generate_results",
      "\n\n"
    ), call. = FALSE)
  }
  for (i in table_metadata[["func"]]) {
    if (!(i %in% c("nested_freq", "freq", "univar"))) {
      stop(paste0(
        "table_metadata field 'func' can either be 'freq',
        'nested_freq', 'univar' for function generate_results",
        "\n\n"
      ), call. = FALSE)
    }
  }

  for (i in table_metadata[["df"]]) {
    if (!exists(i, envir = env)) {
      stop(paste0(
        "table_metadata field 'df' points to data frame that does not exist for function generate_results",
        "\n\n"
      ), call. = FALSE)
    }
  }
}

#' check_elements
#'
#' @param source_func name of function (for warning and error clarification)
#' @param args list of a arguments from function
#' @param el argument to be checked
#' @param func function to be used to check the argument
#' @param level stop/warning level of function to be used if issues are found
#' @param msg message to be evaluated if test fails
#' @param req_df does test require the df argument to test
#' @param df_field name of df from args that will be checked
#'
#' @return list of errors and warnings generated
#' @noRd
check_elements <- function(source_func, args, el,
                           func, level, msg, req_df, df_field) {
  element <- as_string(el)
  vec <- args[element]

  # get the stats out of statlist
  if (el == "statlist") {
    vec <- vec$statlist[["stats"]]
  }

  # unlist things so we can run them through the do.call
  if (c(el) %in% c("statlist", "cutoff_stat", "variable_names")) {
    vec <- unlist(vec)
  }

  field_to_grab <- df_field
  if (req_df) {
    passed_vec <- list(vec, args[[field_to_grab]])
  } else {
    passed_vec <- list(vec)
  }

  # run test functions
  failures <- vec %>%
    discard(~ do.call(func, passed_vec)) %>%
    unique()


  # create output messages
  if (length(failures) > 0) {
    if (is.language(msg)) {
      string <- eval(msg)
    } else if (!is.na(msg)) {
      string <- msg
    } else {
      string <- paste0(
        "Argument ", element,
        " to function ", source_func,
        " fails ", func, " check \n"
      )
    }
  } else {
    string <- NULL
  }

  # assign output depending on level
  if (level == "stop") {
    error_string <- string
    warning_string <- NULL
  } else if (level == "warning") {
    error_string <- NULL
    warning_string <- string
  }

  list(warning = warning_string, error = error_string)
}


#' check_args
#'
#' @param func function that is being checked
#' @param arglist list of arguments to be used
#' @param tests tibble of tests to be used
#' @param field dataframe name for testing
#' @noRd
check_args <- function(func, arglist, tests, field = "df") {
  messages <- pmap(
    tests,
    ~ check_elements(
      func, get(..1), sym(..2), ..3, ..4, ..5, ..6, field
    )
  )

  # errors
  errors <- map(messages, "error") %>%
    compact() %>%
    paste0(., collapse = "\n\n")
  if (errors != "") {
    stop(paste0(errors, "\n\n"), call. = FALSE)
  }

  # warnings
  warnings <- map(messages, "warning") %>%
    compact() %>%
    paste0(., collapse = "\n\n")
  if (warnings != "") {
    warning(paste0(warnings, "\n\n"), call. = FALSE)
  }
}

#' check_univar
#'
#' @param arglist arguments to univar function
#'
#' @noRd
check_univar <- function(arglist) {
  if (!check_is_statlist(arglist[["statlist"]])) {
    stop("statlist must be of type statlist EX. statlist(c('N'))")
  }

  check_req_vars("univar", arglist, c("df", "colvar", "rowvar"))

  var_type_check <- c(
    "df" = "data.frame", "wide" = "boolean",
    "colvar" = "character", "rowvar" = "character",
    "rowbyvar" = "character", "decimal" = "numeric",
    "precisionby" = "character", "precisionon" = "character",
    "rowtext" = "character", "row_header" = "character",
    ".keep" = "boolean", " .ord" = "boolean",
    "alpha" = "numeric"
  )



  var_type_check <- var_type_check[names(var_type_check) %in% names(arglist)]
  check_var_types("univar", arglist, var_type_check)
  # run stats tests
  check_in_df("univar", arglist)
  check_var_duplicates("univar", arglist)
  check_statlist("univar", arglist[["statlist"]]$stats)
  check_geo_stats(arglist)
  check_alpha(arglist[["alpha"]])
}

#' check_freq
#'
#' @param arglist arguments to freq function
#'
#' @noRd
check_freq <- function(arglist) {
  if (!check_is_statlist(arglist[["statlist"]])) {
    stop("statlist must be of type statlist EX. statlist(c('n'))")
  }

  check_req_vars("freq", arglist, c("df", "colvar", "rowvar"))

  var_type_check <- c(
    "df" = "data.frame", "denom_df" = "data.frame",
    "colvar" = "character", "rowvar" = "character",
    "rowbyvar" = "character", "decimal" = "numeric",
    "cutoff_stat" = "character", "descending_by" = "character",
    "display_missing" = "boolean", "pad" = "boolean",
    "rowtext" = "character", "row_header" = "character",
    ".keep" = "boolean", ".ord" = "boolean",
    "nested" = "boolean"
  )
  var_type_check <- var_type_check[names(var_type_check) %in% names(arglist)]
  check_var_types("freq", arglist, var_type_check)
  # run stats tests
  check_in_df("freq", arglist)
  if (!is.null(arglist["denom_df"])) {
    check_in_df("freq", arglist, df_field = "denom_df",
                args_req_check = c("colvar"))
  }
  check_var_duplicates("freq", arglist)
  check_statlist("freq", arglist[["statlist"]]$stats)
  check_statlist_N("freq", arglist[["statlist"]]$stats)
  check_cutoff_stat("freq", arglist[["cutoff_stat"]])
  check_cutoff_format("freq", arglist[["cutoff"]])
}
#' check_nested_freq
#'
#' @param arglist arguments to nested_freq function
#'
#' @noRd
check_nested_freq <- function(arglist) {
  if (!check_is_statlist(arglist[["statlist"]])) {
    stop("statlist must be of type statlist EX. statlist(c('N'))")
  }
  # statlist is passed through as a vector so need to evaluate
  arglist[["statlist"]] <- eval_tidy(arglist[["statlist"]])



  check_req_vars("nested_freq", arglist, c("df", "colvar", "rowvar"))

  var_type_check <- c(
    "df" = "data.frame", "denom_df" = "data.frame",
    "colvar" = "character", "rowvar" = "character",
    "rowbyvar" = "character", "decimal" = "numeric",
    "cutoff_stat" = "character", "descending_by" = "character",
    "display_missing" = "boolean", "pad" = "boolean",
    "rowtext" = "character", "row_header" = "character",
    ".keep" = "boolean", ".ord" = "boolean",
    "nested" = "boolean"
  )
  var_type_check <- var_type_check[names(var_type_check) %in% names(arglist)]
  check_var_types("nested_freq", arglist, var_type_check)
  check_nested_rowvar("nested_freq", arglist)
  check_in_df("nested_freq", arglist)
  check_var_duplicates("nested_freq", arglist)
  check_statlist("nested_freq", arglist[["statlist"]]$stats)
  check_statlist_N("nested_freq", arglist[["statlist"]]$stats)
  check_cutoff_stat("nested_freq", arglist[["cutoff_stat"]])
  check_cutoff_format("nested_freq", arglist[["cutoff"]])
}

#' check_metadata
#'
#' @param arglist arguments to generate_results function
#'
#' @noRd
check_generate_results <- function(arglist) {
  check_req_vars("generate_results", arglist, c("table_metadata"))

  check_table_metadata(arglist[["table_metadata"]], arglist[["env"]])

  if (!is.null(arglist[["column_metadata_file"]])) {
    check_column_metadata_file(arglist[["column_metadata_file"]])
  } else {
    check_column_metadata(arglist[["column_metadata"]])
  }
}

#' check_gentlg
#'
#' @param arglist arguments to gentlg function
#'
#' @noRd
check_gentlg <- function(arglist) {
  check_req_vars("gentlg", arglist, c("tlf", "format"))


  # build tier two tests
  tier_two_tests <- tribble(
    ~ arglist, ~ el, ~ func, ~ issue, ~ msg, ~ use_df,
    "arglist", "tlf", "check_arg_tlf", "stop",
    quote(paste0(
      "The following values are included in argument ", element,
      " to function ", source_func, " but are not allowed: ", all_fails,
      "\nIf you would like to see guidance on this argument ",
      "type `?gentlg` in the console"
    )), FALSE,
    "arglist", "format", "check_arg_format", "stop",
    quote(paste0(
      "The following values are included in argument ", element,
      " to function ", source_func, " but are not allowed: ", all_fails,
      "\nIf you would like to see guidance on this argument ",
      "type `?gentlg` in the console"
    )), FALSE
  )
  # run tests
  check_args("gentlg", arglist, tier_two_tests)
  check_opath(arglist[["opath"]])
  check_plotnames(arglist[["plotnames"]])
  check_in_df("gentlg", arglist, df_field = "huxme")
  if (substr(arglist[["tlf"]], 1, 1) %in% c("t", "T", "L", "l")) {
    check_wcol(arglist[["huxme"]], arglist[["wcol"]])
  }
  check_orientation(arglist[["orientation"]])
}

#' check_add_format
#'
#' @param arglist arguments to gentlg function
#'
#' @noRd
check_add_format <- function(arglist) {
  check_req_vars("add_format", arglist, c("df"))

  var_type_check <- c(
    "df" = "data.frame", "tableby" = "character",
    "groupby" = "character", ".keep" = "boolean"
  )

  var_type_check <- var_type_check[names(var_type_check) %in% names(arglist)]
  check_var_types("add_format", arglist, var_type_check)
  check_in_df("add_format", arglist)
  check_var_duplicates("add_format", arglist)
  check_req_fields("add_format", arglist, c("anbr"))
}

#' check_add_format
#'
#' @param arglist arguments to gentlg function
#'
#' @noRd
check_tlgsetup <- function(arglist) {
  check_req_vars("check_tlgsetup", arglist, c("df"))

  var_type_check <- c(
    "df" = "data.frame", "var" = "character",
    "column_metadata_file" = "character", "column_metadata" = "data.frame",
    "tbltype" = "character"
  )

  var_type_check <- var_type_check[names(var_type_check) %in% names(arglist)]
  check_var_types("check_tlgsetup", arglist, var_type_check)
  check_in_df("check_tlgsetup", arglist)
  check_file_exists("check_tlgsetup", arglist, "column_metadata_file")
}

check_bind_table <- function(dfs, arglist) {
  if (!is.null(arglist[["add_count"]]) && arglist[["add_count"]]) {
    check_req_vars("bind_table", arglist, c("colvar"))
  }
  check_dfs(dfs)
  var_type_check <- c(
    "tablebyvar" = "character", "prefix" = "character",
    "groupby" = "character", ".keep" = "boolean",
    "add_count" = "boolean", "add_format" = "boolean",
    "column_metadata_file" = "character", "column_metadata" = "data.frame",
    "tbltype" = "character", "rowbyvar" = "character",
    "colvar" = "character"
  )

  var_type_check <- var_type_check[names(var_type_check) %in% names(arglist)]
  check_var_types("bind_table", arglist, var_type_check)
  check_in_df_bind_table("bind_table", dfs, arglist)
  check_var_duplicates("bind_table", arglist)
  check_dfs_formats(dfs)
  check_file_exists("bind_table", arglist, "column_metadata_file")
}
