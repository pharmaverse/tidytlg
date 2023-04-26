#'Descriptive statistics
#'
#'Univariate statitstics for a variables by treatment and/or group.
#'
#' @param df (required) dataframe containing records to summarize by treatment
#' @param colvar (required) character vector of the treatment variable within
#'   the dataframe
#' @param tablebyvar (optional) repeat entire table by variable within df
#' @param rowvar (required) character vector of variable to summarize within the
#'   dataframe
#' @param rowbyvar (optional) repeat `rowvar` by variable within df
#' @param statlist (optional) statlist object of stats to keep (default =
#'   statlist(c("N", "MEANSD", "MEDIAN", "RANGE", "IQRANGE")))
#' @param decimal (optional) decimal precision root level, when using
#'   `presisionby` this will be used as the base decimal cap (default = 1)
#' @param precisionby (optional) vector of by variable(s) to use when
#'   calculating parameter based precision
#' @param precisionon (optional) variable to use when calculating parameter
#'   based precision. If `precisionby` is specified but not `precisionon` this will
#'   default to `rowvar`
#' @param wide (optional) logical indicating to convert labels to column and
#'   columns to labels (default = FALSE)
#' @param alpha (optional) alpha level for 2-sided confidence interval (default
#'   = 0.05)
#' @param rowtext (optional) A text string to replace the `label` value on the
#'   table. Useful for tables with a single row.
#' @param row_header (optional) A row to add as a header for the table.
#' @param .keep (optional) Should the `rowbyvar` and `tablebyvar` be output in the
#'   table.  If FALSE, `rowbyvar` will still be output in the `label` column.
#'   (default = TRUE)
#' @param .ord Should the ordering columns be output with the table? This is
#'   useful if a table needs to be merged or reordered in any way after build.
#' @param ... (optional) Named arguments to be included as columns on the table.
#'
#'@return dataframe of results
#'@export
#'
#' @examples
#' adsl <-
#'   structure(
#'     list(
#'       USUBJID = c("DEMO-101", "DEMO-102", "DEMO-103", "DEMO-104",
#'                   "DEMO-105", "DEMO-106"),
#'       AGE = c(59, 51, 57, 65, 21, 80),
#'       SEX = c("F", "M", "F", "M", "F", "M"),
#'       WEIGHTBL = c(83.6, 75, 84, 90, 65, 70),
#'       colnbr = structure(
#'         c(1L, 3L, 2L, 2L, 3L, 1L),
#'         .Label = c("Placebo", "Low", "High"),
#'         class = "factor"
#'       )
#'     ),
#'     row.names = c(NA, 6L),
#'     class = "data.frame"
#'   )
#'
#' # N, Mean(SD), Median, Range, IQ Range for a rowvar by colvar
#' univar(adsl
#'        ,colvar = "colnbr"
#'        ,rowvar = "AGE")
#'
#' # N and Mean for a rowvar by colvar
#' univar(adsl
#'        ,colvar   = "colnbr"
#'        ,rowvar   = "AGE"
#'        ,statlist = statlist(c("N", "MEAN")))
#'
#' # N and Mean for a rowvar by colvar and a by variable
#' univar(adsl
#'        ,colvar   = "colnbr"
#'        ,rowvar   = "AGE"
#'        ,rowbyvar = "SEX"
#'        ,statlist = statlist(c("N", "MEAN")))
#'
#' # Below illustrates how make the same calls to univar() as above, using table
#' # and column metadata # along with generate_results().
#'
#' column_metadata <- tibble::tribble(
#'   ~tbltype, ~coldef,   ~decode,
#'   "type1",     "0",  "Placebo",
#'   "type1",     "54",     "Low",
#'   "type1",     "81",    "High"
#' )
#'
#' # N, Mean(SD), Median, Range, IQ Range for a rowvar by colvar
#' table_metadata <- tibble::tribble(
#'   ~anbr,  ~func,    ~df, ~rowvar, ~tbltype, ~colvar,
#'   "1", "univar", "cdisc_adae",   "AGE",  "type1", "TRTA"
#' )
#'
#' generate_results(table_metadata, column_metadata = column_metadata,
#'                  tbltype = "type1")
#'
#'
#' # N and Mean for a rowvar by colvar
#' table_metadata <- tibble::tribble(
#'   ~anbr,  ~func,    ~df, ~rowvar, ~tbltype,  ~colvar, ~statlist,
#'   "1", "univar", "cdisc_adae",   "AGE",  "type1", "TRTA",
#'   statlist(c("N","MEAN"))
#' )
#'
#' generate_results(table_metadata, column_metadata = column_metadata,
#'                  tbltype = "type1")
#'
#'
#' # N and Mean for a rowvar by colvar and a by variable
#' table_metadata <- tibble::tribble(
#'   ~anbr,  ~func,    ~df, ~rowvar, ~tbltype,  ~colvar, ~statlist,  ~by,
#'   "1", "univar", "cdisc_adae",   "AGE",  "type1", "TRTA",
#'   statlist(c("N","MEAN")), "SEX"
#' )
#'
#' generate_results(table_metadata, column_metadata = column_metadata,
#'                  tbltype = "type1")
univar <- function(df,
                    colvar = NULL,
                    tablebyvar = NULL,
                    rowvar = NULL,
                    rowbyvar = NULL,
                    statlist = getOption("tidytlg.univar.statlist.default"),
                    decimal = 1,
                    precisionby = NULL,
                    precisionon = NULL,
                    wide = FALSE,
                    alpha = 0.05,
                    rowtext = NULL,
                    row_header = NULL,
                    .keep = TRUE,
                    .ord = FALSE,
                    ...) {

  # check all the arguments being passed in except ...
  arglist <- list()
  args_to_chk <- names(formals())[names(formals()) != "..."]
  purrr::walk(
    args_to_chk,
    .f = function(x) {
      arglist[[x]] <<- eval(rlang::sym(x))
      }
  )
  check_univar(arglist)

  # get just the stats out of statlist for use in function
  statlist <- statlist[["stats"]]

  # build precision data for later
  if (!is.null(precisionby) ||
      !is.null(precisionon)) {
    if (!all(c(precisionby) %in% (c(tablebyvar, rowbyvar)))) {
      stop(
        "All values of argument `precisionby` must be a part of either
          `tablebyvar` or `rowbyvar` arguments",
        call. = FALSE
      )
    }
    if (is.null(precisionon))
      precisionon <- rowvar
    precision_data <-
      make_precision_data(df, decimal, precisionby, precisionon)
  } else {
    precision_data <- NA
  }

  univars <- df %>%
    # Derive the statistics for the table. Logic in this function will determine
    # what needs to be done based on statlist. Results in a long dataframe with
    # one value per row containing all necessary statistics
    derive_univar(colvar, tablebyvar, rowbyvar, rowvar, statlist, alpha) %>%
    # Numeric values are rounded in this function.
    round_univar(statlist, decimal, precisionby, precision_data) %>%
    # Cells that contain multiple statistics (i.e. MEANSD) are merged here.
    # Still a long dataframe with one row per value but merged statistics
    # are present but their individual components (i.e. SD) are not.
    arrange_univar(statlist, colvar, tablebyvar, rowbyvar, alpha) %>%
    # The final table is pivoted here.
    pivot_univar(wide = wide,
                 statlist = statlist,
                 tablebyvar = tablebyvar,
                 colvar = colvar,
                 rowbyvar = rowbyvar,
                 rowtext = rowtext,
                 row_header = row_header,
                 rowvar = rowvar,
                 .keep = .keep,
                 .ord = .ord,
                 ...)

  attr(univars, "colvar") <- colvar
  attr(univars, "tablebyvar") <- tablebyvar
  attr(univars, "rowbyvar") <- rowbyvar
  attr(univars, "rowvar") <- rowvar
  attr(univars, "statlist") <- statlist
  attr(univars, "decimal") <- decimal
  attr(univars, "precisionby") <- precisionby
  attr(univars, "wide") <- wide
  attr(univars, "alpha") <- alpha
  attr(univars, "rowtext") <- rowtext
  attr(univars, "row_header") <- row_header

  structure(univars,
            class = c("tidytlg.univar", class(univars)))

}

#' Derive statistics for univar table
#'
#' @return A long dataframe with all the stats needed to display or arrange
#'
#' @noRd
derive_univar <-
  function(df,
           colvar,
           tablebyvar,
           rowbyvar,
           rowvar,
           statlist,
           alpha) {
    name_levels <- unique(replace_statlist(statlist))
    name_levels <- name_levels[name_levels != "tval"]

suppressWarnings(
  df %>%
    # Drop any NA values in df before derivation
    tidyr::drop_na(.data[[rowvar]]) %>%
    dplyr::group_by_at((c(colvar, tablebyvar, rowbyvar))) %>%
    # Get summaries to derive. See get summaries for the interface between what
    # the user passes and what is derived
    dplyr::summarise(
      rowvar = rowvar,
      !!!get_summaries(statlist, rowvar, alpha)
    ) %>%
    dplyr::ungroup() %>%
    # Flip the columns from the summaries into a long dataframe
    pivot_longer(cols = any_of(replace_statlist(statlist))) %>%
    # remove tval if there
    filter(name != "tval") %>%
    # make the name a factor
    mutate(name = factor(name, levels = name_levels)) %>%
    # Logic for completing the factors in the data.frame. This could be replaced
    # by a preprocessing call similar to char2factor
    complete(!!!syms(c(colvar, tablebyvar, rowbyvar, "name")),
             fill = list(rowvar = rowvar, value = NaN)) %>%

    mutate(
      across(.cols = any_of(c(rowbyvar, tablebyvar)),
                  .fns = function(var) {
                    if (is.factor(var)) {
                      unclass(var)
                    } else {
                      var2 <- as.factor(var)
                      unclass(var2)
                    }
                  },
                  .names = "{.col}_ord")
      ))

}

#' Get number of decimals
#'
#' @return a vector of how many decimals there are
#'
#' @noRd
get_decimals <- function(v) {
  v %>%
    replace_na(0) %>%
    map(function(n) {
      if (str_detect(as.character(n), "\\.")) {
        nchar(str_split(as.character(n), "\\.", simplify = TRUE))[, 2]
      } else {
        0
      }
    }) %>%
    unlist()
}

#' Get precision from data
#'
#' @return a dataframe with precision by variables and data precision values
#'
#' @noRd
make_precision_data <-
  function(df, decimal, precisionby, precisionon) {
    if (!is.null(precisionby)) {
      df %>%
        group_by(!!!syms(precisionby)) %>%
        summarise(decimal = min(max(get_decimals(
          !!!syms(precisionon)
        )), decimal)) %>%
        ungroup() %>%
        complete(!!!syms(c(precisionby)), fill = list(decimal = decimal)) %>%
        select(!!!syms(c(precisionby, "decimal"))) %>%
        unique()
    } else {
      df %>%
        summarise(decimal = min(max(get_decimals(
          !!!syms(precisionon)
        )), decimal)) %>%
        select("decimal") %>%
        unique()
    }
  }

#' Round values from derived dataframe
#'
#' @return returns update of dataframe replacing missing values
#'
#' @noRd
update_missing <- function(df) {
  missing_update <- c("N"       = "0",
                        "SUM"     = "-",
                        "MEAN"    = "-",
                        "GeoMEAN" = "-",
                        "SD"      = "-",
                        "SE"      = "-",
                        "CV"      = "-",
                        "GSD"     = "-",
                        "GSE"     = "-",
                        "MEANSD"  = "-",
                        "MEANSE"  = "-",
                        "MEDIAN"  = "-",
                        "MIN"     = "-",
                        "MAX"     = "-",
                        "RANGE"   = "(-; -)",
                        "Q1"      = "-",
                        "Q3"      = "-",
                        "IQRANGE" = "(-; -)",
                        "MEDRANGE"   = "(-; -)",
                        "MEDIQRANGE" = "(-; -)",
                        "MEAN_CI"    = "- (-; -)",
                        "GeoMEAN_CI" = "- (-; -)")

  missing_base <-   c("N"          = "-",
                        "SUM"        = "-",
                        "MEAN"       = "-",
                        "GeoMEAN"    = "-",
                        "SD"         = "-",
                        "SE"         = "-",
                        "CV"         = "-",
                        "GSD"        = "-",
                        "GSE"        = "-",
                        "MEANSD"     = "- (-)",
                        "MEANSE"     = "- (-)",
                        "MEDIAN"     = "-",
                        "MIN"        = "-",
                        "MAX"        = "-",
                        "RANGE"      = "(-; -)",
                        "Q1"         = "-",
                        "Q3"         = "-",
                        "IQRANGE"    = "(-; -)",
                        "MEDRANGE"   = "(-; -)",
                        "MEDIQRANGE" = "(-; -)",
                        "MEAN_CI"    = "- (-; -)",
                        "GeoMEAN_CI" = "- (-; -)")
  ret <- df %>%
    group_by(name) %>%
    mutate(
      value = ifelse(value == missing_base[[name[1]]],
                     missing_update[[name[1]]], value),
      value = ifelse(
        value %>% stringr::str_detect(" ") &
          missing_update[[name[1]]]  %>% str_detect(" ") &
      value %>% stringr::str_detect("$[0-9]+(\\.[0-9]+)* (\\(-\\)|\\(-; -\\))"),
        paste0(
          value %>% stringr::str_extract("$[0-9]+(\\.[0-9]+)*"),
          missing_update[[name[1]]] %>% stringr::str_extract(" .+")
        ),
        value
      )
    ) %>%
    ungroup()
  return(ret)
}

#' Round values from derived dataframe
#'
#' @return A long dataframe with rounded values.
#'
#' @noRd
round_univar <-
  function(df,
           statlist,
           decimal,
           precisionby,
           precision_data) {
    # The default lengths
    dg <- getOption("tidytlg.precision.extra")

    if (!is.null(precision_data)) {
      dvec <- dg %>%
        crossing(precision_data) %>%
        mutate(prec = if_else(stat == "N", 0, extra + decimal))

      if (!is.null(precisionby)) {
        df %>%
          left_join(dvec, by = c(precisionby, "name" = "stat")) %>%
          rowwise() %>%
          mutate(value = unname(roundSAS(
            value,
            digits = prec,
            as_char = TRUE,
            na_char = "-"
          ))) %>%
          ungroup()
      } else {
        df %>%
          left_join(dvec, by = c("name" = "stat")) %>%
          rowwise() %>%
          mutate(value = unname(roundSAS(
            value,
            digits = prec,
            as_char = TRUE,
            na_char = "-"
          ))) %>%
          ungroup()
      }
    } else {
      dvec <- dg %>%
        mutate(prec = if_else(stat == "N", 0, extra + decimal))

      df %>%
        left_join(dvec, by = c("name" = "stat")) %>%
        rowwise() %>%
        mutate(value = unname(roundSAS(
          value,
          digits = prec,
          as_char = TRUE,
          na_char = "-"
        ))) %>%
        ungroup()
    }

  }

#' Arrange values for merged stats
#'
#' Some stats are composed of different stats (MEANSD)
#'
#' @return A dataframe where values are arranged with respect to
#' merged statistics
#'
#' @noRd
arrange_univar <-
  function(df,
           statlist,
           colvar,
           tablebyvar,
           rowbyvar,
           alpha) {
  # The data.frame used to turn the stat into stat labels. Options are defaulted
  # in tidytlg.R.  Confidence Interval is updated if alpha is not 0.05
    stat_labels <- getOption("tidytlg.stat_labels")
    if (alpha != 0.05) {
      pctile <- 100 - (alpha * 100)
      stat_labels[stat_labels$stat == "MEAN_CI", "label"] <-
        paste0("Mean (", as.character(pctile), "% C.I.)")
      stat_labels[stat_labels$stat == "GeoMEAN_CI", "label"] <-
        paste0("Geometric Mean (", as.character(pctile), "% C.I.)")
    }
    # The stats that are composed of multiple statistics
    merged_stats <-
      statlist[!(statlist %in% replace_statlist(statlist))]
    # The stats that are used to derive other stats, but not displayed
    dropped_vars <-
      replace_statlist(statlist)[!(replace_statlist(statlist) %in% statlist)]

    # Expressions for how the results will be displayed for merged stats
    arrange_expr <- exprs(
      MEANSD = paste0(.data$MEAN, " (", .data$SD, ")"),
      MEANSE = paste0(.data$MEAN, " (", .data$SE, ")"),
      RANGE = paste0("(", .data$MIN, "; ", .data$MAX, ")"),
      IQRANGE = paste0("(", .data$Q1, "; ", .data$Q3, ")"),
      MEDRANGE = paste0(.data$MEDIAN, " (", .data$MIN, "; ", .data$MAX, ")"),
      MEDIQRANGE = paste0(.data$MEDIAN, " (", .data$Q1, "; ", .data$Q3, ")"),
      MEAN_CI = paste0(.data$MEAN, " (", .data$LCL_MEAN, "; ",
                       .data$UCL_MEAN, ")"),
      GeoMEAN_CI = paste0(
        .data$GeoMEAN,
        " (",
        .data$LCL_GeoMEAN,
        "; ",
        .data$UCL_GeoMEAN,
        ")"
      )
    )[merged_stats]

    # process merged stats if there are any
    if (length(merged_stats) && nrow(df) > 0) {
      df %>%
        # Temporarily pivot the table to allow for easy arranging
        pivot_wider(
          id_cols = c(any_of(
            c("rowvar", colvar, tablebyvar, rowbyvar)
          ), ends_with("_ord")),
          names_from = "name",
          values_from = "value"
        ) %>%
        # Arrange the stats that need to be arranged
        dplyr::mutate(!!!arrange_expr) %>%
        # Only select the merged statistics
        select(c(all_of(
          c("rowvar", !!colvar, !!tablebyvar, !!rowbyvar, merged_stats)
        ), ends_with("_ord"))) %>%
        # Pivot the merged statistics dataframe to a long dataframe
        pivot_longer(cols = c(any_of(merged_stats))) %>%
 # The . is the merged stats and df is the original data.frame. Now it contains
 # The merged stats and the original stats
        bind_rows(df) %>%
        # Drop the stats that aren't being displayed
        filter(!(name %in% dropped_vars)) %>%
        rowwise() %>%
        # Change original names to labels to display
        mutate(label = ifelse(
          as.character(name) %in% stat_labels$stat,
          stat_labels[stat_labels$stat == as.character(name), "label"][[1]],
          as.character(name)
        )) %>%
        update_missing()
    } else {
      # if there are no merged stats we just need to make the label column
      df %>%
        # Drop the stats that aren't being displayed
        filter(!(name %in% dropped_vars)) %>%
        rowwise() %>%
        # Change original names to labels to display
        mutate(label = ifelse(
          as.character(name) %in% stat_labels$stat,
          stat_labels[stat_labels$stat == as.character(name), "label"][[1]],
          as.character(name)
        )) %>%
        update_missing()
    }


  }

#' Pivot univar table
#'
#' @return The final table that is ready for column headers and formatting
#'
#' @noRd
pivot_univar <-
  function(df,
           wide,
           statlist,
           tablebyvar,
           colvar,
           rowbyvar,
           rowtext,
           row_header,
           rowvar,
           .keep,
           .ord,
           ...) {
    ord_columns <- names(df)[endsWith(names(df), "_ord")]
    if (.keep)
      drop_columns <- "rowvar"
    else
      drop_columns <- c("rowvar", rowbyvar, tablebyvar)
    if (!.ord)
      drop_columns <- c(drop_columns,
                        ord_columns)

    if (!wide) {
      df2 <- df %>%
        arrange(!!!syms(c(colvar, tablebyvar, rowbyvar)), "rowvar",
                match(df$name, statlist)) %>%
        pivot_wider(id_cols = any_of(
          c(
            colvar,
            tablebyvar,
            rowbyvar,
            "rowvar",
            "label",
            ord_columns
          )
        ), names_from = all_of(c(colvar))) %>%
        mutate(row_type = ifelse(label == "N", "N", "VALUE")) %>%
        mutate(
          across(
            .cols = - ends_with("_ord"),
            .fns = ~ as.character(.x)
          ),
          across(
            .cols = - ends_with("_ord"),
            .fns = ~ replace_na_with_blank(.x)
          )
        ) %>%
        byvar_merge_univar(
          rowvar = rowvar,
          rowbyvar = rowbyvar,
          tablebyvar = tablebyvar,
          rename_col = "label",
          row_header = row_header
        )

    } else {
      df2 <- df %>%
        arrange(!!!syms(c(colvar, tablebyvar, rowbyvar)),
                match(df$name, statlist)) %>%
        pivot_wider(id_cols = c(any_of(c(
          colvar, tablebyvar, rowbyvar
        )), ends_with("_ord")),
        names_from = "label") %>%
        mutate(row_type = "VALUE") %>%
        mutate(
          across(
            .cols = - ends_with("_ord"),
            .fns = ~ as.character(.x)
          ),
          across(
            .cols = - ends_with("_ord"),
            .fns = ~ replace_na_with_blank(.x)
          )
        ) %>%
        byvar_merge_univar(
          rowvar = NULL,
          rowbyvar = rowbyvar,
          tablebyvar = tablebyvar,
          rename_col = colvar,
          row_header = row_header
        )
    }
    df2 %>%
      rowtext_univar(rowtext, rowvar, rowbyvar, tablebyvar, row_header) %>%
      # adding on the group_level variable for indentation
      add_group_level(rowbyvar) %>%
      select(- any_of(drop_columns)) %>%
      mutate(!!!list(...),
             across(
               .cols = - ends_with("_ord"),
               .fns = ~ replace_na_with_blank(.)
             ))
  }

#' Return the summaries that need to be calculated
#'
#' @return A list of expressions to evaluate on the data.
#'
#' @noRd
get_summaries <- function(statlist, rowvar, alpha) {
  # Replace statlist with all needed summaries to compute
  statlist <- replace_statlist(statlist)

  summaries <- exprs(
    N = dplyr::n(),
    SUM = base::sum(.data[[rowvar]]),
    MEAN = base::mean(.data[[rowvar]]),
    SD = stats::sd(.data[[rowvar]]),
    SE = .data$SD / base::sqrt(.data$N),
    CV = (.data$SD / .data$MEAN) * 100,
    MEDIAN = stats::median(.data[[rowvar]]),
    MIN = ifelse(all(is.na(.data[[rowvar]])), NA_real_,
                 base::min(.data[[rowvar]])),
    MAX = ifelse(all(is.na(.data[[rowvar]])), NA_real_,
                 base::max(.data[[rowvar]])),
    Q1 = stats::quantile(.data[[rowvar]], 0.25, type = 2),
    Q3 = stats::quantile(.data[[rowvar]], 0.75, type = 2),
    GeoMEAN = ifelse(any(.data[[rowvar]] < 0), NA_real_, exp(base::mean(log(
      .data[[rowvar]][.data[[rowvar]] > 0]
    )))),
    GSD = ifelse(any(.data[[rowvar]] < 0), NA_real_, exp(stats::sd(log(
      .data[[rowvar]][.data[[rowvar]] > 0]
    )))),
    GSE = ifelse(any(.data[[rowvar]] < 0), NA_real_, exp(stats::sd(log(
      .data[[rowvar]][.data[[rowvar]] > 0]
    )) / sqrt(length(
      .data[[rowvar]][.data[[rowvar]] > 0]
    )))),
    tval = ifelse(.data$N > 1, stats::qt(1 - (alpha / 2),
                                         df = (.data$N - 1)), NA_real_),
    LCL_MEAN = .data$MEAN - .data$tval * .data$SE,
    UCL_MEAN = .data$MEAN + .data$tval * .data$SE,
    LCL_GeoMEAN = ifelse(
      any(.data[[rowvar]] < 0),
      NA_real_,
      exp(
        base::mean(log(.data[[rowvar]][.data[[rowvar]] > 0])) -
          .data$tval * stats::sd(log(.data[[rowvar]][.data[[rowvar]] > 0])) /
          sqrt(length(.data[[rowvar]][.data[[rowvar]] > 0]))
      )
    ),
    UCL_GeoMEAN = ifelse(
      any(.data[[rowvar]] < 0),
      NA_real_,
      exp(
        base::mean(log(.data[[rowvar]][.data[[rowvar]] > 0])) +
          .data$tval * stats::sd(log(.data[[rowvar]][.data[[rowvar]] > 0])) /
          sqrt(length(.data[[rowvar]][.data[[rowvar]] > 0]))
      )
    )
  )

  summaries[statlist][order(match(statlist, names(summaries)))]


}

#' Convert a user list of stats into stats that need to be derived
#'
#' @return A vector of stats that need to be derived. Converted from a list that
#'   may or may not include stats that are merged versions.
#'
#' @noRd
replace_statlist <- function(statlist) {
  replaced_stats <- c("MEANSD", "MEANSE", "RANGE", "IQRANGE", "MEDRANGE",
                      "MEDIQRANGE", "MEAN_CI", "GeoMEAN_CI", "SE", "CV",
                      "LCL_MEAN", "UCL_MEAN", "LCL_GeoMEAN", "UCL_GeoMEAN")
  new_stats <- list(MEANSD = c("MEAN", "SD", "N"),
                    MEANSE = c("MEAN", "SE", "SD", "N"),
                    RANGE = c("MIN", "MAX"),
                    IQRANGE = c("Q1", "Q3"),
                    MEDRANGE = c("MEDIAN", "MIN", "MAX"),
                    MEDIQRANGE = c("MEDIAN", "Q1", "Q3"),
                    MEAN_CI = c("MEAN", "LCL_MEAN", "UCL_MEAN", "tval", "SE",
                                "SD", "N"),
                    GeoMEAN_CI = c("GeoMEAN", "LCL_GeoMEAN", "UCL_GeoMEAN",
                                   "tval", "N"),
                    SE = c("SE", "SD", "N"),
                    CV = c("CV", "SD", "MEAN"),
                    LCL_MEAN = c("LCL_MEAN", "MEAN", "tval", "SE", "SD", "N"),
                    UCL_MEAN = c("UCL_MEAN", "MEAN", "tval", "SE", "SD", "N"),
                    LCL_GeoMEAN = c("LCL_GeoMEAN", "tval", "N"),
                    UCL_GeoMEAN = c("UCL_GeoMEAN", "tval", "N"))

  # Logic for replacing statlist
  present_replacements <- statlist[statlist %in% replaced_stats]
  present_replacements %>%
    map(~ new_stats[[.]]) %>%
    unlist() %>%
    c(statlist[!(statlist %in% replaced_stats)])
}

byvar_merge_univar <-
  function(df,
           rowvar,
           rowbyvar,
           tablebyvar,
           rename_col,
           row_header) {
    if (!is.null(row_header)) {
      df <- df %>%
        nest(nest = - any_of(c(
          rowbyvar,
          rowvar,
          tablebyvar,
          paste0(rowbyvar, "_ord"),
          paste0(tablebyvar, "_ord")
        ))) %>%
        rowwise() %>%
        mutate(nest = list(
          add_row(
            nest,
            !!rename_col := row_header,
            row_type = "HEADER",
            .before = 1
          )
        )) %>%
        unnest(nest)
    }

    if (is.null(rowbyvar))
      return(df)

    if (length(rowbyvar) == 1) {
      df %>%
        nest(nest = - any_of(c(
          rowbyvar, rowvar, tablebyvar, paste0(rowbyvar, "_ord")
        ))) %>%
        rowwise() %>%
        mutate(nest = list(
          add_row(
            nest,
            !!rename_col := as.character(!!sym(rowbyvar)),
            row_type = "BY_HEADER1",
            .before = 1
          ) %>%
            mutate(across(
              .cols = - ends_with("_ord"),
              .fns = ~ replace_na_with_blank(.x)
            ))
        )) %>%
        unnest(nest)
    } else {
      df %>%
        nest(nest = - any_of(c(
          rowbyvar, rowvar, tablebyvar, paste0(rowbyvar, "_ord")
        ))) %>%
        rowwise() %>%
        mutate(nest = list(
          add_row(
            nest, !!rename_col := as.character(!!sym(rowbyvar[2])),
            row_type = "BY_HEADER2",
            .before = 1
          ) %>%
            mutate(across(
              .cols = - ends_with("_ord"),
              .fns = ~ replace_na_with_blank(.x)
            ))
        )) %>%
        unnest(nest) %>%
        nest(nest = - any_of(c(
          rowbyvar[1], rowvar, tablebyvar, paste0(rowbyvar[1], "_ord")
        ))) %>%
        rowwise() %>%
        mutate(nest = list(
          add_row(
            nest,
            !!rename_col := as.character(!!sym(rowbyvar[1])),
            row_type = "BY_HEADER1", !!paste0(rowbyvar[[2]], "_ord") := 0,
            .before = 1
          ) %>%
            mutate(across(
              .cols = - ends_with("_ord"),
              .fns = ~ replace_na_with_blank(.x)
            ))
        )) %>%
        unnest(nest)
    }


  }

rowtext_univar <-
  function(res,
           rowtext,
           rowvar,
           rowbyvar,
           tablebyvar,
           row_header) {
    if (is.null(rowtext)) {
      return(res)
    }

    res_group <- c(rowbyvar, tablebyvar)
    len_oner <-
      #length of single row table
      as.numeric(!is.null(row_header)) + as.numeric(!is.null(rowbyvar)) + 1
    res <- res %>% group_by_at(res_group)
    unq_labels        <- unique(res$label)
    names(unq_labels) <- unq_labels
    if (is_named(rowtext)) {
      unq_labels[which(unq_labels %in% rowtext)] <-
        names(rowtext)[which(rowtext %in% unq_labels)]
    }

    suppressWarnings(res <-
                       res %>% mutate(
                         label = case_when(
                           is_named(rowtext) ~ unq_labels[label],
                           length(rowtext) == 1 &
                             len_oner == n() & row_number() == n() ~ rowtext[1],
                           TRUE ~ label
                         )
                       ))

    res <- res %>% ungroup()
  }
