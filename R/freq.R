#'Frequency counts and percentages
#'
#'Frequency counts and percentages for a variable by treatment and/or group.
#'
#'@param df (required) dataframe containing records to summarize by treatment
#'@param denom_df (optional) dataframe used for population based denominators
#'  (default = df)
#'@param colvar (required) treatment variable within df to use to summarize
#'@param tablebyvar (optional) repeat entire table by variable within df
#'@param rowvar (required) character vector of variables to summarize within the
#'  dataframe
#'@param rowbyvar (optional) repeat `rowvar` by variable within df
#'@param statlist (optional) statlist object of stats to keep of length 1 or 2 specifying list
#'  of statistics and format desired (e.g statlist(c("N", "n (x.x\%)"))) (default = statlist(c("n
#'  (x.x)")))
#'@param decimal (optional) decimal precision root level default (default = 1)
#'@param nested (optional) INTERNAL USE ONLY. The default should not be changed.
#'  Switch on when this function is called by `nested_freq()` so we will not include
#'  the by variables as part of the group denominators (default = FALSE)
#'@param cutoff (optional) percentage cutoff threshold. This can be passed as a
#'  numeric cutoff, in that case any rows with greater than or equal to that
#'  cutoff will be preserved, others will be dropped. To specify a single column
#'  to define the cutoff logic, pass a character value of the form "{colName} >=
#'  {value}" and only that column will be used.
#'@param cutoff_stat (optional) The value to cutoff by, n or pct. (default =
#'  'pct'). Can be done with multiple columns by adding & or | ex. `col1` >=
#'  `val1` & `col2` >= `val2`
#'@param subset (optional) An R expression that will be passed to a
#'  `dplyr::filter()` function to subset the data.frame. This is performed on
#'  the numerator before any other derivations. Denominators must be
#'  preprocessed and passed through using `denom_df`.
#'@param descending_by (optional) The column or columns to sort descending
#'  counts. Can also provide a named list to do ascending order ex.
#'  c("VarName1" = "asc", "VarName2" = "desc") would sort by VarName1 in
#'  ascending order and VarName2 in descending order. In case of a tie in count
#'  or `descending_by` not provided, the columns will be sorted alphabetically.
#'@param display_missing (optional) Should the "missing" values be displayed? If
#'  missing values are displayed, denominators will include missing values.
#'  (default = FALSE)
#'@param rowtext (optional) A character vector used to rename the `label`
#'  column. If named, names will give the new level and values will be the
#'  replaced value. If unnamed, and the table has only one row, the rowtext will
#'  rename the label of the row. If the rowtext is unnamed, the table has no
#'  rows, and there is a subset, the table will be populated with zeros and the
#'  label will be the only row.
#'@param row_header (optional) A character vector to be added to the table.
#'@param .keep (optional) Should the `rowbyvar` and `tablebyvar` be output in the
#'  table.  If FALSE, `rowbyvar` will still be output in the `label` column.
#'  (default = TRUE)
#'@param .ord Should the ordering columns be output with the table? This is
#'  useful if a table needs to be merged or reordered in any way after build.
#'@param pad (optional) A boolean that controls if levels with zero records
#'  should be included in the final table. (default = TRUE)
#'@param ... (optional) Named arguments to be included as columns on the table.
#'
#'@section Sorting a 'freq' table:
#'
#'  By default, a frequency table is sorted based on the factor level of the
#'  `rowvar` variable. If the `rowvar` variable isn't a factor, it will be
#'  sorted alphabetically. This behavior can be modified in two ways, the first
#'  is the `char2factor()` function that offers a interface for releveling a
#'  variable based on a numeric variable, like VISITN. The second is based on
#'  the `descending_by` argument which will sort based on counts on a variable.
#'
#'@return A dataframe of results
#'
#' @importFrom rlang enexprs
#'
#'@export
#'
#' @examples
#' adsl <- data.frame(
#'       USUBJID = c("DEMO-101", "DEMO-102", "DEMO-103"),
#'       RACE = c("WHITE", "BLACK", "ASIAN"),
#'       SEX = c("F", "M", "F"),
#'       colnbr = factor(c("Placebo", "Low", "High"))
#'   )
#'
#'# Unique subject count of a single variable
#' freq(adsl
#'      ,colvar = "colnbr"
#'      ,rowvar = "RACE"
#'      ,statlist = statlist("n"))
#'
#'# Unique subject count and percent of a single variable
#' freq(adsl
#'      ,colvar = "colnbr"
#'      ,rowvar = "RACE"
#'      ,statlist = statlist(c("N", "n (x.x%)")))
#'
#'# Unique subject count of a variable by another variable
#' freq(adsl
#'      ,colvar = "colnbr"
#'      ,rowvar = "RACE"
#'      ,rowbyvar = "SEX"
#'      ,statlist = statlist("n"))
#'
#' # Unique subject count of a variable by another variable using colvar and
#' # group to define the denominator
#' freq(adsl
#'      ,colvar = "colnbr"
#'      ,rowvar = "RACE"
#'      ,rowbyvar = "SEX"
#'      ,statlist = statlist("n (x.x%)", denoms_by = c("colnbr", "SEX")))
#'
#'# Cut records where count meets threshold for any column
#' freq(cdisc_adsl
#'      ,rowvar = "ETHNIC"
#'      ,colvar = "TRT01P"
#'      ,statlist = statlist("n (x.x%)")
#'      ,cutoff = "5"
#'      ,cutoff_stat = "n")
#'
#'# Cut records where count meets threshold for a specific column
#' freq(cdisc_adsl
#'      ,rowvar = "ETHNIC"
#'      ,colvar = "TRT01P"
#'      ,statlist = statlist("n (x.x%)")
#'      ,cutoff = "Placebo >= 3"
#'      ,cutoff_stat = "n")
#'
#' # Below illustrates how to make the same calls to freq() as above, using
#' # table and column metadata.
#'
#' # Unique subject count of a single variable
#' table_metadata <- tibble::tribble(
#'   ~anbr,  ~func,          ~df,   ~rowvar,      ~statlist,  ~colvar,
#'   1,     "freq", "cdisc_adsl",  "ETHNIC",  statlist("n"), "TRT01PN"
#' )
#'
#' generate_results(table_metadata,
#'                  column_metadata = column_metadata,
#'                  tbltype = "type1")
#'
#' # Unique subject count and percent of a single variable
#' table_metadata <- tibble::tribble(
#'   ~anbr,  ~func,    ~df,     ~rowvar,     ~statlist,            ~colvar,
#'   "1", "freq", "cdisc_adsl", "ETHNIC", statlist(c("N", "n (x.x%)")),"TRT01PN"
#' )
#'
#' generate_results(table_metadata,
#'                  column_metadata = column_metadata,
#'                  tbltype = "type1")
#'
#' # Cut records where count meets threshold for any column
#' table_metadata <- tibble::tibble(
#'   anbr= "1", func = "freq", df = "cdisc_adsl", rowvar = "ETHNIC",
#'   statlist = statlist("n (x.x%)"), colvar = "TRT01PN", cutoff = 5,
#'   cutoff_stat = "n")
#'
#' generate_results(table_metadata,
#'                  column_metadata = column_metadata,
#'                  tbltype = "type1")
#'
#' # Cut records where count meets threshold for a specific column
#' table_metadata <- tibble::tibble(
#'   anbr= 1, func = "freq", df = "cdisc_adsl", rowvar = "ETHNIC",
#'   statlist = statlist("n (x.x%)"), colvar = "TRT01PN",
#'   cutoff = 'col1 >= 3', cutoff_stat = "n")
#'
#' generate_results(table_metadata,
#'                  column_metadata = column_metadata,
#'                  tbltype = "type1")
freq <- function(df,
                 denom_df = df,
                 colvar = NULL,
                 tablebyvar = NULL,
                 rowvar = NULL,
                 rowbyvar = NULL,
                 statlist = getOption("tidytlg.freq.statlist.default"),
                 decimal = 1,
                 nested = FALSE,
                 cutoff = NULL,
                 cutoff_stat = "pct",
                 subset = TRUE,
                 descending_by = NULL,
                 display_missing = FALSE,
                 rowtext = NULL,
                 row_header = NULL,
                 .keep = TRUE,
                 .ord = FALSE,
                 pad = TRUE,
                 ...) {

  subset <- enexprs(subset)
  if (length(subset) > 0 && is.character(subset[[1]]))
    subset <- as.list(parse(text = subset))
  has_subset <- is_call(subset[[1]])
  rowtext <- expr(!!rowtext)

  # check all the arguments being passed in except ...
  arglist <- list()
  args_to_chk <- names(formals())[names(formals()) != "..."]
  walk(args_to_chk, .f = function(x) arglist[[x]] <<- eval(sym(x)))
  check_freq(arglist)

  if (!pad && is.null(cutoff)) {
    cutoff <- 1
    cutoff_stat <- "n"
  }

  # attempt to use colvar, tablebyvar, rowbyvar to define denominator
  # if all are not present in denom_df, use what is present and alert user to
  # what was used
  vars_in_denom_names <- names(denom_df) %in% c(colvar, tablebyvar, rowbyvar)
  vars_in_denom_df <- names(denom_df)[vars_in_denom_names]

  # If the statlist has a denoms_by config, use that. Otherwise default to
  # colvar, tablebyvar, and rowbyvar
  denoms_by <- statlist[["denoms_by"]] %||% vars_in_denom_df

  # Added to close JCEV-10
  denom_df <- denom_df %>%  {
    if (!display_missing && identical(df, denom_df))
      filter(., !is.na(!!sym(rowvar)))
    else .
  }

  if (getOption("tidytlg.denoms.message"))
    message("denominator set using ", paste0(denoms_by, collapse = ", "),
            ". You can overwrite using the denom_by argument in statlist.\n",
            sep = "")

  colvar_cols <- levels(as.factor(df[[colvar]]))

  freqs <- df %>%
    # filter out based on subset
    filter(!!!subset) %>%
    # Derive counts and percents
    derive_freq(
      denom_df = denom_df,
      colvar = colvar,
      rowbyvar = rowbyvar,
      tablebyvar = tablebyvar,
      rowvar = rowvar,
      statlist = statlist,
      nested = nested,
      rowtext = rowtext,
      has_subset = has_subset,
      denoms_by = denoms_by,
      display_missing = display_missing
    ) %>%
    # Drop values based on 'cutoff' argument
    drop_freq(
      cutoff = cutoff,
      rowvar = rowvar,
      cutoff_stat = cutoff_stat,
      colvar = colvar,
      drop_by = c(tablebyvar, rowbyvar)
    )  %>%
    # round values
    round_freq(decimal = decimal) %>%
    # Arrange values from n and pct into values
    arrange_freq(statlist = statlist) %>%
    # merge in bigN row, tablebyvar, and rowbyvar
    byvar_merge_freq(
      tablebyvar = tablebyvar,
      rowbyvar = rowbyvar,
      colvar = colvar,
      rowvar = rowvar,
      hasBigN = "N" %in% statlist[["stats"]],
      row_header = row_header
    ) %>%
    # Pivot table
    pivot_freq(
      colvar = colvar,
      rowvar = rowvar,
      rowbyvar = rowbyvar,
      tablebyvar = tablebyvar,
      statlist = statlist,
      rowtext = rowtext,
      row_header = row_header,
      display_missing = display_missing,
      nested = nested,
      .keep = .keep,
      .ord = .ord,
      ... = ...
    ) %>%
    # Sort rows based on 'descending_by'
    sort_freq(
      rowvar = rowvar,
      descending_by = descending_by,
      tablebyvar = tablebyvar,
      rowbyvar = rowbyvar,
      colvar_cols = colvar_cols,
      colvar = colvar
    )


  attr(freqs, "colvar") <- colvar
  attr(freqs, "rowbyvar") <- rowbyvar
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
            class = c("tidytlg.freq", class(freqs)))
}

#' Derive frequency
#'
#' @return A long dataframe with the columns: `colvar`, `rowbyvar`, `rowvar`, n,
#'   N, denom, and pct.
#'
#' @noRd
derive_freq <- function(df, denom_df, colvar, rowbyvar, tablebyvar, rowvar,
                        statlist, nested, rowtext, has_subset, denoms_by,
                        display_missing) {

  # Denom dataframe, same as bigN, except the name.
  denom <- denom_df %>%
    derive_count(denoms_by, statlist, "denom")

  # The dataframe with counts will contain the n, N, pct, and denoms.
  res <- df %>%
    derive_count(
      by = c(colvar, rowbyvar, tablebyvar, rowvar),
      statlist = statlist,
      count_name = "n"
    ) %>%
    left_join(denom, by = intersect(names(.), names(denom))) %>%
    mutate(
      pct = n / denom * 100
    )

  # Logic for determining if the row_type is VALUE, or if the rowtext will
  # turn this into a HEADER
  if (nested) row_type_value <- "NESTED"
  else if (!is.null({{rowtext}}) &&
          !is_named({{rowtext}}) &&
          nrow(res) == length(unique(res[[colvar]])) &&
          !("N" %in% statlist[["stats"]])) {
    row_type_value <- "HEADER"
  } else row_type_value <- "VALUE"


  df <- res %>%
    mutate(
      row_type = row_type_value
    )

  if (has_subset && nrow(df) == 0 && !is.null(rowtext)) {
    complete_logic <- list(0, 0, 0, rowtext, "HEADER") %>%
      setNames(c("n", "denom", "pct", rowvar, "row_type"))
    df <- df %>%
      mutate(!!sym(rowvar) := as.character(!!sym(rowvar))) %>%
      complete(!!sym(colvar), fill = complete_logic)  %>%
      # Recalculate the denoms and pcts because they can be lost depending on
      # the structure of denoms_df and df
      select(-"denom") %>%
      left_join(denom, by = denoms_by) %>%
      mutate(
        pct = n / denom * 100
      )
  }

  df %>%
    mutate(
      across(.cols = any_of(c(rowvar, rowbyvar, tablebyvar)),
             .fns = ~ unclass(.),
             .names = "{.col}_ord")
    )
}

#' Freq Workhorse
#'
#' This is where the actual counting is happening. Is generalized to take the
#' variables to count across and the name of the variable to save the count as.
#'
#' @return A dataframe with the counts of 'df' by 'by' with the name of
#'   'count_name'. Is made distinct if the statlist notes subject level counts.
#'
#' @noRd
derive_count <- function(df, by, statlist, count_name) {
  # A list object to fill the missing denom values. Here because unquoting a
  # variable to be used as a name in a list is messy
  complete_fill <- list(0)
  names(complete_fill) <- count_name

  # The logic for completing is different for nested and non-nested layers
  if (attr(df, "nested") %||% FALSE && count_name == "n") {
    # For nested layers, use 'nesting' on the by values, becuase we only want
    # the combinations that appear in the data.
    complete_logic <- exprs(
      nesting(
      !!!syms(by[(by %in% c(attr(df, "nested_level"), attr(df, "rowbyvar")))])),
      !!!syms(by[!(by %in% c(attr(df, "nested_level"), attr(df, "rowbyvar")))]
      )
    )
  } else {
    # Otherwise, get all the combinations even if they aren't present in the
    # data.
    complete_logic <- exprs(!!!syms(unname(by)))
  }

  df %>%
    # Convert variables we're counting across to factors if they aren't already
    mutate(across(.cols = all_of(unname(by)), as.factor)) %>%
    # If the data should be distinct, make it so, otherwise do nothing
    distinct_check(by, statlist) %>%
    # Group variables to count across and count them
    group_by(across(all_of(unname(by)))) %>%
    summarise(!!count_name := n()) %>%
    # Ungroup to make sure you're working with a clean dataframe
    ungroup() %>%
    # If there is any combination of factors that don't appear, make their count
    # 0 Don't complete here if its nested, logic for that is in nested_freq.R
    complete(., !!!complete_logic, fill = complete_fill)
}

#' Return a distinct dataframe based on statlist
#'
#' @return Depending on the statlist, either return the dataframe unmodified, or
#'   make the data.frame distinct based on 'distinct_by'.
#'
#' @noRd
distinct_check <- function(df, by, statlist) {

  distinct_by <- statlist$distinct_by %||% "USUBJID"

  # If there is a group count, make the count distinct. Logic taken from legacy
  # freq code.
  if (statlist[["distinct"]] %||% TRUE)
    distinct(df, !!!syms(c(distinct_by, by)), .keep_all = TRUE)
  else df
}

#' Drop values in the table based on a cutoff
#'
#' @return The table less the dropped values.
#'
#' @noRd
drop_freq <- function(df, cutoff, rowvar, cutoff_stat, colvar, drop_by) {

  # If there isn't a tablebyvar, run it through drop_values
  if (is.null(drop_by)) {
    df %>%
      drop_values(cutoff, rowvar, cutoff_stat, colvar, drop_by)
  } else {
    # If there is a tablebyvar, we need to run 'drop_values' for each value of
    # tablebyvar. So filter them one-by-one and run it through drop_values. Then
    # row_bind them together.

      drop_splits <- split(unique(df[, drop_by]),
                           as.numeric(rownames(unique(df[, drop_by]))))

      map_dfr(drop_splits,
              function(x) {
                filter_logic <- unname(
                  imap(x,
                       ~expr(!!sym(.y) == !!as.character(.x))
                  )
                )
                df %>%
                  filter(!!!filter_logic) %>%
                  drop_values(cutoff, rowvar, cutoff_stat, colvar, drop_by)
              }
      )
  }
}

drop_values <- function(df, cutoff, rowvar, cutoff_stat, colvar, drop_by) {
  # If there is no cutoff, just return the dataframe
  if (is.null(cutoff)) df

  # If there is a cutoff:
  else {
    # If the cutoff specifies a column name:
    if (grepl(">=", cutoff)) {

      cutoff <- paste0("`", cutoff %>%
                         stringr::str_replace_all("& ", "& `") %>%
                         stringr::str_replace_all("\\| ", "| `") %>%
                         stringr::str_replace_all(" >=", "` >="))
      cutoff <- as.list(parse(text = cutoff))

      pivot_id_cols <- unname(c(drop_by, rowvar, "nested_level", "row_type"))
      kept_values <- suppressMessages(
        df %>%
          pivot_wider(id_cols = any_of(pivot_id_cols),
                      names_from = all_of(colvar),
                      values_from = all_of(c(cutoff_stat)),
                      values_fill = 0) %>%
          # Get a list of values from specified column that are >= to the cutoff
          filter(!!!cutoff) %>%
          select(!!rowvar) %>%
          unlist())

      # If the column name doesn't specify a column name:
    } else {

      # Determine the values to keep
      kept_values <- df %>%
        # Get a list of values that have a value >= the cutoff
        filter(!!sym(cutoff_stat) >= as.numeric(trimws(cutoff))) %>%
        select(!!rowvar) %>%
        unlist()

    }
    # Filter everything that wasn't found above.
    df %>%
      filter(.data[[rowvar]] %in% kept_values)
  }
}

#' Sort rows in long data.frame
#'
#' If no descending by is passed it defaults to sorting everything by the factor
#' alphabetically.
#'
#' @noRd
sort_freq <- function(df,
                      rowvar,
                      descending_by,
                      tablebyvar,
                      rowbyvar,
                      colvar_cols,
                      colvar) {


  if (is.null(descending_by)) {
    return(df %>% select(-any_of(paste0("n", colvar_cols))))
  }

  orig_order <- names(df)
  ord_cols <- c(rowvar, tablebyvar, rowbyvar)
  ord_cols <- paste0(ord_cols[!is.null(ord_cols)], "_ord")

  row_header_location <- which(df$row_type == "HEADER")

  if (length(row_header_location) != 0) {
    row_header_df <- df %>% slice(row_header_location)
    df       <- df %>% slice(-row_header_location)
  }

  df_ord   <- df %>% select(dplyr::contains(ord_cols))
  df       <- df %>% select(-dplyr::contains(ord_cols))

  if ("nested_level" %in% orig_order) {
    for (i in 1:(length(rowvar) - 1)) {
      df <- df %>%
        mutate(
          across(.cols = "nested_level",
                 function(x) x != (i - 1), .names = paste0("ntop", rowvar[i]))
        )
    }
  }

  df  <- df %>% dplyr::arrange(label)

  if ("nested_level" %in% orig_order) {
    df <- df %>% dplyr::arrange(nested_level)
  }

  if (is_named(descending_by)) {
    df <- df %>% group_by(!!!syms(c(tablebyvar, rowbyvar)))
    for (i in rev(seq_len(length(descending_by)))) {
      sort_col <- paste0("n", names(descending_by)[[i]])
      type     <- descending_by[[i]]
      if (type == "asc") {
        df <- df %>%
          mutate(
            dplyr::across(
              .cols = sort_col,
              function(x) {
                ifelse(row_type %>% stringr::str_detect("(HEADER|^N$)"), -x, x)
              }
            )
          ) %>%
          arrange(!!sym(sort_col))
      }else{
        df <- df %>% arrange(desc(!!sym(sort_col)))
      }
    }
  }
  else{
    df <- df %>% group_by(!!!syms(c(tablebyvar, rowbyvar)))
    for (i in rev(seq_len(length(descending_by)))) {
      df <- df %>% arrange(desc(!!sym(paste0("n", descending_by[i]))))
    }
  }

  if (length(rowvar) > 1) {
    sortvar <- c()
    for (i in 1:(length(rowvar) - 1)) {
      df <- df %>% mutate(
        sorter = ifelse(!(!!sym(paste0("ntop", rowvar[i]))), row_number(), NA)
      ) %>%
        group_by(
          !!!syms(c(tablebyvar, rowbyvar, rowvar[1:i]))
        ) %>%
        fill(
          sorter, .direction = "downup"
        ) %>%
        ungroup() %>%
        mutate(
          across(.cols = "sorter", function(x) x, .names = paste0("sort", rowvar[i]))
        ) %>%
        select(-sorter)
      sortvar <- c(
        sortvar,
        paste0("sort", rowvar[i]),
        paste0("ntop", rowvar[i])
      )
    }
    df <- df %>%
      arrange(!!!syms(sortvar)) %>%
      select(-any_of(sortvar))
  }

  df <- df %>%
    ungroup() %>%
    arrange(!!!syms(c(tablebyvar, rowbyvar))) %>%
    bind_cols(df_ord) %>%
    select(!!!syms(orig_order))

  if (length(row_header_location) != 0) {
    for (i in seq_len(nrow(row_header_df))) {
      df <- df %>%
        add_row(row_header_df %>% slice(i), .before = row_header_location[i])
    }
  }
  return(df %>% select(-any_of(paste0("n", colvar_cols))))
}

#' Round values in a freq table
#'
#' @return Table with rounded 'pct'
#'
#' @noRd
round_freq <- function(df, decimal) {

  # format pct using roundSAS
  df %>%
    mutate(pct = roundSAS(pct,  digits = as.numeric(decimal), as_char = TRUE))

}

#' Arrange values from n and pct into results
#'
#' @return A table with combined results
#'
#' @noRd
arrange_freq <- function(df, statlist) {
  format_as <- list(
    n               = expr(as.character(.data$n)),
    `n (x.x)`       = expr(paste0(.data$n, " (", pct, ")")),
    `n (x.x%)`      = expr(paste0(.data$n, " (", pct, "%)")),
    `n/N`           = expr(paste0(.data$n, "/", .data$denom)),
    `n/N (x.x)`     = expr(paste0(.data$n, "/", .data$denom, " (", pct, ")")),
    `n/N (x.x%)`    = expr(paste0(.data$n, "/", .data$denom, " (", pct, "%)"))
  )
  lsformats <- statlist[["stats"]][statlist[["stats"]] != "N"]

  # Format N counts as character
  df <- df %>% mutate(N = as.character(denom))

  purrr::map_dfr(lsformats, function(format_) {
    df %>%
      mutate(
        # Evaluate the expression from format_as
        stat = format_,
        results =   eval(format_as[[format_]]),
        # Replace results with '-' if n/N is 0 or NA
        results = case_when(
          .data$denom %in% c(0, NA) ~ statlist[["zero_denom"]] %||% "-",
          .data$n %in% c(0, NA) & grepl("/N", format_) ~
            str_replace(results, "\\s.*$", ""),
          .data$n %in% c(0, NA) & !grepl(format_, "/N") ~
            statlist[["zero_n"]] %||% "0",
          TRUE ~ results
        )
      )})
}

#' Pivot freq table
#'
#' @return The (nearly) final wide table for display
#'
#' @noRd

pivot_freq <- function(df, colvar, rowvar, rowbyvar, tablebyvar, statlist,
                       rowtext, row_header, display_missing = FALSE,
                       nested = FALSE, .keep = FALSE, .ord, ...) {

  ord_columns <- names(df)[endsWith(names(df), "_ord")]
  # The columns to output for the table. If .keep is true, add the rowbyvar
  output_columns <- c(
    "label",
    levels(df[[colvar]]),
    paste0("n", levels(df[[colvar]])),
    "row_type",
    "nested_level",
    "group_level"
  )
  if (.keep)
    output_columns <- c(output_columns, rowbyvar, tablebyvar)
  if (.ord)
    output_columns <- c(output_columns, ord_columns)

  missing_name <- "Missing"
  # Create the result dataframe and add "VALUE" as the row_type
  pivot_id_cols <- unname(
    c(
      rowbyvar, rowvar, tablebyvar, "nested_level", "row_type", ord_columns,
      "stat"
    )
  )
  res <- suppressMessages(
    df %>%
      pivot_wider(id_cols = any_of(pivot_id_cols),
                  names_from = all_of(colvar),
                  values_from = c("results"),
                  values_fill = "0") %>%
      left_join(
        suppressWarnings(
          df %>% mutate(n = ifelse(row_type == "N", as.numeric(results), n))
        ) %>%
          pivot_wider(id_cols = any_of(pivot_id_cols),
                      names_from = all_of(colvar),
                      values_from = c("n"),
                      values_fill = 0,
                      names_prefix = "n")
      ) %>%
      select(-stat) %>%
      mutate(
        # For all the na values of rowvar, rename them to missing.
        !!rowvar[1] := ifelse(is.na(!!sym(rowvar)),
                              missing_name,
                              as.character(!!sym(rowvar)))
      ) %>%
      {  # drop out the missings unless display_missing is true
        if (display_missing) . else filter(., !!sym(rowvar[1]) != missing_name)
      }
  )

  if (!is.null(rowbyvar) && length(rowbyvar) == 1)
    res <- res %>% tidyr::fill(!!sym(rowbyvar), .direction = "up")
  if (nrow(res) != 0) {
    res <- res %>%
      mutate(
        across(.cols = paste0("n", levels(df[[colvar]])),
               function(x) {
                 ifelse(row_type %>%
                          stringr::str_detect("HEADER"),
                        max(x, na.rm = TRUE) + n() + 1 - row_number(), x)
               }
        )
      )
  }

  res <-  res %>%
    rowtext_freq(rowtext,
                 rowvar,
                 rowbyvar,
                 tablebyvar,
                 row_header)
  # If your table has zero rows, just return the zero rows.
  if (nrow(res) == 0)
    return(res)

  # Bind the rowtextHeader table to add the rowtext header if it exists
  res %>%
    mutate(
      label = as.character(label),

      across(.cols = -any_of(c("nested_level", ord_columns)),
             .fns = ~ replace_na_with_blank(.x))
    ) %>%
    add_meta_freq(df, colvar, tablebyvar) %>%
    # add on the group_level variable for indentation
    add_group_level(rowbyvar) %>%
    select(
      any_of(output_columns)
    ) %>%
    mutate(
      !!!list(...)
    )
}

#' Label rename for rowtext
#'
#' @return Either a named character vector with the name being the new label.
#'   or null if its not needed
#'
#' @noRd
rowtext_freq <- function(res, rowtext, rowvar, rowbyvar, tablebyvar,
                         row_header) {
  # If you have a rowtext, its unnamed, and there is only one row. Change
  # rowtext so it relevels the label
  res <- res %>% mutate(label = !!sym(rowvar))
  if (is.null(rowtext)) {
    return(res)
  } else if (is_named(rowtext)) {
    return(
      suppressWarnings(
        res %>%
          mutate(label = as.character(fct_recode(!!sym(rowvar),!!!rowtext))))
    )
  }

  res_group <- c(rowbyvar, tablebyvar)
  len_oner <- as.numeric(!is.null(row_header)) +
    as.numeric(!is.null(rowbyvar)) + 1 #length of single row table
  res <- res %>%
    group_by_at(res_group)

  #if row text is named list it will refactor, if length one will replace the
  # single row, otherwise does nothing
  suppressWarnings(
    res <- res %>%
      mutate(label = case_when(
        length(rowtext) == 1 & len_oner == n() & row_number() == n() ~
          rowtext[1],
        TRUE  ~ label))
  )
  res <- res %>% ungroup()

}

#' Logic for merging in bigN and rowbyvar
#'
#' @param hasBigN boolean. Does the table have a big N in the statlist?
#'
#' @noRd
byvar_merge_freq <- function(df, tablebyvar, rowbyvar, colvar, rowvar, hasBigN,
                             row_header) {

  df %>%
    bigN_merge(colvar, rowvar, rowbyvar, tablebyvar, hasBigN) %>%
    row_header_merge(colvar, rowvar, rowbyvar, tablebyvar, row_header) %>%
    rowbyvar_merge(colvar, rowvar, rowbyvar, tablebyvar)

}

#' Add metadata to a freq table
#'
#' This adds the 'denom' to columns for the bind_table function
#'
#' @noRd
add_meta_freq <- function(table, df, colvar, tablebyvar) {

  denom_ <- df %>%
    select(!!colvar, !!tablebyvar, denom) %>%
    filter(!is.na(denom)) %>%
    unique() %>%
    rename_with(
      ~ paste0("denom_", .),
      .cols = -all_of("denom")
    )

  attr(table, "denom") <- denom_

  table
}

#' Merge in bigN rows
#' @noRd
bigN_merge <- function(df, colvar, rowvar, rowbyvar, tablebyvar, hasBigN) {

  if (hasBigN) {

    df %>%
      var_merge(nestby = c(colvar, rowbyvar, tablebyvar),
                rowvar = rowvar,
                rowvar_name = "N",
                result_value = as.character(data_nest[["N"]][1]),
                row_type_value = "N")

  } else{
    df
  }
}

row_header_merge <- function(df, colvar, rowvar, rowbyvar, tablebyvar,
                             row_header) {

  if (is.null(row_header)) df
  else {
    df %>%
      var_merge(nestby = c(colvar, rowbyvar, tablebyvar),
                rowvar = rowvar,
                rowvar_name = row_header,
                result_value = "",
                row_type_value = "HEADER")
  }
}

#' Merge in rowbyvar rows
#'
#' Split up for logic when there are 0, 1, and 2 rowbyvars
#'
#' @noRd
rowbyvar_merge <- function(df, colvar, rowvar, rowbyvar, tablebyvar) {

  if (is.null(rowbyvar)) df
  else if (length(rowbyvar) == 1)
    df %>%
    var_merge(nestby = c(colvar, rowbyvar, tablebyvar),
              rowvar = rowvar,
              rowvar_name = sym(rowbyvar),
              result_value = "",
              row_type_value = "BY_HEADER1")
  else
    df %>%
    var_merge(nestby = c(colvar, rowbyvar[1], tablebyvar),
              rowvar = rowvar,
              rowvar_name = sym(rowbyvar[1]),
              result_value = "",
              row_type_value = "BY_HEADER1") %>%
    var_merge(nestby = c(colvar, rowbyvar, tablebyvar),
              rowvar = rowvar,
              rowvar_name = sym(rowbyvar[2]),
              result_value = "",
              row_type_value = "BY_HEADER2")
}

#' Workhorse for merging in a freq table
#' @noRd
var_merge <- function(df, nestby, rowvar, rowvar_name, result_value,
                      row_type_value) {

  result_value <- enexpr(result_value)
  ord_values_all <- names(df)[endsWith(names(df), "_ord")]
  ord_in_nest <- map_chr(str_split(ord_values_all, "_"), `[[`, 1) %in% nestby
  ord_values <- ord_values_all[ord_in_nest]
  ord_values_fill <- ord_values_all[!ord_in_nest]
  ord_values_fill <- setNames(as.list(rep(0, length(ord_values_fill))),
                              ord_values_fill)

  a_nest <- c(nestby, ord_values)


  df <- df %>%
    nest(data_nest = -any_of(a_nest)) %>%
    rowwise() %>%
    mutate(
      data_nest = list(
        tibble(!!rowvar := !!rowvar_name,
               results = !!result_value,
               row_type = row_type_value,
               !!!ord_values_fill) %>%
          bind_rows(data_nest)
      )
    ) %>%
    unnest(data_nest) %>%
    ungroup()
  df
}
