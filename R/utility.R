
#' Replace NA with ""
#'
#' Used to swap in "" for by variables so the headers sort correctly to the top
#'
#' @param x variable to check for NA and replace with "".
#'
#' @return x with NA's replaced with "".  Factors will add ""
#' as the first level.
#' @export
#'
#' @examples
#'replace_na_with_blank(c("a", "b", NA))
#'
#'replace_na_with_blank(factor(c("a", "b", NA), levels = c("a", "b")))
replace_na_with_blank <- function(x) {

  if (sum(is.na(x)) > 0) {
    if (is.factor(x)) {
      x <- forcats::fct_relevel(forcats::fct_expand(x, ""), "", after = 0)
    }

    tidyr::replace_na(x, "")
  } else {
    x
  }

}

#' @title roxygen2_data
#' @description Creates the Roxygen2 documentation for a given data frame.
#'
#' @author Steven Haesendonckx {shaesen2@@its.jnj.com}
#'
#' @param df data frame.
#' @param study study name.
#' @param pkg package path.
#'
#' @noRd
#'
#' @return Roxygen2 documentation for the data frame.
roxygen2_data <- function(df, study = NULL, pkg = "~/tidytlg") {
  name  <-  deparse(substitute(df))

  title <-  paste0("#' @title ", name)

  descr <-  paste0("#' @description ", name, " copied from ", study)
  src   <-
    "#' @source Synthetic data on S drive, general compound."

  fmt   <-
    paste0("#' @format A data frame with ",
           nrow(df),
           " rows and ",
           ncol(df),
           " variables:")
  itemize <- data.frame(
    colname = colnames(df),
    label    = purrr::map_chr(seq_along(colnames(df)), function(x) {
      attr(df[[x]], "label")
      }),
    stringsAsFactors = FALSE
  )
  item <-
    paste0("#'  \\item{", itemize$colname, "}{", itemize$label, "}")
  item <- c("#' \\describe{", item, "#' }")
  seealso <-
    paste0("#' @seealso ",
           paste0("\\code{\\link{", noquote(gsub(
             ".rda", "", list.files(path = paste0(pkg, "/data"))
           )), "}}", collapse = " "))
  key     <-
    paste0("#' @keywords datasets synthetic ",
           tolower(stringr::word(gsub("_", " ", name), 1)))
  atname  <- paste0("#' @name ", tolower(name))
  examp   <-
    c(
      "#' @examples",
      paste0("#'  data(\"", deparse(substitute(df)), "\")"),
      paste0("\"", deparse(substitute(df)), "\"")
    )

  description <-
    c(title,
      "#'",
      descr,
      src,
      "#'",
      fmt,
      item,
      seealso,
      key,
      atname,
      examp,
      "")

  return(writeLines(description))
}

#' Convert character variable to a factor based off it's numeric variable
#' counterpart.
#'
#' @param df data frame.
#' @param c_var character variable within the data frame.
#' @param n_var numeric variable counter part within the data frame to control
#'   the levels.
#'
#' @return A factor.
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#'   ~TRT01P, ~TRT01PN,
#'   "Placebo",   1,
#'   "Low Dose",  2,
#'   "High Dose", 3
#'   )
#'
#' # alphabetical order
#' dplyr::arrange(df, TRT01P)
#'
#' # change to factor with char2factor
#' df$TRT01P <- char2factor(df, "TRT01P", "TRT01PN")
#'
#' # factor order
#' dplyr::arrange(df, TRT01P)
char2factor <- function(df, c_var, n_var) {
  lbl <- attr(df[[c_var]], "label")
  new_variable <- forcats::fct_reorder(as.factor(df[[c_var]]), df[[n_var]])
  attr(new_variable, "label") <- lbl
  new_variable
}


#' add group_level variable for indentation
#' @noRd
add_group_level <- function(df, rowbyvar) {
  # assign default value
  df$group_level <- 0
  if (length(rowbyvar) > 1) {
    # if more than one rowbyvar exists iterate over adding group_level
    for (var in rowbyvar) {
      current_level <- match(var, rowbyvar) - 1
      df$group_level <- ifelse(as.character(df[[var]]) != "",
                               current_level, df$group_level)
    }
  }
  df
}

#' Workhorse for finding a file in a working directory
#'
#' @param base_path Base path to start looking for a file
#' @param name File name sans extention
#' @param ext Vector of possible extensions
#' @noRd
find_file <- function(base_path, name, ext) {
  if (!dir.exists(base_path))
    stop(paste("Directory:", base_path, "does not exist"))
}

#' Helper functions for returning files used in gentlg
#'
#' @param path Working directory of the project
#'
#' @return A character vector to the requested file.
#'
#' @export
#'
#' @rdname files
tidytlg_titles <- function(path) {
  list.files(path, pattern = "(^titles\\.xl)", full.names = TRUE,
             ignore.case = TRUE)
}

#' @rdname files
#' @export
tidytlg_metadata <- function(path) {
  list.files(path, pattern = "(^column_metadata\\.xl)", full.names = TRUE,
             ignore.case = TRUE)
}

first_class <- function(.) {
  class(.)[1]
}
