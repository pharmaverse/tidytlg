#' Print tidytlg table to the R Console
#'
#' When inputting a variable to the console, print is implicitly called on the object.
#' This S3 method prints out a nice format for the user.
#' @export
#' @noRd
print.tidytlg.freq <- function(x, ...) {
  cat(paste0(c("Column Variables: ", attr(x, "colvar"), "\n")))

  if (!is.null(attr(x, "rowbyvar")))
    cat(paste0(c("Row By Variables Variables: ", attr(x, "rowbyvar"), "\n")))

  if (!is.null(attr(x, "tablebyvar")))
    cat(paste0(c("Table By Variables: ", attr(x, "tablebyvar"), "\n")))

  cat(paste0(c("Row Variable: ", attr(x, "rowvar"), "\n")))

  cat(paste0(c("Statistic Formatting: ", attr(x, "statlist"), "\n")))

  if (!is.null(attr(x, "cutoff")))
    cat(paste0(c("Value Cutoffs: ", attr(x, "cutoff"), " by variable ",
                 attr(x, "cutoff_stat"), "\n")))

  cat(paste0(c("Descending By: ",
               attr(x, "descending_by") %||% "Factored Sorting", "\n")))

  if (!is.null(attr(x, "rowtext")))
    cat(paste0(c("Row Label Values: ", attr(x, "rowtext"), "\n")))

  if (!is.null(attr(x, "row_header")))
    cat(paste0(c("Row Header: ", attr(x, "row_header"), "\n")))

  if (is.call(attr(x, "subset")))
    cat(paste0(c("Subset Logic: ", as_name(attr(x, "subset")[[1]]))))

  if (!is.null(attr(x, "denoms_by")))
    cat(paste0(c("Denominators By Variables: ", attr(x, "denoms_by"), "\n")))

  NextMethod("print")
}

#' @export
print.tidytlg.nested_freq <- function(x, ...) {
  NextMethod("print")
}

#' @export
print.tidytlg.univar <- function(x, ...) {
  cat(paste0(c("Column Variables: ", attr(x, "colvar"), "\n")))

  if (!is.null(attr(x, "rowbyvar")))
    cat(paste0(c("Row By Variables Variables: ", attr(x, "rowbyvar"), "\n")))

  if (!is.null(attr(x, "tablebyvar")))
    cat(paste0(c("Table By Variables: ", attr(x, "tablebyvar"), "\n")))

  cat(paste0(c("Row Variable: ", attr(x, "rowvar"), "\n")))

  cat(paste0(c("Statistic Formatting: ", attr(x, "statlist"), "\n")))

  cat(underline("Statistic Presentation"), "\n")
  cat(paste0(c("Decimal Precision: ", attr(x, "decimal"), "\n")))
  cat(paste0(c("Alpha for CI Intervals: ", attr(x, "alpha"), "\n")))
  cat(paste0(c("Parameter Based PrecisionL ",
               attr(x, "precisionby") %||% "None", "\n")))

  if (!is.null(attr(x, "rowtext")))
    cat(paste0(c("Row Label Values: ", attr(x, "rowtext"), "\n")))

  if (!is.null(attr(x, "row_header")))
    cat(paste0(c("Row Header: ", attr(x, "row_header"), "\n")))

  NextMethod("print")
}

#' @export
print.statlist <- function(x, ...) {
  cat(bold("tidytlg Statlist"), "\n")
  cat(underline("stats"), "\n")
  cat(paste0(" ", x$stats, sep = "\n", collapse = ""))
  if (length(x) > 1) {
    cat(underline("Configurations"), "\n")
    for (i in seq_len(length(x[-1]))) {
      cat(bold(names(x[-1])[i]), "-", paste0(x[-1][[i]], collapse = ","), "\n")
    }
  }
  invisible(x)
}
