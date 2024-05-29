#' @title Inserts empty rows into a data frame
#'
#' @details
#' `gentlg` allows for formatting the input table based on formatting columns
#' (see [gentlg()]). One of the formatting mechanisms is empty row insertion. This
#' function inserts the empty rows based on the `newrows` column in the data
#' frame. The new rows are inserted before the rows with value `1` in the
#' `newrows` column.
#'
#' @param huxme `data.frame` The input data frame.
#'
#' @return A data frame with added new empty rows.
#' @keywords internal
insert_empty_rows <- function(huxme, newrows = huxme$newrows) {
  if (is.null(newrows)) {
    return(huxme)
  }
  assertthat::assert_that(nrow(huxme) == length(newrows))

  format_columns <- c("row_type", "anbr", "indentme", "roworder", "newrows")
  data_columns <- names(huxme)[!names(huxme) %in% format_columns]

  emptyrow <- huxme[NA, ][1, ]
  emptyrow[, data_columns] <- ""

  has_roworder <- "roworder" %in% names(huxme)
  if (has_roworder) {
    emptyrow[, "roworder"] <- 0
  }
  if ("newrows" %in% names(huxme)) {
    emptyrow$newrows <- 0
  }
  if ("row_type" %in% names(huxme)) {
    emptyrow$row_type <- "EMPTY"
  }
  if ("indentme" %in% names(huxme)) {
    emptyrow[, "indentme"] <- 0
  }
  if ("newpage" %in% names(huxme)) {
    emptyrow[, "newpage"] <- 0
  }

  cum_sum <- cumsum(newrows)
  df_chunks <- split(huxme, cum_sum)
  if ("anbr" %in% names(huxme)) {
    df_chunks <- lapply(df_chunks, function(chunk) {
      emptyrow$anbr <- chunk$anbr[1]
      rbind(chunk, emptyrow)
    })
  } else {
    df_chunks <- lapply(df_chunks, function(chunk) {
      rbind(chunk, emptyrow)
    })
  }

  names(df_chunks) <- NULL
  merged_df <- do.call(rbind, df_chunks)
  merged_df <- merged_df[-nrow(merged_df), ]
  if (has_roworder) {
    merged_df$roworder <- seq_len(nrow(merged_df))
  }

  attr(merged_df, "row.names") <- as.integer(seq_len(nrow(merged_df)))
  if ("newrows" %in% names(huxme)) {
    merged_df$newrows <- 0
  }
  merged_df
}
