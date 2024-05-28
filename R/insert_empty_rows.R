#' @title Inserts empty rows into a data frame
#'
#' @details
#' `gentlg` allows for formatting the input table based on formatting columns
#' (see [gentlg()]). One of the formatting mechanisms is empty row insertion. This
#' function inserts the empty rows based on the `newrows` column in the data
#' frame. The new rows are inserted after the rows with value `1` in the
#' `newrows` column.
#'
#' @param huxme `data.frame` The input data frame.
#'
#' @return A data frame with added new empty rows.
insert_empty_rows <- function(huxme, newrows = huxme$newrows) {
  browser()
  format_columns <- c("row_type", "anbr", "indentme", "roworder", "newrows")
  data_columns <- names(huxme)[!names(huxme) %in% format_columns]

  emptyrow <- huxme[NA, ][1, ]
  emptyrow[, data_columns] <- ""
  emptyrow$newrows <- 0

  has_roworder <- "roworder" %in% names(huxme)
  if (has_roworder) {
    emptyrow[, "roworder"] <- 0
  }
  if ("row_type" %in% names(huxme)) {
    emptyrow$row_type <- "EMPTY"
  }
  if ("indentme" %in% names(huxme)) {
    emptyrow[, "indentme"] <- 0
  }

  cum_sum <- cumsum(huxme$newrows)
  shifted_cum_sum <- c(0, cum_sum)[-(length(cum_sum) + 1)]
  df_chunks <- split(huxme, shifted_cum_sum)
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
  if (merged_df$newrows[nrow(merged_df) - 1] == 0) {
    merged_df <- merged_df[-nrow(merged_df), ]
  }
  if (has_roworder) {
    merged_df$roworder <- seq_len(nrow(merged_df))
  }

  browser()
  attr(merged_df, "row.names") <- as.integer(seq_len(nrow(merged_df)))
  merged_df$newrows <- 0
  merged_df
}
