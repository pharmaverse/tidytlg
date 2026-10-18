datetime_fixture <- function() {
  data.frame(label = c("First row", "Last row"), col1 = c("10", "20"))
}

datetime_table <- function(format, tlf = "Table", footers = NULL,
                           footers_one_row = FALSE, add_datetime = FALSE) {
  withr::local_options(list(tidytlg.add_datetime = add_datetime))
  gentlg(
    huxme = datetime_fixture(),
    format = format,
    tlf = tlf,
    file = "datetime_fixture",
    title = "Datetime regression",
    footers = footers,
    footers_one_row = footers_one_row,
    print.hux = FALSE
  )[[1]]
}

test_that("disabling datetime ends tables and listings at their last data row", {
  for (format in c("rtf", "html")) {
    for (tlf in c("Table", "Listing")) {
      ht <- datetime_table(format, tlf)
      contents <- as.data.frame(ht)

      # A title, a column header and the two input rows; no trailing blank row.
      expect_equal(nrow(ht), 4L, info = paste(format, tlf))
      expect_match(contents[4, 1], "Last row", fixed = TRUE)
      expect_match(contents[4, 2], "20", fixed = TRUE)
      expect_equal(huxtable::align(ht)[4, 1], "left")

      if (format == "rtf") {
        expect_true(all(huxtable::bottom_border(ht)$thickness[4, ] > 0))
        expect_true(all(huxtable::top_border(ht)$thickness[4, ] == 0))
      } else {
        expect_match(contents[4, 1], "border-bottom:1pt solid", fixed = TRUE)
      }
    }
  }
})

test_that("single-row RTF tables close below the data without a datetime row", {
  withr::local_options(list(tidytlg.add_datetime = FALSE))
  for (footers in list(NULL, NA, character(0))) {
    ht <- gentlg(
      huxme = datetime_fixture()[1, , drop = FALSE],
      file = "datetime_fixture",
      title = "Single-row regression",
      footers = footers,
      print.hux = FALSE
    )[[1]]

    expect_equal(nrow(ht), 3L)
    expect_equal(as.data.frame(ht)[3, 1], "First row")
    # The data row shares the column header's lower border.
    expect_equal(huxtable::top_border(ht)$thickness[3, ], huxtable::bottom_border(ht)$thickness[2, ])
    expect_gt(huxtable::top_border(ht)$thickness[3, 2], 0)
    expect_true(all(huxtable::bottom_border(ht)$thickness[3, ] > 0))
  }
})

test_that("disabling datetime retains footnotes and their data boundary", {
  footers <- c("First footnote", "Last footnote")
  for (format in c("rtf", "html")) {
    for (tlf in c("Table", "Listing")) {
      for (one_row in c(FALSE, TRUE)) {
        ht <- datetime_table(format, tlf, footers, one_row)
        contents <- as.data.frame(ht)

        expect_equal(nrow(ht), if (one_row) 5L else 6L)
        expect_match(contents[4, 1], "Last row", fixed = TRUE)
        expect_match(contents[5, 1], "First footnote", fixed = TRUE)
        expect_match(contents[nrow(ht), 1], "Last footnote", fixed = TRUE)
        expect_equal(huxtable::align(ht)[nrow(ht), 1], "left")

        if (format == "rtf") {
          expect_true(all(huxtable::bottom_border(ht)$thickness[4, ] > 0))
          expect_true(all(huxtable::top_border(ht)$thickness[4, ] == 0))
          expect_true(all(huxtable::top_border(ht)$thickness[5, ] > 0))
          if (!one_row) {
            expect_true(all(huxtable::top_border(ht)$thickness[6, ] == 0))
          }
        } else {
          expect_match(contents[5, 1], "border-top:1pt solid", fixed = TRUE)
          expect_match(contents[nrow(ht), 1], "border-bottom:1pt solid", fixed = TRUE)
        }
      }
    }
  }
})

test_that("enabling datetime retains one final metadata row", {
  for (format in c("rtf", "html")) {
    for (tlf in c("Table", "Listing")) {
      for (footers in list(NULL, c("First footnote", "Last footnote"))) {
        without_datetime <- datetime_table(format, tlf, footers)
        ht <- datetime_table(format, tlf, footers, add_datetime = TRUE)
        contents <- as.data.frame(ht)

        expect_equal(nrow(ht), nrow(without_datetime) + 1L)
        expect_match(contents[4, 1], "Last row", fixed = TRUE)
        expect_match(contents[nrow(ht), 1], paste0("[datetimefixture.", format, "]"), fixed = TRUE)
        expect_match(contents[nrow(ht), 1], "[0-9]{2}:[0-9]{2}")
        expect_true(all(huxtable::align(ht)[nrow(ht), ] == "right"))

        if (format == "rtf") {
          expect_true(all(huxtable::bottom_border(ht)$thickness[4, ] > 0))
          expect_true(all(huxtable::top_border(ht)$thickness[4, ] == 0))
          expect_true(all(huxtable::top_border(ht)$thickness[nrow(ht), ] > 0))
        } else {
          expect_match(contents[nrow(ht), 1], "border-bottom:1pt solid", fixed = TRUE)
        }
      }
    }
  }
})

test_that("disabling datetime removes the extra row from every table in a result", {
  withr::local_options(list(tidytlg.add_datetime = FALSE))
  for (format in c("rtf", "html")) {
    hts <- gentlg(
      huxme = list(datetime_fixture(), datetime_fixture()),
      format = format,
      file = "datetime_fixture",
      title = list("First table", "Second table"),
      footers = list(NULL, c("First footnote", "Last footnote")),
      print.hux = FALSE
    )

    expect_length(hts, 2L)
    expect_equal(vapply(hts, nrow, integer(1)), c(4L, 6L))
    expect_match(as.data.frame(hts[[1]])[4, 1], "Last row", fixed = TRUE)
    expect_match(as.data.frame(hts[[2]])[6, 1], "Last footnote", fixed = TRUE)
  }
})

test_that("RTF export includes a datetime row only when requested", {
  output_directory <- withr::local_tempdir()
  for (add_datetime in c(FALSE, TRUE)) {
    withr::local_options(list(tidytlg.add_datetime = add_datetime))
    gentlg(
      huxme = list(datetime_fixture(), datetime_fixture()),
      file = "datetime_fixture",
      title = list("First table", "Second table"),
      footers = list(NULL, c("First footnote", "Last footnote")),
      opath = output_directory
    )

    rtf_file <- file.path(output_directory, "datetimefixture.rtf")
    rtf <- readChar(rtf_file, file.info(rtf_file)$size, useBytes = TRUE)
    # Each table has four title/header/data rows; the second has two footnotes.
    expect_equal(stringr::str_count(rtf, stringr::fixed("\\trowd")), if (add_datetime) 12L else 10L)
    expect_equal(grepl("[datetimefixture.rtf]", rtf, fixed = TRUE), add_datetime)
  }
})
