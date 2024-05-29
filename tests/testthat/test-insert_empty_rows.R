newrows <- "newrows"
cast_to_char <- function(df) {
  lapply(df, as.character)
}

testthat::test_that("Does not insert rows if the newrows column does not exist", {
  i <- iris[1:10, ]
  i[] <- cast_to_char(i)

  actual <- insert_empty_rows(i)
  testthat::expect_identical(insert_empty_rows(i), i)
})

testthat::test_that("Does not insert rows if the newrows column is all 0s", {
  i <- subset(iris[1:10, ], select = -c(Species))
  i[] <- cast_to_char(i)
  i[[newrows]] <- rep(0, nrow(i))

  testthat::expect_identical(insert_empty_rows(i), i)
})

testthat::test_that("Inserts rows when there are 1s in the newrows column", {
  i <- subset(iris[1:10, ], select = -c(Species))
  i[] <- cast_to_char(i)
  i[[newrows]] <- c(0, 1, 0, 1, 1, 0, 0, 0, 0, 0)

  actual <- insert_empty_rows(i)
  expected_rownames <- as.integer(seq_len(nrow(actual)))
  testthat::expect_identical(attr(actual, "row.names", expected_rownames), expected_rownames)
})

testthat::test_that("Inserts an empty row when the 1 is in the last row of newrows", {
  i <- subset(iris[1:10, ], select = -c(Species))
  i[] <- cast_to_char(i)
  i[[newrows]] <- c(rep(0, 9), 1)

  actual <- insert_empty_rows(i)
  expected_rownames <- as.integer(seq_len(nrow(actual)))
  testthat::expect_identical(attr(actual, "row.names", expected_rownames), expected_rownames)
})

testthat::test_that("Inserts empty rows when there are 2 1s in the last two rows of newros", {
  i <- subset(iris[1:10, ], select = -c(Species))
  i[] <- cast_to_char(i)
  i[[newrows]] <- c(rep(0, 8), 1, 1)

  actual <- insert_empty_rows(i)
  expected_rownames <- as.integer(seq_len(nrow(actual)))
  testthat::expect_identical(attr(actual, "row.names", expected_rownames), expected_rownames)
})

testthat::test_that("Zeroes the newrows column", {
  i <- subset(iris[1:10, ], select = -c(Species))
  i[] <- cast_to_char(i)
  i[[newrows]] <- c(rep(0, 8), 1, 1)

  actual <- insert_empty_rows(i)
  testthat::expect_identical(actual$newrows, rep(0, nrow(actual)))
})

testthat::test_that("Preserves the formatting columns", {
  limit <- 9
  i <- subset(iris[1:limit, ], select = -c(Species))
  i[] <- cast_to_char(i)
  i[[newrows]] <- c(rep(0, limit - 1), 1)
  row_type <- sample(c("S", "T"), size = nrow(i), replace = TRUE)
  anbr <- sample(c("1"), size = nrow(i), replace = TRUE)
  indentme <- sample(c("Y", "N"), size = nrow(i), replace = TRUE)
  roworder <- seq_len(nrow(i))
  i$row_type <- row_type
  i$anbr <- anbr
  i$indentme <- indentme
  i$roworder <- roworder

  actual <- insert_empty_rows(i)
  testthat::expect_identical(actual$row_type, c(row_type[-length(row_type)], "EMPTY", row_type[length(row_type)]))
  testthat::expect_identical(actual$anbr, c(anbr, "1"))
  testthat::expect_identical(actual$indentme, c(indentme[-length(row_type)], "0", indentme[length(row_type)]))
  testthat::expect_identical(actual$roworder, seq_len(nrow(actual)))
})

testthat::test_that("Inserts empty rows when the input table has newrows", {
  output_directory <- "test_outputs/emptyrowsinsertions"
  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      library(dplyr)
      library(tidytlg)

      # Note cdisc_adsl is built into the package for use
      ittpop <- cdisc_adsl %>%
        filter(ITTFL == "Y")

      # frequency of Intend-to-Treat patients by planned treatment
      tbl1 <- freq(ittpop,
        rowvar = "ITTFL",
        statlist = statlist("n"),
        colvar = "TRT01P",
        rowtext = "Analysis Set:  Intend-to-Treat Population",
        subset = ITTFL == "Y"
      )

      # N, MEAN (SD), MEDIAN, RANGE, IQ Range of age by planned treatment
      tbl2 <- univar(ittpop,
        rowvar = "AGE",
        colvar = "TRT01P",
        row_header = "Age (Years)"
      )

      # frequency of Race by planned treatment
      tbl3 <- freq(ittpop,
        rowvar = "RACE",
        statlist = statlist(c("N", "n (x.x%)")),
        colvar = "TRT01P",
        row_header = "Race, n(%)"
      )

      # combine results together
      tbl <- bind_table(tbl1, tbl2, tbl3)

      # conver to hux object
      withr::with_dir(
        new = testthat::test_path(output_directory),
        code = {
          gentlg(
            huxme = tbl,
            orientation = "landscape",
            file = "inserts",
            title = "Custom Method",
            footers = "Produced with tidytlg",
            colspan = list(c("", "", "Xanomeline", "Xanomeline")),
            colheader = c("", "Placebo", "High", "Low"),
            wcol = .30
          )
        }
      )
    }
  )
  expect_snapshot_file(test_path(sprintf("%s/inserts.rtf", output_directory)))
})
