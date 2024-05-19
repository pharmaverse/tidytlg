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

  testthat::expect_warning(
    actual <- insert_empty_rows(i)
  )

  expected_rownames <- as.integer(c(1, 2, NA, 3, 4, NA, 5, NA, 6:10))
  testthat::expect_identical(attr(actual, "row.names", expected_rownames), expected_rownames)
})

testthat::test_that("Inserts an empty row when the 1 is in the last row of newrows", {
  i <- subset(iris[1:10, ], select = -c(Species))
  i[] <- cast_to_char(i)
  i[[newrows]] <- c(rep(0, 9), 1)

  testthat::expect_warning(
    actual <- insert_empty_rows(i)
  )

  expected_rownames <- as.integer(c(1:10, NA))
  testthat::expect_identical(attr(actual, "row.names", expected_rownames), expected_rownames)
})

testthat::test_that("Inserts empty rows when there are 2 1s in the last two rows of newros", {
  i <- subset(iris[1:10, ], select = -c(Species))
  i[] <- cast_to_char(i)
  i[[newrows]] <- c(rep(0, 8), 1, 1)

  testthat::expect_warning(
    actual <- insert_empty_rows(i)
  )

  expected_rownames <- as.integer(c(1:9, NA, 10, NA))
  testthat::expect_identical(attr(actual, "row.names", expected_rownames), expected_rownames)
})

testthat::test_that("Zeroes the newrows column", {
  i <- subset(iris[1:10, ], select = -c(Species))
  i[] <- cast_to_char(i)
  i[[newrows]] <- c(rep(0, 8), 1, 1)

  testthat::expect_warning(
    actual <- insert_empty_rows(i)
  )

  testthat::expect_identical(actual$newrows, rep(0, nrow(actual)))
})
