#' @title roundSAS() requirements
#' @section Last updated by:
#' Sheng-Wei Wang
#' @section Last update date:
#' 13-APR-2021
#' @section Category:
#' Generate Results

library(dplyr)

# Requirements --------------------------------------------------------------------------------
#' T1. The function produces expected decimal rounding ...
#' T1.1 for rounding up midpoint values
#' T1.2 for rounding up midpoint values and convert to character
#' T2. The function creates expected missing value label ...
#' T2.1 when converting to character with missing values
#' T2.2 when converting to character and na_char is specified

# context("roundSAS - T1. The function produces expected decimal rounding ...")

test_that("T1.1 for rounding up midpoint values", {

  x <- c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)

  expect_equal(roundSAS(x, digits = 0),
               c(-3, -2, -1, 1, 2, 3))
})

test_that("T1.2 for rounding up midpoint values and convert to character", {

  x <- c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)

  expect_equal(roundSAS(x, digits = 0, as_char = TRUE),
               c("-3", "-2", "-1", "1", "2", "3"))
})

# context("roundSAS - T2. The function creates expected missing value label ...")

test_that("T2.1 when converting to character with missing values", {

  y <- c(8.65, 8.75, NA, 9.85, 9.95)

  expect_equal(roundSAS(y, digits = 1, as_char = TRUE),
               c("8.7", "8.8", "NA", "9.9", "10.0"))

})

test_that("T2.2 when converting to character and na_char is specified", {

  y <- c(8.65, 8.75, NA, 9.85, 9.95)

  expect_equal(roundSAS(y, digits = 1, as_char = TRUE, na_char = "NE"),
               c("8.7", "8.8", "NE", "9.9", "10.0"))

})




