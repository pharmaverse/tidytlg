#' @title Utility function requirements
#' @section Last updated by:
#' Nicholas Masel
#' @section Last update date:
#' 13-JAN-2020
#' @section Category:
#' Helper

library(dplyr)


# Requirements replace_na_with_blank ---------------------------------------------------------------
#' T1. Returns vector with ...
#' T1.1 no changes if no NA.
#' T1.2 blanks in place of NA for character.
#' T1.3 blanks in place of NA for factor.
#' T1.4 blank added as a factor level for factor.

# context("replace_na_with_blank - T1. Returns vector with ...")

test_that("T1.1 no changes if no NA.", {

  v <- paste(rep("test"), 1:3)

  expect_equal(replace_na_with_blank(v),
               c("test 1", "test 2", "test 3"))


})

test_that("T1.2 blanks in place of NA for character.", {

  v <- c(NA, "test 1", "test 2", "test 3")

  expect_equal(replace_na_with_blank(v),
               c("", "test 1", "test 2", "test 3"))

})

test_that("T1.3 blanks in place of NA for factor.", {

  v <- factor(c(NA, "test 1", "test 2", "test 3"),
              levels =c("test 1", "test 2", "test 3"))

  expect_equal(as.character(replace_na_with_blank(v)),
               c("", "test 1", "test 2", "test 3"))

})

test_that("T1.4 NA added as a factor level for factor.", {

  v <- factor(c(NA, "test 1", "test 2", "test 3"),
              levels =c("test 1", "test 2", "test 3"))

  expect_equal(levels(replace_na_with_blank(v)),
               c("", "test 1", "test 2", "test 3"))

})


# Requirements char2factor ---------------------------------------------------------------
#' T1. Returns factor with ...
#' T1.1 label preserved.
#' T1.2 levels based off the numeric variable.

# context("char2factor - T1. Returns factor with ...")

df_char2factor <- tibble::tribble(
  ~TRT01P, ~TRT01PN,
  "Placebo",   1,
  "Low Dose",  2,
  "High Dose", 3
)

attr(df_char2factor$TRT01P, "label") <- "Planned Treatment"

test_that("T1.1 label preserved.", {
  expect_equal(attr(char2factor(df_char2factor, "TRT01P", "TRT01PN"), "label"),
               "Planned Treatment")
})

test_that("T1.2 levels based off the numeric variable.", {
  expect_equal(levels(char2factor(df_char2factor, "TRT01P", "TRT01PN")),
               c("Placebo", "Low Dose", "High Dose"))
})
