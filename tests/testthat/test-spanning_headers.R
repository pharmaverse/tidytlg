#' @title spanning_headers() requirements
#' @section Last updated by:
#' Nicholas Masel
#' @section Last update date:
#' 26-OCT-2020
#' @section Category:
#' Format Results

library(dplyr)

# Requirements --------------------------------------------------------------------------------
#' T1. Accounts for ...
#' T1.1 NA.
#' T1.2 ''.
#' T2. Accounts for ...
#' T2.1 one spanning header.
#' T2.2 two spanning headers.
#' T2.3 three spanning headers.


# Tests ---------------------------------------------------------------------------------------
# context("spanning_headers - T1. Accounts for ...")

test_that("T1.1 NA.", {

  column_metadata <- tibble::tribble(
    ~tbltype, ~coldef,   ~decode, ~span1,
    "type1",      "0", "Placebo",     NA,
    "type1",      "1",     "Low",     NA,
    "type1",      "2",    "High",     NA
  )

  expect_equal(spanning_headers(column_metadata), NULL)
})

test_that("T1.2 ''.", {

  column_metadata <- tibble::tribble(
    ~tbltype, ~coldef,   ~decode, ~span1,
    "type1",      "0", "Placebo",     "",
    "type1",      "1",     "Low",     "",
    "type1",      "2",    "High",     ""
  )

  expect_equal(spanning_headers(column_metadata), NULL)
})


# context("spanning_headers - T2. Is accurate for ...")

test_that("T2.1 one spanning header.", {

  column_metadata <- tibble::tribble(
    ~tbltype, ~coldef,   ~decode,   ~span1,
    "type1",      "0", "Placebo",       NA,
    "type1",      "1",     "Low", "Active",
    "type1",      "2",    "High", "Active"
  )

  expect_equal(spanning_headers(column_metadata), list(span1 = c("", "", "Active", "Active")))
})

test_that("T2.2 two spanning headers.", {

  column_metadata <- tibble::tribble(
    ~tbltype, ~coldef,   ~decode,   ~span1,     ~span2,
    "type1",      "0", "Placebo",       NA, "Header 2",
    "type1",      "1",     "Low", "Active", "Header 2",
    "type1",      "2",    "High", "Active", "Header 2"
  )

  expect_equal(spanning_headers(column_metadata), list(span1 = c("", "", "Active", "Active"),
                                                       span2 = c("", "Header 2", "Header 2", "Header 2")))
})

test_that("T2.3 three spanning headers.", {

  column_metadata <- tibble::tribble(
    ~tbltype, ~coldef,   ~decode,   ~span1,     ~span2,     ~span3,
    "type1",      "0", "Placebo",       NA, "Header 2",         NA,
    "type1",      "1",     "Low", "Active", "Header 2",         NA,
    "type1",      "2",    "High", "Active", "Header 2", "Header 3"
  )

  expect_equal(spanning_headers(column_metadata), list(span1 = c("", "", "Active", "Active"),
                                                       span2 = c("", "Header 2", "Header 2", "Header 2"),
                                                       span3 = c("", "", "", "Header 3")))
})
