library(tibble)

test_that("pad argument works as expected", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID") %>%
    mutate(cyl = as.factor(cyl))

  mtcars2 <- mtcars2 %>%
    filter(cyl != 8)

  t1 <- freq(
    mtcars2,
    colvar = "gear",
    rowvar = "cyl"
  )
  t2 <- freq(
    mtcars2,
    colvar = "gear",
    rowvar = "cyl",
    pad = FALSE
  )
  t3 <- freq(
    mtcars2,
    colvar = "gear",
    rowvar = "cyl",
    rowbyvar = "am",
    pad = FALSE
  )

  expected <- tibble::tribble(
    ~label,        ~X3,        ~X4,        ~X5, ~row_type, ~group_level,
    "4", "1 (33.3)", "8 (66.7)", "2 (66.7)",   "VALUE",            0,
    "6", "2 (66.7)", "4 (33.3)", "1 (33.3)",   "VALUE",            0,
    "8",        "0",        "0",        "0",   "VALUE",            0
  )

  expect_equal(t1,
               expected,
               ignore_attr = TRUE)

  expected <- tibble::tribble(
    ~label,        ~X3,        ~X4,        ~X5, ~row_type, ~group_level,
    "4", "1 (33.3)", "8 (66.7)", "2 (66.7)",   "VALUE",            0,
    "6", "2 (66.7)", "4 (33.3)", "1 (33.3)",   "VALUE",            0
  )


  expect_equal(t2,
               expected,
               ignore_attr = TRUE)


  expected <- tibble::tribble(
    ~label,        ~X3,        ~X4,        ~X5,    ~row_type, ~group_level, ~am,
    "0",         "",         "",         "", "BY_HEADER1",            0, "0",
    "4", "1 (33.3)", "2 (50.0)",        "-",      "VALUE",            0, "0",
    "6", "2 (66.7)", "2 (50.0)",        "-",      "VALUE",            0, "0",
    "1",         "",         "",         "", "BY_HEADER1",            0, "1",
    "4",        "-", "6 (75.0)", "2 (66.7)",      "VALUE",            0, "1",
    "6",        "-", "2 (25.0)", "1 (33.3)",      "VALUE",            0, "1"
  )

  expected$am <- factor(expected$am)

  expect_equal(t3,
               expected,
               ignore_attr = TRUE)

})
