
test_that("sparse data is handled correctly", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID") %>%
    mutate(across(.cols = everything(), .fns = as.factor)) %>%
    filter(FALSE)

  expected <- tibble::tribble(
    ~label, ~X4, ~X6, ~X8, ~row_type, ~group_level,
    "3", "-", "-", "-",   "VALUE",            0,
    "4", "-", "-", "-",   "VALUE",            0,
    "5", "-", "-", "-",   "VALUE",            0
  )

  expect_equal(freq(mtcars2,colvar = "cyl",rowvar = "gear",statlist = statlist("n (x.x%)")),expected,ignore_attr = TRUE)

  expected <- tibble::tribble(
    ~label, ~X4, ~X6, ~X8, ~row_type, ~group_level,
    "3", "-", "-", "-",   "VALUE",            0,
    "4", "-", "-", "-",   "VALUE",            0,
    "5", "-", "-", "-",   "VALUE",            0
  )


  expect_equal(freq(mtcars2,colvar = "cyl",rowvar = "gear",statlist = statlist(c("n"))),expected,ignore_attr = TRUE)

  expected <- tibble::tribble(
    ~label, ~X4, ~X6, ~X8,    ~row_type, ~group_level, ~am,
    "0",  "",  "",  "", "BY_HEADER1",            0, "0",
    "3", "-", "-", "-",      "VALUE",            0, "0",
    "4", "-", "-", "-",      "VALUE",            0, "0",
    "5", "-", "-", "-",      "VALUE",            0, "0",
    "1",  "",  "",  "", "BY_HEADER1",            0, "1",
    "3", "-", "-", "-",      "VALUE",            0, "1",
    "4", "-", "-", "-",      "VALUE",            0, "1",
    "5", "-", "-", "-",      "VALUE",            0, "1"
  )
  expected$am <- factor(expected$am)

  expect_equal(
    freq(mtcars2,
           colvar = "cyl",
           rowvar = "gear",
           rowbyvar = "am",
           statlist = statlist(c("n"))),
    expected,ignore_attr = TRUE)

  expected <- tibble::tribble(
    ~label, ~X4, ~X6, ~X8, ~row_type, ~group_level, ~am,
    "3", "-", "-", "-",   "VALUE",            0, "0",
    "4", "-", "-", "-",   "VALUE",            0, "0",
    "5", "-", "-", "-",   "VALUE",            0, "0",
    "3", "-", "-", "-",   "VALUE",            0, "1",
    "4", "-", "-", "-",   "VALUE",            0, "1",
    "5", "-", "-", "-",   "VALUE",            0, "1"
  )
  expected$am <- factor(expected$am)

  expect_equal(
      freq(mtcars2,
           colvar = "cyl",
           rowvar = "gear",
           tablebyvar = "am",
           statlist = statlist(c("n" ))),
      expected,ignore_attr = TRUE
  )

})
