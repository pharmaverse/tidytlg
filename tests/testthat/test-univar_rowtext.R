
test_that("rowtext works as expected for univar tables", {

  expected <- tibble::tribble(
    ~label,              ~X3,              ~X4,              ~X5, ~row_type, ~group_level,
    "Miles per Gal",               "",               "",               "",  "HEADER",            0,
    "N",             "15",             "12",              "5",       "N",            0,
    "Mean (SD)",  "16.11 (3.372)",  "24.53 (5.277)",  "21.38 (6.659)",   "VALUE",            0,
    "Median",          "15.50",          "22.80",          "19.70",   "VALUE",            0,
    "Range",   "(10.4; 21.5)",   "(17.8; 33.9)",   "(15.0; 30.4)",   "VALUE",            0,
    "IQ range", "(14.30; 18.70)", "(21.00; 28.85)", "(15.80; 26.00)",   "VALUE",            0
  )

  expect_equal(
      mtcars %>%
        univar(
          colvar = "gear",
          rowvar = "mpg",
          row_header = "Miles per Gal"
        ),
      expected,
      ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~label,     ~X3,     ~X4,     ~X5, ~row_type, ~group_level,
    "Mean label", "16.11", "24.53", "21.38",   "VALUE",            0
  )

  expect_equal(
      mtcars %>%
        univar(
          colvar = "gear",
          rowvar = "mpg",
          rowtext = "Mean label",
          statlist = statlist("MEAN")
        ),
      expected,
      ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~cyl,                 ~label,     ~X3,     ~X4,     ~X5, ~row_type, ~group_level,
    "4", "Multiple Mean Labels", "21.50", "26.93", "28.20",   "VALUE",            0,
    "6", "Multiple Mean Labels", "19.75", "19.75", "19.70",   "VALUE",            0,
    "8", "Multiple Mean Labels", "15.05",     "-", "15.40",   "VALUE",            0
  )

  expect_equal(
      mtcars %>%
        univar(
          colvar = "gear",
          rowvar = "mpg",
          rowtext = "Multiple Mean Labels",
          tablebyvar = "cyl",
          statlist = statlist("MEAN")
        ),
      expected,
      ignore_attr = TRUE
  )

})
