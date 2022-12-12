library(tibble)

test_that("freq tables can be ordered as expected", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID")

  expected <- tibble::tribble(
    ~label,         ~X3,        ~X4,        ~X5, ~row_type, ~group_level, ~cyl_ord,
    "4",   "1 (6.7)", "8 (66.7)", "2 (40.0)",   "VALUE",            0,       1L,
    "6",  "2 (13.3)", "4 (33.3)", "1 (20.0)",   "VALUE",            0,       2L,
    "8", "12 (80.0)",        "0", "2 (40.0)",   "VALUE",            0,       3L
  )
  expect_equal(freq(mtcars2,rowvar = "cyl",colvar = "gear",.ord = TRUE),expected,ignore_attr = TRUE)

expected <-  tibble::tribble(
     ~label,         ~X3,        ~X4,        ~X5, ~row_type, ~group_level, ~cyl_ord,
        "4",   "1 (6.7)", "8 (66.7)", "2 (40.0)",   "VALUE",            0,       1L,
        "8", "12 (80.0)",        "0", "2 (40.0)",   "VALUE",            0,       2L,
        "6",  "2 (13.3)", "4 (33.3)", "1 (20.0)",   "VALUE",            0,       3L)

expect_equal(freq(mtcars2,rowvar = "cyl",colvar = "gear",descending_by = "5",.ord = TRUE),expected,ignore_attr = TRUE)

expected <- tibble::tribble(
  ~label,          ~X3,         ~X4,         ~X5,    ~row_type, ~group_level, ~vs, ~vs_ord, ~cyl_ord,
  "0",           "",          "",          "", "BY_HEADER1",            0, "0",      1L,        0,
  "4",          "0",         "0",  "1 (25.0)",      "VALUE",            0, "0",      1L,        1,
  "6",          "0", "2 (100.0)",  "1 (25.0)",      "VALUE",            0, "0",      1L,        2,
  "8", "12 (100.0)",         "0",  "2 (50.0)",      "VALUE",            0, "0",      1L,        3,
  "1",           "",          "",          "", "BY_HEADER1",            0, "1",      2L,        0,
  "4",   "1 (33.3)",  "8 (80.0)", "1 (100.0)",      "VALUE",            0, "1",      2L,        1,
  "6",   "2 (66.7)",  "2 (20.0)",         "0",      "VALUE",            0, "1",      2L,        2,
  "8",          "0",         "0",         "0",      "VALUE",            0, "1",      2L,        3
)
expected$vs <- factor(expected$vs)

expect_equal(freq(mtcars2,rowvar = "cyl",colvar = "gear",rowbyvar = "vs",.ord = TRUE),expected,ignore_attr = TRUE)

expected <- tibble::tribble(
  ~label,          ~`3`,         ~`4`,         ~`5`,    ~row_type, ~group_level, ~vs, ~am, ~vs_ord, ~am_ord, ~cyl_ord,
  "0",           "",          "",          "", "BY_HEADER1",            0, "0", "0",      1L,      1L,        0,
  "4",          "0",         "-",         "-",      "VALUE",            0, "0", "0",      1L,      1L,        1,
  "6",          "0",         "-",         "-",      "VALUE",            0, "0", "0",      1L,      1L,        2,
  "8", "12 (100.0)",         "-",         "-",      "VALUE",            0, "0", "0",      1L,      1L,        3,
  "0",           "",          "",          "", "BY_HEADER1",            0, "0", "1",      1L,      2L,        0,
  "4",          "-",         "0",  "1 (25.0)",      "VALUE",            0, "0", "1",      1L,      2L,        1,
  "6",          "-", "2 (100.0)",  "1 (25.0)",      "VALUE",            0, "0", "1",      1L,      2L,        2,
  "8",          "-",         "0",  "2 (50.0)",      "VALUE",            0, "0", "1",      1L,      2L,        3,
  "1",           "",          "",          "", "BY_HEADER1",            0, "1", "0",      2L,      1L,        0,
  "4",   "1 (33.3)",  "2 (50.0)",         "-",      "VALUE",            0, "1", "0",      2L,      1L,        1,
  "6",   "2 (66.7)",  "2 (50.0)",         "-",      "VALUE",            0, "1", "0",      2L,      1L,        2,
  "8",          "0",         "0",         "-",      "VALUE",            0, "1", "0",      2L,      1L,        3,
  "1",           "",          "",          "", "BY_HEADER1",            0, "1", "1",      2L,      2L,        0,
  "4",          "-", "6 (100.0)", "1 (100.0)",      "VALUE",            0, "1", "1",      2L,      2L,        1,
  "6",          "-",         "0",         "0",      "VALUE",            0, "1", "1",      2L,      2L,        2,
  "8",          "-",         "0",         "0",      "VALUE",            0, "1", "1",      2L,      2L,        3
)
expected$vs <- factor(expected$vs)
expected$am <- factor(expected$am)

expect_equal(freq(mtcars2,rowvar = "cyl",colvar = "gear",rowbyvar = "vs",tablebyvar = "am",.ord = TRUE),expected,ignore_attr = TRUE)

expected <- tibble::tribble(
  ~label,         ~`4`,        ~`6`,          ~`8`,    ~row_type, ~group_level, ~am, ~vs, ~am_ord, ~vs_ord, ~gear_ord,
  "0",          "",         "",           "", "BY_HEADER1",            0, "0",  "",      1L,       0,         0,
  "0",          "",         "",           "", "BY_HEADER2",            1, "0", "0",      1L,       1,         0,
  "3",         "-",        "-", "12 (100.0)",      "VALUE",            1, "0", "0",      1L,       1,         1,
  "4",         "-",        "-",          "0",      "VALUE",            1, "0", "0",      1L,       1,         2,
  "5",         "-",        "-",          "0",      "VALUE",            1, "0", "0",      1L,       1,         3,
  "1",          "",         "",           "", "BY_HEADER2",            1, "0", "1",      1L,       2,         0,
  "3",  "1 (33.3)", "2 (50.0)",          "-",      "VALUE",            1, "0", "1",      1L,       2,         1,
  "4",  "2 (66.7)", "2 (50.0)",          "-",      "VALUE",            1, "0", "1",      1L,       2,         2,
  "5",         "0",        "0",          "-",      "VALUE",            1, "0", "1",      1L,       2,         3,
  "1",          "",         "",           "", "BY_HEADER1",            0, "1",  "",      2L,       0,         0,
  "0",          "",         "",           "", "BY_HEADER2",            1, "1", "0",      2L,       1,         0,
  "3",         "0",        "0",          "0",      "VALUE",            1, "1", "0",      2L,       1,         1,
  "4",         "0", "2 (66.7)",          "0",      "VALUE",            1, "1", "0",      2L,       1,         2,
  "5", "1 (100.0)", "1 (33.3)",  "2 (100.0)",      "VALUE",            1, "1", "0",      2L,       1,         3,
  "1",          "",         "",           "", "BY_HEADER2",            1, "1", "1",      2L,       2,         0,
  "3",         "0",        "-",          "-",      "VALUE",            1, "1", "1",      2L,       2,         1,
  "4",  "6 (85.7)",        "-",          "-",      "VALUE",            1, "1", "1",      2L,       2,         2,
  "5",  "1 (14.3)",        "-",          "-",      "VALUE",            1, "1", "1",      2L,       2,         3
)

expected$vs <- factor(expected$vs)
expected$am <- factor(expected$am)

expect_equal(freq(mtcars2,colvar = "cyl",rowvar = "gear",rowbyvar = c("am", "vs"),.ord = TRUE),expected,ignore_attr = TRUE)

})
