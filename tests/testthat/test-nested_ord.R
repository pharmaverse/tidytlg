library(tibble)


test_that("nested tables work with .ord as expected", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID")
  mtcars2$grp <- rep(c("A", "B"), length.out = 32)

  expected <- tibble::tribble(
    ~label,        ~X3,        ~X4,        ~X5, ~row_type, ~nested_level, ~group_level, ~grp, ~grp_ord, ~cyl_ord,
    "A", "8 (53.3)", "5 (41.7)", "3 (60.0)",  "NESTED",             0,            0,  "A",       1L,       NA,
    "4",  "1 (6.7)", "3 (25.0)", "1 (20.0)",  "NESTED",             1,            0,  "A",       1L,       1L,
    "6",        "0", "2 (16.7)",        "0",  "NESTED",             1,            0,  "A",       1L,       2L,
    "8", "7 (46.7)",        "0", "2 (40.0)",  "NESTED",             1,            0,  "A",       1L,       3L,
    "B", "7 (46.7)", "7 (58.3)", "2 (40.0)",  "NESTED",             0,            0,  "B",       2L,       NA,
    "4",        "0", "5 (41.7)", "1 (20.0)",  "NESTED",             1,            0,  "B",       2L,       1L,
    "6", "2 (13.3)", "2 (16.7)", "1 (20.0)",  "NESTED",             1,            0,  "B",       2L,       2L,
    "8", "5 (33.3)",        "0",        "0",  "NESTED",             1,            0,  "B",       2L,       3L
  )

  expected$grp <- factor(expected$grp)

  expect_equal(
    nested_freq(
      mtcars2,
      colvar = "gear",
      rowvar = "grp*cyl",
      .ord = TRUE
    ),
    expected,
    ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~label,        ~X3,        ~X4,        ~X5, ~row_type, ~nested_level, ~group_level, ~grp, ~grp2, ~grp_ord, ~grp2_ord, ~cyl_ord,
    "A", "8 (53.3)", "5 (41.7)", "3 (60.0)",  "NESTED",             0,            1,  "A",   "A",       1L,        NA,       NA,
    "X", "4 (26.7)", "4 (33.3)",        "0",  "NESTED",             1,            1,  "A",   "X",       1L,        1L,       NA,
    "4",        "0", "2 (16.7)",        "0",  "NESTED",             2,            1,  "A",   "X",       1L,        1L,       1L,
    "6",        "0", "2 (16.7)",        "0",  "NESTED",             2,            1,  "A",   "X",       1L,        1L,       2L,
    "8", "4 (26.7)",        "0",        "0",  "NESTED",             2,            1,  "A",   "X",       1L,        1L,       3L,
    "Y", "4 (26.7)",  "1 (8.3)", "3 (60.0)",  "NESTED",             1,            1,  "A",   "Y",       1L,        2L,       NA,
    "4",  "1 (6.7)",  "1 (8.3)", "1 (20.0)",  "NESTED",             2,            1,  "A",   "Y",       1L,        2L,       1L,
    "8", "3 (20.0)",        "0", "2 (40.0)",  "NESTED",             2,            1,  "A",   "Y",       1L,        2L,       3L,
    "B", "7 (46.7)", "7 (58.3)", "2 (40.0)",  "NESTED",             0,            1,  "B",   "B",       2L,        NA,       NA,
    "X", "5 (33.3)", "3 (25.0)",        "0",  "NESTED",             1,            1,  "B",   "X",       2L,        1L,       NA,
    "4",        "0",  "1 (8.3)",        "0",  "NESTED",             2,            1,  "B",   "X",       2L,        1L,       1L,
    "6", "2 (13.3)", "2 (16.7)",        "0",  "NESTED",             2,            1,  "B",   "X",       2L,        1L,       2L,
    "8", "3 (20.0)",        "0",        "0",  "NESTED",             2,            1,  "B",   "X",       2L,        1L,       3L,
    "Y", "2 (13.3)", "4 (33.3)", "2 (40.0)",  "NESTED",             1,            1,  "B",   "Y",       2L,        2L,       NA,
    "4",        "0", "4 (33.3)", "1 (20.0)",  "NESTED",             2,            1,  "B",   "Y",       2L,        2L,       1L,
    "6",        "0",        "0", "1 (20.0)",  "NESTED",             2,            1,  "B",   "Y",       2L,        2L,       2L,
    "8", "2 (13.3)",        "0",        "0",  "NESTED",             2,            1,  "B",   "Y",       2L,        2L,       3L
  )

  expected$grp <- factor(expected$grp)
  expected$grp2 <- factor(expected$grp2)

  mtcars2$grp2 <- rep(c("X", "Y"), each = 16)
  expect_equal(
    nested_freq(
      mtcars2,
      colvar = "gear",
      rowvar = "grp*grp2*cyl",
      .ord = TRUE
    ),
    expected,
    ignore_attr = TRUE
  )

})
