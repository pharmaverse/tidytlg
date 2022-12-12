library(tibble)
library(haven)

test_that("rowtext will relabel the row if unnamed and only one row is passed", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID")

  mtcars3 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "cyl",
                  statlist = statlist("n (x.x%)"),
                  subset = cyl == 4,
                  rowtext = "Cars with 4 cylinders")

  expected_df <- tribble( ~label,      ~`0`,        ~`1`,        ~row_type, ~group_level,
                          "Cars with 4 cylinders","3 (15.8%)", "8 (61.5%)",    "HEADER", 0) %>%
    structure(class = c("freqs", class(.)))

  expect_equal(mtcars3,
               expected_df,
               ignore_attr = TRUE)

  expected <- tibble::tribble(
    ~label, ~X0, ~X1, ~row_type, ~group_level,
    "Cars with 0 cylinders", "0", "0",  "HEADER",            0
  )

  expect_equal(
    freq(mtcars2,
         colvar = "am",
         rowvar = "cyl",
         statlist = statlist("n (x.x%)", distinct = TRUE),
         subset = cyl == 0,
         rowtext = "Cars with 0 cylinders"),
    expected,
    ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~label,          ~X0,         ~X1, ~row_type, ~group_level,
    "Cars with 6 cylinders",  "4 (21.1%)", "3 (23.1%)",   "VALUE",            0,
    "Cars with 8 cylinders", "12 (63.2%)", "2 (15.4%)",   "VALUE",            0
  )

  expect_equal(
    freq(
      mtcars2,
      colvar = "am",
      rowvar = "cyl",
      statlist = statlist("n (x.x%)", distinct = TRUE),
      subset = cyl > 4,
      rowtext = c("Cars with 6 cylinders" = "6", "Cars with 8 cylinders" = "8")
    ),
    expected,
    ignore_attr = TRUE
  )
})

test_that("row_header will add a header row", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID") %>%
    mutate(am = factor(am),
           cyl = factor(cyl),
           gear = factor(gear))

  mtcars3 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "cyl",
                  statlist = statlist(c("N","n (x.x%)")),
                  row_header = "Cars cylinders")

  expected_df <- tribble(~label,          ~`0`,         ~`1`, ~row_type, ~group_level,
                         "Cars cylinders",           "",          "",  "HEADER", 0,
                         "N",         "19",        "13",       "N", 0,
                         "4",  "3 (15.8%)", "8 (61.5%)",   "VALUE", 0,
                         "6",  "4 (21.1%)", "3 (23.1%)",   "VALUE", 0,
                         "8", "12 (63.2%)", "2 (15.4%)",   "VALUE", 0) %>%
    structure(class = c("freqs", class(.)))

  expect_equal(mtcars3, expected_df,
               ignore_attr = TRUE)


  mtcars4 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "cyl",
                  statlist = statlist("n (x.x%)"),
                  row_header = "Cars cylinders")

  expected_df <- tribble(~label, ~`0`, ~`1`, ~row_type, ~group_level,
                         "Cars cylinders", "", "", "HEADER", 0,
                         "4", "3 (15.8%)", "8 (61.5%)", "VALUE", 0,
                         "6", "4 (21.1%)", "3 (23.1%)", "VALUE", 0,
                         "8", "12 (63.2%)", "2 (15.4%)", "VALUE", 0) %>%
    structure(class = c("freqs", class(.)))

  expect_equal(mtcars4, expected_df,
               ignore_attr = TRUE)
})
