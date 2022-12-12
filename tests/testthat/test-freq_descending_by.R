library(tibble)
library(haven)

test_that("descending by will sort by the column named passed, but will ", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID") %>%
    mutate(am = factor(am),
           cyl = factor(cyl),
           gear = factor(gear))

  mtcars3 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "cyl",
                  statlist = statlist("n (x.x%)"))
  expect_equal(mtcars3[["0"]],
               c("3 (15.8%)", "4 (21.1%)", "12 (63.2%)"),
               ignore_attr = TRUE)

  mtcars4 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "cyl",
                  statlist = statlist("n (x.x%)"),
                  descending_by = "0")
  expect_equal(mtcars4[["0"]],
               c("12 (63.2%)", "4 (21.1%)", "3 (15.8%)"),
               ignore_attr = TRUE)
})
