library(tibble)
library(haven)

test_that("cutoff and cutoff_stat work as expected", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID") %>%
    mutate(am = factor(am),
           cyl = factor(cyl),
           gear = factor(gear))

  mtcars3 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "cyl",
                  cutoff = 25,
                  statlist = statlist("n (x.x%)"))

  expect_equal(mtcars3[["0"]],
               c("3 (15.8%)", "12 (63.2%)"),
               ignore_attr = TRUE)
  expect_equal(mtcars3[["1"]],
               c("8 (61.5%)", "2 (15.4%)"),
               ignore_attr = TRUE)

  mtcars4 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "cyl",
                  cutoff = "0 >= 25",
                  statlist = statlist("n (x.x%)"))
  expect_equal(mtcars4[["0"]],
               "12 (63.2%)",
               ignore_attr = TRUE)

  mtcars5 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "cyl",
                  cutoff_stat = "n",
                  cutoff = "0 >= 10",
                  statlist = statlist("n (x.x%)"))
  expect_equal(mtcars5[["0"]],
               "12 (63.2%)",
               ignore_attr = TRUE)
})
