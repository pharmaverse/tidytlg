library(tibble)
library(haven)

test_that("subset will filter records as expected", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID") %>%
    mutate(am = factor(am),
           cyl = factor(cyl),
           gear = factor(gear))

  mtcars3 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "cyl",
                  subset = gear == 4,
                  statlist = statlist("n (x.x%)"))

  expect_equal(mtcars3[["0"]],
               c("2 (10.5%)", "2 (10.5%)", "0"),
               ignore_attr = TRUE)
})
