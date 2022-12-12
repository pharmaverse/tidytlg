

mtcars2 <- mtcars %>%
  rownames_to_column(var = "USUBJID")

t1 <- univar(mtcars2,
             colvar = "gear",
             rowvar = "drat",
             tablebyvar = 'am',
             rowbyvar = "carb")

t2 <- univar(mtcars2,
             colvar = "gear",
             rowvar = "wt",
             tablebyvar = 'am',
             rowbyvar = "carb") %>%
  mutate(anbr = 'hi')

t3 <- univar(mtcars2,
             colvar = "gear",
             rowvar = "hp",
             tablebyvar = 'am',
             rowbyvar = "carb") %>%
  mutate(anbr = "7")

tbl1 <- bind_table(t1, t2, t3,
                  colvar = "gear",
                  rowbyvar = "carb")

tbl2 <- bind_table(t1, t2, t3,
                  colvar = "gear",
                  tablebyvar = "am",
                  rowbyvar = "carb")

test_that("anbr is assigned correctly", {
  expect_equal(tbl1$anbr, c(rep(1, nrow(t1)), rep(2, nrow(t2)), rep(7, nrow(t3))))
  expect_equal(tbl1$anbr, c(rep(1, nrow(t1)), rep(2, nrow(t2)), rep(7, nrow(t3))))
})

test_that("format variables are added", {
  expect_true(all(c("anbr", "indentme", "roworder", "newrows", "newpage") %in% names(tbl1)))
  expect_true(all(c("anbr", "indentme", "roworder", "newrows", "newpage") %in% names(tbl2)))
})

