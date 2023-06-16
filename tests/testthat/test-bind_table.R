

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

test_that("bind_table can handle non tidytlg tables", {

  adae <- cdisc_adae %>%
    filter(SAFFL == "Y", TRTEMFL == "Y") %>%
    rename(TRT01AN = TRTAN)

  adsl <- cdisc_adsl %>%
    filter(SAFFL == "Y")

  t1 <- adsl%>%
    freq(colvar = 'TRT01AN',
         rowvar = 'SAFFL',
         rowtext = 'Analysis set: Safety',
         statlist = statlist("n"))

  t2 <- adae %>%
    freq(denom_df = adsl,
         colvar = 'TRT01AN',
         rowvar ='TRTEMFL',
         rowtext = 'Subjects with 1 or more AEs',
         statlist = statlist("n (x.x%)"),
         subset = TRTEMFL == "Y")

  t3.1 <- data.frame(label = c("System organ class", "Preferred term"),
                     row_type = "HEADER")

  t3 <- adae %>%
    nested_freq(denom_df = adsl,
                colvar = 'TRT01AN',
                rowvar = 'AEBODSYS*AEDECOD',
                subset = TRTEMFL == "Y",
                statlist = statlist("n (x.x%)"))

  expect_silent(
    {
      bind_table(t1, t2, t3.1, t3,
                 colvar = "TRT01AN",
                 rowbyvar = "AEBODSYS")
    }
  )

})
