mtcars_test <- mtcars
mtcars_test$negnum <- range(-3,2,5)
mtcars_test$allna <- as.numeric(NA)
mtcars_test$somena <- rep(c(NA, 4))
mtcars_test$str <- 'string'
mtcars_test$USUBJID = row.names(mtcars_test)
test_eval <- "am"
test_that("checks for univar function correctly", {
  # df
  expect_visible(univar(mtcars_test, colvar = "gear", rowvar = "wt"))
  expect_visible(univar(mtcars_test, colvar = test_eval, rowvar = "wt"))
  expect_error(univar(c(1,2), colvar = "gear", rowvar = "wt"),
               "Argument df to function univar should be type data.frame")
  # rowvar
  expect_visible(univar(mtcars_test, colvar = "gear", rowvar = "somena"))
  expect_error(univar(mtcars_test, colvar = "gear", rowvar = "allna"))
  # geometric estimable check
  expect_error(univar(mtcars_test, colvar = "gear", rowvar = "negnum", statlist = statlist(c("GSD", "GSE"))))
  # wide bool check
  expect_visible(univar(mtcars_test, colvar = "gear", rowvar = "wt", wide = TRUE))
  expect_error(univar(mtcars_test, colvar = "gear", rowvar = "wt", wide = 'str'))
  # check statlist
  expect_error(univar(mtcars_test, colvar = "gear", rowvar = "wt", statlist = statlist(c("NOT A STAT"))))
})

test_that("checks for freq function correctly", {
  # df
  expect_visible(freq(mtcars_test, colvar = "gear", rowvar = "cyl"))
  expect_visible(freq(mtcars_test, colvar = "gear", rowvar = test_eval))
  expect_error(freq(c(1,2), colvar = "gear", rowvar = "cyl"),
               "")
})

test_that("checks for nested_freq function correctly", {
  # df
  expect_visible(nested_freq(mtcars_test, colvar = "gear", rowvar = "cyl*carb"))
  expect_visible(nested_freq(mtcars_test, colvar = test_eval, rowvar = "cyl*carb"))
  expect_error(nested_freq(c(1,2), colvar = "gear", rowvar = "cyl*carb"),
               "Argument df to function nested_freq should be type data.frame")
  # rowvar nesting
  expect_visible(nested_freq(mtcars_test, colvar = "gear", rowvar = "cyl*carb"))
  expect_visible(nested_freq(mtcars_test, colvar = "gear", rowvar = "cyl*carb*am"))
  expect_error(nested_freq(mtcars_test, colvar = "gear", rowvar = "cyl"),
               "Argument rowvar to function nested_freq is incorrectly formatted.")
  expect_error(nested_freq(mtcars_test, colvar = "gear", rowvar = "cyl*am*vs*disp"),
               "Argument rowvar to function nested_freq is incorrectly formatted.")
})

test_that("check updates to check gentlg", {

  basefreq <- freq(cdisc_adsl,
                   rowvar = "ITTFL",
                   colvar = "TRT01PN",
                   statlist = statlist(c("n")))

  suppressWarnings(expect_error(gentlg(huxme       = basefreq,
                                       file        = "vignettes/validation/test_code/test_code_output/test",
                                       title       = "Test",
                                       format      = "RTF",
                                       opath       = "ERROR",
                                       colheader   = c("Test","One","Two","The")),"opath 'ERROR' does not exist for function gentlg"))


  expect_error(gentlg(huxme       = basefreq,
                      file        = "vignettes/validation/test_code/test_code_output/test",
                      title       = "Test",
                      format      = "ERROR",
                      colheader   = c("Test","One","Two","The")))


  suppressWarnings(expect_error(gentlg(huxme       = basefreq,
                                       file        = "vignettes/validation/test_code/test_code_output/test",
                                       title       = "Test",
                                       format      = "RTF",
                                       wcol = c(1,1),
                                       colheader   = NA)),"wcol's length must be 1 or the length of final output")


  suppressWarnings(expect_error(gentlg(huxme       = basefreq,
                                       tlf         = "Figure",
                                       file        = "vignettes/validation/test_code/test_code_output/test",
                                       title       = "Test",
                                       format      = "RTF",
                                       plotnames   = "ERROR",
                                       colheader   = NA),"plotnames 'ERROR' does not exist for function gentlg"))


  expect_error(gentlg(huxme       = basefreq,
                      tlf         = "Figure",
                      file        = "vignettes/validation/test_code/test_code_output/test",
                      title       = "Test",
                      format      = "RTF",
                      idvars      = "ERROR",
                      colheader   = NA),"idvars 'ERROR' does not exist in huxme or is all NA for function: gentlg")


  expect_error(gentlg(huxme       = basefreq,
                      file        = "vignettes/validation/test_code/test_code_output/test",
                      title       = "Test",
                      format      = "RTF",
                      orientation = "ERROR"),"orientation 'ERROR' is not either 'landscape' or 'portrait' for function gentlg")
})


test_that("test freq update", {

  expect_error(freq(cdisc_adsl,
                    rowvar = "RACE",
                    colvar = NA,
                    statlist = statlist(c("n"))),
               "colvar to function freq should be type character")

  expect_error(suppressWarnings(freq(cdisc_adsl,
                                     rowvar = "RACE",
                                     colvar = '',
                                     statlist = statlist(c("n")))),
               "colvar '' does not exist in df or is all NA for function: freq")

  expect_error(suppressWarnings(freq(cdisc_adsl,
                                     rowvar = "RACE",
                                     colvar = 'MISSING',
                                     statlist = statlist(c("n")))),
               "colvar 'MISSING' does not exist in df or is all NA for function: freq")

  expect_error(suppressWarnings(freq(cdisc_adsl,
                                     rowvar = NA,
                                     colvar = "RACE",
                                     statlist = statlist(c("n")))),
               "Argument rowvar to function freq should be type character")

  expect_error(freq(cdisc_adsl,
                    rowvar = '',
                    colvar = 'RACE',
                    statlist = statlist(c("n"))),
               "rowvar '' does not exist in df or is all NA for function: freq")

  expect_error(freq(cdisc_adsl,
                    rowvar = 'MISSING',
                    colvar = 'RACE',
                    statlist = statlist(c("n"))),
               "rowvar 'MISSING' does not exist in df or is all NA for function: freq")

  expect_error(freq(cdisc_adsl,
                    rowvar = 'RACE',
                    colvar = 'TRT01P',
                    tablebyvar = "MISSING",
                    statlist = statlist(c("n"))),
               "tablebyvar 'MISSING' does not exist in df or is all NA for function: freq")

  expect_error(freq(cdisc_adsl,
                    rowvar = 'RACE',
                    colvar = 'TRT01P',
                    rowbyvar = "MISSING",
                    statlist = statlist(c("n"))),
               "rowbyvar 'MISSING' does not exist in df or is all NA for function: freq")


  expect_error(freq(cdisc_adsl,
                    rowvar = "ITTFL",
                    colvar = "TRT01PN",
                    statlist = statlist(c("N"))))

  expect_error(freq(cdisc_adsl,
                    rowvar = "ITTFL",
                    colvar = "TRT01PN",
                    statlist = statlist(c())))
  expect_error(freq(cdisc_adsl,
                    rowvar = "ITTFL",
                    colvar = "TRT01PN",
                    statlist = statlist(c("ERROR"))))

  expect_error(freq(cdisc_adsl,
                    rowvar = "ITTFL",
                    colvar = "ITTFL"),paste0(paste0(c("colvar","rowvar"),collapse = ", ")," all have value ", "ITTFL"," for function ","freq","\n\n"))

  expect_error(freq(cdisc_adsl,
                    rowvar = "ITTFL",
                    colvar = "TRT01PN",
                    rowbyvar = "ITTFL"),paste0(paste0(c("rowvar","rowbyvar"),collapse = ", ")," all have value ", "ITTFL"," for function ","freq","\n\n"))

  expect_error(freq(cdisc_adsl,
                    rowvar = "ITTFL",
                    colvar = "TRT01PN",
                    tablebyvar = "ITTFL"),paste0(paste0(c("tablebyvar","rowvar"),collapse = ", ")," all have value ", "ITTFL"," for function ","freq","\n\n"))

  expect_error(freq(cdisc_adsl,
                    rowvar = "ITTFL",
                    colvar = "TRT01PN",
                    rowbyvar = "TRT01PN"),paste0(paste0(c("colvar","rowbyvar"),collapse = ", ")," all have value ", "TRT01PN"," for function ","freq","\n\n"))

  expect_error(freq(cdisc_adsl,
                    rowvar = "ITTFL",
                    colvar = "TRT01PN",
                    rowbyvar = "TRT01PN"),paste0(paste0(c("colvar","rowbyvar"),collapse = ", ")," all have value ", "TRT01PN"," for function ","freq","\n\n"))

  expect_error(freq(cdisc_adsl,
                    rowvar = "ITTFL",
                    colvar = "TRT01PN",
                    tablebyvar = "TRT01PN"),paste0(paste0(c("colvar","tablebyvar"),collapse = ", ")," all have value ", "TRT01PN"," for function ","freq","\n\n"))

  expect_error(freq(cdisc_adsl,
                    rowvar = "ITTFL",
                    colvar = "TRT01PN",
                    rowbyvar = "SEX",
                    tablebyvar = "SEX"),paste0(paste0(c("tablebyvar","rowbyvar"),collapse = ", ")," all have value ", "SEX"," for function ","freq","\n\n"))
})

#' @editor Aidan Ceney
#' @editDate 2022-03-02
test_that("Test univar new checks", {

  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = NULL,
           rowvar = "AVAL",
           statlist = statlist(c("N", "MEAN_CI","MIN","MAX")),
           alpha = .10),"Argument colvar to function univar is required but no value has been supplied")

  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = NULL,
           statlist = statlist(c("N", "MEAN_CI","MIN","MAX")),
           alpha = .10),"Argument rowvar to function univar is required but no value has been supplied")

  expect_error(
    #Supress addtional warning
    suppressWarnings({
      univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
             colvar = "MISSING",
             rowvar = "AVAL",
             statlist = statlist(c("N", "MEAN_CI","MIN","MAX")),
             alpha = .10)
    }),"colvar 'MISSING' does not exist in df or is all NA for function: univar")

  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "MISSING",
           statlist = statlist(c("N", "MEAN_CI","MIN","MAX")),
           alpha = .10),"rowvar 'MISSING' does not exist in df or is all NA for function: univar")

  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           tablebyvar = "MISSING",
           statlist = statlist(c("N", "MEAN_CI","MIN","MAX")),
           alpha = .10),"tablebyvar 'MISSING' does not exist in df or is all NA for function: univar")

  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           rowbyvar = "MISSING",
           statlist = statlist(c("N", "MEAN_CI","MIN","MAX")),
           alpha = .10),"rowbyvar 'MISSING' does not exist in df or is all NA for function: univar")


  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           statlist = statlist(c()),
           alpha = .10))

  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           statlist = statlist(c("")),
           alpha = .10))

  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           statlist = statlist(c("ERROR")),
           alpha = .10))


  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           tablebyvar = "TRTA",
           alpha = .10),"colvar, tablebyvar all have value TRTA for function univar")




  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           rowbyvar = "TRTA",
           alpha = .10),"colvar, rowbyvar all have value TRTA for function univar")


  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           rowbyvar = "SEX",
           tablebyvar = "SEX",
           alpha = .10),"tablebyvar, rowbyvar all have value SEX for function univar")





  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           alpha = 1.5), "alpha must be a number in between 0 and 1 for function univar")


  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           alpha = -.5),  "alpha must be a number in between 0 and 1 for function univar")


  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           precisionby = "SEX"))

  expect_error(
    univar(cdisc_advs %>% filter(PARAMCD %in% c("HEIGHT","WEIGHT")),
           colvar = "TRTA",
           rowvar = "AVAL",
           precisionon  = "MISSING"),"precisionon 'MISSING' does not exist in df or is all NA for function: univar")

})

test_that("new check errors nested freq", {

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*RACE",
                           colvar = NA,
                           statlist = statlist(c("n"))),"Argument colvar to function nested_freq should be type character")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "RACE",
                           colvar = '',
                           statlist = statlist(c("n"))),"Argument rowvar to function nested_freq is incorrectly formatted.")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*RACE",
                           colvar = 'MISSING',
                           statlist = statlist(c("n"))),"colvar 'MISSING' does not exist in df or is all NA for function: nested_freq")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = NA,
                           colvar = "RACE",
                           statlist = statlist(c("n"))),"Argument rowvar to function nested_freq should be type character")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = '',
                           colvar = 'RACE',
                           statlist = statlist(c("n"))),"Argument rowvar to function nested_freq is incorrectly formatted.")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = 'MISSING',
                           colvar = 'RACE',
                           statlist = statlist(c("n"))),"Argument rowvar to function nested_freq is incorrectly formatted.")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*RACE",
                           colvar = 'TRT01P',
                           tablebyvar = "MISSING",
                           statlist = statlist(c("n"))),"tablebyvar 'MISSING' does not exist in df or is all NA for function: nested_freq")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*RACE",
                           colvar = 'TRT01P',
                           rowbyvar = "MISSING",
                           statlist = statlist(c("n"))),"rowbyvar 'MISSING' does not exist in df or is all NA for function: nested_freq")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "TRT01PN",
                           statlist = statlist(c("N"))))
  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "TRT01PN",
                           statlist = statlist(c())))
  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "TRT01PN",
                           statlist = statlist(c("ERROR"))))

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "TRT01PN",
                           cutoff = 2,
                           cutoff_stat = "ERROR"),"Cutoff stat ERROR for function nested_freq")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "ITTFL"),"colvar, nested rowvar 2 all have value ITTFL for function nested_freq")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "TRT01PN",
                           rowbyvar = "ITTFL"), "rowbyvar, nested rowvar 2 all have value ITTFL for function nested_freq")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "TRT01PN",
                           tablebyvar = "ITTFL"), "tablebyvar, nested rowvar 2 all have value ITTFL for function nested_freq")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "TRT01PN",
                           rowbyvar = "TRT01PN"),"colvar, rowbyvar all have value TRT01PN for function nested_freq")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "TRT01PN",
                           rowbyvar = "TRT01PN"),"colvar, rowbyvar all have value TRT01PN for function nested_freq")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "TRT01PN",
                           tablebyvar = "TRT01PN"),"tablebyvar all have value TRT01PN for function nested_freq")

  expect_error(nested_freq(cdisc_adsl,
                           rowvar = "STUDYID*ITTFL",
                           colvar = "TRT01PN",
                           rowbyvar = "SEX",
                           tablebyvar = "SEX"),"tablebyvar, rowbyvar all have value SEX for function nested_freq")
})


test_that("Errors for check generate results", {
  expect_error(generate_results(NULL))

  expect_error(generate_results(table_metadata))


  expect_error(generate_results(tibble::tribble(~Test,~tbltype,"TEST","Test")))


  table_metadata <- tibble::tribble(
    ~anbr, ~func,  ~tbltype,  ~df,~colvar,~rowvar,
    1,     "freq", "Test","Missing","TRT01PN","ITTFL"
  )
  col_metadata <- tibble::tribble(
    ~tbltype,	~coldef,	  ~decode,	~span1,
    "Test",  "Xanomeline High Dose",  "High Dose", "Xanomeline")
  expect_error(generate_results(tibble::tribble(
    ~anbr, ~func,  ~tbltype,  ~df,~colvar,~rowvar,
    1,     "freq", "Test","Missing","TRT01PN","ITTFL"),column_metadata   = col_metadata,tbltype = "Test"))


  table_metadata <- tibble::tribble(
    ~anbr, ~func,  ~tbltype,  ~df,~colvar,~rowvar,
    1,     "ERROR", "Test","cdisc_adsl","TRT01PN","ITTFL"
  )
  col_metadata <- tibble::tribble(
    ~tbltype,	~coldef,	  ~decode,	~span1,
    "Test",  "Xanomeline High Dose",  "High Dose", "Xanomeline")
  expect_error(generate_results(tibble::tribble(
    ~anbr, ~func,  ~tbltype,  ~df,~colvar,~rowvar,
    1,     "ERROR", "Test","cdisc_adsl","TRT01PN","ITTFL"),column_metadata   = col_metadata,tbltype = "Test"))

})

test_that("Test add_format", {

  basefreq <- freq(cdisc_adsl,
                   rowvar = "ITTFL",
                   colvar = "TRT01PN",
                   statlist = statlist(c("n")))


  expect_error(basefreq %>% add_format(tableby = ''),"tableby '' does not exist in df or is all NA for function: add_format")
  expect_error(basefreq %>% add_format(tableby = NA),"Argument tableby to function add_format should be type character")
  expect_error(basefreq %>% add_format(tableby = 'MISSING'),"tableby 'MISSING' does not exist in df or is all NA for function: add_format")
  expect_error(basefreq %>% add_format(groupby = ''),"groupby '' does not exist in df or is all NA for function: add_format")
  expect_error(basefreq %>% add_format(groupby = NA),"Argument groupby to function add_format should be type character")
  expect_error(basefreq %>% add_format(groupby = 'MISSING'),"groupby 'MISSING' does not exist in df or is all NA for function: add_format")

  expect_error(basefreq %>% dplyr::select(-row_type) %>% add_format(),"Reqiured fields anbr,  for function add_format")
})

test_that("Sparse dfs pass check_wcol works as expected with sparse dfs", {

  a_freq <- freq(cdisc_adsl,
                 rowvar = "ITTFL",
                 colvar = "TRT01PN") %>%
    select(-c("row_type", "group_level"))

  expect_equal(check_wcol(a_freq, c(1,1,1,1)), NULL)
})
