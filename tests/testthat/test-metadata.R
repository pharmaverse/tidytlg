library(tibble)


test_that("the metadata table method works as expected", {

  expected <- tibble::tribble(
            ~label,            ~col1,            ~col2,            ~col3,            ~col4, ~row_type,         ~func, ~anbr,          ~subset,       ~STUDYID, ~indentme, ~roworder, ~newrows, ~newpage,
               "Y",      "5 (100.0)",      "5 (100.0)",      "5 (100.0)",     "15 (100.0)",   "VALUE",        "freq",     1,               NA,             NA,         1,        1L,        0,        0,
               "N",              "5",              "5",              "5",             "15",       "N",      "univar",     2,   "TRT01P != ''",             NA,         1,        1L,        1,        0,
       "Mean (SD)", "69.60 (14.398)",  "75.60 (6.731)",  "72.20 (9.230)", "72.47 (10.148)",   "VALUE",      "univar",     2,   "TRT01P != ''",             NA,         2,        2L,        0,        0,
          "Median",          "64.00",          "74.00",          "75.00",          "74.00",   "VALUE",      "univar",     2,   "TRT01P != ''",             NA,         2,        3L,        0,        0,
           "Range",   "(52.0; 85.0)",   "(68.0; 84.0)",   "(57.0; 81.0)",   "(52.0; 85.0)",   "VALUE",      "univar",     2,   "TRT01P != ''",             NA,         2,        4L,        0,        0,
        "IQ range", "(63.00; 84.00)", "(71.00; 81.00)", "(71.00; 77.00)", "(64.00; 81.00)",   "VALUE",      "univar",     2,   "TRT01P != ''",             NA,         2,        5L,        0,        0,
    "CDISCPILOT01",      "5 (100.0)",      "5 (100.0)",      "5 (100.0)",     "15 (100.0)",  "NESTED", "nested_freq",     3,               NA, "CDISCPILOT01",         0,        1L,        1,        0,
               "F",       "2 (40.0)",       "1 (20.0)",       "3 (60.0)",       "6 (40.0)",  "NESTED", "nested_freq",     3,               NA, "CDISCPILOT01",         1,        2L,        0,        0,
               "M",       "3 (60.0)",       "4 (80.0)",       "2 (40.0)",       "9 (60.0)",  "NESTED", "nested_freq",     3,               NA, "CDISCPILOT01",         1,        3L,        0,        0
    )
expected$STUDYID <- factor(expected$STUDYID)

  tables <- tibble(
    func = c("freq", "univar", "nested_freq"),
    colvar = "TRT01P",
    rowvar = c("SAFFL", "AGE", "STUDYID*SEX"),
    df = "cdisc_adsl",
    anbr = c(1,2,3),
    subset = "TRT01P != ''"
  )

  tab <- generate_results(
    tibble(
      func = c("freq", "univar", "nested_freq"),
      colvar = "TRT01PN",
      rowvar = c("SAFFL", "AGE", "STUDYID*SEX"),
      df = "cdisc_adsl",
      anbr = c(1,2,3),
      subset = "TRT01P != ''"
    ),
    column_metadata_file = tidytlg_metadata(test_path("test_data")),
    tbltype = "type1")

  expect_equal(tab,expected,ignore_attr = TRUE)
})

