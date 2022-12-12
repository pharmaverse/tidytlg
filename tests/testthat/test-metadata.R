library(tibble)


test_that("the metadata table method works as expected", {

  expected <- tibble::tribble(
    ~label,            ~col1,    ~col2,            ~col3, ~row_type,         ~func, ~anbr,          ~subset,       ~STUDYID, ~indentme, ~roworder, ~newrows, ~newpage,
    "Y",      "5 (100.0)",      "-",      "5 (100.0)",   "VALUE",        "freq",     1,               NA,             NA,         1,        1L,        0,        0,
    "N",              "5",      "0",              "5",       "N",      "univar",     2,   "TRT01P != ''",             NA,         1,        1L,        1,        0,
    "Mean (SD)", "69.60 (14.398)",      "-", "69.60 (14.398)",   "VALUE",      "univar",     2,   "TRT01P != ''",             NA,         2,        2L,        0,        0,
    "Median",          "64.00",      "-",          "64.00",   "VALUE",      "univar",     2,   "TRT01P != ''",             NA,         2,        3L,        0,        0,
    "Range",   "(52.0; 85.0)", "(-; -)",   "(52.0; 85.0)",   "VALUE",      "univar",     2,   "TRT01P != ''",             NA,         2,        4L,        0,        0,
    "IQ range", "(63.00; 84.00)", "(-; -)", "(63.00; 84.00)",   "VALUE",      "univar",     2,   "TRT01P != ''",             NA,         2,        5L,        0,        0,
    "CDISCPILOT01",      "5 (100.0)",      "-",      "5 (100.0)",  "NESTED", "nested_freq",     3,               NA, "CDISCPILOT01",         0,        1L,        1,        0,
    "F",       "2 (40.0)",      "-",       "2 (40.0)",  "NESTED", "nested_freq",     3,               NA, "CDISCPILOT01",         1,        2L,        0,        0,
    "M",       "3 (60.0)",      "-",       "3 (60.0)",  "NESTED", "nested_freq",     3,               NA, "CDISCPILOT01",         1,        3L,        0,        0
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

  # meta <- tibble(
  #   decode = c("Placebo", "Apalutamide", "Total"),
  #   coldef = c("Placebo", "Apalutamide", "Placebo+Apalutamide")
  # )

  tab <- generate_results(
    tibble(
      func = c("freq", "univar", "nested_freq"),
      colvar = "TRT01P",
      rowvar = c("SAFFL", "AGE", "STUDYID*SEX"),
      df = "cdisc_adsl",
      anbr = c(1,2,3),
      subset = "TRT01P != ''"
    ),
    column_metadata_file = tidytlg_metadata(test_path("test_data")),
    tbltype = "type1")

  expect_equal(tab,expected,ignore_attr = TRUE)
})

