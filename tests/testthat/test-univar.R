t1 <- univar(mtcars,
             colvar = "gear",
             rowvar = "wt")

t2 <- univar(mtcars,
             colvar = "gear",
             rowvar = "wt",
             decimal = 1,
             precisionon = "wt",
             statlist = statlist(c("N","SUM","MEAN","GeoMEAN","SD","SE","CV","GSD","GSE",
                                   "MEANSD","MEANSE","MEDIAN","MIN","MAX","RANGE","Q1","Q3",
                                   "IQRANGE","MEDRANGE","MEDIQRANGE","MEAN_CI","GeoMEAN_CI")))
t3 <- univar(mtcars,
             colvar = "gear",
             rowvar = "wt",
             tablebyvar = "vs",
             statlist = statlist(c("N","SUM","MEAN","GeoMEAN","SD","SE","CV","GSD","GSE",
                                   "MEANSD","MEANSE","MEDIAN","MIN","MAX","RANGE","Q1","Q3",
                                   "IQRANGE","MEDRANGE","MEDIQRANGE","MEAN_CI","GeoMEAN_CI")),
             precisionby = c("vs"),
             decimal = 3)

t4 <- univar(mtcars,
             colvar = "gear",
             rowvar = "wt",
             rowbyvar = "am",
             statlist = statlist(c("N","SUM","MEAN","GeoMEAN","SD","SE","CV","GSD","GSE",
                                   "MEANSD","MEANSE","MEDIAN","MIN","MAX","RANGE","Q1","Q3",
                                   "IQRANGE","MEDRANGE","MEDIQRANGE","MEAN_CI","GeoMEAN_CI")))

t5 <- univar(mtcars,
             colvar = "gear",
             rowvar = "wt",
             rowbyvar = "am",
             tablebyvar = "vs",
             statlist = statlist(c("N","SUM","MEAN","GeoMEAN","SD","SE","CV","GSD","GSE",
                                   "MEANSD","MEANSE","MEDIAN","MIN","MAX","RANGE","Q1","Q3",
                                   "IQRANGE","MEDRANGE","MEDIQRANGE","MEAN_CI","GeoMEAN_CI")))

t6 <- univar(mtcars,
             colvar = "gear",
             rowvar = "wt",
             wide = TRUE)

t7 <- univar(mtcars,
             colvar = "gear",
             rowvar = "wt",
             statlist = statlist(c("N","SUM","MEAN")),
             wide = TRUE)

t8 <- univar(mtcars,
             colvar = "gear",
             rowvar = "wt",
             tablebyvar = "vs",
             wide = TRUE)

t9 <- univar(mtcars,
             colvar = "gear",
             rowvar = "wt",
             rowbyvar = "am",
             wide = TRUE)

t10 <- univar(mtcars,
              colvar = "gear",
              rowvar = "wt",
              rowbyvar = "am",
              tablebyvar = "vs",
              wide = TRUE)

test_that("univar outputs are named as expected",{
  expect_setequal(names(t1), c("label","row_type","3","4","5", "group_level"))

  expect_setequal(names(t2), c("label","row_type","3","4","5", "group_level"))

  expect_setequal(names(t3), c("vs","label","row_type","3","4","5", "group_level"))

  expect_setequal(names(t4), c("label","3","4","5","row_type", "am", "group_level"))

  expect_setequal(names(t5), c("vs","label","row_type","3","4","5", "am", "group_level"))

  expect_setequal(names(t6), c("gear","Mean (SD)","Range","IQ range","Median","N","row_type", "group_level"))

  expect_setequal(names(t7), c("gear","Mean","N","Sum","row_type", "group_level"))

  expect_setequal(names(t8), c("gear", "vs", "Mean (SD)","Range","IQ range","Median","N","row_type", "group_level"))

  expect_setequal(names(t9), c("gear","am","Mean (SD)","Range","IQ range","Median","N","row_type", "group_level"))

  expect_setequal(names(t10), c("vs", "gear","am","Mean (SD)","Range","IQ range","Median","N","row_type", "group_level"))
})


test_that("univar computes statistics as expected",{
  expect_equal(t2, tibble::tribble(
    ~label,                      ~`3`,                  ~`4`,                  ~`5`,                  ~row_type, ~group_level,
    "N",                         "15",                  "12",                  "5",                   "N",      0,
    "Sum",                       "58.4",                "31.4",                "13.2",                "VALUE",  0,
    "Mean",                      "3.89",                "2.62",                "2.63",                "VALUE",  0,
    "Geometric Mean",            "3.81",                "2.54",                "2.52",                "VALUE",  0,
    "Std. Dev.",                 "0.833",               "0.633",               "0.819",               "VALUE",  0,
    "Std. Error",                "0.215",               "0.183",               "0.366",               "VALUE",  0,
    "CV (%)",                    "21.40",               "24.18",               "31.11",               "VALUE",  0,
    "Geometric Std. Dev.",       "1.231",               "1.292",               "1.409",               "VALUE",  0,
    "Geometric Std. Error",      "1.055",               "1.077",               "1.166",               "VALUE",  0,
    "Mean (SD)",                 "3.89 (0.833)",        "2.62 (0.633)",        "2.63 (0.819)",        "VALUE",  0,
    "Mean (SE)",                 "3.89 (0.215)",        "2.62 (0.183)",        "2.63 (0.366)",        "VALUE",  0,
    "Median",                    "3.73",                "2.70",                "2.77",                "VALUE",  0,
    "Minimum",                   "2.5",                 "1.6",                 "1.5",                 "VALUE",  0,
    "Maximum",                   "5.4",                 "3.4",                 "3.6",                 "VALUE",  0,
    "Range",                     "(2.5; 5.4)",          "(1.6; 3.4)",          "(1.5; 3.6)",          "VALUE",  0,
    "First quartile",            "3.44",                "2.07",                "2.14",                "VALUE",  0,
    "Third quartile",            "4.07",                "3.17",                "3.17",                "VALUE",  0,
    "IQ range",                  "(3.44; 4.07)",        "(2.07; 3.17)",        "(2.14; 3.17)",        "VALUE",  0,
    "Median (Range)",            "3.73 (2.5; 5.4)",     "2.70 (1.6; 3.4)",     "2.77 (1.5; 3.6)",     "VALUE",  0,
    "Median (Q1; Q3)",           "3.73 (3.44; 4.07)",   "2.70 (2.07; 3.17)",   "2.77 (2.14; 3.17)",   "VALUE",  0,
    "Mean (95% C.I.)",           "3.89 (3.431; 4.354)", "2.62 (2.215; 3.019)", "2.63 (1.616; 3.649)", "VALUE",  0,
    "Geometric Mean (95% C.I.)", "3.81 (3.399; 4.280)", "2.54 (2.161; 2.991)", "2.52 (1.646; 3.856)", "VALUE",  0,
  ), ignore_attr = TRUE)
})

test_that("rowtext works as expected with univar", {

  expected <- tibble::tribble(
    ~label,              ~X3,              ~X4,              ~X5, ~row_type, ~group_level,
    "Mpg Details",               "",               "",               "",  "HEADER",            0,
    "N",             "15",             "12",              "5",       "N",            0,
    "Mean (SD)",  "16.11 (3.372)",  "24.53 (5.277)",  "21.38 (6.659)",   "VALUE",            0,
    "Median",          "15.50",          "22.80",          "19.70",   "VALUE",            0,
    "Range",   "(10.4; 21.5)",   "(17.8; 33.9)",   "(15.0; 30.4)",   "VALUE",            0,
    "IQ range", "(14.30; 18.70)", "(21.00; 28.85)", "(15.80; 26.00)",   "VALUE",            0
  )

  expect_equal(
      mtcars %>%
        univar(
          colvar = "gear",
          row_header = "Mpg Details",
          rowvar = "mpg"
        ),
      expected,
      ignore_attr = TRUE

  )

  expected <- tibble::tribble(
    ~am,        ~label,              ~X3,              ~X4,              ~X5,    ~row_type, ~group_level,
    "0",           "0",               "",               "",               "", "BY_HEADER1",            0,
    "0", "Mpg Details",               "",               "",               "",     "HEADER",            0,
    "0",           "N",             "15",              "4",              "0",          "N",            0,
    "0",   "Mean (SD)",  "16.11 (3.372)",  "21.05 (3.070)",              "-",      "VALUE",            0,
    "0",      "Median",          "15.50",          "21.00",              "-",      "VALUE",            0,
    "0",       "Range",   "(10.4; 21.5)",   "(17.8; 24.4)",         "(-; -)",      "VALUE",            0,
    "0",    "IQ range", "(14.30; 18.70)", "(18.50; 23.60)",         "(-; -)",      "VALUE",            0,
    "1",           "1",               "",               "",               "", "BY_HEADER1",            0,
    "1", "Mpg Details",               "",               "",               "",     "HEADER",            0,
    "1",           "N",              "0",              "8",              "5",          "N",            0,
    "1",   "Mean (SD)",              "-",  "26.28 (5.414)",  "21.38 (6.659)",      "VALUE",            0,
    "1",      "Median",              "-",          "25.05",          "19.70",      "VALUE",            0,
    "1",       "Range",         "(-; -)",   "(21.0; 33.9)",   "(15.0; 30.4)",      "VALUE",            0,
    "1",    "IQ range",         "(-; -)", "(21.20; 31.40)", "(15.80; 26.00)",      "VALUE",            0
  )

  expect_equal(
      mtcars %>%
        univar(
          colvar = "gear",
          row_header = "Mpg Details",
          rowvar = "mpg",
          rowbyvar = "am"
        ),
      expected,
      ignore_attr = TRUE

  )

  expected <- tibble::tribble(
    ~label,     ~X3,     ~X4,     ~X5, ~row_type, ~group_level,
    "Mpg mean", "16.11", "24.53", "21.38",   "VALUE",            0
  )

  expect_equal(

      mtcars %>%
        univar(
          colvar = "gear",
          rowtext = "Mpg mean",
          rowvar = "mpg",
          statlist = statlist("MEAN")
        ),
      expected,
      ignore_attr = TRUE
  )
})

test_that("univar can handle sparse data as expected", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID") %>%
    mutate(across(.col = -any_of("mpg"), .fns = as.factor)) %>%
    filter(FALSE)

  expected <- tibble::tribble(
    ~label,      ~X4,      ~X6,      ~X8, ~row_type, ~group_level,
    "N",      "0",      "0",      "0",       "N",            0,
    "Mean (SD)",      "-",      "-",      "-",   "VALUE",            0,
    "Median",      "-",      "-",      "-",   "VALUE",            0,
    "Range", "(-; -)", "(-; -)", "(-; -)",   "VALUE",            0,
    "IQ range", "(-; -)", "(-; -)", "(-; -)",   "VALUE",            0
  )

  expect_equal(
      univar(
        mtcars2,
        colvar = "cyl",
        rowvar = "mpg"
    ),
    expected,
    ignore_attr = TRUE
  )
})

test_that("MEANSE statlist works as expected", {

  t1 <- mtcars %>%
    univar(
      colvar = "cyl",
      rowvar = "mpg",
      statlist = statlist(c("MEANSE"))
    )
  expected <- tibble::tribble(
    ~label,             ~X4,             ~X6,             ~X8, ~row_type, ~group_level,
    "Mean (SE)", "26.66 (1.360)", "19.74 (0.549)", "15.10 (0.684)",   "VALUE",            0
  )

  expect_equal(t1,expected,ignore_attr = TRUE)
})

