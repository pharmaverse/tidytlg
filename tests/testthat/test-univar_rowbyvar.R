test_that("rowbyvar works as expected for univar", {
  expected <- tibble::tribble(
    ~cyl, ~label, ~X3, ~X4, ~X5, ~row_type, ~group_level,
    "4", "4", "", "", "", "BY_HEADER1", 0,
    "4", "N", "1", "8", "2", "N", 0,
    "4", "Mean (SD)", "21.50 (-)", "26.93 (4.807)", "28.20 (3.111)", "VALUE", 0,
    "4", "Median", "21.50", "25.85", "28.20", "VALUE", 0,
    "4", "Range", "(21.5; 21.5)", "(21.4; 33.9)", "(26.0; 30.4)", "VALUE", 0,
    "4", "IQ range", "(21.50; 21.50)", "(22.80; 31.40)", "(26.00; 30.40)", "VALUE", 0,
    "6", "6", "", "", "", "BY_HEADER1", 0,
    "6", "N", "2", "4", "1", "N", 0,
    "6", "Mean (SD)", "19.75 (2.333)", "19.75 (1.552)", "19.70 (-)", "VALUE", 0,
    "6", "Median", "19.75", "20.10", "19.70", "VALUE", 0,
    "6", "Range", "(18.1; 21.4)", "(17.8; 21.0)", "(19.7; 19.7)", "VALUE", 0,
    "6", "IQ range", "(18.10; 21.40)", "(18.50; 21.00)", "(19.70; 19.70)", "VALUE", 0,
    "8", "8", "", "", "", "BY_HEADER1", 0,
    "8", "N", "12", "0", "2", "N", 0,
    "8", "Mean (SD)", "15.05 (2.774)", "-", "15.40 (0.566)", "VALUE", 0,
    "8", "Median", "15.20", "-", "15.40", "VALUE", 0,
    "8", "Range", "(10.4; 19.2)", "(-; -)", "(15.0; 15.8)", "VALUE", 0,
    "8", "IQ range", "(13.80; 16.85)", "(-; -)", "(15.00; 15.80)", "VALUE", 0
  )

  expect_equal(
    mtcars %>%
      univar(
        colvar = "gear",
        rowvar = "mpg",
        rowbyvar = "cyl"
      ),
    expected,
    ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~cyl, ~gear, ~N, ~Mean..SD., ~Median, ~Range, ~IQ.range, ~row_type, ~group_level,
    "4", "4", "", "", "", "", "", "BY_HEADER1", 0,
    "4", "3", "1", "21.50 (-)", "21.50", "(21.5; 21.5)", "(21.50; 21.50)", "VALUE", 0,
    "4", "4", "8", "26.93 (4.807)", "25.85", "(21.4; 33.9)", "(22.80; 31.40)", "VALUE", 0,
    "4", "5", "2", "28.20 (3.111)", "28.20", "(26.0; 30.4)", "(26.00; 30.40)", "VALUE", 0,
    "6", "6", "", "", "", "", "", "BY_HEADER1", 0,
    "6", "3", "2", "19.75 (2.333)", "19.75", "(18.1; 21.4)", "(18.10; 21.40)", "VALUE", 0,
    "6", "4", "4", "19.75 (1.552)", "20.10", "(17.8; 21.0)", "(18.50; 21.00)", "VALUE", 0,
    "6", "5", "1", "19.70 (-)", "19.70", "(19.7; 19.7)", "(19.70; 19.70)", "VALUE", 0,
    "8", "8", "", "", "", "", "", "BY_HEADER1", 0,
    "8", "3", "12", "15.05 (2.774)", "15.20", "(10.4; 19.2)", "(13.80; 16.85)", "VALUE", 0,
    "8", "4", "0", "-", "-", "(-; -)", "(-; -)", "VALUE", 0,
    "8", "5", "2", "15.40 (0.566)", "15.40", "(15.0; 15.8)", "(15.00; 15.80)", "VALUE", 0
  )

  expect_equal(
    mtcars %>%
      univar(
        colvar = "gear",
        rowvar = "mpg",
        rowbyvar = "cyl",
        wide = TRUE
      ),
    expected,
    ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~vs, ~am,      ~label,              ~X3,              ~X4,              ~X5,    ~row_type, ~group_level,
    "0",  "",         "0",               "",               "",               "", "BY_HEADER1",            0,
    "0", "0",         "0",               "",               "",               "", "BY_HEADER2",            1,
    "0", "0",         "N",             "12",              "0",              "0",          "N",            1,
    "0", "0", "Mean (SD)",  "15.05 (2.774)",              "-",              "-",      "VALUE",            1,
    "0", "0",    "Median",          "15.20",              "-",              "-",      "VALUE",            1,
    "0", "0",     "Range",   "(10.4; 19.2)",         "(-; -)",         "(-; -)",      "VALUE",            1,
    "0", "0",  "IQ range", "(13.80; 16.85)",         "(-; -)",         "(-; -)",      "VALUE",            1,
    "0", "1",         "1",               "",               "",               "", "BY_HEADER2",            1,
    "0", "1",         "N",              "0",              "2",              "4",          "N",            1,
    "0", "1", "Mean (SD)",              "-",  "21.00 (0.000)",  "19.13 (5.022)",      "VALUE",            1,
    "0", "1",    "Median",              "-",          "21.00",          "17.75",      "VALUE",            1,
    "0", "1",     "Range",         "(-; -)",   "(21.0; 21.0)",   "(15.0; 26.0)",      "VALUE",            1,
    "0", "1",  "IQ range",         "(-; -)", "(21.00; 21.00)", "(15.40; 22.85)",      "VALUE",            1,
    "1",  "",         "1",               "",               "",               "", "BY_HEADER1",            0,
    "1", "0",         "0",               "",               "",               "", "BY_HEADER2",            1,
    "1", "0",         "N",              "3",              "4",              "0",          "N",            1,
    "1", "0", "Mean (SD)",  "20.33 (1.935)",  "21.05 (3.070)",              "-",      "VALUE",            1,
    "1", "0",    "Median",          "21.40",          "21.00",              "-",      "VALUE",            1,
    "1", "0",     "Range",   "(18.1; 21.5)",   "(17.8; 24.4)",         "(-; -)",      "VALUE",            1,
    "1", "0",  "IQ range", "(18.10; 21.50)", "(18.50; 23.60)",         "(-; -)",      "VALUE",            1,
    "1", "1",         "1",               "",               "",               "", "BY_HEADER2",            1,
    "1", "1",         "N",              "0",              "6",              "1",          "N",            1,
    "1", "1", "Mean (SD)",              "-",  "28.03 (5.119)",      "30.40 (-)",      "VALUE",            1,
    "1", "1",    "Median",              "-",          "28.85",          "30.40",      "VALUE",            1,
    "1", "1",     "Range",         "(-; -)",   "(21.4; 33.9)",   "(30.4; 30.4)",      "VALUE",            1,
    "1", "1",  "IQ range",         "(-; -)", "(22.80; 32.40)", "(30.40; 30.40)",      "VALUE",            1
  )


  expect_equal(
    mtcars %>%
      univar(
        colvar = "gear",
        rowvar = "mpg",
        rowbyvar = c("vs", "am")
      ),
    expected,
    ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~vs, ~am, ~gear,   ~N,      ~Mean..SD., ~Median,         ~Range,        ~IQ.range,    ~row_type, ~group_level,
    "0",  "",   "0",   "",              "",      "",             "",               "", "BY_HEADER1",            0,
    "0", "0",   "0",   "",              "",      "",             "",               "", "BY_HEADER2",            1,
    "0", "0",   "3", "12", "15.05 (2.774)", "15.20", "(10.4; 19.2)", "(13.80; 16.85)",      "VALUE",            1,
    "0", "0",   "4",  "0",             "-",     "-",       "(-; -)",         "(-; -)",      "VALUE",            1,
    "0", "0",   "5",  "0",             "-",     "-",       "(-; -)",         "(-; -)",      "VALUE",            1,
    "0", "1",   "1",   "",              "",      "",             "",               "", "BY_HEADER2",            1,
    "0", "1",   "3",  "0",             "-",     "-",       "(-; -)",         "(-; -)",      "VALUE",            1,
    "0", "1",   "4",  "2", "21.00 (0.000)", "21.00", "(21.0; 21.0)", "(21.00; 21.00)",      "VALUE",            1,
    "0", "1",   "5",  "4", "19.13 (5.022)", "17.75", "(15.0; 26.0)", "(15.40; 22.85)",      "VALUE",            1,
    "1",  "",   "1",   "",              "",      "",             "",               "", "BY_HEADER1",            0,
    "1", "0",   "0",   "",              "",      "",             "",               "", "BY_HEADER2",            1,
    "1", "0",   "3",  "3", "20.33 (1.935)", "21.40", "(18.1; 21.5)", "(18.10; 21.50)",      "VALUE",            1,
    "1", "0",   "4",  "4", "21.05 (3.070)", "21.00", "(17.8; 24.4)", "(18.50; 23.60)",      "VALUE",            1,
    "1", "0",   "5",  "0",             "-",     "-",       "(-; -)",         "(-; -)",      "VALUE",            1,
    "1", "1",   "1",   "",              "",      "",             "",               "", "BY_HEADER2",            1,
    "1", "1",   "3",  "0",             "-",     "-",       "(-; -)",         "(-; -)",      "VALUE",            1,
    "1", "1",   "4",  "6", "28.03 (5.119)", "28.85", "(21.4; 33.9)", "(22.80; 32.40)",      "VALUE",            1,
    "1", "1",   "5",  "1",     "30.40 (-)", "30.40", "(30.4; 30.4)", "(30.40; 30.40)",      "VALUE",            1
  )

  expect_equal(
    mtcars %>%
      univar(
        colvar = "gear",
        rowvar = "mpg",
        rowbyvar = c("vs", "am"),
        wide = TRUE
      ),
    expected,
    ignore_attr = TRUE
  )
})
