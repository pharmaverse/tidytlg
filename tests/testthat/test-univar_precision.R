test_that("univar computes by parameter precision", {
  expected <- tibble::tribble(
    ~am, ~vs,      ~label,              ~X3,              ~X4,              ~X5,    ~row_type, ~group_level,
    "0", "0",         "0",               "",               "",               "", "BY_HEADER1",            0,
    "0", "0",         "N",             "12",              "0",              "0",          "N",            0,
    "0", "0", "Mean (SD)", "4.104 (0.7683)",              "-",              "-",      "VALUE",            0,
    "0", "0",    "Median",          "3.810",              "-",              "-",      "VALUE",            0,
    "0", "0",     "Range",   "(3.44; 5.42)",         "(-; -)",         "(-; -)",      "VALUE",            0,
    "0", "0",  "IQ range", "(3.545; 4.660)",         "(-; -)",         "(-; -)",      "VALUE",            0,
    "0", "1",         "1",               "",               "",               "", "BY_HEADER1",            0,
    "0", "1",         "N",              "3",              "4",              "0",          "N",            0,
    "0", "1", "Mean (SD)", "3.047 (0.5184)", "3.305 (0.1567)",              "-",      "VALUE",            0,
    "0", "1",    "Median",          "3.215",          "3.315",              "-",      "VALUE",            0,
    "0", "1",     "Range",   "(2.47; 3.46)",   "(3.15; 3.44)",         "(-; -)",      "VALUE",            0,
    "0", "1",  "IQ range", "(2.465; 3.460)", "(3.170; 3.440)",         "(-; -)",      "VALUE",            0,
    "1", "0",         "0",               "",               "",               "", "BY_HEADER1",            0,
    "1", "0",         "N",              "0",              "2",              "4",          "N",            0,
    "1", "0", "Mean (SD)",              "-", "2.748 (0.1803)", "2.913 (0.6098)",      "VALUE",            0,
    "1", "0",    "Median",              "-",          "2.748",          "2.970",      "VALUE",            0,
    "1", "0",     "Range",         "(-; -)",   "(2.62; 2.88)",   "(2.14; 3.57)",      "VALUE",            0,
    "1", "0",  "IQ range",         "(-; -)", "(2.620; 2.875)", "(2.455; 3.370)",      "VALUE",            0,
    "1", "1",         "1",               "",               "",               "", "BY_HEADER1",            0,
    "1", "1",         "N",              "0",              "6",              "1",          "N",            0,
    "1", "1", "Mean (SD)",              "-", "2.114 (0.4129)",      "1.513 (-)",      "VALUE",            0,
    "1", "1",    "Median",              "-",          "2.068",          "1.513",      "VALUE",            0,
    "1", "1",     "Range",         "(-; -)",   "(1.62; 2.78)",   "(1.51; 1.51)",      "VALUE",            0,
    "1", "1",  "IQ range",         "(-; -)", "(1.835; 2.320)", "(1.513; 1.513)",      "VALUE",            0
  )

  expect_equal(
    mtcars %>%
      univar(
        colvar = "gear",
        rowvar = "wt",
        rowbyvar = "vs",
        tablebyvar = "am",
        precisionby = c("vs", "am"),
        decimal = 2
      ),
    expected,
    ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~am, ~vs,      ~label,                ~X3,                ~X4,                ~X5,    ~row_type, ~group_level,
    "0", "0",         "0",                 "",                 "",                 "", "BY_HEADER1",            0,
    "0", "0",         "N",               "12",                "0",                "0",          "N",            0,
    "0", "0", "Mean (SD)", "4.1041 (0.76831)",                "-",                "-",      "VALUE",            0,
    "0", "0",    "Median",           "3.8100",                "-",                "-",      "VALUE",            0,
    "0", "0",     "Range",   "(3.435; 5.424)",           "(-; -)",           "(-; -)",      "VALUE",            0,
    "0", "0",  "IQ range", "(3.5450; 4.6600)",           "(-; -)",           "(-; -)",      "VALUE",            0,
    "0", "1",         "1",                 "",                 "",                 "", "BY_HEADER1",            0,
    "0", "1",         "N",                "3",                "4",                "0",          "N",            0,
    "0", "1", "Mean (SD)", "3.0467 (0.51842)", "3.3050 (0.15674)",                "-",      "VALUE",            0,
    "0", "1",    "Median",           "3.2150",           "3.3150",                "-",      "VALUE",            0,
    "0", "1",     "Range",   "(2.465; 3.460)",   "(3.150; 3.440)",           "(-; -)",      "VALUE",            0,
    "0", "1",  "IQ range", "(2.4650; 3.4600)", "(3.1700; 3.4400)",           "(-; -)",      "VALUE",            0,
    "1", "0",         "0",                 "",                 "",                 "", "BY_HEADER1",            0,
    "1", "0",         "N",                "0",                "2",                "4",          "N",            0,
    "1", "0", "Mean (SD)",                "-", "2.7475 (0.18031)", "2.9125 (0.60983)",      "VALUE",            0,
    "1", "0",    "Median",                "-",           "2.7475",           "2.9700",      "VALUE",            0,
    "1", "0",     "Range",           "(-; -)",   "(2.620; 2.875)",   "(2.140; 3.570)",      "VALUE",            0,
    "1", "0",  "IQ range",           "(-; -)", "(2.6200; 2.8750)", "(2.4550; 3.3700)",      "VALUE",            0,
    "1", "1",         "1",                 "",                 "",                 "", "BY_HEADER1",            0,
    "1", "1",         "N",                "0",                "6",                "1",          "N",            0,
    "1", "1", "Mean (SD)",                "-", "2.1142 (0.41286)",       "1.5130 (-)",      "VALUE",            0,
    "1", "1",    "Median",                "-",           "2.0675",           "1.5130",      "VALUE",            0,
    "1", "1",     "Range",           "(-; -)",   "(1.615; 2.780)",   "(1.513; 1.513)",      "VALUE",            0,
    "1", "1",  "IQ range",           "(-; -)", "(1.8350; 2.3200)", "(1.5130; 1.5130)",      "VALUE",            0
  )

  expect_equal(
    mtcars %>%
      univar(
        colvar = "gear",
        rowvar = "wt",
        rowbyvar = "vs",
        tablebyvar = "am",
        precisionby = c("vs", "am"),
        decimal = Inf
      ),
    expected,
    ignore_attr = TRUE
  )
})
