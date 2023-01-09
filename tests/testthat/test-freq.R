library(tibble)
library(haven)


test_that("tablebyvar tables can be binded together", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID") %>%
    mutate(am = factor(am),
           cyl = factor(cyl),
           gear = factor(gear))

  #TODO: Add test without rowtext/row_header

  mtcars3 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "cyl",
                  tablebyvar = "gear",
                  statlist = statlist(c("n","n (x.x%)")),
                  row_header = "Cars cylinders",
                  .keep = TRUE)
  mtcars4 <- freq(mtcars2,
                  colvar = "am",
                  rowvar = "vs",
                  tablebyvar = "gear",
                  statlist = statlist(c("n", "n/N (x.x%)")),
                  row_header = "Cars vs",
                  .keep = TRUE)

  expected <- tibble::tribble(
    ~label,             ~X0,           ~X1,         ~row_type, ~anbr, ~indentme, ~roworder, ~newrows, ~newpage,
    "Car Gears: 3",              NA,            NA, "TABLE_BY_HEADER",     0,         0,        1L,        1,        0,
    "Cars cylinders",              "",            "",          "HEADER",     1,         1,        1L,        1,        0,
    "4",             "1",           "-",           "VALUE",     1,         2,        2L,        0,        0,
    "6",             "2",           "-",           "VALUE",     1,         2,        3L,        0,        0,
    "8",            "12",           "-",           "VALUE",     1,         2,        4L,        0,        0,
    "4",      "1 (6.7%)",           "-",           "VALUE",     1,         2,        5L,        0,        0,
    "6",     "2 (13.3%)",           "-",           "VALUE",     1,         2,        6L,        0,        0,
    "8",    "12 (80.0%)",           "-",           "VALUE",     1,         2,        7L,        0,        0,
    "Cars vs",              "",            "",          "HEADER",     2,         1,        1L,        1,        0,
    "0",            "12",           "-",           "VALUE",     2,         2,        2L,        0,        0,
    "1",             "3",           "-",           "VALUE",     2,         2,        3L,        0,        0,
    "0", "12/15 (80.0%)",           "-",           "VALUE",     2,         2,        4L,        0,        0,
    "1",  "3/15 (20.0%)",           "-",           "VALUE",     2,         2,        5L,        0,        0,
    "Car Gears: 4",              NA,            NA, "TABLE_BY_HEADER",     0,         0,        1L,        0,        1,
    "Cars cylinders",              "",            "",          "HEADER",     1,         1,        1L,        1,        0,
    "4",             "2",           "6",           "VALUE",     1,         2,        2L,        0,        0,
    "6",             "2",           "2",           "VALUE",     1,         2,        3L,        0,        0,
    "8",             "0",           "0",           "VALUE",     1,         2,        4L,        0,        0,
    "4",     "2 (50.0%)",   "6 (75.0%)",           "VALUE",     1,         2,        5L,        0,        0,
    "6",     "2 (50.0%)",   "2 (25.0%)",           "VALUE",     1,         2,        6L,        0,        0,
    "8",             "0",           "0",           "VALUE",     1,         2,        7L,        0,        0,
    "Cars vs",              "",            "",          "HEADER",     2,         1,        1L,        1,        0,
    "0",             "0",           "2",           "VALUE",     2,         2,        2L,        0,        0,
    "1",             "4",           "6",           "VALUE",     2,         2,        3L,        0,        0,
    "0",           "0/4", "2/8 (25.0%)",           "VALUE",     2,         2,        4L,        0,        0,
    "1",  "4/4 (100.0%)", "6/8 (75.0%)",           "VALUE",     2,         2,        5L,        0,        0,
    "Car Gears: 5",              NA,            NA, "TABLE_BY_HEADER",     0,         0,        1L,        0,        1,
    "Cars cylinders",              "",            "",          "HEADER",     1,         1,        1L,        1,        0,
    "4",             "-",           "2",           "VALUE",     1,         2,        2L,        0,        0,
    "6",             "-",           "1",           "VALUE",     1,         2,        3L,        0,        0,
    "8",             "-",           "2",           "VALUE",     1,         2,        4L,        0,        0,
    "4",             "-",   "2 (40.0%)",           "VALUE",     1,         2,        5L,        0,        0,
    "6",             "-",   "1 (20.0%)",           "VALUE",     1,         2,        6L,        0,        0,
    "8",             "-",   "2 (40.0%)",           "VALUE",     1,         2,        7L,        0,        0,
    "Cars vs",              "",            "",          "HEADER",     2,         1,        1L,        1,        0,
    "0",             "-",           "4",           "VALUE",     2,         2,        2L,        0,        0,
    "1",             "-",           "1",           "VALUE",     2,         2,        3L,        0,        0,
    "0",             "-", "4/5 (80.0%)",           "VALUE",     2,         2,        4L,        0,        0,
    "1",             "-", "1/5 (20.0%)",           "VALUE",     2,         2,        5L,        0,        0
  )

  expect_equal(bind_table(mtcars3, mtcars4,colvar = "am",tablebyvar = "gear",prefix = "Car Gears: "),expected,ignore_attr = TRUE)

  expected <- tibble::tribble(
    ~label,             ~X0,           ~X1,         ~row_type, ~anbr, ~indentme, ~roworder, ~newrows, ~newpage,
    "Car Gears: 3",            "15",           "0", "TABLE_BY_HEADER",    NA,         0,        1L,        1,        0,
    "Cars cylinders",              "",            "",          "HEADER",     1,         1,        1L,        1,        0,
    "4",             "1",           "-",           "VALUE",     1,         2,        2L,        0,        0,
    "6",             "2",           "-",           "VALUE",     1,         2,        3L,        0,        0,
    "8",            "12",           "-",           "VALUE",     1,         2,        4L,        0,        0,
    "4",      "1 (6.7%)",           "-",           "VALUE",     1,         2,        5L,        0,        0,
    "6",     "2 (13.3%)",           "-",           "VALUE",     1,         2,        6L,        0,        0,
    "8",    "12 (80.0%)",           "-",           "VALUE",     1,         2,        7L,        0,        0,
    "Cars vs",              "",            "",          "HEADER",     2,         1,        1L,        1,        0,
    "0",            "12",           "-",           "VALUE",     2,         2,        2L,        0,        0,
    "1",             "3",           "-",           "VALUE",     2,         2,        3L,        0,        0,
    "0", "12/15 (80.0%)",           "-",           "VALUE",     2,         2,        4L,        0,        0,
    "1",  "3/15 (20.0%)",           "-",           "VALUE",     2,         2,        5L,        0,        0,
    "Car Gears: 4",             "4",           "8", "TABLE_BY_HEADER",    NA,         0,        1L,        0,        1,
    "Cars cylinders",           "",            "",          "HEADER",     1,         1,        1L,        1,        0,
    "4",             "2",           "6",           "VALUE",     1,         2,        2L,        0,        0,
    "6",             "2",           "2",           "VALUE",     1,         2,        3L,        0,        0,
    "8",             "0",           "0",           "VALUE",     1,         2,        4L,        0,        0,
    "4",     "2 (50.0%)",   "6 (75.0%)",           "VALUE",     1,         2,        5L,        0,        0,
    "6",     "2 (50.0%)",   "2 (25.0%)",           "VALUE",     1,         2,        6L,        0,        0,
    "8",             "0",           "0",           "VALUE",     1,         2,        7L,        0,        0,
    "Cars vs",              "",            "",          "HEADER",     2,         1,        1L,        1,        0,
    "0",             "0",           "2",           "VALUE",     2,         2,        2L,        0,        0,
    "1",             "4",           "6",           "VALUE",     2,         2,        3L,        0,        0,
    "0",           "0/4", "2/8 (25.0%)",           "VALUE",     2,         2,        4L,        0,        0,
    "1",  "4/4 (100.0%)", "6/8 (75.0%)",           "VALUE",     2,         2,        5L,        0,        0,
    "Car Gears: 5",             "0",           "5", "TABLE_BY_HEADER",    NA,         0,        1L,        0,        1,
    "Cars cylinders",              "",            "",          "HEADER",     1,         1,        1L,        1,        0,
    "4",             "-",           "2",           "VALUE",     1,         2,        2L,        0,        0,
    "6",             "-",           "1",           "VALUE",     1,         2,        3L,        0,        0,
    "8",             "-",           "2",           "VALUE",     1,         2,        4L,        0,        0,
    "4",             "-",   "2 (40.0%)",           "VALUE",     1,         2,        5L,        0,        0,
    "6",             "-",   "1 (20.0%)",           "VALUE",     1,         2,        6L,        0,        0,
    "8",             "-",   "2 (40.0%)",           "VALUE",     1,         2,        7L,        0,        0,
    "Cars vs",              "",            "",          "HEADER",     2,         1,        1L,        1,        0,
    "0",             "-",           "4",           "VALUE",     2,         2,        2L,        0,        0,
    "1",             "-",           "1",           "VALUE",     2,         2,        3L,        0,        0,
    "0",             "-", "4/5 (80.0%)",           "VALUE",     2,         2,        4L,        0,        0,
    "1",             "-", "1/5 (20.0%)",           "VALUE",     2,         2,        5L,        0,        0
  )

  expect_equal(bind_table(mtcars3, mtcars4,colvar = "am",tablebyvar = "gear",prefix = "Car Gears: ",add_count = TRUE),expected,ignore_attr = TRUE)

})

test_that("rowbyvar creates tables as expected", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID") %>%
    mutate(am = factor(am),
           cyl = factor(cyl),
           gear = factor(gear))

  expected <- tibble::tribble(
    ~label,          ~X0,         ~X1,    ~row_type, ~group_level, ~gear, ~gear_ord, ~cyl_ord,
    "3",           "",          "", "BY_HEADER1",            0,   "3",        1L,        0,
    "Cars cylinders",           "",          "",     "HEADER",            0,   "3",        1L,        0,
    "4",          "1",         "-",      "VALUE",            0,   "3",        1L,        1,
    "6",          "2",         "-",      "VALUE",            0,   "3",        1L,        2,
    "8",         "12",         "-",      "VALUE",            0,   "3",        1L,        3,
    "4",   "1 (6.7%)",         "-",      "VALUE",            0,   "3",        1L,        1,
    "6",  "2 (13.3%)",         "-",      "VALUE",            0,   "3",        1L,        2,
    "8", "12 (80.0%)",         "-",      "VALUE",            0,   "3",        1L,        3,
    "4",           "",          "", "BY_HEADER1",            0,   "4",        2L,        0,
    "Cars cylinders",           "",          "",     "HEADER",            0,   "4",        2L,        0,
    "4",          "2",         "6",      "VALUE",            0,   "4",        2L,        1,
    "6",          "2",         "2",      "VALUE",            0,   "4",        2L,        2,
    "8",          "0",         "0",      "VALUE",            0,   "4",        2L,        3,
    "4",  "2 (50.0%)", "6 (75.0%)",      "VALUE",            0,   "4",        2L,        1,
    "6",  "2 (50.0%)", "2 (25.0%)",      "VALUE",            0,   "4",        2L,        2,
    "8",          "0",         "0",      "VALUE",            0,   "4",        2L,        3,
    "5",           "",          "", "BY_HEADER1",            0,   "5",        3L,        0,
    "Cars cylinders",           "",          "",     "HEADER",            0,   "5",        3L,        0,
    "4",          "-",         "2",      "VALUE",            0,   "5",        3L,        1,
    "6",          "-",         "1",      "VALUE",            0,   "5",        3L,        2,
    "8",          "-",         "2",      "VALUE",            0,   "5",        3L,        3,
    "4",          "-", "2 (40.0%)",      "VALUE",            0,   "5",        3L,        1,
    "6",          "-", "1 (20.0%)",      "VALUE",            0,   "5",        3L,        2,
    "8",          "-", "2 (40.0%)",      "VALUE",            0,   "5",        3L,        3
  )

  expected$gear <- factor(expected$gear)

  expect_equal(freq(mtcars2,colvar = "am",rowvar = "cyl",rowbyvar = "gear",statlist = statlist(c("n","n (x.x%)")),row_header = "Cars cylinders",.ord = TRUE,.keep = TRUE)
               ,expected,ignore_attr = TRUE)

  expected <-   tibble::tribble(
    ~label,    ~Placebo, ~Xanomeline.High.Dose, ~Xanomeline.Low.Dose,    ~row_type, ~group_level,                              ~RACE, ~COMP8FL, ~RACE_ord, ~COMP8FL_ord, ~SEX_ord,
  "AMERICAN INDIAN OR ALASKA NATIVE",          "",                    "",                   "", "BY_HEADER1",            0, "AMERICAN INDIAN OR ALASKA NATIVE",       "",        1L,            0,        0,
  "N",          "",                    "",                   "", "BY_HEADER2",            1, "AMERICAN INDIAN OR ALASKA NATIVE",      "N",        1L,            1,        0,
  "F",         "-",                   "-",                  "-",      "VALUE",            1, "AMERICAN INDIAN OR ALASKA NATIVE",      "N",        1L,            1,        1,
  "M",         "-",                   "-",                  "-",      "VALUE",            1, "AMERICAN INDIAN OR ALASKA NATIVE",      "N",        1L,            1,        2,
  "Y",          "",                    "",                   "", "BY_HEADER2",            1, "AMERICAN INDIAN OR ALASKA NATIVE",      "Y",        1L,            2,        0,
  "F",         "-",                   "0",                  "-",      "VALUE",            1, "AMERICAN INDIAN OR ALASKA NATIVE",      "Y",        1L,            2,        1,
  "M",         "-",           "1 (100.0)",                  "-",      "VALUE",            1, "AMERICAN INDIAN OR ALASKA NATIVE",      "Y",        1L,            2,        2,
  "BLACK OR AFRICAN AMERICAN",          "",                    "",                   "", "BY_HEADER1",            0,        "BLACK OR AFRICAN AMERICAN",       "",        2L,            0,        0,
  "N",          "",                    "",                   "", "BY_HEADER2",            1,        "BLACK OR AFRICAN AMERICAN",      "N",        2L,            1,        0,
  "F",         "-",            "3 (75.0)",          "1 (100.0)",      "VALUE",            1,        "BLACK OR AFRICAN AMERICAN",      "N",        2L,            1,        1,
  "M",         "-",            "1 (25.0)",                  "0",      "VALUE",            1,        "BLACK OR AFRICAN AMERICAN",      "N",        2L,            1,        2,
  "Y",          "",                    "",                   "", "BY_HEADER2",            1,        "BLACK OR AFRICAN AMERICAN",      "Y",        2L,            2,        0,
  "F",  "5 (62.5)",            "3 (60.0)",          "5 (100.0)",      "VALUE",            1,        "BLACK OR AFRICAN AMERICAN",      "Y",        2L,            2,        1,
  "M",  "3 (37.5)",            "2 (40.0)",                  "0",      "VALUE",            1,        "BLACK OR AFRICAN AMERICAN",      "Y",        2L,            2,        2,
  "WHITE",      "",                    "",                   "", "BY_HEADER1",            0,                            "WHITE",       "",        3L,            0,        0,
  "N",          "",                    "",                   "", "BY_HEADER2",            1,                            "WHITE",      "N",        3L,            1,        0,
  "F", "10 (83.3)",           "11 (45.8)",          "12 (52.2)",      "VALUE",            1,                            "WHITE",      "N",        3L,            1,        1,
  "M",  "2 (16.7)",           "13 (54.2)",          "11 (47.8)",      "VALUE",            1,                            "WHITE",      "N",        3L,            1,        2,
  "Y",          "",                    "",                   "", "BY_HEADER2",            1,                            "WHITE",      "Y",        3L,            2,        0,
  "F", "38 (57.6)",           "23 (46.0)",          "32 (58.2)",      "VALUE",            1,                            "WHITE",      "Y",        3L,            2,        1,
  "M", "28 (42.4)",           "27 (54.0)",          "23 (41.8)",      "VALUE",            1,                            "WHITE",      "Y",        3L,            2,        2
)
  expected$RACE <- factor(expected$RACE)
  expected$COMP8FL <- factor(expected$COMP8FL)

  expect_equal(read_sas("test_data/adsl.sas7bdat") %>% freq(colvar = "TRT01A", rowvar = "SEX", rowbyvar = c("RACE", "COMP8FL"), .ord = TRUE, .keep = TRUE),
               expected,ignore_attr = TRUE)


})

test_that("missing values are displayed as expected", {
  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID") %>%
    mutate(am = factor(am),
           gear = factor(gear))
  mtcars2[1:5, "cyl"] <- NA

  expected <- tibble::tribble(
    ~label,          ~X0,          ~X1,    ~row_type, ~group_level, ~gear,
    "3",           "",           "", "BY_HEADER1",            0,   "3",
    "Cars cylinders",           "",           "",     "HEADER",            0,   "3",
    "4",          "1",          "-",      "VALUE",            0,   "3",
    "6",          "1",          "-",      "VALUE",            0,   "3",
    "8",         "11",          "-",      "VALUE",            0,   "3",
    "4",   "1 (7.7%)",          "-",      "VALUE",            0,   "3",
    "6",   "1 (7.7%)",          "-",      "VALUE",            0,   "3",
    "8", "11 (84.6%)",          "-",      "VALUE",            0,   "3",
    "4",           "",           "", "BY_HEADER1",            0,   "4",
    "Cars cylinders",           "",           "",     "HEADER",            0,   "4",
    "4",          "2",          "5",      "VALUE",            0,   "4",
    "6",          "2",          "0",      "VALUE",            0,   "4",
    "8",          "0",          "0",      "VALUE",            0,   "4",
    "4",  "2 (50.0%)", "5 (100.0%)",      "VALUE",            0,   "4",
    "6",  "2 (50.0%)",          "0",      "VALUE",            0,   "4",
    "8",          "0",          "0",      "VALUE",            0,   "4",
    "5",           "",           "", "BY_HEADER1",            0,   "5",
    "Cars cylinders",           "",           "",     "HEADER",            0,   "5",
    "4",          "-",          "2",      "VALUE",            0,   "5",
    "6",          "-",          "1",      "VALUE",            0,   "5",
    "8",          "-",          "2",      "VALUE",            0,   "5",
    "4",          "-",  "2 (40.0%)",      "VALUE",            0,   "5",
    "6",          "-",  "1 (20.0%)",      "VALUE",            0,   "5",
    "8",          "-",  "2 (40.0%)",      "VALUE",            0,   "5")

  expected$gear <- factor(expected$gear)

  expect_equal(freq(mtcars2,colvar = "am",rowvar = "cyl",rowbyvar = "gear",statlist = statlist(c("n","n (x.x%)")),row_header = "Cars cylinders"),
               expected,ignore_attr = TRUE)


  expected <- tibble::tribble(
    ~label,          ~X0,         ~X1,    ~row_type, ~group_level, ~gear,
    "3",           "",          "", "BY_HEADER1",            0,   "3",
    "Cars cylinders",           "",          "",     "HEADER",            0,   "3",
    "4",          "1",         "-",      "VALUE",            0,   "3",
    "6",          "1",         "-",      "VALUE",            0,   "3",
    "8",         "11",         "-",      "VALUE",            0,   "3",
    "Missing",          "2",         "-",      "VALUE",            0,   "3",
    "4",   "1 (6.7%)",         "-",      "VALUE",            0,   "3",
    "6",   "1 (6.7%)",         "-",      "VALUE",            0,   "3",
    "8", "11 (73.3%)",         "-",      "VALUE",            0,   "3",
    "Missing",  "2 (13.3%)",         "-",      "VALUE",            0,   "3",
    "4",           "",          "", "BY_HEADER1",            0,   "4",
    "Cars cylinders",           "",          "",     "HEADER",            0,   "4",
    "4",          "2",         "5",      "VALUE",            0,   "4",
    "6",          "2",         "0",      "VALUE",            0,   "4",
    "8",          "0",         "0",      "VALUE",            0,   "4",
    "Missing",          "0",         "3",      "VALUE",            0,   "4",
    "4",  "2 (50.0%)", "5 (62.5%)",      "VALUE",            0,   "4",
    "6",  "2 (50.0%)",         "0",      "VALUE",            0,   "4",
    "8",          "0",         "0",      "VALUE",            0,   "4",
    "Missing",          "0", "3 (37.5%)",      "VALUE",            0,   "4",
    "5",           "",          "", "BY_HEADER1",            0,   "5",
    "Cars cylinders",           "",          "",     "HEADER",            0,   "5",
    "4",          "-",         "2",      "VALUE",            0,   "5",
    "6",          "-",         "1",      "VALUE",            0,   "5",
    "8",          "-",         "2",      "VALUE",            0,   "5",
    "Missing",          "-",         "0",      "VALUE",            0,   "5",
    "4",          "-", "2 (40.0%)",      "VALUE",            0,   "5",
    "6",          "-", "1 (20.0%)",      "VALUE",            0,   "5",
    "8",          "-", "2 (40.0%)",      "VALUE",            0,   "5",
    "Missing",          "-",         "0",      "VALUE",            0,   "5"
  )

  expected$gear <- factor(expected$gear)

  # expect_equal(freq(mtcars2,colvar = "am",rowvar = "cyl",rowbyvar = "gear",statlist = statlist(c("n","n (x.x%)")),row_header = "Cars cylinders",display_missing = TRUE),
  #              expected,ignore_attr = TRUE)


  expected <- tibble::tribble(
    ~label,          ~X0,         ~X1,    ~row_type, ~group_level, ~gear,
    "3",           "",          "", "BY_HEADER1",            0,   "3",
    "Cars cylinders",           "",          "",     "HEADER",            0,   "3",
    "4",          "1",         "-",      "VALUE",            0,   "3",
    "6",          "1",         "-",      "VALUE",            0,   "3",
    "8",         "11",         "-",      "VALUE",            0,   "3",
    "Missing",          "2",         "-",      "VALUE",            0,   "3",
    "4",   "1 (6.7%)",         "-",      "VALUE",            0,   "3",
    "6",   "1 (6.7%)",         "-",      "VALUE",            0,   "3",
    "8", "11 (73.3%)",         "-",      "VALUE",            0,   "3",
    "Missing",  "2 (13.3%)",         "-",      "VALUE",            0,   "3",
    "4",           "",          "", "BY_HEADER1",            0,   "4",
    "Cars cylinders",           "",          "",     "HEADER",            0,   "4",
    "4",          "2",         "5",      "VALUE",            0,   "4",
    "6",          "2",         "0",      "VALUE",            0,   "4",
    "8",          "0",         "0",      "VALUE",            0,   "4",
    "Missing",          "0",         "3",      "VALUE",            0,   "4",
    "4",  "2 (50.0%)", "5 (62.5%)",      "VALUE",            0,   "4",
    "6",  "2 (50.0%)",         "0",      "VALUE",            0,   "4",
    "8",          "0",         "0",      "VALUE",            0,   "4",
    "Missing",          "0", "3 (37.5%)",      "VALUE",            0,   "4",
    "5",           "",          "", "BY_HEADER1",            0,   "5",
    "Cars cylinders",           "",          "",     "HEADER",            0,   "5",
    "4",          "-",         "2",      "VALUE",            0,   "5",
    "6",          "-",         "1",      "VALUE",            0,   "5",
    "8",          "-",         "2",      "VALUE",            0,   "5",
    "Missing",          "-",         "0",      "VALUE",            0,   "5",
    "4",          "-", "2 (40.0%)",      "VALUE",            0,   "5",
    "6",          "-", "1 (20.0%)",      "VALUE",            0,   "5",
    "8",          "-", "2 (40.0%)",      "VALUE",            0,   "5",
    "Missing",          "-",         "0",      "VALUE",            0,   "5"
  )

  expected$gear <- factor(expected$gear)

  # expect_equal(freq(mtcars2,colvar = "am",rowvar = "cyl",rowbyvar = "gear",statlist = statlist(c("n","n (x.x%)")),row_header = "Cars cylinders",display_missing = TRUE),
  #              expected,ignore_attr = TRUE)

expected <- tibble::tribble(
  ~label,          ~X0,         ~X1,    ~row_type, ~group_level, ~gear,
  "3",           "",          "", "BY_HEADER1",            0,   "3",
  "Cars cylinders",           "",          "",     "HEADER",            0,   "3",
  "4",          "1",         "-",      "VALUE",            0,   "3",
  "6",          "1",         "-",      "VALUE",            0,   "3",
  "8",         "11",         "-",      "VALUE",            0,   "3",
  "Missing",          "2",         "-",      "VALUE",            0,   "3",
  "4",   "1 (6.7%)",         "-",      "VALUE",            0,   "3",
  "6",   "1 (6.7%)",         "-",      "VALUE",            0,   "3",
  "8", "11 (73.3%)",         "-",      "VALUE",            0,   "3",
  "Missing",  "2 (13.3%)",         "-",      "VALUE",            0,   "3",
  "4",           "",          "", "BY_HEADER1",            0,   "4",
  "Cars cylinders",           "",          "",     "HEADER",            0,   "4",
  "4",          "2",         "5",      "VALUE",            0,   "4",
  "6",          "2",         "0",      "VALUE",            0,   "4",
  "8",          "0",         "0",      "VALUE",            0,   "4",
  "Missing",          "0",         "3",      "VALUE",            0,   "4",
  "4",  "2 (50.0%)", "5 (62.5%)",      "VALUE",            0,   "4",
  "6",  "2 (50.0%)",         "0",      "VALUE",            0,   "4",
  "8",          "0",         "0",      "VALUE",            0,   "4",
  "Missing",          "0", "3 (37.5%)",      "VALUE",            0,   "4",
  "5",           "",          "", "BY_HEADER1",            0,   "5",
  "Cars cylinders",           "",          "",     "HEADER",            0,   "5",
  "4",          "-",         "2",      "VALUE",            0,   "5",
  "6",          "-",         "1",      "VALUE",            0,   "5",
  "8",          "-",         "2",      "VALUE",            0,   "5",
  "Missing",          "-",         "0",      "VALUE",            0,   "5",
  "4",          "-", "2 (40.0%)",      "VALUE",            0,   "5",
  "6",          "-", "1 (20.0%)",      "VALUE",            0,   "5",
  "8",          "-", "2 (40.0%)",      "VALUE",            0,   "5",
  "Missing",          "-",         "0",      "VALUE",            0,   "5"
)

expected$gear <- factor(expected$gear)
# expect_equal(freq(mtcars2,colvar = "am",rowvar = "cyl",rowbyvar = "gear",statlist = statlist(c("n","n (x.x%)")),row_header = "Cars cylinders",display_missing = TRUE),
#              expected,ignore_attr = TRUE
#              )


})

test_that("default denoms_by is set properly", {

  mtcars2 <- mtcars
  mtcars2[1,1] <- 0

  expected <- tibble::tribble(
    ~label,             ~X0,           ~X1,    ~row_type, ~group_level, ~gear,
    "3",              "",            "", "BY_HEADER1",            0,   "3",
    "4",   "1/15 (6.7%)",           "-",      "VALUE",            0,   "3",
    "6",  "2/15 (13.3%)",           "-",      "VALUE",            0,   "3",
    "8", "12/15 (80.0%)",           "-",      "VALUE",            0,   "3",
    "4",              "",            "", "BY_HEADER1",            0,   "4",
    "4",   "2/4 (50.0%)", "6/8 (75.0%)",      "VALUE",            0,   "4",
    "6",   "2/4 (50.0%)", "2/8 (25.0%)",      "VALUE",            0,   "4",
    "8",           "0/4",         "0/8",      "VALUE",            0,   "4",
    "5",              "",            "", "BY_HEADER1",            0,   "5",
    "4",             "-", "2/5 (40.0%)",      "VALUE",            0,   "5",
    "6",             "-", "1/5 (20.0%)",      "VALUE",            0,   "5",
    "8",             "-", "2/5 (40.0%)",      "VALUE",            0,   "5"
  )

  expected$gear <- factor(expected$gear)
  expect_equal(freq(mtcars,colvar = "am",rowvar = "cyl",rowbyvar = "gear",statlist = statlist(c("n/N (x.x%)"), distinct = FALSE)),
               expected,ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~label,             ~X0,           ~X1,    ~row_type, ~group_level, ~gear,
    "3",              "",            "", "BY_HEADER1",            0,   "3",
    "4",   "1/15 (6.7%)",           "-",      "VALUE",            0,   "3",
    "6",  "2/15 (13.3%)",           "-",      "VALUE",            0,   "3",
    "8", "12/15 (80.0%)",           "-",      "VALUE",            0,   "3",
    "4",              "",            "", "BY_HEADER1",            0,   "4",
    "4",   "2/4 (50.0%)", "6/8 (75.0%)",      "VALUE",            0,   "4",
    "6",   "2/4 (50.0%)", "2/8 (25.0%)",      "VALUE",            0,   "4",
    "8",           "0/4",         "0/8",      "VALUE",            0,   "4",
    "5",              "",            "", "BY_HEADER1",            0,   "5",
    "4",             "-", "2/5 (40.0%)",      "VALUE",            0,   "5",
    "6",             "-", "1/5 (20.0%)",      "VALUE",            0,   "5",
    "8",             "-", "2/5 (40.0%)",      "VALUE",            0,   "5"
  )
  expected$gear <- factor(expected$gear)

  expect_equal(freq(mtcars,denom_df = mtcars2,colvar = "am",rowvar = "cyl",rowbyvar = "gear",statlist = statlist(c("n/N (x.x%)"), distinct = FALSE)),
                    expected,ignore_attr = TRUE)




})
