library(dplyr)
library(haven)
library(tibble)

adsl <- cdisc_adsl %>%
  filter(TRT01P != "")

adae <- read_sas("test_data/adae.sas7bdat")

test_that("Row sorting works as expected with and without a descending_by argument", {

  expected <- tibble::tribble(
    ~label,    ~Placebo, ~Xanomeline.High.Dose, ~Xanomeline.Low.Dose, ~row_type, ~nested_level, ~group_level, ~SEX,
    "F", "2 (40.0%)",           "3 (60.0%)",          "1 (20.0%)",  "NESTED",             0,            0,  "F",
    "WHITE", "2 (40.0%)",           "3 (60.0%)",          "1 (20.0%)",  "NESTED",             1,            0,  "F",
    "M", "3 (60.0%)",           "2 (40.0%)",          "4 (80.0%)",  "NESTED",             0,            0,  "M",
    "WHITE", "3 (60.0%)",           "2 (40.0%)",          "4 (80.0%)",  "NESTED",             1,            0,  "M"
  )
  expected$SEX <- factor(expected$SEX)

  expect_equal(nested_freq(adsl,rowvar = "SEX*RACE",colvar = "TRT01P",statlist = statlist("n (x.x%)")),expected,ignore_attr = TRUE)

  expected <- tibble::tribble(
    ~label,    ~Placebo, ~Xanomeline.High.Dose, ~Xanomeline.Low.Dose, ~row_type, ~nested_level, ~group_level, ~SEX,
    "M", "3 (60.0%)",           "2 (40.0%)",          "4 (80.0%)",  "NESTED",             0,            0,  "M",
    "WHITE", "3 (60.0%)",           "2 (40.0%)",          "4 (80.0%)",  "NESTED",             1,            0,  "M",
    "F", "2 (40.0%)",           "3 (60.0%)",          "1 (20.0%)",  "NESTED",             0,            0,  "F",
    "WHITE", "2 (40.0%)",           "3 (60.0%)",          "1 (20.0%)",  "NESTED",             1,            0,  "F"
  )
  expected$SEX <- factor(expected$SEX)

  expect_equal(nested_freq(adsl,
                  rowvar = "SEX*RACE",
                  colvar = "TRT01P",
                  statlist = statlist("n (x.x%)"),
                  descending_by = "Placebo"),
               expected,
               ignore_attr = TRUE)

})

test_that("Three level nested sorting works as expected", {

  adsl <- read_sas("test_data/adsl.sas7bdat") %>%
    filter(SAFFL == "Y") %>%
    mutate(REGION = case_when(SITEGR1 %in% c("716", "718") ~ "EU",
                              TRUE ~ "NE"),
           COUNTRY = case_when(SITEGR1 == "900" ~ "Canada",
                               SITEGR1 %in% c("716", "718") ~ "BEL",
                               TRUE ~ "USA")) %>%
    mutate(TRT01AN = factor(TRT01AN))

  expected <- tibble::tribble(
    ~label,          ~X0,         ~X54,         ~X81, ~row_type, ~nested_level, ~group_level, ~SEX,                              ~RACE,
    "F", "53 (61.6%)", "50 (59.5%)", "40 (47.6%)",  "NESTED",             0,            1,  "F",                                "F",
    "BLACK OR AFRICAN AMERICAN",   "5 (5.8%)",   "6 (7.1%)",   "6 (7.1%)",  "NESTED",             1,            1,  "F",        "BLACK OR AFRICAN AMERICAN",
    "701",   "2 (2.3%)",   "1 (1.2%)",          "0",  "NESTED",             2,            1,  "F",        "BLACK OR AFRICAN AMERICAN",
    "703",          "0",   "2 (2.4%)",   "1 (1.2%)",  "NESTED",             2,            1,  "F",        "BLACK OR AFRICAN AMERICAN",
    "705",   "1 (1.2%)",   "1 (1.2%)",   "2 (2.4%)",  "NESTED",             2,            1,  "F",        "BLACK OR AFRICAN AMERICAN",
    "706",   "1 (1.2%)",          "0",          "0",  "NESTED",             2,            1,  "F",        "BLACK OR AFRICAN AMERICAN",
    "708",   "1 (1.2%)",   "1 (1.2%)",   "1 (1.2%)",  "NESTED",             2,            1,  "F",        "BLACK OR AFRICAN AMERICAN",
    "710",          "0",          "0",   "1 (1.2%)",  "NESTED",             2,            1,  "F",        "BLACK OR AFRICAN AMERICAN",
    "713",          "0",   "1 (1.2%)",          "0",  "NESTED",             2,            1,  "F",        "BLACK OR AFRICAN AMERICAN",
    "718",          "0",          "0",   "1 (1.2%)",  "NESTED",             2,            1,  "F",        "BLACK OR AFRICAN AMERICAN",
    "WHITE", "48 (55.8%)", "44 (52.4%)", "34 (40.5%)",  "NESTED",             1,            1,  "F",                            "WHITE",
    "701",   "5 (5.8%)",   "4 (4.8%)",   "6 (7.1%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "702",          "0",   "1 (1.2%)",          "0",  "NESTED",             2,            1,  "F",                            "WHITE",
    "703",   "4 (4.7%)",   "2 (2.4%)",   "3 (3.6%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "704",   "4 (4.7%)",   "5 (6.0%)",   "2 (2.4%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "705",   "4 (4.7%)",   "3 (3.6%)",   "2 (2.4%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "706",          "0",   "1 (1.2%)",   "1 (1.2%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "707",          "0",   "1 (1.2%)",          "0",  "NESTED",             2,            1,  "F",                            "WHITE",
    "708",   "5 (5.8%)",   "3 (3.6%)",   "4 (4.8%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "709",   "4 (4.7%)",   "5 (6.0%)",   "2 (2.4%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "710",   "8 (9.3%)",   "6 (7.1%)",   "3 (3.6%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "711",          "0",   "1 (1.2%)",   "2 (2.4%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "713",   "1 (1.2%)",   "2 (2.4%)",   "1 (1.2%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "714",   "2 (2.3%)",   "1 (1.2%)",          "0",  "NESTED",             2,            1,  "F",                            "WHITE",
    "715",   "3 (3.5%)",   "1 (1.2%)",   "1 (1.2%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "716",   "5 (5.8%)",   "2 (2.4%)",   "6 (7.1%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "717",   "2 (2.3%)",   "2 (2.4%)",          "0",  "NESTED",             2,            1,  "F",                            "WHITE",
    "718",   "1 (1.2%)",   "4 (4.8%)",   "1 (1.2%)",  "NESTED",             2,            1,  "F",                            "WHITE",
    "M", "33 (38.4%)", "34 (40.5%)", "44 (52.4%)",  "NESTED",             0,            1,  "M",                                "M",
    "AMERICAN INDIAN OR ALASKA NATIVE",          "0",          "0",   "1 (1.2%)",  "NESTED",             1,            1,  "M", "AMERICAN INDIAN OR ALASKA NATIVE",
    "701",          "0",          "0",   "1 (1.2%)",  "NESTED",             2,            1,  "M", "AMERICAN INDIAN OR ALASKA NATIVE",
    "BLACK OR AFRICAN AMERICAN",   "3 (3.5%)",          "0",   "3 (3.6%)",  "NESTED",             1,            1,  "M",        "BLACK OR AFRICAN AMERICAN",
    "708",   "2 (2.3%)",          "0",          "0",  "NESTED",             2,            1,  "M",        "BLACK OR AFRICAN AMERICAN",
    "710",          "0",          "0",   "1 (1.2%)",  "NESTED",             2,            1,  "M",        "BLACK OR AFRICAN AMERICAN",
    "711",   "1 (1.2%)",          "0",          "0",  "NESTED",             2,            1,  "M",        "BLACK OR AFRICAN AMERICAN",
    "714",          "0",          "0",   "1 (1.2%)",  "NESTED",             2,            1,  "M",        "BLACK OR AFRICAN AMERICAN",
    "718",          "0",          "0",   "1 (1.2%)",  "NESTED",             2,            1,  "M",        "BLACK OR AFRICAN AMERICAN",
    "WHITE", "30 (34.9%)", "34 (40.5%)", "40 (47.6%)",  "NESTED",             1,            1,  "M",                            "WHITE",
    "701",   "7 (8.1%)",   "8 (9.5%)",   "7 (8.3%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "703",   "2 (2.3%)",   "2 (2.4%)",   "2 (2.4%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "704",   "5 (5.8%)",   "3 (3.6%)",   "6 (7.1%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "705",          "0",   "1 (1.2%)",   "2 (2.4%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "707",   "1 (1.2%)",          "0",          "0",  "NESTED",             2,            1,  "M",                            "WHITE",
    "708",   "1 (1.2%)",   "4 (4.8%)",   "3 (3.6%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "709",   "3 (3.5%)",   "2 (2.4%)",   "5 (6.0%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "710",   "3 (3.5%)",   "4 (4.8%)",   "5 (6.0%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "713",   "2 (2.3%)",          "0",   "2 (2.4%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "714",          "0",   "1 (1.2%)",   "1 (1.2%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "715",          "0",   "2 (2.4%)",   "1 (1.2%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "716",   "3 (3.5%)",   "6 (7.1%)",   "2 (2.4%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "717",          "0",          "0",   "3 (3.6%)",  "NESTED",             2,            1,  "M",                            "WHITE",
    "718",   "3 (3.5%)",   "1 (1.2%)",   "1 (1.2%)",  "NESTED",             2,            1,  "M",                            "WHITE"
  )
  expected$SEX <- factor(expected$SEX)


  expect_equal(

      nested_freq(adsl,
                  colvar = "TRT01AN",
                  rowvar = "SEX*RACE*SITEID",
                  statlist = statlist("n (x.x%)", denoms_by = c("TRT01AN"))) %>% mutate(RACE = as.character(RACE)),
      expected,
      ignore_attr = TRUE
  )

})

test_that("nested values can span over higher levels", {
  mtcars2 <- mtcars
  mtcars2$grp <- rep(c("A", "B"), 16)

  expected <- tibble::tribble(
    ~label, ~X4, ~X6, ~X8, ~row_type, ~nested_level, ~group_level, ~grp,
    "A", "5", "2", "9",  "NESTED",             0,            0,  "A",
    "0", "1", "1", "9",  "NESTED",             1,            0,  "A",
    "1", "4", "1", "0",  "NESTED",             1,            0,  "A",
    "B", "6", "5", "5",  "NESTED",             0,            0,  "B",
    "0", "0", "2", "5",  "NESTED",             1,            0,  "B",
    "1", "6", "3", "0",  "NESTED",             1,            0,  "B"
  )
  expected$grp <- factor(expected$grp)
  expect_equal(

      nested_freq(
        mtcars2,
        colvar= "cyl",
        rowvar = "grp*vs",
        statlist = statlist("n", distinct = FALSE)
    ),
    expected,
    ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~label, ~X4, ~X6, ~X8, ~row_type, ~nested_level, ~group_level, ~grp,
    "A", "5", "2", "9",  "NESTED",             0,            0,  "A",
    "0", "1", "1", "9",  "NESTED",             1,            0,  "A"
  )
  expected$grp <- factor(expected$grp)

  expect_equal(

      nested_freq(
        mtcars2,
        colvar = "cyl",
        rowvar = "grp*vs",
        statlist = statlist("n", distinct = FALSE),
        cutoff = "8 >= 6",
        cutoff_stat = "n"

    ),
    expected,
    ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~label, ~X4, ~X6, ~X8, ~row_type, ~nested_level, ~group_level, ~grp, ~vs, ~carb,
    "A", "2", "0", "-",  "NESTED",             0,            1,  "A", "A",   "1",
    "0", "1", "0", "-",  "NESTED",             2,            1,  "A", "1",   "1",
    "1", "2", "0", "-",  "NESTED",             1,            1,  "A", "1",   "1",
    "1", "1", "0", "-",  "NESTED",             2,            1,  "A", "1",   "1",
    "A", "3", "-", "3",  "NESTED",             0,            1,  "A", "A",   "2",
    "0", "1", "-", "3",  "NESTED",             1,            1,  "A", "0",   "2",
    "0", "0", "-", "3",  "NESTED",             2,            1,  "A", "0",   "2",
    "1", "1", "-", "0",  "NESTED",             2,            1,  "A", "0",   "2",
    "0", "1", "-", "0",  "NESTED",             2,            1,  "A", "1",   "2",
    "1", "2", "-", "0",  "NESTED",             1,            1,  "A", "1",   "2",
    "1", "1", "-", "0",  "NESTED",             2,            1,  "A", "1",   "2",
    "A", "-", "-", "1",  "NESTED",             0,            1,  "A", "A",   "3",
    "0", "-", "-", "1",  "NESTED",             1,            1,  "A", "0",   "3",
    "0", "-", "-", "1",  "NESTED",             2,            1,  "A", "0",   "3",
    "A", "-", "2", "4",  "NESTED",             0,            1,  "A", "A",   "4",
    "0", "-", "1", "4",  "NESTED",             1,            1,  "A", "0",   "4",
    "0", "-", "0", "3",  "NESTED",             2,            1,  "A", "0",   "4",
    "1", "-", "1", "1",  "NESTED",             2,            1,  "A", "0",   "4",
    "0", "-", "1", "0",  "NESTED",             2,            1,  "A", "1",   "4",
    "1", "-", "1", "0",  "NESTED",             1,            1,  "A", "1",   "4",
    "1", "-", "0", "0",  "NESTED",             2,            1,  "A", "1",   "4",
    "A", "-", "-", "1",  "NESTED",             0,            1,  "A", "A",   "8",
    "0", "-", "-", "1",  "NESTED",             1,            1,  "A", "0",   "8",
    "0", "-", "-", "0",  "NESTED",             2,            1,  "A", "0",   "8",
    "1", "-", "-", "1",  "NESTED",             2,            1,  "A", "0",   "8",
    "B", "3", "2", "-",  "NESTED",             0,            1,  "B", "B",   "1",
    "0", "0", "2", "-",  "NESTED",             2,            1,  "B", "1",   "1",
    "1", "3", "2", "-",  "NESTED",             1,            1,  "B", "1",   "1",
    "1", "3", "0", "-",  "NESTED",             2,            1,  "B", "1",   "1",
    "B", "3", "-", "1",  "NESTED",             0,            1,  "B", "B",   "2",
    "0", "0", "-", "1",  "NESTED",             1,            1,  "B", "0",   "2",
    "0", "0", "-", "1",  "NESTED",             2,            1,  "B", "0",   "2",
    "0", "1", "-", "0",  "NESTED",             2,            1,  "B", "1",   "2",
    "1", "3", "-", "0",  "NESTED",             1,            1,  "B", "1",   "2",
    "1", "2", "-", "0",  "NESTED",             2,            1,  "B", "1",   "2",
    "B", "-", "-", "2",  "NESTED",             0,            1,  "B", "B",   "3",
    "0", "-", "-", "2",  "NESTED",             1,            1,  "B", "0",   "3",
    "0", "-", "-", "2",  "NESTED",             2,            1,  "B", "0",   "3",
    "B", "-", "2", "2",  "NESTED",             0,            1,  "B", "B",   "4",
    "0", "-", "1", "2",  "NESTED",             1,            1,  "B", "0",   "4",
    "0", "-", "0", "2",  "NESTED",             2,            1,  "B", "0",   "4",
    "1", "-", "1", "0",  "NESTED",             2,            1,  "B", "0",   "4",
    "0", "-", "1", "0",  "NESTED",             2,            1,  "B", "1",   "4",
    "1", "-", "1", "0",  "NESTED",             1,            1,  "B", "1",   "4",
    "1", "-", "0", "0",  "NESTED",             2,            1,  "B", "1",   "4",
    "B", "-", "1", "-",  "NESTED",             0,            1,  "B", "B",   "6",
    "0", "-", "1", "-",  "NESTED",             1,            1,  "B", "0",   "6",
    "0", "-", "0", "-",  "NESTED",             2,            1,  "B", "0",   "6",
    "1", "-", "1", "-",  "NESTED",             2,            1,  "B", "0",   "6"
  )

  expected$grp <- factor(expected$grp)
  expected$carb <- factor(expected$carb)

  expect_equal(
      nested_freq(
        mtcars2,
        colvar = "cyl",
        rowvar = "grp*vs*am",
        statlist = statlist("n", denoms_by = c("cyl", "carb"), distinct = FALSE),
        tablebyvar = "carb"

    ) %>% mutate(vs = as.character(vs)),
    expected,
    ignore_attr = TRUE
  )
})



test_that("subValues in a nested count layer should respect rowbyvar", {
  # If a value is found in one table by var but not another it should only be
  # found in one tablebyvar

  mtcars2 <- mtcars %>%
    rownames_to_column(var = "USUBJID")
  mtcars2$grp <- rep(c("A", "B"), length.out = 32)

  expected <- tibble::tribble(
    ~label,        ~X3,        ~X4,        ~X5, ~row_type, ~nested_level, ~group_level, ~carb, ~grp,
    "A",  "1 (6.7)",  "1 (8.3)",        "0",  "NESTED",             0,            1,   "1",  "A",
    "4",  "1 (6.7)",  "1 (8.3)",        "0",  "NESTED",             1,            1,   "1",  "A",
    "A", "3 (20.0)", "2 (16.7)", "1 (20.0)",  "NESTED",             0,            1,   "2",  "A",
    "4",        "0", "2 (16.7)", "1 (20.0)",  "NESTED",             1,            1,   "2",  "A",
    "8", "3 (20.0)",        "0",        "0",  "NESTED",             1,            1,   "2",  "A",
    "A",  "1 (6.7)",        "0",        "0",  "NESTED",             0,            1,   "3",  "A",
    "8",  "1 (6.7)",        "0",        "0",  "NESTED",             1,            1,   "3",  "A",
    "A", "3 (20.0)", "2 (16.7)", "1 (20.0)",  "NESTED",             0,            1,   "4",  "A",
    "6",        "0", "2 (16.7)",        "0",  "NESTED",             1,            1,   "4",  "A",
    "8", "3 (20.0)",        "0", "1 (20.0)",  "NESTED",             1,            1,   "4",  "A",
    "A",        "0",        "0", "1 (20.0)",  "NESTED",             0,            1,   "8",  "A",
    "8",        "0",        "0", "1 (20.0)",  "NESTED",             1,            1,   "8",  "A",
    "B", "2 (13.3)", "3 (25.0)",        "0",  "NESTED",             0,            1,   "1",  "B",
    "4",        "0", "3 (25.0)",        "0",  "NESTED",             1,            1,   "1",  "B",
    "6", "2 (13.3)",        "0",        "0",  "NESTED",             1,            1,   "1",  "B",
    "B",  "1 (6.7)", "2 (16.7)", "1 (20.0)",  "NESTED",             0,            1,   "2",  "B",
    "4",        "0", "2 (16.7)", "1 (20.0)",  "NESTED",             1,            1,   "2",  "B",
    "8",  "1 (6.7)",        "0",        "0",  "NESTED",             1,            1,   "2",  "B",
    "B", "2 (13.3)",        "0",        "0",  "NESTED",             0,            1,   "3",  "B",
    "8", "2 (13.3)",        "0",        "0",  "NESTED",             1,            1,   "3",  "B",
    "B", "2 (13.3)", "2 (16.7)",        "0",  "NESTED",             0,            1,   "4",  "B",
    "6",        "0", "2 (16.7)",        "0",  "NESTED",             1,            1,   "4",  "B",
    "8", "2 (13.3)",        "0",        "0",  "NESTED",             1,            1,   "4",  "B",
    "B",        "0",        "0", "1 (20.0)",  "NESTED",             0,            1,   "6",  "B",
    "6",        "0",        "0", "1 (20.0)",  "NESTED",             1,            1,   "6",  "B"
  )

  expected$grp <- factor(expected$grp)
  expected$carb <- factor(expected$carb)

  expect_equal(

      nested_freq(
        mtcars2,
        colvar = "gear",
        rowvar = "grp*cyl",
        rowbyvar = "carb"
      ),
      expected,
      ignore_attr = TRUE
  )

})

