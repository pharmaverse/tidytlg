

adsl <- cdisc_adsl %>%
  filter(TRT01A != "")

test_that("nested_freq can handle subset argument", {

  expected <- tibble::tribble(
    ~label,   ~Placebo, ~Xanomeline.High.Dose, ~Xanomeline.Low.Dose, ~row_type, ~nested_level, ~group_level, ~SEX,
    "F", "2 (40.0)",            "3 (60.0)",           "1 (20.0)",  "NESTED",             0,            0,  "F",
    "WHITE", "2 (40.0)",            "3 (60.0)",           "1 (20.0)",  "NESTED",             1,            0,  "F",
    "M", "3 (60.0)",            "2 (40.0)",           "4 (80.0)",  "NESTED",             0,            0,  "M",
    "WHITE", "3 (60.0)",            "2 (40.0)",           "4 (80.0)",  "NESTED",             1,            0,  "M"
  )

  expected$SEX <- factor(expected$SEX)

  expect_equal(

      nested_freq(
        adsl,
        rowvar = "SEX*RACE",
        colvar = "TRT01A",
        subset = ITTFL == "Y"
      ),
    expected,
    ignore_attr = TRUE
  )


})

test_that("nested_freq can handle row_header", {

  expected <- tibble::tribble(
    ~label,   ~Placebo, ~Xanomeline.High.Dose, ~Xanomeline.Low.Dose, ~row_type, ~nested_level, ~group_level, ~SEX,
    "Geo, country",         "",                    "",                   "",  "HEADER",            NA,            0,  "F",
    "F", "2 (40.0)",            "3 (60.0)",           "1 (20.0)",  "NESTED",             0,            0,  "F",
    "WHITE", "2 (40.0)",            "3 (60.0)",           "1 (20.0)",  "NESTED",             1,            0,  "F",
    "M", "3 (60.0)",            "2 (40.0)",           "4 (80.0)",  "NESTED",             0,            0,  "M",
    "WHITE", "3 (60.0)",            "2 (40.0)",           "4 (80.0)",  "NESTED",             1,            0,  "M"
  )

  expected$SEX <- factor(expected$SEX)

  expect_equal(

      nested_freq(
        adsl,
        rowvar = "SEX*RACE",
        colvar = "TRT01A",
        row_header ="Geo, country"

    ),
    expected,
    ignore_attr = TRUE)

})
