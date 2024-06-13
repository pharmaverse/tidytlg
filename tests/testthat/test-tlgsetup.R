test_that("tlgsetup will factorize the colnbr", {
  mtcars2 <- mtcars %>%
    filter(cyl != 8)

  mtcars3 <- mtcars2 %>%
    tlgsetup(
      var = "cyl",
      column_metadata_file = test_path("test_data/column_metadata_mtcars.xlsx"),
      tbltype = "mtcars2"
    ) %>%
    freq(
      colvar = "colnbr",
      rowvar = "gear",
      statlist = statlist("n", distinct = FALSE),
      .keep = FALSE
    )

  expected <- tibble::tribble(
    ~label, ~col1, ~col2, ~col3, ~col4, ~col5, ~row_type, ~group_level,
    "3", "1", "2", "-", "2", "3", "VALUE", 0,
    "4", "8", "4", "-", "4", "12", "VALUE", 0,
    "5", "2", "1", "-", "1", "3", "VALUE", 0
  )

  expect_equal(mtcars3, expected, ignore_attr = TRUE)


  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      withr::with_dir(
        new = test_path("test_outputs"),
        code = {
          gentlg(
            mtcars3,
            colheader = c("label", "col1", "col2", "col3", "col4", "col5"),
            file = "testtlgsetup"
          )
        }
      )
    }
  )

  expect_snapshot_file(test_path("test_outputs/testtlgsetup.rtf"))
})
