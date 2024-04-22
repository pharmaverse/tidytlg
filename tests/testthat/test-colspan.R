library(tibble)

test_that("colspan works as expected", {
  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      mtcars2 <- mtcars %>%
        rownames_to_column(var = "USUBJID") %>%
        mutate(
          am = factor(am),
          cyl = factor(cyl),
          gear = factor(gear)
        )

      tab <- freq(
        mtcars2,
        colvar = "cyl",
        rowvar = "am"
      ) %>%
        select(-c("row_type", "group_level"))

      withr::with_dir(
        new = test_path("test_outputs"),
        code = {
          gentlg(
            tab,
            file = "colspan1",
            colspan = list(c("", "All", "All", "All"), c("", "Low", "High", "High"))
          )
        }
      )

      tab <- freq(
        mtcars2 %>%
          tlgsetup(
            var = "cyl",
            column_metadata_file = test_path("test_data/column_metadata_mtcars.xlsx"),
            tbltype = "mtcars2"
          ),
        colvar = "colnbr",
        rowvar = "am"
      ) %>%
        bind_table(
          column_metadata_file = test_path("test_data/column_metadata_mtcars.xlsx"),
          tbltype = "mtcars2"
        ) %>%
        select("label", starts_with("col"))

      withr::with_dir(
        new = test_path("test_outputs"),
        code = {
          gentlg(
            tab,
            file = "colspan2"
          )
        }
      )

      tab <- freq(
        mtcars2,
        colvar = "cyl",
        rowvar = "am"
      ) %>%
        select(-c("row_type", "group_level"))

      withr::with_dir(
        new = test_path("test_outputs"),
        code = {
          gentlg(
            tab,
            file = "colspan3",
            colspan = list(c("", "Low", "High", "High"), c("", "All", "All", "All"))
          )
        }
      )
    }
  )
  expect_snapshot_file(test_path("test_outputs/colspan1.rtf"))
  expect_snapshot_file(test_path("test_outputs/colspan2.rtf"))
  expect_snapshot_file(test_path("test_outputs/colspan3.rtf"))
})
