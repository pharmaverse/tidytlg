library(tibble)

test_that("multiple tables are printed to one rtf file", {
  output_directory <- "test_outputs/multiple_tables_in_one_file"

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

      tab2 <- freq(
        mtcars2 %>%
          tlgsetup(
            var = "cyl",
            column_metadata_file = testthat::test_path("test_data/column_metadata_mtcars.xlsx"),
            tbltype = "mtcars2"
          ),
        colvar = "colnbr",
        rowvar = "am"
      ) %>%
        bind_table(
          column_metadata_file = testthat::test_path("test_data/column_metadata_mtcars.xlsx"),
          tbltype = "mtcars2"
        ) %>%
        select("label", starts_with("col"))

      withr::with_dir(
        new = testthat::test_path(output_directory),
        code = {
          gentlg(
            list(tab, tab2),
            file = "multipletabs",
            title = list("Title 1", "Title 2"),
            footers = list("Footer 1", "Footer 2")
          )
        }
      )
    }
  )

  expect_snapshot_file(test_path(sprintf("%s/multipletabs.rtf", output_directory)))

  # Verify only one file was created
  files <- list.files(path = testthat::test_path(output_directory), pattern = "*.rtf")
  testthat::expect_identical(
    length(files),
    1L,
    label = sprintf("Expected to find only one file. Found: %s", paste0(files, collapse = " "))
  )
})
