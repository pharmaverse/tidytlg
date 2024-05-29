library(tibble)

test_that("Page numbers are added on each page", {
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
      tab2 <- tab

      withr::with_dir(
        new = test_path("test_outputs"),
        code = {
          gentlg(
            list(tab, tab2),
            file = "pagenumbers",
            tlf = "Listing",
            pagenum = TRUE
          )
        }
      )
    }
  )
  expect_snapshot_file(test_path("test_outputs/pagenumbers.rtf"))
})

test_that("The page number mentions the table", {
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
      tab2 <- tab

      withr::with_dir(
        new = test_path("test_outputs"),
        code = {
          gentlg(
            list(tab, tab2),
            file = "pagenumberstable",
            pagenum = TRUE
          )
        }
      )
    }
  )
  expect_snapshot_file(test_path("test_outputs/pagenumberstable.rtf"))
})
