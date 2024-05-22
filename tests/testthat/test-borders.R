library(tibble)

output_directory <- "test_outputs/borders"
testthat::test_that("default uses old format", {
  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      tab <- iris[1:5, ]
      tab$label <- c("Label", rep("", 4))

      withr::with_dir(
        new = testthat::test_path(output_directory),
        code = {
          gentlg(
            tab,
            file = "oldformat",
            title = "Title1",
            colspan = list(c("", rep("Measures", 4), "")),
            footers = "Footer1"
          )
        }
      )
    }
  )

  expect_snapshot_file(test_path(sprintf("%s/oldformat.rtf", output_directory)))
})

testthat::test_that("No borders generates rtf with no borders", {
  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      tab <- iris[1:5, ]
      tab$label <- c("Label", rep("", 4))

      withr::with_dir(
        new = testthat::test_path(output_directory),
        code = {
          gentlg(
            tab,
            file = "noborders",
            title = "Title1",
            colspan = list(c("", rep("Measures", 4), "")),
            footers = "Footer1",
            border_fns = list(no_borders)
          )
        }
      )
    }
  )

  expect_snapshot_file(test_path(sprintf("%s/noborders.rtf", output_directory)))
})

testthat::test_that("Spanning borders in the first row", {
  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      tab <- iris[1:5, ]
      tab$label <- c("Label", rep("", 4))

      withr::with_dir(
        new = testthat::test_path(output_directory),
        code = {
          gentlg(
            tab,
            file = "spanningbordersrow1",
            title = "Title1",
            colspan = list(c("", rep("Measures", 4), "")),
            footers = "Footer1",
            border_fns = list(no_borders, spanning_borders(1))
          )
        }
      )
    }
  )

  expect_snapshot_file(test_path(sprintf("%s/spanningbordersrow1.rtf", output_directory)))
})
