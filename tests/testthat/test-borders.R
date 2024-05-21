library(tibble)

output_directory <- "test_outputs/borders"
testthat::test_that("default does not add any borders", {
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
            footers = "Footer1"
          )
        }
      )
    }
  )

  expect_snapshot_file(test_path(sprintf("%s/noborders.rtf", output_directory)))
})

testthat::test_that("")
