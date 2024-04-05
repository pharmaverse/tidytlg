
library(tibble)

test_that("colheader works as expected for listings", {
  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      mtcars2 <- mtcars %>%
        rownames_to_column(var = "USUBJID") %>%
        mutate(am = factor(am),
               cyl = factor(cyl),
               gear = factor(gear)) %>%
        select("USUBJID", "mpg", "cyl", "disp", "hp")

      withr::with_dir(
        test_path("test_outputs"),
        {
          gentlg(
            mtcars2,
            file = "colheader1",
            tlf = "l"
          )
        }
      )

      mtcars3 <- mtcars2
      attr(mtcars3$mpg, "label") <- "Miles/(US) gallon"
      attr(mtcars3$cyl, "label") <- "Number of cylinders"
      attr(mtcars3$disp, "label") <- "Displacement (cu.in.)"
      attr(mtcars3$hp , "label") <- "Gross horsepower"

      withr::with_dir(
        test_path("test_outputs"),
        {
          gentlg(
            mtcars3,
            file = "colheader2",
            tlf = "l"
          )
        }
      )
    })

  expect_snapshot_file(test_path("test_outputs/colheader1.rtf"))
  expect_snapshot_file(test_path("test_outputs/colheader2.rtf"))
})
