
library(tibble)
test_that("titles_file addes titles as expected", {
  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      mtcars2 <- mtcars %>%
        rownames_to_column(var = "USUBJID") %>%
        mutate(am = factor(am),
               cyl = factor(cyl),
               gear = factor(gear))

      tab <- freq(
        mtcars2,
        colvar = "cyl",
        rowvar = "am"
      )

      withr::with_dir(
        new = test_path("test_outputs"),
        code = {
          gentlg(
            tab,
            file = "TSIDEM01",
            title_file = "titles.xls",
            colheader = c("label", "4", "6", "8")
          )
        }
      )
      # expect_snapshot_file(test_path("test_outputs/tsidem01.rtf"))

      withr::with_dir(
        new = test_path("test_outputs"),
        code = {
          gentlg(
            tab,
            file = "GSVIT01A",
            title_file = "titles.xls",
            colheader = c("label", "4", "6", "8")
          )
        }
      )
      expect_snapshot_file(test_path("test_outputs/gsvit01a.rtf"))
    })
})
