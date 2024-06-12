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

testthat::test_that("Inserts separated borders in the second row", {
  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      bmat2 <- matrix(c(
        0, 1, 1, 1, 0, 0,
        0, 0, 0, 0, 1, 2,
        1, 1, 1, 1, 1, 1
      ), nrow = 3, ncol = 6, byrow = TRUE)

      tbl <- data.frame(
        label = "Subjects with ≥ 1 concomitant medication",
        col1 = "1 (100.0%)",
        col2 = "1 (100.0%)",
        col3 = "2 (100.0%)",
        col4 = "1 (100.0%)",
        col5 = "3 (100.0%)",
        row_type = "VALUE"
      )

      withr::with_dir(
        new = testthat::test_path(output_directory),
        code = {
          gentlg(
            huxme = tbl,
            orientation = "landscape",
            file = "stubborntest",
            title = "Summary of Concomitant Medications",
            colspan = list(
              c("", "Active Study Agent", "Active Study Agent", "Active Study Agent", "", ""),
              c("", "Treatment 1", "Treatment 2", "Combined", "Placebo", "Total")
            ),
            colheader = c(" Standardized medication name", "N=1", "N=1", "N=2", "N=1", "N=3"),
            wcol = .30,
            bottom_borders = bmat2,
            border_fns = list()
          )
        }
      )
    }
  )

  expect_snapshot_file(test_path(sprintf("%s/stubborntest.rtf", output_directory)))
})


testthat::test_that("Inserts separated borders under the first row for a listing", {
  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      adsl <- cdisc_adsl
      adae <- cdisc_adae

      adsl <- adsl %>%
        filter(SAFFL == "Y") %>%
        select(USUBJID, SAFFL, TRT01AN, TRT01A)

      adae <- adae %>%
        filter(SAFFL == "Y" & TRTEMFL == "Y") %>%
        mutate(
          BSPT = paste(AEBODSYS, "[", AEDECOD, "]"),
          SAEFL = if_else(AESER == "Y", "Yes", "No"),
          DTHFL = if_else(AEOUT == "FATAL", "Yes", "No")
        ) %>%
        select(USUBJID, ASTDY, TRTA, BSPT, AETERM, SAEFL, DTHFL)

      tbl <- inner_join(adsl, adae, by = "USUBJID") %>%
        arrange(TRT01AN, USUBJID, ASTDY) %>%
        select(TRT01A, USUBJID, ASTDY, TRTA, BSPT, AETERM, SAEFL, DTHFL) %>%
        filter(USUBJID %in% c("01-701-1015", "01-701-1023"))

      withr::with_dir(
        new = testthat::test_path(output_directory),
        code = {
          gentlg(
            huxme = tbl,
            tlf = "l",
            orientation = "landscape",
            file = "oldformatlisting",
            title = "Listing of Adverse Events",
            idvars = c("TRT01A", "USUBJID"),
            wcol = 0.15,
            colheader = c(
              "Treatment Group",
              "Subject ID",
              "Study Day of AE",
              "Treatment Period",
              "Body System [Preferred Term]",
              "Verbatim Term",
              "Serious",
              "Fatal"
            )
          )
        }
      )
    }
  )

  expect_snapshot_file(test_path(sprintf("%s/oldformatlisting.rtf", output_directory)))
})
