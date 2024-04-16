library(ggplot2)

test_that("gentlg will preserve aspect ratio of pngs", {
  png(filename = test_path("test_outputs/png1.png"), height = 325, width = 200, type = "cairo-png")
  plot(mtcars$cyl)
  if (dev.cur() != 1) dev.off()
  png(filename = test_path("test_outputs/png2.png"), height = 193, width = 538, type = "cairo-png")
  plot(mtcars$cyl)
  if (dev.cur() != 1) dev.off()

  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      withr::with_dir(
        test_path("test_outputs"),
        {
          gentlg(
            plotnames = "png1.png",
            tlf = "f",
            file = "png1"
          )
          gentlg(
            plotnames = "png2.png",
            tlf = "f",
            file = "png2"
          )
        }
      )
    }
  )
  skip_if(Sys.getenv("RENV_PATHS_CACHE") == "")
  expect_snapshot_file(test_path("test_outputs/png1.rtf"))
  expect_snapshot_file(test_path("test_outputs/png2.rtf"))
})
# This is a fragile step so its hard to test
# test_that("gentlg can take a ggplot object as an input", {
#   library(ggplot2)
#   gg <- mtcars %>%
#     ggplot(aes(hp, mpg)) +
#     geom_point()
#
#   withr::with_options(
#     list(tidytlg.add_datetime = FALSE),
#     {
#       withr::with_dir(
#         test_path("test_outputs"),
#         {
#           gentlg(
#             gg,
#             tlf = 'g',
#             file = "png3",
#           )
#         })
#     })
#   skip_on_cran()
#   expect_snapshot_file(test_path("test_outputs/png3.rtf"), compare = compare_file_text)
# })

test_that("gentlg can take multiple plotnames arguments", {
  a_tempdir <- test_path("test_outputs")

  adsl <- cdisc_adsl %>%
    filter(SAFFL == "Y")

  tblid <- "test2plots"

  plot1 <- ggplot(data = adsl, aes(x = HEIGHTBL, y = WEIGHTBL)) +
    geom_point() +
    labs(
      x = "Baseline Height (cm)",
      y = "Baseline Weight (kg)"
    )

  png(paste0(a_tempdir, "/plot1.png"), width = 2800, height = 1800, res = 300, type = "cairo")

  plot1

  if (dev.cur() != 1) dev.off()

  plot2 <- ggplot(data = adsl, aes(x = WEIGHTBL, y = HEIGHTBL)) +
    geom_point() +
    labs(
      x = "Baseline Weight (kg)",
      y = "Baseline Height (cm)"
    )

  png(paste0(a_tempdir, "/plot2.png"), width = 2800, height = 1800, res = 300, type = "cairo")

  plot2

  if (dev.cur() != 1) dev.off()

  gentlg(
    tlf = "g",
    plotnames = paste0(a_tempdir, "/", c("plot1.png", "plot2.png")),
    opath = a_tempdir,
    plotwidth = 8,
    plotheight = 5,
    orientation = "landscape",
    file = tblid,
    title = "Scatter plot"
  )


  withr::with_options(
    list(tidytlg.add_datetime = FALSE),
    {
      expect_snapshot_file(test_path("test_outputs/test2plots.rtf"))
    }
  )
})
