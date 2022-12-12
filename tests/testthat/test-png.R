

test_that("gentlg will preserve aspect ratio of pngs", {


  png(filename = test_path("test_outputs/png1.png"), height = 325, width = 200, type="cairo-png")
  plot(mtcars$cyl)
  dev.off()
  png(filename = test_path("test_outputs/png2.png"), height = 193, width = 538, type="cairo-png")
  plot(mtcars$cyl)
  dev.off()

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
        })
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
