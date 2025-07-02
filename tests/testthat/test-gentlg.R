test_that("custom alignments work", {
  df <- data.frame(
    label = c("boy", 'girl'),
    name = c("Bob", "Lily"),
    age = c(12, 15)
  )

  # `alignments` must be a list of named lists
  expect_error(
    gentlg(huxme = df, print.hux = FALSE, alignments = 1),
    "`alignments` must be a list"
  )

  expect_error(
    gentlg(huxme = df, print.hux = FALSE, alignments = list(1)),
    "Each item of `alignments` must be a list"
  )

  hux_table <- gentlg(
    huxme = df,
    print.hux = FALSE,
    alignments = list(
      list(row = 1:4, col = 2, value = "left"), # Column `name` to the left
      list(row = 3, col = 3, value = "right") # Cell `12` to the right
    )
  )[[1]]

  align_property <- huxtable::align(hux_table)

  expect_equal(align_property[4, 2], "left")
  expect_equal(align_property[3, 3], "right")
})
