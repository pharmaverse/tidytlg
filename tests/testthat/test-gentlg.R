test_that("custom alignments work", {
  df <- data.frame(label = c("boy", "girl"), name = c("Bob", "Lily"), age = c(12, 15))

  # `alignments` must be a list of named lists
  expect_error(gentlg(huxme = df, print.hux = FALSE, alignments = 1), "`alignments` must be a list")

  expect_error(gentlg(huxme = df, print.hux = FALSE, alignments = list(1)), "Each item of `alignments` must be a list")

  # Apply alignments to one data frame
  hux_table <- gentlg(
    huxme = df,
    print.hux = FALSE,
    alignments = list(
      list(
        row = 1:4, col = 2, value = "left"
      ), # Column `name` to the left
      list(row = 3, col = 3, value = "right") # Cell `12` to the right
    )
  )[[
    1
  ]]

  align_property <- huxtable::align(hux_table)
  expect_equal(align_property[4, 2], "left")
  expect_equal(align_property[3, 3], "right")

  # Apply alignments to two data frames
  hux_tables <- gentlg(
    huxme = list(df, df),
    print.hux = FALSE,
    alignments = list( # Column `name` to the left for the first data frame
      list(list(row = 1:4, col = 2, value = "left")), # Cell `12` to the right for the second data frame
      list(list(row = 3, col = 3, value = "right"))
    )
  )

  align_property_1 <- huxtable::align(hux_tables[[1]])
  expect_equal(align_property_1[4, 2], "left")
  expect_equal(align_property_1[3, 3], "center")

  align_property_2 <- huxtable::align(hux_tables[[2]])
  expect_equal(align_property_2[4, 2], "center")
  expect_equal(align_property_2[3, 3], "right")
})

test_that("replace_lead_whitespaces_ind() is replacing whitespaces", {
  # case when there are 2 leading whitespaces, should insert 180 twips
  df <- data.frame(label = c("boy", "girl"), name = c("Bob", "Lily"), age = c(12, 15))
  expect_no_error(res <- gentlg(huxme = df,
         print.hux = FALSE,
         colheader = c("  Gender", "Name", "Age")))
  res <- as.character(res[[1]][2, 1])
  expect_equal(res, "\\keepn\\trhdr \\intbl\\li180\\fi0 Gender")

  # case when there are no leading whitespaces, should leave it as is
  expect_no_error(res <- gentlg(huxme = df,
                                print.hux = FALSE,
                                colheader = c("Gender", "Name", "Age")))
  res <- as.character(res[[1]][2, 1])
  expect_equal(res, "\\keepn\\trhdr Gender")
})
