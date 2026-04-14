df <- data.frame(label = c("boy", "girl"), name = c("Bob", "Lily"), age = c(12, 15))

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

test_that("gentlg() sets the right colwidths when passing a combination of vectors and numeric values", {
  wcol <- list(c(0.5, 0.3, 0.2), c(0.25))
  expect_no_error(hux_tables <- gentlg(
    huxme = list(df, df),
    wcol = wcol,
    print.hux = FALSE
  ))

  for (i in seq_along(wcol)) {
    ht <- hux_tables[[i]]
    num_cols <- ncol(ht)
    actual_colwidths <- as.numeric(huxtable::col_width(ht))
    if (length(wcol[[i]]) == 1) {
      expected_colwidths <- c(wcol[[i]], rep((1 - wcol[[i]]) / (num_cols - 1), num_cols - 1))
    } else {
      expected_colwidths <- wcol[[i]]
    }
    expect_equal(actual_colwidths, expected_colwidths)
  }
})

test_that("gentlg() sets the right colwidths when passing all colwidths explicitly", {
  wcol <- list(c(0.5, 0.3, 0.2), c(0.25, 0.5, 0.25))
  expect_no_error(hux_tables <- gentlg(
    huxme = list(df, df),
    wcol = wcol,
    print.hux = FALSE
  ))

  for (i in seq_along(wcol)) {
    ht <- hux_tables[[i]]
    num_cols <- ncol(ht)
    expected_colwidths <- wcol[[i]]
    actual_colwidths <- as.numeric(huxtable::col_width(ht))
    expect_equal(expected_colwidths, actual_colwidths)
  }
})

test_that("gentlg() validates that if hux is a single data.frame, wcol cannot be a list", {
  wcol <- list(c(0.5), c(0.4, 0.4, 0.2))
  expect_error(hux_tables <- gentlg(
    huxme = df,
    wcol = wcol,
    print.hux = FALSE
  ),
  "\\'wcol\\' appears to be"
  )
})

test_that("gentlg() validates each element in wcol has the correct length", {
  wcol <- list(c(0.5, 0.3), c(0.4, 0.4, 0.2))
  expect_error(hux_tables <- gentlg(
    huxme = list(df, df),
    wcol = wcol,
    print.hux = FALSE
  ),
  "wcol\\'s length must be 1 or the length of final output"
  )
})

test_that("gentlg() validates that the sum of colwidths is 1", {
  wcol <- list(c(0.5, 0.2, 0.5), c(0.4, 0.4, 0.2))
  expect_error(hux_tables <- gentlg(
    huxme = list(df, df),
    wcol = wcol,
    print.hux = FALSE
  ),
  "wcol not defined properly"
  )
})

test_that("gentlg() validates hux length equals wcol length if wcol is a list", {
  wcol <- list(c(0.5), c(0.4, 0.4, 0.2))
  expect_error(hux_tables <- gentlg(
    huxme = list(df, df, df),
    wcol = wcol,
    print.hux = FALSE
  ),
  "Arguments \\'wcol\\' and \\'huxme\\' must have the same length."
  )
})
