testthat::test_that("Removes zero padding (real rtf)", {
  rtf_con <- file(testthat::test_path("test_data", "zero_padding", "input.rtf"))
  input_file <- paste(readLines(rtf_con), collapse = "\n")
  expected_con <- file(testthat::test_path("test_data", "zero_padding", "expected.rtf"))
  expected <- paste(readLines(expected_con), collapse = "\n")

  actual <- remove_zero_padding(input_file)
  testthat::expect_equal(actual, expected)
})

testthat::test_that("Removes zero padding" ,{
  example <- paste0(
    "\\clbrdrt\\clbrdrl\\clbrdrb\\clbrdrr\\clvertalb\\clpadfl3",
    "\\clpadl0 \\clpadft3\\clpadt60 \\clpadfb3\\clpadb0 \\clpadfr3\\clpadr60"
  )
  actual <- remove_zero_padding(example)
  expected <- "\\clbrdrt\\clbrdrl\\clbrdrb\\clbrdrr\\clvertalb\\clpadft3\\clpadt60 \\clpadfr3\\clpadr60"
  testthat::expect_equal(actual, expected)
})
