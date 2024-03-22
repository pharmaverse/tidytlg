test_that("errors work as expected", {
  expect_error(
    quick_rtf_jnj(borders = c(TRUE, FALSE))
  )
  expect_error(
    quick_rtf_jnj(open = c(NA))
  )
})
