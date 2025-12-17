test_that("getFileNameBatch returns the expected results", {
  cmd_args1 <- c(
    "--arg1",
    "someThing",
    "-e",
    paste0(
      'logrx::axecute(file~+~=~+~"/asdf/programs/CopyOftest_demo.R",log_name',
      '="CopyOftest_demo.R.log",log_path="/wrkdir/output")'
    )
  )
  cmd_args2 <- c(
    "--arg1",
    "someThing",
    "-e",
    'logrx::axecute(file="/asdf/programs/CopyOftest_demo.R",log_name="CopyOftest_demo.R.log",log_path="/wrkdir/output")'
  )
  cmd_args3 <- c("--arg1", "someThing", "--file=/asdf/programs/CopyOftest_demo.R")

  skip_on_os(c("windows", "mac", "solaris"))

  expect_equal(suppressWarnings(getFileNameBatch(cmd_args1)), "/asdf/programs/CopyOftest_demo.R")

  expect_equal(suppressWarnings(getFileNameBatch(cmd_args2)), "/asdf/programs/CopyOftest_demo.R")

  expect_equal(suppressWarnings(getFileNameBatch(cmd_args3)), "/asdf/programs/CopyOftest_demo.R")
})
