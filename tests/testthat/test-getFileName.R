test_that("getFileNameBatch returns the expected results", {

  cmdArgs1 <- c("--arg1", "someThing", "-e", 'timber::axecute(file~+~=~+~"/asdf/programs/CopyOftest_demo.R",log_name="CopyOftest_demo.R.log",log_path="/wrkdir/output")')
  cmdArgs2 <- c("--arg1", "someThing", "-e", 'timber::axecute(file="/asdf/programs/CopyOftest_demo.R",log_name="CopyOftest_demo.R.log",log_path="/wrkdir/output")')
  cmdArgs3 <- c("--arg1", "someThing", "--file=/asdf/programs/CopyOftest_demo.R")

  skip_on_os(c("windows", "mac", "solaris"))

  expect_equal(
    suppressWarnings(getFileNameBatch(cmdArgs1)),
    "/asdf/programs/CopyOftest_demo.R"
  )

  expect_equal(
    suppressWarnings(getFileNameBatch(cmdArgs2)),
    "/asdf/programs/CopyOftest_demo.R"
  )

  expect_equal(
    suppressWarnings(getFileNameBatch(cmdArgs3)),
    "/asdf/programs/CopyOftest_demo.R"
  )

})
