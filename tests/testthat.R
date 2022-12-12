library(tidytlg)
library(testthat)
library(tibble)

withr::with_options(
  # Set to make test snapshots constant between environments
  list(width = 86),
  {
    test_check("tidytlg")

  }
)

