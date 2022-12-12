
library(readxl)

column_metadata <- read_excel("~/tidytlg/inst/extdata/column_metadata.xlsx",
                              sheet = 1)

usethis::use_data(column_metadata, overwrite = TRUE)
