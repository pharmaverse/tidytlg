library(dplyr)
library(tidytlg)

table_metadata <- tibble::tribble(
  ~func, ~df, ~rowvar, ~decimal, ~rowtext, ~row_header, ~statlist, ~subset,
  "freq", "adsl", "ITTFL", NA, "Analysis set: ITT", NA, statlist("n"), "ITTFL == 'Y'",
  "univar", "adsl", "AGE", 0, NA, "Age (Years)", NA, NA,
  "freq", "adsl", "SEX", NA, NA, "Gender", statlist(c("N", "n (x.x%)")), NA
) %>%
  mutate(colvar = "TRT01PN", tablebyvar = NA, rowbyvar = NA, denom_df = NA)

usethis::use_data(table_metadata, overwrite = TRUE)
