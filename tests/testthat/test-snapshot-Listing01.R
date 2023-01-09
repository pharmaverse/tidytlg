# library(dplyr)
#
# test_that("Listing01 rtf/html renders as expected", {
#
#   dir_out <- tempdir()
#
#   # tableid -----------------------------------------------------------------
#   tblid <- "Listing01"
#
#   # data manipulation -------------------------------------------------------
#   data("cdisc_adsl")
#
#   # convert to factor so all levels will show in the results and show in the correct order
#   adsl <- cdisc_adsl %>%
#     filter(ITTFL == "Y") %>%
#     select(TRT01P, SITEID, SUBJID, AGE, RACE, SEX) %>%
#     arrange(TRT01P, SITEID, SUBJID, AGE, SEX, RACE)
#
#
#   htmlOutput <- capture.output(gentlg(huxme       = adsl
#                            ,tlf         = "l"
#                            ,format      = "HTML"
#                            ,idvars      = NULL
#                            ,opath       = dir_out
#                            ,orientation = "landscape"
#                            ,wcol        = 0.20
#                            ,file        = tblid
#                            ,colheader = c("Treatment Group", "Site ID", "Subject ID", "Age", "Race", "Sex")
#                            ,title       = "Listing of Demographic Characteristics"
#                            ))
#   # Remove the htmloutput that has the timestamp
#   htmlOutput <- htmlOutput[-(length(htmlOutput) - 1)]
#   writeLines(htmlOutput, con = paste0(dir_out, "/listing01.html"))
#
#   expect_snapshot_file(paste0(dir_out,"/listing01.html"))
#
# })
