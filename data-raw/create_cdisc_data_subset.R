
library(dplyr)
library(haven)

adsl <- read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adsl.xpt"))
adae <- read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adae.xpt"))
adlb <- read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/adlbc.xpt"))
advs <- read_xpt(url("https://github.com/phuse-org/TestDataFactory/raw/main/Updated/TDF_ADaM/advs.xpt"))


adsl2 <- adsl %>%
  filter(SAFFL == "Y") %>%
  group_by(TRT01PN) %>%
  slice_head(n = 5) %>%
  ungroup()

usid <- adsl2 %>% select(USUBJID)

adae2 <- adae %>%
  inner_join(usid, by = "USUBJID")

adlb2 <- adlb %>%
  inner_join(usid, by = "USUBJID")

advs2 <- advs %>%
  inner_join(usid, by = "USUBJID")

cdisc_adsl <- adsl2
cdisc_adae <- adae2
cdisc_adlb <- adlb2
cdisc_advs <- advs2

save(cdisc_adsl, file = "data/cdisc_adsl.rda")
save(cdisc_adae, file = "data/cdisc_adae.rda")
save(cdisc_adlb, file = "data/cdisc_adlb.rda")
save(cdisc_advs, file = "data/cdisc_advs.rda")



