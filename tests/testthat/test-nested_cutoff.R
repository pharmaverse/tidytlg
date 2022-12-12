library(dplyr)
library(haven)
library(tibble)

adsl <- cdisc_adsl %>%
  filter(TRT01P != "")

adae <- read_sas("test_data/adae.sas7bdat")

test_that("the cutoff argument in nested drops values with no subordiate items", {

  expected <- tibble::tribble(
    ~label,    ~Placebo, ~Xanomeline.High.Dose, ~Xanomeline.Low.Dose, ~row_type, ~nested_level, ~group_level, ~SEX,
    "F", "2 (40.0%)",           "3 (60.0%)",          "1 (20.0%)",  "NESTED",             0,            0,  "F",
    "WHITE", "2 (40.0%)",           "3 (60.0%)",          "1 (20.0%)",  "NESTED",             1,            0,  "F",
    "M", "3 (60.0%)",           "2 (40.0%)",          "4 (80.0%)",  "NESTED",             0,            0,  "M",
    "WHITE", "3 (60.0%)",           "2 (40.0%)",          "4 (80.0%)",  "NESTED",             1,            0,  "M"
  )
  expected$SEX <- factor(expected$SEX)

  expect_equal(
      nested_freq(adsl,
                  rowvar = "SEX*RACE",
                  colvar = "TRT01P",
                  statlist = statlist("n (x.x%)"),
                  cutoff = 10),
      expected,
      ignore_attr = TRUE
  )

  expected <- tibble::tribble(
    ~label,    ~Placebo, ~Xanomeline.High.Dose, ~Xanomeline.Low.Dose,         ~row_type,                                              ~AEBODSYS, ~anbr, ~indentme, ~roworder, ~newrows, ~newpage,
    "F",          NA,                    NA,                   NA, "TABLE_BY_HEADER",                                                     NA,     0,         0,        1L,        1,        0,
    "CARDIAC DISORDERS",  "1 (50.0)",                   "0",                  "0",          "NESTED",                                    "CARDIAC DISORDERS",     1,         1,        1L,        1,        0,
    "BUNDLE BRANCH BLOCK LEFT",  "1 (50.0)",                   "0",                  "0",          "NESTED",                                    "CARDIAC DISORDERS",     1,         2,        2L,        0,        0,
    "GASTROINTESTINAL DISORDERS", "2 (100.0)",                   "0",                  "0",          "NESTED",                           "GASTROINTESTINAL DISORDERS",     1,         1,        3L,        0,        0,
    "DIARRHOEA",  "1 (50.0)",                   "0",                  "0",          "NESTED",                           "GASTROINTESTINAL DISORDERS",     1,         2,        4L,        0,        0,
    "HIATUS HERNIA",  "1 (50.0)",                   "0",                  "0",          "NESTED",                           "GASTROINTESTINAL DISORDERS",     1,         2,        5L,        0,        0,
    "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",  "1 (50.0)",           "3 (100.0)",                  "0",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         1,        6L,        0,        0,
    "APPLICATION SITE ERYTHEMA",  "1 (50.0)",            "2 (66.7)",                  "0",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,        7L,        0,        0,
    "APPLICATION SITE IRRITATION",         "0",            "1 (33.3)",                  "0",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,        8L,        0,        0,
    "APPLICATION SITE PAIN",         "0",            "1 (33.3)",                  "0",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,        9L,        0,        0,
    "APPLICATION SITE PRURITUS",  "1 (50.0)",           "3 (100.0)",                  "0",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       10L,        0,        0,
    "APPLICATION SITE VESICLES",         "0",            "1 (33.3)",                  "0",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       11L,        0,        0,
    "FATIGUE",         "0",            "2 (66.7)",                  "0",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       12L,        0,        0,
    "INFECTIONS AND INFESTATIONS",  "1 (50.0)",                   "0",          "1 (100.0)",          "NESTED",                          "INFECTIONS AND INFESTATIONS",     1,         1,       13L,        0,        0,
    "CELLULITIS",         "0",                   "0",          "1 (100.0)",          "NESTED",                          "INFECTIONS AND INFESTATIONS",     1,         2,       14L,        0,        0,
    "LOCALISED INFECTION",         "0",                   "0",          "1 (100.0)",          "NESTED",                          "INFECTIONS AND INFESTATIONS",     1,         2,       15L,        0,        0,
    "UPPER RESPIRATORY TRACT INFECTION",  "1 (50.0)",                   "0",                  "0",          "NESTED",                          "INFECTIONS AND INFESTATIONS",     1,         2,       16L,        0,        0,
    "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",         "0",                   "0",          "1 (100.0)",          "NESTED",      "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",     1,         1,       17L,        0,        0,
    "ARTHRALGIA",         "0",                   "0",          "1 (100.0)",          "NESTED",      "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",     1,         2,       18L,        0,        0,
    "RENAL AND URINARY DISORDERS",         "0",                   "0",          "1 (100.0)",          "NESTED",                          "RENAL AND URINARY DISORDERS",     1,         1,       19L,        0,        0,
    "MICTURITION URGENCY",         "0",                   "0",          "1 (100.0)",          "NESTED",                          "RENAL AND URINARY DISORDERS",     1,         2,       20L,        0,        0,
    "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",         "0",            "1 (33.3)",          "1 (100.0)",          "NESTED",               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",     1,         1,       21L,        0,        0,
    "ERYTHEMA",         "0",                   "0",          "1 (100.0)",          "NESTED",               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",     1,         2,       22L,        0,        0,
    "PRURITUS",         "0",                   "0",          "1 (100.0)",          "NESTED",               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",     1,         2,       23L,        0,        0,
    "RASH",         "0",            "1 (33.3)",                  "0",          "NESTED",               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",     1,         2,       24L,        0,        0,
    "M",          NA,                    NA,                   NA, "TABLE_BY_HEADER",                                                     NA,     0,         0,        1L,        0,        1,
    "CARDIAC DISORDERS",  "1 (33.3)",                   "0",                  "0",          "NESTED",                                    "CARDIAC DISORDERS",     1,         1,        1L,        1,        0,
    "ATRIOVENTRICULAR BLOCK SECOND DEGREE",  "1 (33.3)",                   "0",                  "0",          "NESTED",                                    "CARDIAC DISORDERS",     1,         2,        2L,        0,        0,
    "EYE DISORDERS",  "1 (33.3)",                   "0",                  "0",          "NESTED",                                        "EYE DISORDERS",     1,         1,        3L,        0,        0,
    "EYE ALLERGY",  "1 (33.3)",                   "0",                  "0",          "NESTED",                                        "EYE DISORDERS",     1,         2,        4L,        0,        0,
    "EYE PRURITUS",  "1 (33.3)",                   "0",                  "0",          "NESTED",                                        "EYE DISORDERS",     1,         2,        5L,        0,        0,
    "EYE SWELLING",  "1 (33.3)",                   "0",                  "0",          "NESTED",                                        "EYE DISORDERS",     1,         2,        6L,        0,        0,
    "GASTROINTESTINAL DISORDERS",         "0",            "1 (50.0)",                  "0",          "NESTED",                           "GASTROINTESTINAL DISORDERS",     1,         1,        7L,        0,        0,
    "DYSPEPSIA",         "0",            "1 (50.0)",                  "0",          "NESTED",                           "GASTROINTESTINAL DISORDERS",     1,         2,        8L,        0,        0,
    "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",  "1 (33.3)",           "2 (100.0)",          "3 (100.0)",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         1,        9L,        0,        0,
    "APPLICATION SITE DERMATITIS",         "0",                   "0",           "1 (33.3)",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       10L,        0,        0,
    "APPLICATION SITE ERYTHEMA",         "0",           "2 (100.0)",           "1 (33.3)",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       11L,        0,        0,
    "APPLICATION SITE IRRITATION",         "0",                   "0",           "1 (33.3)",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       12L,        0,        0,
    "APPLICATION SITE PRURITUS",         "0",           "2 (100.0)",           "2 (66.7)",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       13L,        0,        0,
    "APPLICATION SITE URTICARIA",         "0",                   "0",           "1 (33.3)",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       14L,        0,        0,
    "APPLICATION SITE VESICLES",         "0",                   "0",           "1 (33.3)",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       15L,        0,        0,
    "FATIGUE",         "0",                   "0",           "1 (33.3)",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       16L,        0,        0,
    "PYREXIA",  "1 (33.3)",                   "0",                  "0",          "NESTED", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",     1,         2,       17L,        0,        0,
    "INFECTIONS AND INFESTATIONS",  "1 (33.3)",            "1 (50.0)",                  "0",          "NESTED",                          "INFECTIONS AND INFESTATIONS",     1,         1,       18L,        0,        0,
    "LOWER RESPIRATORY TRACT INFECTION",         "0",            "1 (50.0)",                  "0",          "NESTED",                          "INFECTIONS AND INFESTATIONS",     1,         2,       19L,        0,        0,
    "URINARY TRACT INFECTION",  "1 (33.3)",                   "0",                  "0",          "NESTED",                          "INFECTIONS AND INFESTATIONS",     1,         2,       20L,        0,        0,
    "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",         "0",            "1 (50.0)",                  "0",          "NESTED",      "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",     1,         1,       21L,        0,        0,
    "FLANK PAIN",         "0",            "1 (50.0)",                  "0",          "NESTED",      "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",     1,         2,       22L,        0,        0,
    "PSYCHIATRIC DISORDERS",         "0",            "1 (50.0)",                  "0",          "NESTED",                                "PSYCHIATRIC DISORDERS",     1,         1,       23L,        0,        0,
    "DEPRESSED MOOD",         "0",            "1 (50.0)",                  "0",          "NESTED",                                "PSYCHIATRIC DISORDERS",     1,         2,       24L,        0,        0,
    "RENAL AND URINARY DISORDERS",         "0",            "1 (50.0)",                  "0",          "NESTED",                          "RENAL AND URINARY DISORDERS",     1,         1,       25L,        0,        0,
    "CALCULUS URETHRAL",         "0",            "1 (50.0)",                  "0",          "NESTED",                          "RENAL AND URINARY DISORDERS",     1,         2,       26L,        0,        0,
    "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",  "2 (66.7)",            "1 (50.0)",           "1 (33.3)",          "NESTED",      "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",     1,         1,       27L,        0,        0,
    "COUGH",  "1 (33.3)",                   "0",                  "0",          "NESTED",      "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",     1,         2,       28L,        0,        0,
    "EPISTAXIS",         "0",            "1 (50.0)",                  "0",          "NESTED",      "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",     1,         2,       29L,        0,        0,
    "NASAL CONGESTION",  "1 (33.3)",                   "0",           "1 (33.3)",          "NESTED",      "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",     1,         2,       30L,        0,        0,
    "PHARYNGOLARYNGEAL PAIN",         "0",                   "0",           "1 (33.3)",          "NESTED",      "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",     1,         2,       31L,        0,        0,
    "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",  "2 (66.7)",            "1 (50.0)",           "2 (66.7)",          "NESTED",               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",     1,         1,       32L,        0,        0,
    "ACTINIC KERATOSIS",         "0",            "1 (50.0)",                  "0",          "NESTED",               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",     1,         2,       33L,        0,        0,
    "ERYTHEMA",  "1 (33.3)",                   "0",           "2 (66.7)",          "NESTED",               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",     1,         2,       34L,        0,        0,
    "PRURITUS",  "1 (33.3)",                   "0",           "1 (33.3)",          "NESTED",               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",     1,         2,       35L,        0,        0,
    "PRURITUS GENERALISED",         "0",                   "0",           "1 (33.3)",          "NESTED",               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",     1,         2,       36L,        0,        0,
    "URTICARIA",         "0",                   "0",           "1 (33.3)",          "NESTED",               "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",     1,         2,       37L,        0,        0
  )
  expected$AEBODSYS <- factor(expected$AEBODSYS)

  expect_equal(

      nested_freq(cdisc_adae,
                  rowvar = "AEBODSYS*AEDECOD",
                  colvar = "TRTA",
                  tablebyvar = "SEX",
                  cutoff = 10,
                  statlist = statlist("n (x.x)", distinct = TRUE, denoms_by = c("TRTA", "SEX"))) %>%
        bind_table(tablebyvar = "SEX", colvar = "TRTA"),
      expected,
      ignore_attr = TRUE
  )
})
