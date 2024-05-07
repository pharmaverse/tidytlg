library(dplyr)
devtools::load_all(".")

tbl <- data.frame(
  label = "Subjects with ≥ 1 concomitant medication",
  col1 = "1 (100.0%)",
  col2 = "1 (100.0%)",
  col3 = "2 (100.0%)",
  col4 = "1 (100.0%)",
  col5 = "3 (100.0%)",
  row_type = "VALUE"
)

# No borders is the default
gentlg(
  huxme = tbl,
  orientation = "landscape",
  file = "TCM01a",
  title = "Summary of Concomitant Medications",
  colspan = list(
    c("", "Active Study Agent", "Active Study Agent", "Active Study Agent", "", ""),
    c("", "Treatment 1", "Treatment 2", "Combined", "Placebo", "Total")
  ),
  colheader = c(" Standardized medication name", "N=1", "N=1", "N=2", "N=1", "N=3"),
  wcol = .30
)

# Creating a bottom border matrix
bottom_borders <- matrix(c(rep(1, 6), rep(0, 30)), nrow = 6, ncol = 6)
print(bottom_borders)
# Borders in the first column only
gentlg(
  huxme = tbl,
  orientation = "landscape",
  file = "TCM01a",
  title = "Summary of Concomitant Medications",
  colspan = list(
    c("", "Active Study Agent", "Active Study Agent", "Active Study Agent", "", ""),
    c("", "Treatment 1", "Treatment 2", "Combined", "Placebo", "Total")
  ),
  colheader = c(" Standardized medication name", "N=1", "N=1", "N=2", "N=1", "N=3"),
  wcol = .30,
  bottom_borders = list(bottom_borders)
)

# Split bottom borders for the treatment groups
bottom_borders <- matrix(c(rep(0, 6), rep(0, 30)), nrow = 6, ncol = 6)
bottom_borders[3, 2] <- 2
bottom_borders[3, 3] <- 3
bottom_borders[3, 4] <- 4
bottom_borders[3, 5] <- 5
print(bottom_borders)

gentlg(
  huxme = tbl,
  orientation = "landscape",
  file = "TCM01a",
  title = "Summary of Concomitant Medications",
  colspan = list(
    c("", "Active Study Agent", "Active Study Agent", "Active Study Agent", "", ""),
    c("", "Treatment 1", "Treatment 2", "Combined", "Placebo", "Total")
  ),
  colheader = c(" Standardized medication name", "N=1", "N=1", "N=2", "N=1", "N=3"),
  wcol = .30,
  bottom_borders = list(bottom_borders)
)

# Borders under treatment and Active Study Agent title
bottom_borders <- matrix(c(rep(0, 6), rep(0, 30)), nrow = 6, ncol = 6)
bottom_borders[2, 2] <- 1
bottom_borders[3, 2] <- 2
bottom_borders[3, 3] <- 3
bottom_borders[3, 4] <- 4
bottom_borders[3, 5] <- 5
print(bottom_borders)

gentlg(
  huxme = tbl,
  orientation = "landscape",
  file = "TCM01a",
  title = "Summary of Concomitant Medications",
  colspan = list(
    c("", "Active Study Agent", "Active Study Agent", "Active Study Agent", "", ""),
    c("", "Treatment 1", "Treatment 2", "Combined", "Placebo", "Total")
  ),
  colheader = c(" Standardized medication name", "N=1", "N=1", "N=2", "N=1", "N=3"),
  wcol = .30,
  bottom_borders = list(bottom_borders)
)

# Continuous border instead of gaps - border issue
bottom_borders <- matrix(c(rep(0, 6), rep(0, 30)), nrow = 6, ncol = 6)
bottom_borders[2, 2] <- 1
bottom_borders[3, 2] <- 2
bottom_borders[3, 3] <- 2
bottom_borders[3, 4] <- 3
bottom_borders[3, 5] <- 3
print(bottom_borders)

gentlg(
  huxme = tbl,
  orientation = "landscape",
  file = "TCM01a",
  colspan = list(
    c("", "Active Study Agent", "Active Study Agent", "Active Study Agent", "", ""),
    c("", "Treatment 1", "Treatment 2", "Combined", "Placebo", "Total")
  ),
  colheader = c(" Standardized medication name", "N=1", "N=1", "N=2", "N=1", "N=3"),
  wcol = .30,
  bottom_borders = list(bottom_borders)
)

# No border issue because the border "islands" do not neighbour each other
bottom_borders <- matrix(c(rep(0, 6), rep(0, 30)), nrow = 6, ncol = 6)
bottom_borders[2, 2] <- 1
bottom_borders[3, 2] <- 2
bottom_borders[3, 3] <- 2
bottom_borders[3, 5] <- 3
bottom_borders[3, 6] <- 3
print(bottom_borders)

gentlg(
  huxme = tbl,
  orientation = "landscape",
  file = "TCM01a",
  colspan = list(
    c("", "Active Study Agent", "Active Study Agent", "Active Study Agent", "", ""),
    c("", "Treatment 1", "Treatment 2", "Combined", "Placebo", "Total")
  ),
  colheader = c(" Standardized medication name", "N=1", "N=1", "N=2", "N=1", "N=3"),
  wcol = .30,
  bottom_borders = list(bottom_borders)
)
