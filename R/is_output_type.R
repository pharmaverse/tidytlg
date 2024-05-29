is_listing <- function(type) {
  substr(tolower(type), 1, 1) == "l"
}

is_table <- function(type) {
  substr(tolower(type), 1, 1) == "t"
}

is_graph <- function(type) {
  substr(tolower(type), 1, 1) %in% c("g", "f")
}

is_format_rtf <- function(format) {
  tolower(format) == "rtf"
}

is_format_html <- function(format) {
  tolower(format) == "html"
}
