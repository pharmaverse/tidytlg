#' SAS rounding in R
#'
#' roundSAS is an alternative rounding function, ensuring that decimals equal or
#' bigger than 5 are rounded upwards to the nearest number and returned as
#' character vector.
#'
#' At the midpoint of a decimal place (e.g. 0.5, 1.5), the round function in R
#' rounds to the nearest even number (i.e. 0.5 is rounded to 0; 1.5 is rounded
#' to 2), whereas SAS rounds to the nearest number (i.e. 0.5 is rounded to 1;
#' 1.5 is rounded to 2). The roundSAS function is an alternative rounding
#' function for R that ensures rounding to the nearest number, as done in SAS.
#' roundSAS comes from this Stack Overflow post https://stackoverflow.com/questions/12688717/round-up-from-5
#'
#' @param x Numeric vector.
#' @param digits An integer specifying the number of decimal places to be
#'   displayed after rounding. Default is 0.
#' @param as_char logical value indicating conversion of rounded numerical
#'   vector to character vector; default is FALSE
#' @param na_char A character string indicating missing value; if not specified,
#'   "NA" is created
#'
#' @return character vector of rounded values
#' @export
#'
#' @examples
#' ### input data vector with midpoint decimals
#' x <- c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)
#'
#' ### rounds to integer
#' roundSAS(x, digits = 0)
#'
#' ### input data vector with a missing value
#' y <- c(8.65, 8.75, NA, 9.85, 9.95)
#'
#' ### rounds to tenths and label the missing value with "NE"
#' roundSAS(y, digits = 1, as_char = TRUE, na_char = "NE")
#'
roundSAS <- function(x,
                     digits = 0,
                     as_char = FALSE,
                     na_char = NULL) {

  # perform SAS rounding ----------------------------------------------------
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^digits
  z <- z * posneg

  # output rounded values ---------------------------------------------------
  if (as_char & is.null(na_char)) {

    ## convert rounded values to character vector
    formatC(z, format = "f", digits = digits)

  } else if (as_char & !is.null(na_char)) {

    ## convert to character vector and use na_char for missing value
    formatC(z, format = "f", digits = digits) %>%
      str_replace(" *(NA|NaN|NULL)", na_char)

  } else {
    ## return numeric vector of rounded values
    z
  }
}
