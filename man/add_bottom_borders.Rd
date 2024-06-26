% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_bottom_borders.R
\name{add_bottom_borders}
\alias{add_bottom_borders}
\title{Adds bottom borders to a huxtable}
\usage{
add_bottom_borders(ht, border_matrix = no_borders(ht), transform_fns = list())
}
\arguments{
\item{ht}{\code{huxtable} A huxtable object}

\item{border_matrix}{(optional) \code{matrix} A matrix indicating where to add the bottom
borders. If \code{NULL}, then no borders are added.}

\item{transform_fns}{(optional) \code{list} of \code{function} A list of functions applied to the
\code{border_matrix}. The functions have to accept two arguments:
\enumerate{
\item The \code{huxtable}.
\item The \code{border_matrix} with dimentions matching \code{huxtable}.
}

The functions in the list are applied sequentially to \code{border_matrix}.}
}
\value{
A huxtable with added borders.
}
\description{
Adds bottom borders to a huxtable
}
\details{
Adds bottom borders to a huxtable based on
a matrix indicating where the borders should be put.

This function is responsible for adding bottom borders to a \code{huxtable} object.
It supports borders spanning multiple columns and borders that are under neighbouring,
single cells (or merged cells), but separate (see examples).

This feature has limitations. Mainly, it does not support both versions of the borders
(continuous and separate) on the same line. In such a case, the borders in the resulting
RTF look misaligned.
}
\section{\code{border_matrix} details}{

You mark where the bottom borders should go in the table by passing a matrix.
The matrix has to have the same number of columns as the passed \code{huxtable}
and the number of rows lower by one than the passed \code{huxtable}. Each cell
in \code{border_matrix} corresponds to a cell in \code{huxtable} (starting from the first row).

Internally, the function adds the first row of 0s to \code{border_matrix} before the execution.
At that point, \code{border_matrix}'s dimensions match \code{ht}'s dimensions.

Table:\tabular{ll}{
   foo \tab bar \cr
   baz \tab bim \cr
}


A border matrix:\tabular{ll}{
   1 \tab 1 \cr
   0 \tab 0 \cr
}


The above border matrix puts a bottom border across the entire first row
and no borders in the second row.

A border matrix:\tabular{ll}{
   1 \tab 2 \cr
   0 \tab 0 \cr
}


The above border matrix puts one border under the first cell in the first row;
and another border (separate from the first one) under the second cell
in the first row. The second row stays without any borders.
}

\section{Functions transforming the border matrix}{


The below functions can be passed to \code{\link[=gentlg]{gentlg()}}'s
\code{border_fns} argument to modify how \code{gentlg} renders
the borders under the cells.

Border functions:
\itemize{
\item \code{\link[=no_borders]{no_borders()}}
\item \code{\link[=spanning_borders]{spanning_borders()}}
\item \code{\link[=col_borders]{col_borders()}}
\item \code{\link[=single_border]{single_border()}}
\item \code{\link[=row_border]{row_border()}}
}

\code{border_fns} will accept your own, custom functions as long as
they adhere to the format.
All the functions passed to \code{border_fns} need to accept two arguments:
\itemize{
\item the first - the printed huxtable object,
\item the second - a border matrix.
}

They also must return a matrix interpreted the same way as \code{border_matrix}
passed to \code{add_bottom_borders} or \code{\link[=gentlg]{gentlg()}}.
}

\examples{
border_matrix <- matrix(c(1, 1, 2, 0, 1, 1, 0, 0, 0), nrow = 3, ncol = 3)
ht <- huxtable::as_huxtable(
  data.frame(a = c(1, 2, 3), b = c("a", "b", "c"), c = c(TRUE, FALSE, TRUE))
)
# By default adds no borders
add_bottom_borders(ht, border_matrix)
# Adds spanning borders under cells with text in the second row
add_bottom_borders(ht, transform_fns = list(spanning_borders(2)))
# Adds spanning borders under cells with text in the second row and a border
# under a cell in row 3 and column 3
add_bottom_borders(ht, transform_fns = list(spanning_borders(2), single_border(3, 3)))

final <- data.frame(
  label = c(
    "Overall", "Safety Analysis Set",
    "Any Adverse event{\\\\super a}", "- Serious Adverse Event"
  ),
  Drug_A = c("", "40", "10 (25\%)", "0"),
  Drug_B = c("", "40", "10 (25\%)", "0"),
  anbr = c(1, 2, 3, 4),
  roworder = c(1, 1, 1, 1),
  boldme = c(1, 0, 0, 0),
  newrows = c(0, 0, 1, 0),
  indentme = c(0, 0, 0, 1),
  newpage = c(0, 0, 0, 0)
)
# Add spanning bottom borders under the cells in the first row
gentlg(
  huxme = final,
  wcol = c(0.70, 0.15, 0.15),
  file = "TSFAEX",
  colheader = c("", "Drug A", "Drug B"),
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\\\super a} Subjects are counted once for any given event."
  ),
  border_fns = list(no_borders, spanning_borders(1))
)

# Tables with no bottom borders
gentlg(
  huxme = final,
  wcol = c(0.70, 0.15, 0.15),
  file = "TSFAEX",
  colheader = c("", "Drug A", "Drug B"),
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\\\super a} Subjects are counted once for any given event."
  ),
  border_fns = list(no_borders)
)

# Tables with a border under cell in the 3nd row and 3rd column,
# and borders under cells in the first row
gentlg(
  huxme = final,
  wcol = c(0.70, 0.15, 0.15),
  file = "TSFAEX",
  colheader = c("", "Drug A", "Drug B"),
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\\\super a} Subjects are counted once for any given event."
  ),
  border_fns = list(no_borders, spanning_borders(1), single_border(3, 3))
)

# We discourage, but you can pass the border matrix directly
mat <- matrix(rep(0, 8 * 3), ncol = 3, nrow = 8)
mat[3, 3] <- 1
gentlg(
  huxme = final,
  wcol = c(0.70, 0.15, 0.15),
  file = "TSFAEX",
  colheader = c("", "Drug A", "Drug B"),
  title = "This is Amazing Demonstration 1",
  footers = c(
    "Note: For demonstrative purposes only",
    "{\\\\super a} Subjects are counted once for any given event."
  ),
  bottom_borders = mat, # The same as a single border under 3nd row and 3rd column
  border_fns = list()
)

# clean up.
file.remove("tsfaex.rtf")

}
