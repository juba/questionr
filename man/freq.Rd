\name{freq}
\alias{freq}
\title{Generate frequency tables.}
\usage{
  freq(x, digits = 1, cum = FALSE, total = FALSE,
    exclude = NULL, sort = "")
}
\arguments{
  \item{x}{either a vector to be tabulated, or a table
  object}

  \item{digits}{number of digits to keep for the
  percentages}

  \item{cum}{if TRUE, display cumulative percentages}

  \item{total}{if TRUE, add a final row with totals}

  \item{exclude}{vector of values to exclude from the
  tabulation}

  \item{sort}{if specified, allow to sort the table by
  increasing ("inc") or decreasing ("dec") frequencies}
}
\value{
  The result is an object of class data.frame.
}
\description{
  Generate and format frequency tables from a variable or a
  table, with percentages and formatting options.
}
\seealso{
  \code{\link{table}}, \code{\link[questionr]{prop}},
  \code{\link[questionr]{cprop}},
  \code{\link[questionr]{rprop}}
}

