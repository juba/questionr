\name{rprop}
\alias{lprop}
\alias{rprop}
\title{Row percentages of a two-way frequency table.}
\usage{
  rprop(tab, digits = 1, total = TRUE, percent = FALSE,
    drop = TRUE)
}
\arguments{
  \item{tab}{frequency table}

  \item{digits}{number of digits to display}

  \item{total}{if \code{TRUE}, add a column with the sum of
  percentages and a row with global percentages}

  \item{percent}{if \code{TRUE}, add a percent sign after
  the values when printing}
}
\value{
  The result is an object of class \code{table} and
  \code{proptab}.
}
\description{
  Return the row percentages of a two-way frequency table
  with formatting and printing options.
}
\examples{
## Sample table
data(Titanic)
tab <- apply(Titanic, c(1,4), sum)
## Column percentages
rprop(tab)
## Column percentages with custom display
rprop(tab, digits=2, percent=TRUE, total=FALSE)
}
\seealso{
  \code{\link[questionr]{cprop}},
  \code{\link[questionr]{prop}}, \code{\link{table}},
  \code{\link{prop.table}}
}

