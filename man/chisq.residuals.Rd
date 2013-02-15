\name{chisq.residuals}
\alias{chisq.residuals}
\alias{residus}
\title{Return the chi-squared residuals of a two-way frequency table.}
\usage{
  chisq.residuals(tab, digits = 2, std = FALSE)
}
\arguments{
  \item{tab}{frequency table}

  \item{digits}{number of digits to display}

  \item{std}{if \code{TRUE}, returns the standardizes
  residuals. Otherwise, returns the Pearson residuals.}
}
\description{
  Return the standardized or Pearson's residuals of a
  chi-squared test on a two-way frequency table.
}
\details{
  This function is just a wrapper around the
  \code{\link{chisq.test}} base R function. See this
  function's help page for details on the computation.
}
\examples{
## Sample table
data(Titanic)
tab <- apply(Titanic, c(1,4), sum)
## Pearson residuals
chisq.residuals(tab)
## Standardized residuals
chisq.residuals(tab, std=TRUE)
}
\seealso{
  \code{\link{chisq.test}}
}

