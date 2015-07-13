\name{chisq.residuals}
\alias{chisq.residuals}
\alias{residus}
\title{Return the chi-squared residuals of a two-way frequency table.}
\usage{
chisq.residuals(tab, digits = 2, std = FALSE, raw = FALSE)
}
\arguments{
  \item{tab}{frequency table}

  \item{digits}{number of digits to display}

  \item{std}{if \code{TRUE}, returns the standardized
  residuals. Otherwise, returns the Pearson residuals.
  Incompatible with \code{raw}.}

  \item{raw}{if \code{TRUE}, returns the raw
  (\code{observed - expected}) residuals. Otherwise,
  returns the Pearson residuals. Incompatible with
  \code{std}.}
}
\description{
Return the raw, standardized or Pearson's residuals (the
default) of a chi-squared test on a two-way frequency
table.
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
chisq.residuals(tab, std = TRUE)
## Raw residuals
chisq.residuals(tab, raw = TRUE)
}
\seealso{
\code{\link{chisq.test}}
}

