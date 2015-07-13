\name{wtd.mean}
\alias{wtd.mean}
\alias{wtd.var}
\title{Weighted mean and variance of a vector}
\usage{
wtd.mean(x, weights = NULL, normwt = "ignored", na.rm = TRUE)
}
\arguments{
  \item{x}{Numeric data vector}

  \item{weights}{Numeric weights vector. Must be the same
  length as \code{x}}

  \item{normwt}{Only for \code{wtd.var}, if \code{TRUE}
  then weights are normalized for the weighted count to be
  the same as the non-weighted one}

  \item{na.rm}{if \code{TRUE}, delete \code{NA} values.}
}
\description{
Compute the weighted mean or weighted variance of a vector.
}
\details{
If \code{weights} is \code{NULL}, then an uniform weighting
is applied.
}
\examples{
data(hdv2003)
mean(hdv2003$age)
wtd.mean(hdv2003$age, weights=hdv2003$poids)
var(hdv2003$age)
wtd.var(hdv2003$age, weights=hdv2003$poids)
}
\author{
These functions are exact copies of the \code{wtd.mean} and
\code{wtd.var} function from the \link[Hmisc]{wtd.stats}
package. They have been created by Frank Harrell,
Department of Biostatistics, Vanderbilt University School
of Medicine, <f.harrell@vanderbilt.edu>.
}
\seealso{
\code{\link{mean}},\code{\link{var}},
\code{\link{wtd.table}} and the \link{survey} package.
}

