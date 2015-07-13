\name{cramer.v}
\alias{cramer.v}
\title{Compute Cramer's V of a two-way frequency table}
\usage{
cramer.v(tab)
}
\arguments{
  \item{tab}{table on which to compute the statistic}
}
\description{
This function computes Cramer's V for a two-way frequency
table
}
\examples{
data(Titanic)
tab <- apply(Titanic, c(4,1), sum)
#' print(tab)
cramer.v(tab)
}
\keyword{univar}

