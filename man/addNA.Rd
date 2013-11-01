\name{addNA}
\alias{addNA}
\title{Transform missing values of a factor to an extra level}
\source{
  Adapted from James
  (\url{http://stackoverflow.com/a/5817181}) by Joseph
  Larmarange <joseph@larmarange.net>
}
\usage{
  addNA(x, ifany = FALSE, as.string = FALSE)
}
\arguments{
  \item{x}{a vector of data, usually taking a small number
  of distinct values.}

  \item{ifany}{only add an \code{NA} level if it is used,
  i.e. if \code{any(is.na(x)}.}

  \item{as.string}{should the extra level name be a string
  (\code{"NA"}) instead of \code{NA} itself. Could also be
  a string to use for the name of this extra level}
}
\value{
  an object of class \code{"factor"}, original missing
  values being coded as an extra level named \code{NA} if
  \code{as.string=FALSE}, \code{"NA"} if
  \code{as.string=TRUE}, as specified by \code{as.string}
  if \code{as.string} is a string.
}
\description{
  This function modifies a factor by turning \code{NA} into
  an extra level (so that \code{NA} values are counted in
  tables, for instance). This version of \code{addNA}
  extends the same function provided in \R by allowing to
  ask for a string name for the extra level (see examples).
}
\examples{
f <- as.factor(c("a","b",NA,"a","b"))
f
addNA(f)
addNA(f, as.string=TRUE)
addNA(f, as.string="missing")
}
\seealso{
  \code{\link[base]{addNA}} (base).
}

