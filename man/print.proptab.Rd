\name{print.proptab}
\alias{print.proptab}
\title{S3 print method for proptab objects.}
\usage{
  \method{print}{proptab} (x, digits = NULL,
    percent = NULL, justify = "right", ...)
}
\arguments{
  \item{x}{object of class proptab}

  \item{digits}{number of digits to display}

  \item{percent}{if not NULL, add a percent sign after each
  value}

  \item{justify}{justification of character vectors. Passed
  to \code{format.default}}

  \item{...}{other arguments to pass to
  \code{format.default}}
}
\description{
  Print an object of class proptab.
}
\seealso{
  \code{\link[questionr]{format.proptab}}
}

