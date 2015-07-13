\name{format.proptab}
\alias{format.proptab}
\title{S3 format method for proptab objects.}
\usage{
\method{format}{proptab}(x, digits = NULL, percent = NULL,
  justify = "right", ...)
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
Format an object of class proptab for printing depending on
its attributes.
}
\details{
This function is designed for internal use only.
}
\seealso{
\code{\link{format.default}},
\code{\link[questionr]{print.proptab}}
}

