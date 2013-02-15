\name{copy.proptab}
\alias{copie.proptab}
\alias{copy.proptab}
\title{Export of a proptab object to HTML}
\usage{
  \method{copy}{proptab} (obj, percent = NULL,
    digits = NULL, justify = "right", ...)
}
\arguments{
  \item{obj}{object to be transformed}

  \item{percent}{whether to add a percent sign in each
  cell}

  \item{digits}{number of digits to display}

  \item{justify}{justification}

  \item{...}{arguments passed to the \code{copy} function}
}
\description{
  Applies the \code{copy} function to a \code{proptab}
  object
}
\details{
  For more informations on arguments, look at the
  \code{format.proptab} and \code{copy} help pages.
}
\examples{
data(iris)
tab <- table(cut(iris$Sepal.Length,8),cut(iris$Sepal.Width,4))
ptab <- rprop(tab, percent=TRUE)
\dontrun{copy(ptab)}
}
\seealso{
  \code{\link[questionr]{copy}},
  \code{\link[questionr]{format.proptab}}
}
\keyword{connection}

