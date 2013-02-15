\name{copy.default}
\alias{copie}
\alias{copie.default}
\alias{copy}
\alias{copy.default}
\title{Transform an object into HTML and copy it for export}
\usage{
  \method{copy}{default} (obj, append = FALSE,
    file = FALSE, filename = "temp.html",
    clipboard.size = 4096, ...)
}
\arguments{
  \item{obj}{object to be copied}

  \item{append}{if TRUE, append to the file instead of
  replacing it}

  \item{file}{if TRUE, export to a file instead of the
  clipboard}

  \item{filename}{name of the file to export to}

  \item{clipboard.size}{under Windows, size of the
  clipboard in kB}

  \item{...}{arguments passed to \code{R2HTML::HTML}}
}
\description{
  This function transforms its argument to HTML and then
  copy it to the clipboard or to a file for later use in an
  external application.
}
\details{
  Under linux, this function requires that \code{xclip} is
  installed on the system to copy to the clipboard.
}
\examples{
data(iris)
tab <- table(cut(iris$Sepal.Length,8),cut(iris$Sepal.Width,4))
\dontrun{copie(tab)}
}
\seealso{
  \code{\link[R2HTML]{HTML}},
  \code{\link[questionr]{copy.proptab}}
}
\keyword{connection}

