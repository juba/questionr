\name{qscan}
\alias{qscan}
\title{Scan R scripts and load/install all detected packages}
\usage{
  qscan(..., load = TRUE, detail = TRUE)
}
\arguments{
  \item{...}{the scripts to scan. Defaults to all R scripts
  in the current working directory.}

  \item{load}{quick-load/install the cited packages (see
  details). Defaults to \code{TRUE}.}

  \item{detail}{show the list of packages found in each
  script. Defaults to \code{TRUE}.}
}
\value{
  The result is a list of packages cited in the scripts.
}
\description{
  This function scans one or more R scripts and tries to
  quick-load/install the packages mentioned by
  \code{library} or \code{require} functions.
}
\details{
  The function calls the \code{qload} function to
  quick-load/install the packages.
}
\examples{
## Scan the working directory.
\dontrun{qscan()}
}
\author{
  François Briatte <f.briatte@gmail.com>
}
\seealso{
  \code{\link[questionr]{qload}}, \code{\link{library}}
}

