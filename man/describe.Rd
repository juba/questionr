\name{describe}
\alias{describe}
\title{Describe the variables of a dataset in R, SPSS or Stata formats}
\usage{
  describe(data, ...)
}
\arguments{
  \item{data}{dataset}

  \item{...}{variable names as character strings. Using the
  "*" or "|" wildcards in a variable name will search for
  it using a regex match.}
}
\description{
  This function describes the variables of a dataset that
  might include labels imported with the foreign or memisc
  packages.
}
\details{
  This function wraps around the \code{description}
  function of the \code{memisc} package and fetches the
  variable labels in datasets imported with the
  \code{foreign} package.
}
\examples{
# Variables in a standard R data frame.
data(hdv2003)
# Find the names of selected variables.
describe(hdv2003, "age", "sexe", "lecture*", "frere|soeur")
# Load memisc package and example data.
require(memisc)
nes1948.por <- UnZip("anes/NES1948.ZIP","NES1948.POR", package="memisc")
nes1948 <- spss.portable.file(nes1948.por)
# Get the labels of selected variables.
describe(nes1948, "v480050", "version|set", "v48004*")
}
\seealso{
  \code{\link{regex}}, \code{\link{description}} in the
  \code{\link{memisc}} package
}

