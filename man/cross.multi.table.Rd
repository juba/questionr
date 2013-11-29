\name{cross.multi.table}
\alias{cross.multi.table}
\title{Two-way frequency table between a multiple choices question and a factor}
\usage{
  cross.multi.table(df, crossvar, weights = NULL, ...)
}
\arguments{
  \item{df}{data frame with the binary variables}

  \item{crossvar}{factor to cross the multiple choices
  question with}

  \item{weights}{optional weighting vector}

  \item{...}{arguments passed to \code{multi.table}}
}
\value{
  Object of class table.
}
\description{
  This function allows to generate a two-way frequency
  table from a multiple choices question and a factor. The
  question's answers must be stored in a series of binary
  variables.
}
\details{
  See the \code{multi.table} help page for details on
  handling of the multiple choices question and
  corresponding binary variables.
}
\examples{
## Sample data frame
sex <- sample(c("Man","Woman"),100,replace=TRUE)
jazz <- sample(c(0,1),100,replace=TRUE)
rock <- sample(c(TRUE, FALSE),100,replace=TRUE)
electronic <- sample(c("Y","N"),100,replace=TRUE)
weights <- runif(100)*2
df <- data.frame(sex,jazz,rock,electronic,weights)
## Two-way frequency table on 'music' variables by sex
cross.multi.table(df[,c("jazz", "rock","electronic")], df$sex, true.codes=list("Y"))
}
\seealso{
  \code{\link[questionr]{multi.table}},
  \code{\link[questionr]{multi.split}}, \code{\link{table}}
}

