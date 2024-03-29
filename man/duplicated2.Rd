% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{duplicated2}
\alias{duplicated2}
\title{Determine all duplicate elements}
\source{
\url{https://forums.cirad.fr/logiciel-R/viewtopic.php?p=2968}
}
\usage{
duplicated2(x)
}
\arguments{
\item{x}{a vector, a data frame or a matrix}
}
\value{
A logical vector indicated wich elements are duplicated in \code{x}.
}
\description{
The native \link{duplicated} function determines which elements of a vector
or data frame are duplicates of elements already observed in the vector or the
data frame provided. Therefore, only the second occurence (or third or nth)
of an element is considered as a duplicate.
\code{duplicated2} is similar but will also mark the first occurence as a
duplicate (see examples).
}
\examples{
df <- data.frame(x = c("a", "b", "c", "b", "d", "c"), y = c(1, 2, 3, 2, 4, 3))
df
duplicated(df)
duplicated2(df)
}
\seealso{
\link{duplicated}
}
