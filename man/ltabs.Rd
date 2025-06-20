% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{ltabs}
\alias{ltabs}
\title{Cross tabulation with labelled variables}
\usage{
ltabs(
  formula,
  data,
  levels = c("prefixed", "labels", "values"),
  variable_label = TRUE,
  ...
)
}
\arguments{
\item{formula}{a formula object (see \code{\link[stats]{xtabs}})}

\item{data}{a data frame}

\item{levels}{the desired levels in case of labelled vector:
"labels" for value labels, "values" for values or "prefixed" for labels prefixed with values}

\item{variable_label}{display variable label if available?}

\item{...}{additional arguments passed to \code{\link[stats]{xtabs}}}
}
\description{
This function is a wrapper around \code{\link[stats]{xtabs}}, adding automatically
value labels for labelled vectors if \pkg{labelled} package eis installed.
}
\examples{
data(fecondite)
ltabs(~radio, femmes)
ltabs(~ radio + tv, femmes)
ltabs(~ radio + tv, femmes, "l")
ltabs(~ radio + tv, femmes, "v")
ltabs(~ radio + tv + journal, femmes)
ltabs(~ radio + tv, femmes, variable_label = FALSE)
}
\seealso{
\code{\link[stats]{xtabs}}.
}
