\name{odds.ratio}
\alias{odds.ratio}
\title{Odds Ratios for logistic regression}
\usage{
  odds.ratio(reg, level = 0.95, digits = 3)
}
\arguments{
  \item{reg}{an object of class \code{"glm"} (with
  \code{family=binomial}) or \code{"multinom"}}

  \item{level}{the confidence level required}

  \item{digits}{number of decimal to display}
}
\description{
  Computes odds ratios (the exponential of model
  coefficients), their confidence intervals and tests if
  they differ from 1.
}
\details{
  Odds ratio could also be obtained with
  \code{exp(coef(reg))} and confidence intervals with
  \code{exp(confint(reg))}.

  For models calculated with \code{glm}, p-value are the
  same as \code{summary(reg)$coefficients[,4]}. For models
  calculated with \code{multinom} (nnet), p-value are
  calculated according to
  \url{http://www.ats.ucla.edu/stat/r/dae/mlogit.htm}.
}
\examples{
data(hdv2003)
reg <- glm(cinema ~ sexe + age, data=hdv2003, family=binomial)
odds.ratio(reg)
}
\author{
  Joseph Larmarange <joseph
}
\seealso{
  \code{\link{glm}} in the \link{stats} package and
  \code{\link[nnet]{multinom}} in the \link[nnet]{nnet}
  package.
}

