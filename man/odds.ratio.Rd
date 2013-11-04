\name{odds.ratio}
\alias{odds.ratio}
\alias{odds.ratio.glm}
\alias{odds.ratio.multinom}
\title{Odds Ratio}
\usage{
  odds.ratio(x, ...)

  \method{odds.ratio}{glm} (x, level = 0.95, digits = 3)

  \method{odds.ratio}{multinom} (x, level = 0.95,
    digits = 3)
}
\arguments{
  \item{x}{object from whom odds ratio will be computed}

  \item{...}{additional parameters}

  \item{level}{the confidence level required}

  \item{digits}{number of decimal to display}
}
\value{
  For \code{glm} or \code{multinom} objects, returns odds
  ratios, their confidence interval and tests if they
  differ from 1.
}
\description{
  S3 method for odds ratio
}
\details{
  Odds ratio could also be obtained with
  \code{exp(coef(x))} and confidence intervals with
  \code{exp(confint(x))}.

  For models calculated with \code{glm}, p-value are the
  same as \code{summary(x)$coefficients[,4]}.

  For models calculated with \code{multinom} (nnet),
  p-value are calculated according to
  \url{http://www.ats.ucla.edu/stat/r/dae/mlogit.htm}.
}
\examples{
data(hdv2003)
reg <- glm(cinema ~ sexe + age, data=hdv2003, family=binomial)
odds.ratio(reg)
}
\author{
  Joseph Larmarange <joseph@larmarange.net>
}
\seealso{
  \code{\link{glm}} in the \link{stats} package.

  \code{\link[nnet]{multinom}} in the \link[nnet]{nnet}
  package.
}

