% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode.R
\name{recode.na}
\alias{recode.na}
\title{Recode values of a variable to missing values, using exact or regular expression matching.}
\usage{
recode.na(x, ..., verbose = FALSE, regex = TRUE, as.numeric = FALSE)
}
\arguments{
\item{x}{variable to recode. The variable is coerced to a factor if necessary.}

\item{...}{levels to recode as missing in the variable. The values are coerced to character strings, meaning that you can pass numeric values to the function.}

\item{verbose}{print a table of missing levels before recoding them as missing. Defaults to \code{FALSE}.}

\item{regex}{use regular expressions to match values that include the "*" or "|" wildcards. Defaults to \code{TRUE}.}

\item{as.numeric}{coerce the recoded variable to \code{numeric}. The function recommends the option when the recode returns only numeric values. Defaults to FALSE.}
}
\value{
The result is a factor with properly encoded missing values. If the recoded variable contains only numeric values, it is converted to an object of class \code{numeric}.
}
\description{
This function recodes selected values of a quantitative or qualitative
variable by matching its levels to exact or regular expression matches.
}
\examples{
data(hdv2003)
## With exact string matches.
hdv2003$nivetud <- recode.na(hdv2003$nivetud, "Inconnu")
## With regular expressions.
hdv2003$relig <- recode.na(hdv2003$relig, "[A|a]ppartenance", "Rejet|NSP")
## Showing missing values.
hdv2003$clso <- recode.na(hdv2003$clso, "Ne sait pas", verbose = TRUE)
## Test results with freq.
freq(recode.na(hdv2003$trav.satisf, "Equilibre"))
## Truncate a count variable (recommends numeric conversion).
freq(recode.na(hdv2003$freres.soeurs, 5:22))
}
\seealso{
\code{\link{regex}}
}
\author{
François Briatte <f.briatte@gmail.com>
}
