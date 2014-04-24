\name{icut}
\alias{icut}
\title{Interactive conversion from numeric to factor}
\usage{
  icut(dfobject, oldvar)
}
\arguments{
  \item{dfobject}{data frame to operate on, as an object or
  a character string}

  \item{oldvar}{name of the variable to be recoded, as a
  character string (possibly without quotes)}
}
\value{
  The function launches a shiny app in the system web
  browser. The recoding code is returned in the console
  when the app is closed with the "Done" button.
}
\description{
  This function launches a shiny app in a web browser in
  order to do interactive conversion of a numeric variable
  into a categorical one.
}
\examples{
\dontrun{data(hdv2003)
icut(hdv2003, "age")
irec(hdv2003, heures.tv) ## this also works}
}
\author{
  Julien Barnier <julien.barnier@ens-lyon.fr>
}

