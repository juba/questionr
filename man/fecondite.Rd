\docType{data}
\name{fecondite}
\alias{fecondite}
\title{A fertility survey}
\format{3 data frames with labelled data (as if data would have been imported from SPSS with \pkg{haven}):
   \itemize{
     \item \code{menages} contains some information from the households selected for the survey;
     \item \code{femmes} contains the questionnaire administered to all 15-49 years old women
       living in the selected households;
     \item\code{enfants} contains one record for each child of the surveyed women.
   }

   Data can be linked using the variables \code{id_menage} and \code{id_femme}.}
\description{
Some fictive results from a fecondity survey.
}
\examples{
data(fecondite)
describe(menages)
describe(femmes)
describe(enfants)
}
\keyword{datasets}

