#' Histoire de vie 2003
#'
#' Sample from 2000 people and 20 variables taken from the \emph{Histoire de
#' Vie} survey, produced in France in 2003 by INSEE.
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 2000 rows and 20 variables
#' @source \url{http://www.insee.fr/fr/themes/detail.asp?ref_id=fd-HDV03}
#' @name hdv2003
NULL

#' 1999 French Census - Cities from the Rhône state
#' 
#' Sample from the 1999 french census for the cities of the Rhône state.
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 301 rows and 21 variables
#' @source \url{http://www.insee.fr/fr/bases-de-donnees/default.asp?page=recensements.htm}
#' @name rp99
NULL

#' A fertility survey
#' 
#' Some fictive results from a fecondity survey.
#' 
#' @docType data
#' @keywords datasets
#' @format 3 data frames with labelled data (as if data would have been imported from SPSS with \pkg{haven}):
#'    \itemize{
#'      \item \code{menages} contains some information from the households selected for the survey;
#'      \item \code{femmes} contains the questionnaire administered to all 15-49 years old women 
#'        living in the selected households;
#'      \item\code{enfants} contains one record for each child of the surveyed women.
#'    }
#'   
#'    Data can be linked using the variables \code{id_menage} and \code{id_femme}.
#' @name fecondite
#' @examples 
#' data(fecondite)
#' describe(menages)
#' describe(femmes)
#' describe(enfants)
NULL

#' A fertility survey - "menages" table
#' 
#' Some fictive results from a fecondity survey.
#' 
#' @docType data
#' @keywords datasets
#' @format a data frame containing some information from the households selected for the \link{fecondite} survey.
#' @name menages
NULL

#' A fertility survey - "femmes" table
#' 
#' Some fictive results from a fecondity survey.
#' 
#' @docType data
#' @keywords datasets
#' @format a data frame containing the questionnaire administered to all 15-49 years old women 
#'        living in the selected households for the \link{fecondite} survey.
#' @name femmes
NULL

#' A fertility survey - "enfants" table
#' 
#' Some fictive results from a fecondity survey.
#' 
#' @docType data
#' @keywords datasets
#' @format a data frame containing one record for each child of the surveyed women in the \link{fecondite} survey.
#' @name enfants
NULL
