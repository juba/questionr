##' Interactive recoding
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive recoding of a categorical variable (character or factor).
##'
##' @param dfobject data frame to operate on, as an object or a character string
##' @param oldvar name of the variable to be recoded, as a character string (possibly without quotes)
##' @return
##' The function launches a shiny app in the system web browser. The recoding code is returned in the console
##' when the app is closed with the "Done" button.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @examples
##' \dontrun{data(hdv2003)
##' irec(hdv2003, "qualif")
##' irec(hdv2003, sexe) ## this also works}
##' @importFrom shiny runApp
##' @importFrom highr hi_html
##' @export

irec <- function(dfobject, oldvar) {
    ## Check if dfobject is an object or a character string
    if (!is.character(dfobject)) dfobject <- deparse(substitute(dfobject))
    ## Prevents get() conflicts
    if (dfobject=="dfobject") stop(sQuote(paste0(dfobject, ' must not be an object named "dfobject".')))
        ## Check if dfobject is a data frame
    if (!is.data.frame(get(dfobject))) stop(sQuote(paste0(dfobject, ' must be a data frame.')))
    options(questionr_irec_df=dfobject)
    ## If oldvar is not a character string, deparse it
    is_char <- FALSE
    try(if(is.character(oldvar)) is_char <- TRUE, silent=TRUE)
    if (!is_char) oldvar <- deparse(substitute(oldvar))
    ## Check if oldvar is a column of dfobject
    if (!(oldvar %in% names(get(dfobject)))) stop(sQuote(paste0(oldvar, ' must be a column of ', dfobject, '.')))    
    options(questionr_irec_oldvar=oldvar)
    ## Run shiny app
    invisible(shiny::runApp(system.file("irec", package="questionr")))
}
