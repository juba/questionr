##' Interactive conversion from numeric to factor
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive conversion of a numeric variable into a categorical one.
##'
##' @param df data frame to operate on, as an object or a character string
##' @param oldvar name of the variable to be recoded, as a character string (possibly without quotes)
##' @return
##' The function launches a shiny app in the system web browser. The recoding code is returned in the console
##' when the app is closed with the "Done" button.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @examples
##' \dontrun{data(hdv2003)
##' icut(hdv2003, "age")
##' irec(hdv2003, heures.tv) ## this also works}
##' @importFrom shiny runApp
##' @export

icut <- function(df, oldvar) {
    ## Check if df is an object or a character string
    if (!is.character(df)) df <- deparse(substitute(df))
    ## Check if df is a data frame
    if (!is.data.frame(get(df))) stop(sQuote(paste0(df, ' must be a data frame.')))
    assign(".questionr_icut_df", df, envir=.GlobalEnv)
    ## If oldvar is not a character string, deparse it
    is_char <- FALSE
    try(if(is.character(oldvar)) is_char <- TRUE, silent=TRUE)
    if (!is_char) oldvar <- deparse(substitute(oldvar))
    ## Check if oldvar is a column of df
    if (!(oldvar %in% names(get(df)))) stop(sQuote(paste0(oldvar, ' must be a column of ', df, '.')))    
    assign(".questionr_icut_oldvar", oldvar, envir=.GlobalEnv)
    ## Run shiny app
    invisible(shiny::runApp(system.file("icut", package="questionr")))
}
