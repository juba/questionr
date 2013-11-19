##' Interactive reordering of factor levels
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive reordering of the levels of a categorical variable (character
##' or factor).
##'
##' @param df data frame to operate on, as an object or a character string
##' @param oldvar name of the variable to be reordered, as a character string (possibly without quotes)
##' @details
##' The generated convert the variable into a factor, as only those allow for levels ordering.
##' @return
##' The function launches a shiny app in the system web browser. The reordering code is returned in the console
##' when the app is closed with the "Done" button.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @examples
##' \dontrun{data(hdv2003)
##' iorder(hdv2003, "qualif")}
##' @importFrom shiny runApp
##' @importFrom highr hi_html
##' @export

iorder <- function(df, oldvar) {
    ## Check if df is an object or a character string
    if (!is.character(df)) df <- deparse(substitute(df))
    ## Check if df is a data frame
    if (!is.data.frame(get(df))) stop(sQuote(paste0(df, ' must be a data frame.')))
    options(questionr_iorder_df=df)
    ## If oldvar is not a character string, deparse it
    is_char <- FALSE
    try(if(is.character(oldvar)) is_char <- TRUE, silent=TRUE)
    if (!is_char) oldvar <- deparse(substitute(oldvar))
    ## Check if oldvar is a column of df
    if (!(oldvar %in% names(get(df)))) stop(sQuote(paste0(oldvar, ' must be a column of ', df, '.')))    
    options(questionr_iorder_oldvar=oldvar)
    ## Run shiny app
    invisible(shiny::runApp(system.file("iorder", package="questionr")))
}
