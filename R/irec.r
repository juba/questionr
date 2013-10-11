##' Interactive recoding
##'
##' Interactive recoding function prototype
##'
##' @param df 
##' @param oldvar 
##' @return 
##' @export

irec <- function(df, oldvar) {
    if (!is.character(df)) df <- deparse(substitute(df))
    if (!is.data.frame(get(df))) stop(sQuote(paste0(df, ' must be a data frame.')))
    assign(".questionr_irec_df", df, envir=.GlobalEnv)
    ## If oldvar is not a character string
    is_char <- FALSE
    try(if(is.character(oldvar)) is_char <- TRUE, silent=TRUE)
    if (!is_char) oldvar <- deparse(substitute(oldvar))
    if (!(oldvar %in% names(get(df)))) stop(sQuote(paste0(oldvar, ' must be a column of ', df, '.')))    
    assign(".questionr_irec_oldvar", oldvar, envir=.GlobalEnv)
    shiny::runApp(system.file("irec", package="questionr"))
}
