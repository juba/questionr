##' Interactive recoding
##'
##' Interactive recoding function prototype
##'
##' @param df 
##' @param oldvar 
##' @return 
##' @export

irec <- function(df, oldvar) {
    assign("*questionr_irec_df*", deparse(substitute(df)), envir=.GlobalEnv)
    assign("*questionr_irec_oldvar*", deparse(substitute(oldvar)), envir=.GlobalEnv)
    shiny::runApp(system.file("irec", package="questionr"))
}
