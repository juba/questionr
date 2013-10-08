##' Graphical recoding
##'
##' Graphical recoding function prototype
##'
##' @param df 
##' @param oldvar 
##' @param newvar 
##' @return 
##' @export

grec <- function(df, oldvar) {
    assign("*questionr_grec_df*", deparse(substitute(df)), envir=.GlobalEnv)
    assign("*questionr_grec_oldvar*", deparse(substitute(oldvar)), envir=.GlobalEnv)
    shiny::runApp(system.file("grec", package="questionr"))
}
