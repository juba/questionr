##' Graphical recoding
##'
##' Graphical recoding function prototype
##'
##' @param df 
##' @param oldvar 
##' @param newvar 
##' @return 
##' @export

grec <- function(df, oldvar, newvar) {
    `*questionr_grec_tmp_df*` <- df
    `*questionr_grec_tmp_oldvar*` <- oldvar
    `*questionr_grec_tmp_newvar*` <- newvar
    shiny::runApp(system.file("grec", package="questionr"))
}
