#' Check if we are currently running as an rstudio addin
#' @import rstudioapi

ifunc_run_as_addin <- function() {
  rstudioapi::isAvailable() && rstudioapi::getActiveDocumentContext()$id != "#console"
}


#' Display an alert, only on first launch for the current session
#' @param run_as_addin TRUE if the function is running as an rstudio addin

ifunc_show_alert <- function(run_as_addin) {
  ## Display the alert only on first time launch
  show_alert <- is.null(getOption("questionr_hide_alert"))
  if (show_alert) {
    options(questionr_hide_alert = TRUE)
    div(class = "alert alert-warning alert-dismissible",
        HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
        HTML(gettext("<strong>Warning :</strong> This interface doesn't do anything by itself.", domain = "R-questionr")),
        if (run_as_addin) {
          HTML(gettext("It will generate R code, insert it in your current R script, and you'll have to run it yourself.", domain = "R-questionr"))
        } else {
          HTML(gettext("It only generates R code you'll have to copy/paste into your script and run yourself.", domain = "R-questionr"))
        }
    )}
}

#' Returns custom CSS content

ifunc_get_css <- function() {
  css.file <- system.file(file.path("shiny", "css", "ifuncs.css"), package = "questionr")
  out <- paste(readLines(css.file),collapse="\n")
  HTML(out)
}

#' Return first non-null of two values
#' @name first_non_null
#' @param x first object
#' @param y second object

`%||%` <- function(x, y) {
    if (!is.null(x)) x else y
}
