#' Scan R scripts and load/install all detected packages
#'
#' This function scans one or more R scripts and tries to quick-load/install 
#' the packages mentioned by \code{library} or \code{require} functions.
#'
#' @param ... the scripts to scan. 
#' Defaults to all R scripts in the current working directory.
#' @param load quick-load/install the cited packages (see details). 
#' Defaults to \code{TRUE}.
#' @param detail show the list of packages found in each script. 
#' Defaults to \code{TRUE}.
#' @details
#' The function calls the \code{qload} function to quick-load/install the packages.
#' @return
#' The result is a list of packages cited in the scripts.
#' @seealso
#' \code{\link[questionr]{qload}}, \code{\link{library}}
#' @author François Briatte <f.briatte@@gmail.com>
#' @examples
#' ## Scan the working directory.
#' \dontrun{qscan()}
#' @export

qscan <- function(..., load = TRUE, detail = TRUE) {
  x = c(...)
  # paths
  if(length(x) < 1)
    x <- dir(pattern="\\.R", ignore.case = TRUE)
  # parse
  libs <- sapply(x, FUN = function(x) {
    conn <- file(x)
    text <- readLines(conn, warn = FALSE)
    text <- text[grepl("(library|require)\\(([a-zA-Z0-9]*)\\)", text)]
    pkgs <- gsub(".*(library|require)\\(([a-zA-Z0-9]*)\\).*", "\\2", text)
    close(conn)
    unique(pkgs)
  })
  # load
  list = unique(unlist(libs))
  if(load) qload(list)
  # format
  if(!detail)
    libs <- list
  return(libs)
}

#' Load one or more packages, installing them first if necessary
#'
#' This function quickly loads one or more packages, installing them quietly 
#' if necessary.
#'
#' @param ... the packages to load/install. Packages are loaded with \code{library} 
#' and installed first with \code{install.packages} if necessary.
#' @param load load the packages. Set to \code{FALSE} to just install any missing packages. 
#' Defaults to \code{TRUE}. 
#' @param silent keep output as silent as possible.
#' Defaults to \code{TRUE}.
#' @details
#' The function probably requires R 3.0.0 or above to make use of the \code{quiet}
#' argument when calling \code{install.packages}. It is not clear what the argument
#' previously achieved in older versions of R.
#' @return
#' The result is a list of packages cited in the scripts.
#' @seealso
#' \code{\link[questionr]{qscan}}, \code{\link{install.packages}}, \code{\link{library}}
#' @author François Briatte <f.briatte@@gmail.com>
#' @examples
#' qload("questionr")
#' qload("questionr", silent = FALSE)
#' @export

qload <- function(..., load = TRUE, silent = TRUE) {
  run = sapply(c(...), FUN = function(x) {
    # install
    if(!suppressMessages(suppressWarnings(require(x, character.only = TRUE)))) {
      dl <- try(utils::install.packages(x, quiet = TRUE))
      if(class(dl) == "try-error")
        stop("The package ", x, "could not be downloaded.")
    }
    # load
    if(load) {
      suppressPackageStartupMessages(library(x, character.only = TRUE))
      if(!silent) message("Loaded package: ", x)
    }
  })
}
