#' Transform missing values of a factor to an extra level
#'
#' This function modifies a factor by turning \code{NA} into an extra level 
#' (so that \code{NA} values are counted in tables, for instance).
#' This version of \code{addNA} extends the same function provided in \R by
#' allowing to specify a string name for the extra level (see examples).
#' 
#' @param x a vector of data, usually taking a small number of distinct values.
#' @param value string to use for the extra level name. If NULL, the extra level is created as NA, and the result is the same as the one of the \code{addNA} function.
#' @param ... arguments passed to \code{addNA}.
#' @return 
#' an object of class \code{"factor"}, original missing values being coded as an
#' extra level named \code{NA} if \code{as.string=FALSE}, \code{"NA"} if 
#' \code{as.string=TRUE}, as specified by \code{as.string} if \code{as.string} is
#' a string.
#' @seealso \code{\link[base]{addNA}} (base).
#' @examples
#' f <- as.factor(c("a","b",NA,"a","b"))
#' f
#' addNAstr(f)
#' addNAstr(f, value="missing")
#' addNAstr(f, value=NULL)
#' @source Adapted from James (\url{http://stackoverflow.com/a/5817181}) 
#' by Joseph Larmarange <joseph@@larmarange.net>
#' @export addNAstr

addNAstr <- function(x, value="NA", ...) {
   x <- addNA(x, ...)
   if (!is.null(value)) levels(x)[is.na(levels(x))] <- value
   x
}

