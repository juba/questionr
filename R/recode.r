#' Transform a quantiative variable into a qualtitative variable
#'
#' This function transforms a quantitative variable into a qualitative
#' one by breaking it into classes with the same frequencies.
#'
#' @param var variable to transform
#' @param nbclass number of classes
#' @param include.lowest,right,dig.lab,... arguments to the \code{cut} function
#' @details
#' This is just a simple wrapper around the \code{cut} and \code{quantile}
#' functions.
#' @return
#' The result is a factor.
#' @seealso
#' \code{\link{cut}}, \code{\link{quantile}}
#' @examples
#' data(iris)
#' sepal.width3cl <- quant.cut(iris$Sepal.Width,3)
#' table(sepal.width3cl)

`quant.cut` <-
function (var, nbclass, include.lowest=TRUE, right=FALSE, dig.lab=5, ...) {
  breaks <- unique(quantile(var,probs=seq(0,1,length=nbclass+1),na.rm=TRUE))
  cut(var,breaks=breaks,dig.lab=dig.lab,right=right, include.lowest=include.lowest, ...)
}
