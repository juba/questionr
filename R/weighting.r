##' Weighted mean and variance of a vector
##'
##' Compute the weighted mean or weighted variance of a vector.
##' 
##' @aliases wtd.var
##' @param x Numeric data vector 
##' @param weights Numeric weights vector. Must be the same length as \code{x}
##' @param normwt Only for \code{wtd.var}, if \code{TRUE} then weights are normalized for the weighted count to be the same as the non-weighted one
##' @param na.rm if \code{TRUE}, delete \code{NA} values.
##' @details
##' If \code{weights} is \code{NULL}, then an uniform weighting is applied.
##' @author
##' These functions are exact copies of the \code{wtd.mean} and \code{wtd.var}
##' function from the \link[Hmisc]{wtd.stats} package. They have been created by Frank Harrell, Department of Biostatistics,
##' Vanderbilt University School of Medicine, <f.harrell@@vanderbilt.edu>.
##' @seealso
##' \code{\link{mean}},\code{\link{var}}, \code{\link{wtd.table}} and the \link{survey} package.
##' @examples
##' data(hdv2003)
##' mean(hdv2003$age)
##' wtd.mean(hdv2003$age, weights=hdv2003$poids)
##' var(hdv2003$age)
##' wtd.var(hdv2003$age, weights=hdv2003$poids)
##' @export wtd.mean wtd.var

`wtd.mean` <-
function (x, weights = NULL, normwt = "ignored", na.rm = TRUE) 
{
    if (!length(weights)) 
        return(mean(x, na.rm = na.rm))
    if (na.rm) {
        s <- !is.na(x + weights)
        x <- x[s]
        weights <- weights[s]
    }
    sum(weights * x)/sum(weights)
}

`wtd.var` <-
function (x, weights = NULL, normwt = FALSE, na.rm = TRUE) 
{
    if (!length(weights)) {
        if (na.rm) 
            x <- x[!is.na(x)]
        return(stats::var(x))
    }
    if (na.rm) {
        s <- !is.na(x + weights)
        x <- x[s]
        weights <- weights[s]
    }
    if (normwt) 
        weights <- weights * length(x)/sum(weights)
    xbar <- sum(weights * x)/sum(weights)
    sum(weights * ((x - xbar)^2))/(sum(weights) - 1)
}


#' Weighted one-way and two-way frequency tables.
#'
#' Generate weighted frequency tables, both for one-way and two-way tables.
#'
#' @param x a vector
#' @param y another optional vector for a two-way frequency table. Must be the same length as \code{x}
#' @param weights vector of weights, must be the same length as \code{x}
#' @param normwt if TRUE, normalize weights so that the total weighted count is the same as the unweighted one
#' @param na.show if TRUE, show NA count in table output
#' @param na.rm if TRUE, remove NA values before computation
#' @details
#' If \code{weights} is not provided, an uniform weghting is used.
#' @return
#' If \code{y} is not provided, returns a weighted one-way frequency table
#' of \code{x}. Otherwise, returns a weighted two-way frequency table of
#' \code{x} and \code{y}
#' @seealso
#' \code{\link[Hmisc]{wtd.table}}, \command{\link{table}}, and the \link{survey} extension.
#' @examples
#' data(hdv2003)
#' wtd.table(hdv2003$sexe, weights=hdv2003$poids)
#' wtd.table(hdv2003$sexe, weights=hdv2003$poids, normwt=TRUE)
#' table(hdv2003$sexe, hdv2003$hard.rock)
#' wtd.table(hdv2003$sexe, hdv2003$hard.rock, weights=hdv2003$poids)
#' @export

`wtd.table` <-
function (x, y = NULL, weights = NULL, normwt = FALSE, na.rm = TRUE, na.show = FALSE) 
{
  if (is.null(weights)) weights <- rep(1, length(x))  
  if (length(x) != length(weights)) stop("x and weights lengths must be the same")
  if (!is.null(y) & (length(x) != length(y))) stop("x and y lengths must be the same")
  if (na.show) {
      x <- addNA(x)
      if (!is.null(y)) y <- addNA(y)
  }
  if (na.rm) {
     s <- !is.na(x) & !is.na(weights)
     if (!is.null(y)) s <- s & !is.na(y)
     x <- x[s, drop = FALSE]
     if (!is.null(y)) y <- y[s, drop = FALSE]
     weights <- weights[s]
  }
  if (normwt) {
    weights <- weights * length(x)/sum(weights)
  }
  if (is.null(y)) {
    result <- tapply(weights, x, sum, simplify=TRUE)
  }
  else {
    result <- tapply(weights, list(x,y), sum, simplify=TRUE)
  }
  result[is.na(result)] <- 0
  as.table(result)
}
