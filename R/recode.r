#' Transform a quantitative variable into a qualitative variable
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
#' @export

`quant.cut` <-
function (var, nbclass, include.lowest=TRUE, right=FALSE, dig.lab=5, ...) {
  breaks <- unique(quantile(var,probs=seq(0,1,length=nbclass+1),na.rm=TRUE))
  cut(var,breaks=breaks,dig.lab=dig.lab,right=right, include.lowest=include.lowest, ...)
}

#' Recode values of a variable to missing values, using exact or regular expression matching.
#'
#' This function recodes selected values of a quantitative or qualitative
#' variable by matching its levels to exact or regular expression matches.
#'
#' @param x variable to recode. The variable is coerced to a factor if necessary.
#' @param ... levels to recode as missing in the variable. The values are coerced to character strings, meaning that you can pass numeric values to the function.
#' @param verbose print a table of missing levels before rencoding them as missing. Defaults to \code{FALSE}.
#' @param regex use regular expressions to match values that include the "*" or "|" wildcards. Defaults to \code{TRUE}.
#' @return
#' The result is a factor with properly encoded missing values. If the recoded variable contains only numeric values, it is converted to an object of class \code{numeric}.
#' @seealso
#' \code{\link{regex}}
#' @examples
#' data(hdv2003)
#' ## With exact string matches.
#' hdv2003$nivetud = recode.na(hdv2003$nivetud, "Inconnu")
#' ## With regular expressions.
#' hdv2003$relig = recode.na(hdv2003$relig, "[A|a]ppartenance", "Rejet|NSP")
#' ## Showing missing values. 
#' hdv2003$clso = recode.na(hdv2003$clso, "Ne sait pas", verbose = TRUE)
#' ## Test results with freq.
#' freq(recode.na(hdv2003$trav.satisf, "Equilibre"))
#' ## Truncate a count variable.
#' freq(recode.na(hdv2003$freres.soeurs, 5:22))
#' @export

recode.na <- function(x, ..., verbose = FALSE, regex = TRUE) {
  if(!is.factor(x)) x = factor(x)
  m = as.character(c(...))
  r = which(grepl("\\*|\\|", m))
  q = m[r]

  # grepl
  r1 = NULL
  if(length(q) & regex) {
    r1 = lapply(q, function(i) which(grepl(gsub("\\*", "", i), levels(x))))
    r1 = unlist(r1)
  }

  q = m
  if(length(r)) m = m[-r]

  # exact
  r2 = NULL
  if(length(q)) {
    r2 = lapply(q, function(i) which(levels(x) %in% i))
    r2 = unlist(r2)
  }

  q = levels(x)[unique(c(r1, r2))]
  m = factor(x[x %in% q])

  r = matrix(table(m))
  rownames(r) = levels(m)
  colnames(r) = "n"
  cat("Recoded", sum(r), "values to NA.")
  if(sum(r) & verbose) print(r)
  x[which(x %in% q)] = NA
  x = factor(x)
  if(all(!is.na(suppressWarnings(as.numeric(na.omit(x)))))) x = as.numeric(x)
  cat(ifelse(is.numeric(x), 
             "\nRecoded variable is numeric.", 
             ""), 
      "\n")
  return(x)
}
