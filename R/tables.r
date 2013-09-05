#' Generate frequency tables.
#'
#' Generate and format frequency tables from a variable or a table, with percentages and formatting options.
#'
#' @param x either a vector to be tabulated, or a table object
#' @param digits number of digits to keep for the percentages
#' @param cum if TRUE, display cumulative percentages
#' @param total if TRUE, add a final row with totals
#' @param exclude vector of values to exclude from the tabulation
#' @param sort if specified, allow to sort the table by increasing ("inc") or decreasing ("dec") frequencies
#' @return
#' The result is an object of class data.frame.
#' @seealso
#' \code{\link{table}}, \code{\link[questionr]{prop}}, \code{\link[questionr]{cprop}}, \code{\link[questionr]{rprop}}
#' @export

`freq` <-
function (x, digits=1, cum=FALSE, total=FALSE, exclude=NULL, sort="") {
  if (is.factor(x)) x <- factor(x, exclude=exclude)
  if (is.table(x)) tab <- x
  else tab <- table(x, exclude=exclude)
  effectifs <- as.vector(tab)
  pourc <- as.vector(effectifs/sum(effectifs)*100)
  result <- data.frame(n=effectifs, pourc=pourc)
  rownames(result) <- ifelse(is.na(names(tab)),"NA",names(tab))
  if (sort=="inc") result <- result[order(result$n),]
  if (sort=="dec") result <- result[order(result$n, decreasing=TRUE),]
  if (total) result <- rbind(result, Total=apply(result,2,sum))
  if (cum) {
    pourc.cum <- cumsum(result$pourc)
    if (total) pourc.cum[length(pourc.cum)] <- 100
    result <- cbind(result, pourc.cum)
  }
  names(result)[which(names(result)=="pourc")] <- "%"
  names(result)[which(names(result)=="pourc.cum")] <- "%cum"
  round(result, digits=digits)
}

#' Generate frequency table of missing values.
#'
#' Generate a frequency table of missing values as raw counts and percentages.
#'
#' @param data either a vector or a data frame object
#' @param ... if \code{x} is a data frame, the names of the variables to examine. When no variable names are provided, the function examines the full data frame and returns the five variables with most missing values.
#' @return
#' The result is an object of class data.frame.
#' @seealso
#' \code{\link{table}}, \code{\link{is.na}}
#' @examples
#' data(hdv2003)
#' ## Examine a single vector.
#' freq.na(hdv2003$qualif)
#' ## Examine a data frame.
#' freq.na(hdv2003)
#' ## Examine several variables.
#' freq.na(hdv2003, "nivetud", "trav.satisf")
#' ## Examine all variables.
#' freq.na(hdv2003, names(hdv2003))
#' @export

freq.na <- function(data, ...) {
  d = NULL
  if (class(data) == "data.frame") {
    if (length(c(...)) < 1) d = names(data)
    d = data[, c(d, ...)]
  }
  else {
    d = as.data.frame(data)
  }
  if (is.null(dim(d))) {
    c = length(d)  
  }
  else {
    c = nrow(d)    
  }
  d = is.na(as.matrix(d))
  d = as.matrix(colSums(d))
  d = cbind(d, 100 * round(d / c, 2))
  d = d[order(d[, 1], decreasing = TRUE), ]
  n = c("missing", "%")
  if(is.null(dim(d)))
    names(d) = n
  else
    colnames(d) = n
  if (length(c(...)) < 1 & class(data) == "data.frame") {
    warning("No variables specified; showing top five results.")
    return(head(d))
  }
  else {
    return(d)
  }
}

#' Column percentages of a two-way frequency table.
#'
#' Return the column percentages of a two-way frequency table with formatting and printing options.
#'
#' @param tab frequency table
#' @param digits number of digits to display
#' @param total if \code{TRUE}, add a row with the sum of percentages and a column with global percentages
#' @param percent if \code{TRUE}, add a percent sign after the values when printing
#' @param drop if \code{TRUE}, lines or columns with a sum of zero, which would generate \code{NaN} percentages, are dropped.
#' @return
#' The result is an object of class \code{table} and \code{proptab}.
#' @seealso
#' \code{\link[questionr]{rprop}}, \code{\link[questionr]{prop}}, \code{\link{table}}, \code{\link{prop.table}}
#' @examples
#' ## Sample table
#' data(Titanic)
#' tab <- apply(Titanic, c(4,1), sum)
#' ## Column percentages
#' cprop(tab)
#' ## Column percentages with custom display
#' cprop(tab, digits=2, percent=TRUE, total=FALSE)
#' @export

`cprop` <-
function (tab, digits = 1, total = TRUE, percent = FALSE, drop = TRUE) {
  # subset to non-empty rows/columns
  if(drop) tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]
  dn <- names(dimnames(tab))
  if (total) tab <- cbind(tab, Ensemble=apply(tab,1,sum))
  tab <- prop.table(tab,2)*100
  if (total) tab <- rbind(tab,Total=apply(tab,2,sum))
  result <- as.table(tab)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  return(result)
}

#' Row percentages of a two-way frequency table.
#'
#' Return the row percentages of a two-way frequency table with formatting and printing options.
#'
#' @aliases lprop
#' @param tab frequency table
#' @param digits number of digits to display
#' @param total if \code{TRUE}, add a column with the sum of percentages and a row with global percentages
#' @param percent if \code{TRUE}, add a percent sign after the values when printing
#' @param drop if \code{TRUE}, lines or columns with a sum of zero, which would generate \code{NaN} percentages, are dropped.
#' @return
#' The result is an object of class \code{table} and \code{proptab}.
#' @seealso
#' \code{\link[questionr]{cprop}}, \code{\link[questionr]{prop}}, \code{\link{table}}, \code{\link{prop.table}}
#' @examples
#' ## Sample table
#' data(Titanic)
#' tab <- apply(Titanic, c(1,4), sum)
#' ## Column percentages
#' rprop(tab)
#' ## Column percentages with custom display
#' rprop(tab, digits=2, percent=TRUE, total=FALSE)
#' @export rprop lprop

`rprop` <-
function(tab, digits = 1, total = TRUE, percent = FALSE, drop = TRUE) {
  # subset to non-empty rows/columns
  if(drop) tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]
  dn <- names(dimnames(tab))
  if (total) tab <- rbind(tab, Ensemble=apply(tab,2,sum))
  tab <- prop.table(tab,1)*100
  if (total) tab <- cbind(tab, Total=apply(tab,1,sum))
  result <- as.table(tab)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  return(result)
}
lprop <- rprop

#' Global percentages of a two-way frequency table.
#'
#' Return the percentages of a two-way frequency table with formatting and printing options.
#'
#' @param tab frequency table
#' @param digits number of digits to display
#' @param total if \code{TRUE}, add a column with the sum of percentages and a row with global percentages
#' @param percent if \code{TRUE}, add a percent sign after the values when printing
#' @param drop if \code{TRUE}, lines or columns with a sum of zero, which would generate \code{NaN} percentages, are dropped.
#' @return
#' The result is an object of class \code{table} and \code{proptab}.
#' @seealso
#' \code{\link[questionr]{rprop}}, \code{\link[questionr]{cprop}}, \code{\link{table}}, \code{\link{prop.table}}
#' @examples
#' ## Sample table
#' data(Titanic)
#' tab <- apply(Titanic, c(1,4), sum)
#' ## Percentages
#' prop(tab)
#' ## Percentages with custom display
#' prop(tab, digits=2, percent=TRUE, total=FALSE)
#' @export

`prop` <-
function (tab, digits = 1, total = TRUE, percent = FALSE, drop = TRUE) {
  # subset to non-empty rows/columns
  if(drop) tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]
  dn <- names(dimnames(tab))
  tmp <- tab/sum(tab)*100
  if (total) {
    tmp <- rbind(tmp,Total=apply(tmp,2,sum))
    tmp <- cbind(tmp,Total=apply(tmp,1,sum))
  }
  result <- as.table(tmp)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  return(result)
}

#' Return the chi-squared residuals of a two-way frequency table.
#'
#' Return the raw, standardized or Pearson's residuals (the default) of a chi-squared test on a two-way frequency table. 
#'
#' @aliases residus
#' @param tab frequency table
#' @param digits number of digits to display
#' @param std if \code{TRUE}, returns the standardized residuals. Otherwise, returns the Pearson residuals. Incompatible with \code{raw}.
#' @param raw if \code{TRUE}, returns the raw (\code{observed - expected}) residuals. Otherwise, returns the Pearson residuals. Incompatible with \code{std}.
#' @details
#' This function is just a wrapper around the \code{\link{chisq.test}} base R function. See this function's help page
#' for details on the computation.
#' @seealso
#' \code{\link{chisq.test}}
#' @export chisq.residuals residus
#' @examples
#' ## Sample table
#' data(Titanic)
#' tab <- apply(Titanic, c(1,4), sum)
#' ## Pearson residuals
#' chisq.residuals(tab)
#' ## Standardized residuals
#' chisq.residuals(tab, std = TRUE)
#' ## Raw residuals
#' chisq.residuals(tab, raw = TRUE)

`chisq.residuals` <-
function (tab, digits = 2, std = FALSE, raw = FALSE) {
  if(all(std, raw))
    stop("Choose between standardized and raw residuals.")

  k = chisq.test(tab)
  if (raw) {
    # raw residuals
    res <- k$observed - k$expected
  }
  else if (std) {
    # standardized residuals
    res <- k$stdres
  }
  else {
    # Pearson residuals
    res <- k$residuals
  }
  round(res, digits)
}
residus <- chisq.residuals

#' S3 format method for proptab objects.
#'
#' Format an object of class proptab for printing depending on its attributes.
#'
#' @param x object of class proptab
#' @param digits number of digits to display
#' @param percent if not NULL, add a percent sign after each value
#' @param justify justification of character vectors. Passed to \code{format.default}
#' @param ... other arguments to pass to \code{format.default}
#' @method format proptab
#' @S3method format proptab
#' @details
#' This function is designed for internal use only.
#' @seealso
#' \code{\link{format.default}}, \code{\link[questionr]{print.proptab}}
#' @export

`format.proptab` <-
function (x, digits=NULL, percent=NULL, justify="right", ...) {
  if (!inherits(x, "proptab")) stop("x must be of class 'proptab'")
  if (is.null(digits)) digits <- attr(x, "digits")
  if (is.null(percent)) percent <- attr(x, "percent")
  if (percent) {
    fmt <- paste("%.",digits,"f%%",sep="")
    x[,] <- sprintf(x, fmt=fmt)
    result <- format.default(x,justify=justify, ...)
  }
  else
    result <- format.default(round(x,digits), ...)
  return(result)
}

#' S3 print method for proptab objects.
#'
#' Print an object of class proptab.
#'
#' @param x object of class proptab
#' @param digits number of digits to display
#' @param percent if not NULL, add a percent sign after each value
#' @param justify justification of character vectors. Passed to \code{format.default}
#' @param ... other arguments to pass to \code{format.default}
#' @method print proptab
#' @S3method print proptab
#' @seealso
#' \code{\link[questionr]{format.proptab}}
#' @export

`print.proptab` <-
function (x, digits=NULL, percent=NULL, justify="right", ...) {
  if (!inherits(x, "proptab")) stop("x must be of class 'proptab'")
  x <- format.proptab(x, digits=digits, percent=percent, justify=justify)
  print.table(x, ...)
}

