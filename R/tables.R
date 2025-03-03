#' Generate frequency tables.
#'
#' Generate and format frequency tables from a variable or a table, with percentages and formatting options.
#'
#' @param x either a vector to be tabulated, or a table object
#' @param digits number of digits to keep for the percentages
#' @param cum if TRUE, display cumulative percentages
#' @param total if TRUE, add a final row with totals
#' @param exclude vector of values to exclude from the tabulation (if \code{x} is a vector)
#' @param sort if specified, allow to sort the table by increasing ("inc") or decreasing ("dec") frequencies
#' @param valid if TRUE, display valid percentages
#' @param levels the desired levels for the factor in case of labelled vector (\pkg{labelled} package
#'    must be installed): "labels" for value labels, "values" for values or 
#'    "prefixed" for labels prefixed with values
#' @param na.last if TRUE, NA values are always be last table row
#' @return
#' The result is an object of class data.frame.
#' @seealso
#' \code{\link{table}}, \code{\link[questionr]{prop}}, \code{\link[questionr]{cprop}}, \code{\link[questionr]{rprop}}
#' @examples 
#' # factor
#' data(hdv2003)
#' freq(hdv2003$qualif)
#' freq(hdv2003$qualif, cum = TRUE, total = TRUE)
#' freq(hdv2003$qualif, cum = TRUE, total = TRUE, sort ="dec")
#' 
#' # labelled data
#' data(fecondite)
#' freq(femmes$region)
#' freq(femmes$region, levels = "l")
#' freq(femmes$region, levels = "v")
#' @export

freq <- function(x, digits = 1, cum = FALSE, total = FALSE, exclude = NULL, sort = "", 
         valid = !(NA %in% exclude), levels = c("prefixed", "labels", "values"),
         na.last = TRUE) {
  
  levels <- match.arg(levels)
  
  if (is.table(x)) {
    tab <- x
  } else {
    tab <- table(labelled::to_factor(x, levels), exclude = exclude)
  }
  
  effectifs <- as.vector(tab)
  pourc <- as.vector(effectifs / sum(effectifs) * 100)
  result <- data.frame(n = effectifs, pourc = pourc)
  
  if (valid) {
    user_na <- unique(as.character(labelled::to_factor(x, levels)[is.na(x)]))
    NA.position <- which(is.na(names(tab)) | names(tab) %in% user_na)
    n.na <- sum(tab[NA.position])
    valid.pourc <- as.vector(effectifs / (sum(effectifs) - n.na) * 100)
    valid.pourc[NA.position] <- 0 # temporary 0 for cumsum
    result <- cbind(result, valid.pourc)
  }
  
  ## Avoid duplicate row names if both NA and "NA" in tab
  if ("NA" %in% names(tab)) {
    names(tab)[names(tab) == "NA"] <- "\"NA\""
  }
  rownames(result) <- ifelse(is.na(names(tab)), "NA", names(tab))
  
  if (sort == "inc") result <- result[order(result$n),]
  if (sort == "dec") result <- result[order(result$n, decreasing = TRUE),]
  
  if (na.last && "NA" %in% rownames(result)) {
    result <- rbind(result[-which(rownames(result) == "NA"), ], result["NA", ])
  }
  
  if (total) result <- rbind(result, Total = apply(result,2,sum))
  if (total & valid) 
    result[length(result$pourc),"valid.pourc"] <- 100
  
  if (cum) {
    pourc.cum <- cumsum(result$pourc)
    if (total) pourc.cum[length(pourc.cum)] <- 100
    result <- cbind(result, pourc.cum)
    if (valid) {
      valid.pourc.cum <- cumsum(result$valid.pourc)
      if (total) valid.pourc.cum[length(valid.pourc.cum)] <- 100
      result <- cbind(result, valid.pourc.cum)
    }
  }
  
  if (valid) {
    NA.position <- which(rownames(result) == "NA" | rownames(result) %in% user_na)
    result[NA.position, "valid.pourc"] <- NA
    if (cum)
      result[NA.position, "valid.pourc.cum"] <- NA
  }
  
  names(result)[names(result) == "pourc"] <- "%"
  names(result)[names(result) == "valid.pourc"] <- "val%"
  names(result)[names(result) == "pourc.cum"] <- "%cum"
  names(result)[names(result) == "valid.pourc.cum"] <- "val%cum"
  
  class(result) <- c("freqtab", class(result))
  
  round(result, digits = digits)
}

#' Generate frequency table of missing values.
#'
#' Generate a frequency table of missing values as raw counts and percentages.
#'
#' @param data either a vector or a data frame object
#' @param ... if \code{x} is a data frame, the names of the variables to examine or keywords to search for such variables. See \code{\link{lookfor}} for more details.
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
#' ## To see only variables with the most number of missing values
#' head(freq.na(hdv2003))
#' @export

freq.na <- function(data, ...) {
  d = NULL
  if (inherits(data, "data.frame")) {
    s <- lookfor(data, ...)$variable
    d = data[, c(s)]
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
  
  return(d)
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
#' @param n if \code{TRUE}, display number of observations per column.
#' @param ... parameters passed to other methods.
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

`cprop` <- function(tab, ...) {
  UseMethod("cprop")
}


##' @rdname cprop
##' @aliases cprop.table
##' @export

cprop.table <- function (tab, digits = 1, total = TRUE, percent = FALSE, drop = TRUE, n=FALSE, ...) {
  # subset to non-empty rows/columns
  if(drop) tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop=FALSE]
  dn <- names(dimnames(tab))
  if (total) {
    .tmp.colnames <- colnames(tab)
    tab <- cbind(tab, apply(tab, 1, sum))
    colnames(tab) <- c(.tmp.colnames, gettext("All", domain="R-questionr"))
  }
  if (n) effectifs <- apply(tab, 2, sum)
  tab <- base::prop.table(tab, 2) * 100
  if (total) {
    .tmp.rownames <- rownames(tab)
    tab <- rbind(tab, Total = apply(tab, 2, sum))
    rownames(tab) <- c(.tmp.rownames, gettext("Total", domain="R-questionr"))
  }
  if (n) tab <- rbind(tab, n = effectifs)
  result <- as.table(tab)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  attr(result, "total") <- total
  attr(result, "row.n") <- n
  return(result)
}

##' @rdname cprop
##' @aliases cprop.data.frame
##' @export 
cprop.data.frame <- cprop.table
##' @rdname cprop
##' @aliases cprop.matrix
##' @export 
cprop.matrix <- cprop.table

##' @rdname cprop
##' @aliases cprop.tabyl
##' @export

cprop.tabyl <- function(tab, digits = 1, total = TRUE, percent = FALSE, n = FALSE, ...) {
  if (total) {
    tab <- janitor::adorn_totals(tab, c("row", "col"))
  }
  tab <- janitor::adorn_percentages(tab, "col")
  tab <- janitor::adorn_pct_formatting(tab, digits = digits, affix_sign = percent)
  if (n) {
    tab <- janitor::adorn_ns(tab)
  }
  tab <- janitor::adorn_title(tab, "combined")
  return(tab)
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
#' @param n if \code{TRUE}, display number of observations per row.
#' @param ... parameters passed to other methods.
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

`rprop` <- function(tab, ...) {
  UseMethod("rprop")
}
lprop <- rprop


##' @rdname rprop
##' @aliases rprop.table
##' @export 
rprop.table <- function(tab, digits = 1, total = TRUE, percent = FALSE, drop = TRUE, n=FALSE, ...) {
  # subset to non-empty rows/columns
  if(drop) tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop=FALSE]
  dn <- names(dimnames(tab))
  if (total) {
    .tmp.rownames <- rownames(tab)
    tab <- rbind(tab, apply(tab, 2, sum))
    rownames(tab) <- c(.tmp.rownames, gettext("All", domain="R-questionr"))
  }
  if (n) effectifs <- apply(tab, 1, sum)
  tab <- base::prop.table(tab, 1) * 100
  if (total) {
    .tmp.colnames <- colnames(tab)
    tab <- cbind(tab, Total = apply(tab, 1, sum))
    colnames(tab) <- c(.tmp.colnames, gettext("Total", domain="R-questionr"))
  }
  if (n) tab <- cbind(tab, n = effectifs)
  result <- as.table(tab)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  attr(result, "total") <- total
  attr(result, "col.n") <- n
  return(result)
}
##' @rdname rprop
##' @aliases rprop.data.frame
##' @export 
rprop.data.frame <- rprop.table
##' @rdname rprop
##' @aliases rprop.matrix
##' @export 
rprop.matrix <- rprop.table

##' @rdname rprop
##' @aliases rprop.tabyl
##' @export

rprop.tabyl <- function(tab, digits = 1, total = TRUE, percent = FALSE, n = FALSE, ...) {
  if (total) {
    tab <- janitor::adorn_totals(tab, c("row", "col"))
  }
  tab <- janitor::adorn_percentages(tab, "row")
  tab <- janitor::adorn_pct_formatting(tab, digits = digits, affix_sign = percent)
  if (n) {
    tab <- janitor::adorn_ns(tab)
  }
  tab <- janitor::adorn_title(tab, "combined")
  return(tab)
}
 


#' Global percentages of a two-way frequency table.
#'
#' Return the percentages of a two-way frequency table with formatting and printing options.
#'
#' @param tab frequency table
#' @param digits number of digits to display
#' @param total if \code{TRUE}, add a column with the sum of percentages and a row with global percentages
#' @param percent if \code{TRUE}, add a percent sign after the values when printing
#' @param drop if \code{TRUE}, lines or columns with a sum of zero, which would generate \code{NaN} percentages, are dropped.
#' @param n if \code{TRUE}, display number of observations per row and per column.
#' @param ... parameters passed to other methods
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
#' prop(tab, digits=2, percent=TRUE, total=FALSE, n=TRUE)
#' @export

prop <- function(tab, ...) {
  ## Dirty hack to avoid overridig base::prop.table
  if (inherits(tab, "table")) {
    return(prop_table(tab, ...))
  }
  UseMethod("prop")
}

##' @rdname prop
##' @aliases prop_table
##' @export 

prop_table <- function (tab, digits = 1, total = TRUE, percent = FALSE, drop = TRUE, n=FALSE, ...) {
  # subset to non-empty rows/columns
  if(drop) tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop=FALSE]
  dn <- names(dimnames(tab))
  if (n) {
    l.effectifs <- apply(tab,1,sum)
    r.effectifs <- apply(tab,2,sum)
  }
  tmp <- tab/sum(tab)*100
  if (total) {
    tmp <- rbind(tmp,Total=apply(tmp,2,sum))
    tmp <- cbind(tmp,Total=apply(tmp,1,sum))
  }
  if (n) {
    ntot <- sum(tab)
    if (total) {
      l.effectifs <- c(l.effectifs,ntot)
      r.effectifs <- c(r.effectifs,ntot)
    }
    tmp <- cbind(tmp, n=l.effectifs)
    tmp <- rbind(tmp, n=c(r.effectifs,ntot))
  }
  result <- as.table(tmp)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  attr(result, "total") <- total
  attr(result, "row.n") <- n
  attr(result, "col.n") <- n
  return(result)
}

##' @rdname prop
##' @aliases prop.data.frame
##' @export 
prop.data.frame <- prop_table
##' @rdname prop
##' @aliases prop.matrix
##' @export 
prop.matrix <- prop_table

##' @rdname prop
##' @aliases prop.tabyl
##' @export

prop.tabyl <- function(tab, digits = 1, total = TRUE, percent = FALSE, n = FALSE, ...) {
  if (total) {
    tab <- janitor::adorn_totals(tab, c("row", "col"))
  }
  tab <- janitor::adorn_percentages(tab, "all")
  tab <- janitor::adorn_pct_formatting(tab, digits = digits, affix_sign = percent)
  if (n) {
    tab <- janitor::adorn_ns(tab)
  }
  tab <- janitor::adorn_title(tab, "combined")
  return(tab)
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

chisq.residuals <- function (tab, digits = 2, std = FALSE, raw = FALSE) {
  if(all(std, raw))
    stop("Choose between standardized and raw residuals.")

  k = stats::chisq.test(tab)
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
#' @details
#' This function is designed for internal use only.
#' @seealso
#' \code{\link{format.default}}, \code{\link[questionr]{print.proptab}}
#' @export

format.proptab <- function (x, digits=NULL, percent=NULL, justify="right", ...) {
  if (!inherits(x, "proptab")) stop("x must be of class 'proptab'")
  if (is.null(digits)) digits <- attr(x, "digits")
  if (is.null(percent)) percent <- attr(x, "percent")
  total <- attr(x, "total"); if (is.null(total)) total <- FALSE
  row.n <- attr(x, "row.n"); if (is.null(row.n)) row.n <- FALSE
  col.n <- attr(x, "col.n"); if (is.null(col.n)) col.n <- FALSE
  tmp <- format.default(round(x,0), ...)
  if (row.n) rn <- tmp[nrow(x),]
  if (col.n) cn <- tmp[,ncol(x)]
  if (percent) {
    fmt <- paste("%.",digits,"f%%",sep="")
    x[,] <- sprintf(x, fmt=fmt)
    result <- format.default(x,justify=justify, ...)
  }
  else
    result <- format.default(round(x,digits), ...)
  if (row.n) result[nrow(x),] <- rn
  if (col.n) result[,ncol(x)] <- cn
  if (total & row.n & col.n) result[nrow(x),ncol(x)] <- ""
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
#' @seealso
#' \code{\link[questionr]{format.proptab}}
#' @export

print.proptab <- function (x, digits=NULL, percent=NULL, justify="right", ...) {
  if (!inherits(x, "proptab")) stop("x must be of class 'proptab'")
  x <- format.proptab(x, digits=digits, percent=percent, justify=justify)
  print.table(x, ...)
}


#' Cross tabulation with labelled variables
#' 
#' This function is a wrapper around \code{\link[stats]{xtabs}}, adding automatically
#' value labels for labelled vectors if \pkg{labelled} package eis installed.
#' 
#' @param formula a formula object (see \code{\link[stats]{xtabs}})
#' @param data a data frame
#' @param levels the desired levels in case of labelled vector: 
#'    "labels" for value labels, "values" for values or "prefixed" for labels prefixed with values
#' @param variable_label display variable label if available?
#' @param ... additional arguments passed to \code{\link[stats]{xtabs}}
#' 
#' @seealso \code{\link[stats]{xtabs}}.
#' @examples 
#' data(fecondite)
#' ltabs(~radio, femmes)
#' ltabs(~radio+tv, femmes)
#' ltabs(~radio+tv, femmes, "l")
#' ltabs(~radio+tv, femmes, "v")
#' ltabs(~radio+tv+journal, femmes)
#' ltabs(~radio+tv, femmes, variable_label = FALSE)
#' @export
#' @importFrom stats as.formula
#' @importFrom stats terms
#' @importFrom stats xtabs 

ltabs <- function(formula, data, levels = c("prefixed", "labels", "values"), variable_label = TRUE, ...){
    levels <- match.arg(levels)
    formula <- stats::as.formula(formula)
    if (!is.data.frame(data))
      data <- as.data.frame(data)
    
    vars <- attr(stats::terms(formula), "term.labels")
    
    dn <- vars
    for (i in 1:length(vars))
      if (!is.null(labelled::var_label(data[[vars[i]]])) & variable_label)
        dn[i] <- paste(vars[i], labelled::var_label(data[[vars[i]]]), sep = ": ")
    
    for (v in vars) 
      data[[v]] <- labelled::to_factor(data[[v]], levels = levels)
    
    tab <- stats::xtabs(formula, data, ...)
    names(dimnames(tab)) <- dn
    return(tab)
  }

#' Frequency table of variables
#' 
#' Generate frequency tables for one or more variables in a data frame
#' or a survey design.
#' 
#' @aliases freqtable.default freqtable.survey.design
#' @param .data a data frame or `survey.design` object
#' @param ... one or more expressions accepted by \code{\link[dplyr]{select}}
#' selecting at least one variable
#' @param na.rm Whether to remove missing values in the variables.
#' @param weights If `.data` is a data frame, an optional expression
#' selecting a weighting variable.
#' If `.data` is a survey design, either `TRUE` (the default) to
#' to use survey weights, or `FALSE` or `NULL` to return unweighted
#' frequencies.
#' 
#' @return The result is an array of class `table`.
#' @seealso \code{\link{freq}}, \code{\link{wtd.table}},
#'          \code{\link{xtabs}}, \code{\link{table}}.
#' @examples 
#' data(hdv2003)
#' freqtable(hdv2003, nivetud, sport)
#' freqtable(hdv2003, nivetud, sport, sexe)
#' freqtable(hdv2003, nivetud, sport, weights=poids)
#' freqtable(hdv2003, starts_with("trav"))
#' 
#' library(survey)
#' hdv2003_wtd <- svydesign(ids=~1, weights=~poids, data=hdv2003)
#' freqtable(hdv2003_wtd, nivetud, sport)
#' @export

freqtable <-
    function(.data, ...) {
        UseMethod("freqtable")
    }

#' @rdname freqtable
#' @export
freqtable.default <- function(.data, ..., na.rm = FALSE, weights = NULL) {
    d <- .data |> dplyr::select(..., .weights = {{ weights }})
    if (!".weights" %in% colnames(d)) d$.weights <- 1L
    xtabs(.weights ~ ., data = d, addNA = !na.rm)
}

#' @rdname freqtable
#' @export
freqtable.survey.design <- function(.data, ..., na.rm = FALSE, weights = TRUE) {
    newdata <- .data$variables
    if(isTRUE(weights)) {
        newdata$.weights <- weights(.data)
        wtsvar <- ".weights"
    }
    else {
        wtsvar <- NULL
    }
    freqtable(newdata, ..., na.rm = na.rm, weights = {{ wtsvar }})
}