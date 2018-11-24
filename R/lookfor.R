#' Look for keywords variable names and descriptions
#' 
#' \code{lookfor} emulates the \code{lookfor} Stata command in R. It supports
#' searching into the variable names of regular R data frames as well as into
#' SPSS and Stata datasets loaded in R via the \pkg{haven}, 
#' in which case it will also search variable descriptions (labels).
#' The command is meant to help users finding variables in large datasets.
#' 
#' @param data a data frame
#' @param ... list of keywords, a character string (or several character strings), which can be formatted as a regular expression suitable for a \code{grep} pattern, or a vector of keywords; displays all variables if not specified
#' @param labels whether or not to search variable labels (descriptions); \code{TRUE} by default
#' @param ignore.case whether or not to make the keywords case sensitive; 
#' \code{TRUE} by default (case is ignored during matching)
#' @param details add details about each variable (see examples)
#' @return a data frame featuring the variable position, name and description 
#' (if it exists) in the original data frame
#' @details The function looks into the variable names for matches to the keywords. If the data frame 
#' has been imported into R with \pkg{haven} package, then variable labels are included in the search scope.
#' If \pkg{labelled} package is installed, variable labels of data.frame imported with \pkg{foreign} or
#' \pkg{memisc} packages will also be taken into account.
#' @author François Briatte <f.briatte@@gmail.com>
#' @examples
#' lookfor(iris)
#' # Look for a single keyword.
#' lookfor(iris, "petal")
#' lookfor(iris, "s")
#' # Look for with a regular expression
#' lookfor(iris, "petal|species")
#' lookfor(iris, "s$")
#' # Look for with several keywords
#' lookfor(iris, "pet", "sp")
#' lookfor(iris, "pet", "sp", "width")
#' # Load memisc package and example data.
#' \dontrun{require(memisc)
#' nes1948.por <- UnZip("anes/NES1948.ZIP","NES1948.POR", package="memisc")
#' nes1948 <- spss.portable.file(nes1948.por)
#' # Look for a vector of keywords.
#' lookfor(nes1948, c("Truman", "Dewey"))
#' # Look for a regular expression.
#' lookfor(nes1948, "truman|dewey")
#' # Look for a phrase.
#' lookfor(nes1948, "personal attribute")}
#' # Labelled data
#' data(fecondite)
#' lookfor(femmes)
#' lookfor(femmes, "date")
#' # Display details
#' lookfor(femmes, details = TRUE)
#' @source Based on the behaviour of the \code{lookfor} command in Stata.
#' @seealso \code{query} in the \code{memisc} package
#' @export

lookfor <- function(data, 
                    ..., 
                    labels = TRUE, 
                    ignore.case = TRUE,
                    details = FALSE) {
  # applying to_labelled
  data <- labelled::to_labelled(data)
  # search scope
  n <- names(data)
  if(!length(n)) stop("there are no names to search in that object")
  # search function
  keywords <- c(...)
  if(is.null(keywords)) keywords <- names(data)
  look <- function(x) { grep(paste(keywords, collapse="|"), x, ignore.case = ignore.case) }
  # names search
  x <- look(n)
  variable <- n[x]
  # variable labels
  l <- unlist(labelled::var_label(data))
  if(length(l) > 0 & labels) {
    # search labels
    y <- look(l)
    variable <- unique(c(variable, names(l[y])))
  } 
  # output
  if(length(variable)) {
    pos <- which(n %in% variable)
    # reordering according to pos
    # not forgetting that some variables don't have a label
    if (length(l))
      res <- data.frame(variable = n[pos], label = l[n[pos]], row.names = pos, stringsAsFactors = FALSE)
    else
      res <- data.frame(variable = n[pos], row.names = pos, stringsAsFactors = FALSE)
    
    if (details) {
      res$class <- ""
      res$type <- ""
      res$levels <- ""
      res$value_labels <- ""
      res$unique_values <- NA
      res$n_na <- NA
      res$na_values <- ""
      res$na_range <- ""
      for (i in 1:nrow(res)) {
        v <- res$variable[i]
        res$class[i] <- paste(class(data[[v]]), collapse = ", ")
        res$type[i] <- paste(typeof(data[[v]]), collapse = ", ")
        if (is.factor(data[[v]]))
          res$levels[i] <- paste(levels(data[[v]]), collapse = "; ")
        if (inherits(data[[v]], "haven_labelled")) {
          res$value_labels[i] <- paste(names(labelled::val_labels(data[[v]], prefixed=TRUE)), collapse = "; ")
          res$na_values[i] <- paste(labelled::na_values(data[[v]]), collapse = ", ")
          res$na_range[i] <- paste(labelled::na_range(data[[v]]), collapse = "-")
        }
        res$unique_values[i] <- length(unique(data[[v]]))
        res$n_na[i] <- sum(is.na(data[[v]]))
      }
    }
    
    return(res)
  } else { 
    message("Nothing found. Sorry.")
  }
}