#' Look for keywords variable names and descriptions
#' 
#' \code{lookfor} emulates the \code{lookfor} Stata command in R. It supports
#' searching into the variable names of regular R data frames as well as into
#' SPSS and Stata datasets loaded in R via the \code{foreign} or \code{memisc}
#' packages, in which case it will also search variable descriptions (labels).
#' The command is meant to help users finding variables in large datasets.
#' 
#' @param data a data frame that can be annotated by the \code{foreign} or \code{memisc} packages.
#' @param keywords a character string, which can be formatted as a regular expression suitable for a \code{grep} pattern, or a vector of keywords; displays all variables by default
#' @param labels whether or not to search variable labels (descriptions); \code{TRUE} by default
#' @param ignore.case whether or not to make the keywords case sensitive; \code{TRUE} by default (case is ignored during matching)
#' @return a data frame featuring the variable position, name and description (if it exists) in the original data frame
#' @details The function looks into the variable names for matches to the keywords. If the data frame has been read into R with a method that provides variable labels (\code{read.dta} or \code{read.spss} in the \code{foreign} package, or any of the importer methods of the \code{memisc} package), then variable labels are included in the search scope.
#' @author François Briatte <f.briatte@@gmail.com>
#' @examples
#' # Look for a single keyword.
#' lookfor(iris, "petal")
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
#' @source Based on the behaviour of the \code{lookfor} command in Stata. Future versions might include fuzzey search as featured by the \code{query} function of the \code{memisc} package, which also searches value labels and therefore offers a wider search scope.
#' @seealso \code{query} in the \code{memisc} package
#' @export

lookfor <- function(data, 
                    keywords = "", 
                    labels = TRUE, 
                    ignore.case = TRUE) {
  # search scope
  n <- names(data)
  if(!length(n)) stop("there are no names to search in that object")
  # search function
  look <- function(x) { grep(paste(keywords, collapse="|"), x, ignore.case = ignore.case) }
  # names search
  x <- look(n)
  variable <- n[x]
  # foreign objects
  l <- attr(data, "variable.labels")
  if(is.null(l)) l <- attr(data, "var.labels")
  # memisc objects
  if(grepl("data.set|importer", class(data))) {
      suppressMessages(suppressWarnings(requireNamespace("memisc")))
      l <- as.vector(memisc::description(data))
  }
  if(length(l) & labels) {
    # search labels
    y <- look(l)
    # remove duplicates, reorder
    x <- sort(c(x, y[!(y %in% x)]))
    # add variable labels
    variable <- n[x]
    label <- l[x]
    variable <- cbind(variable, label)
  }
  # output
  if(length(x)) return(as.data.frame(variable, x))
    else message("Nothing found. Sorry.")
}
