#' Describe the variables of a dataset in R, SPSS or Stata formats
#'
#' This function describes the variables of a dataset that might
#' include labels imported with the foreign or memisc packages.
#'
#' @param data dataset
#' @param ... variable names as character strings. Using the "*" or "|" wildcards in a variable name will search for it using a regex match.
#' @details
#' This function wraps around the \code{description} function of the 
#' \code{memisc} package and fetches the variable labels in datasets
#' imported with the \code{foreign} package.
#' @seealso
#' \code{\link{regex}}, \code{description} in the \code{memisc} package
#' @author François Briatte <f.briatte@@gmail.com>
#' @examples
#' # Variables in a standard R data frame.
#' data(hdv2003)
#' # Find the names of selected variables.
#' describe(hdv2003, "age", "sexe", "lecture*", "frere|soeur")
#' # Load memisc package and example data.
#' \dontrun{require(memisc)
#' nes1948.por <- UnZip("anes/NES1948.ZIP","NES1948.POR", package="memisc")
#' nes1948 <- spss.portable.file(nes1948.por)
#' # Get the labels of selected variables.
#' describe(nes1948, "v480050", "version|set", "v48004*")}
#' @export

describe <- function(data, ...) {
  # select variables
  x <- c(...)
  if(is.null(x)) x <- names(data)
  # multiple matcher
  q <- x[grepl("\\||\\*", x)]
  if(length(q)) {
    m <- sapply(q, function(i) grepl(gsub("\\*", "", i), names(data)))
    m <- unlist(lapply(1:nrow(m), function(i) any(m[i, ])))
    x <- c(names(data)[m], x[!grepl("\\||\\*", x)])
  }
  # memisc objects
  if(grepl("data.set|importer", class(data))) {
      suppressMessages(suppressWarnings(requireNamespace("memisc")))
      l <- memisc::description(data[, x])
  }
  else {
    # foreign objects
    l <- as.vector(attr(data, "variable.labels"))
    if(is.null(l)) l <- attr(data, "var.labels")
    l <- cbind(variable = names(data), label = l)
    l <- l[which(l[, 1] %in% x), ]
  }
  return(l)
}
