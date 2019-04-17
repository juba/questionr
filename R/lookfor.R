#' Look for keywords variable names and descriptions
#' 
#' \code{lookfor} emulates the \code{lookfor} Stata command in R. It supports
#' searching into the variable names of regular R data frames as well as into
#' SPSS and Stata datasets loaded in R via the \pkg{haven}, 
#' in which case it will also search variable descriptions (labels).
#' The command is meant to help users finding variables in large datasets.
#' 
#' @inheritParams labelled::lookfor
#' @importFrom labelled lookfor
#' @return a data frame featuring the variable position, name and description 
#' (if it exists) in the original data frame
#' @details The function, imported from \pkg{labelled}, looks into the variable names for matches to the keywords. 
#' If the data frame has been imported into R with \pkg{haven} package, 
#' then variable labels are included in the search scope.
#' If \pkg{labelled} package is installed, variable labels of data.frame 
#' imported with \pkg{foreign} or
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
#' # Labelled data
#' data(fecondite)
#' lookfor(femmes)
#' lookfor(femmes, "date")
#' # Display details
#' lookfor(femmes, details = TRUE)
#' @source Based on the behaviour of the \code{lookfor} command in Stata.
#' @seealso \code{query} in the \code{memisc} package
#' @export

lookfor <- labelled::lookfor

#' @rdname lookfor
#' @inheritParams labelled::look_for
#' @importFrom labelled look_for
#' @export
look_for <- labelled::look_for