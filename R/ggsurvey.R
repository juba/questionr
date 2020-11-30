#' Easy ggplot2 with survey object
#'
#' A function to facilitate \code{ggplot2} graphs using a survey object.
#' It will initiate a ggplot and map survey weights to the
#' corresponding aesthetic.
#'
#' Graphs will be correct as long as only weights are required
#' to compute the graph. However, statistic or geometry requiring
#' correct variance computation (like 
#' \code{\link[ggplot2:geom_smooth]{ggplot2::geom_smooth()}}) will
#' be statistically incorrect.
#'
#' @param design A survey design object, usually created with 
#' \code{\link[survey:svydesign]{survey::svydesign()}}
#' @param mapping Default list of aesthetic mappings to use for plot,
#' to be created with \code{\link[ggplot2:aes]{ggplot2::aes()}}.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @importFrom stats weights
#' @export
#' @examples
#' if (require(survey) & require(ggplot2)) {
#'   data(api)
#'   dstrat <- svydesign(
#'     id = ~1, strata = ~stype,
#'     weights = ~pw, data = apistrat,
#'     fpc = ~fpc
#'   )
#'   ggsurvey(dstrat) +
#'     aes(x = cnum, y = dnum) +
#'     geom_count()
#'
#'   d <- as.data.frame(Titanic)
#'   dw <- svydesign(ids = ~1, weights = ~Freq, data = d)
#'   ggsurvey(dw) + 
#'     aes(x = Class, fill = Survived) + 
#'     geom_bar(position = "fill")
#' }
ggsurvey <- function(design = NULL, mapping = NULL, ...) {
  if (!inherits(design, "survey.design")) {
    stop("'design' should be a 'survey.design' object.")
  }
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("survey package is required.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required.")
  }
  data <- design$variables
  data$.weights <- weights(design)
  
  if(is.null(mapping)) {
    mapping <- ggplot2::aes()
  }
  
  mapping$weight <- ggplot2::aes_string(weight = ".weights")$weight
  
  ggplot2::ggplot(data, mapping, ...)
}
