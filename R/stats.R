#' Compute Cramer's V of a two-way frequency table
#'
#' This function computes Cramer's V for a two-way frequency table
#'
#' @param tab table on which to compute the statistic
#' @keywords univar
#' @export
#' @examples
#' data(Titanic)
#' tab <- apply(Titanic, c(4,1), sum)
#' #' print(tab)
#' cramer.v(tab)

`cramer.v` <-
function(tab) {
  n <- sum(tab)
  chid <- stats::chisq.test(tab,correct=FALSE)$statistic
  dim <- min(nrow(tab),ncol(tab)) - 1
  as.numeric(sqrt(chid/(n*dim)))
}

