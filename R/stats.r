#' Compute Cramer's V of a two-way frequency table
#'
#' This function computes Cramer's V for a two-way frequency table
#'
#' @param tab table on which to compute the statistic
#' @keyword univar
#' @export
#' @examples
#' data(hdv2003)
#' 
#' tab <- table(hdv2003$sexe, hdv2003$peche.chasse))
#' print(tab)
#' cramer.v(tab)

`cramer.v` <-
function(tab) {
  n <- sum(tab)
  chid <- chisq.test(tab,correct=FALSE)$statistic
  dim <- min(nrow(tab),ncol(tab)) - 1
  as.numeric(sqrt(chid/(n*dim)))
}

