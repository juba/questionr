#' Odds Ratio
#' 
#' S3 method for odds ratio
#' 
#' @param x object from whom odds ratio will be computed
#' @param ... further arguments passed to or from other methods
#' @author Joseph Larmarange <joseph@@larmarange.net>
#' @export

`odds.ratio` <-
function (x, ...) {
    UseMethod("odds.ratio")
}

#' @rdname odds.ratio
#' @aliases odds.ratio.glm
#' @param level the confidence level required
#' @param digits number of decimal to display
#' @details
#' For models calculated with \code{glm}, \code{x} should have
#' been calculated with \code{family=binomial}.
#' p-value are the same as \code{summary(x)$coefficients[,4]}. 
#' Odds ratio could also be obtained with \code{exp(coef(x))} and 
#' confidence intervals with \code{exp(confint(x))}.
#' @return
#' For \code{glm} or \code{multinom} objects, returns odds ratios, 
#' their confidence interval and tests if they differ from 1.
#' @examples
#' data(hdv2003)
#' reg <- glm(cinema ~ sexe + age, data=hdv2003, family=binomial)
#' odds.ratio(reg)
#' @seealso 
#' \code{\link{glm}} in the \link{stats} package.
#' @export

`odds.ratio.glm` <- 
function(x, level=0.95, digits=3, ...) {
    if (!inherits(x, "glm")) stop("x must be of class 'glm'.")
    if(x$family$family != "binomial" & x$family$family != "quasibinomial")
        stop('x should be a glm with family=binomial or family=quasibinomial.')
    r <- cbind(exp(stats::coef(x)),exp(stats::confint(x, level=level)),summary(x)$coefficients[,4])
    r[,1:3] <- round(r[,1:3],digits=digits)
    colnames(r)[1] <- "OR"
    colnames(r)[4] <- "p"
    stats::printCoefmat(r,signif.stars=TRUE,has.Pvalue=TRUE)
}


#' @rdname odds.ratio
#' @aliases odds.ratio.multinom
#' @details
#' For models calculated with \code{multinom} (nnet),
#' p-value are calculated according to
#' \url{http://www.ats.ucla.edu/stat/r/dae/mlogit.htm}.
#' @seealso 
#' \code{\link[nnet]{multinom}} in the \link[nnet]{nnet} package.
#' @export

`odds.ratio.multinom` <- 
function(x, level=0.95, digits=3, ...) {
    if (!inherits(x, "multinom")) stop("x must be of class 'multinom'.")
    OR <- exp(summary(x)$coefficients)
    ci <- exp(stats::confint(x,level=level))
    ## From http://www.ats.ucla.edu/stat/r/dae/mlogit.htm
    s <- summary(x)
    z <- s$coefficients/s$standard.errors
    p <- p <- (1 - stats::pnorm(abs(z), 0, 1)) * 2
    d <- dim(ci)
    if (is.na(d[3])) { # If only 2 dimensions
      r <- cbind(OR, ci, p)
    } else {
      r <- array(NA,c(d[1]*d[3],d[2]+2))
      dimnames(r)[[1]]<-rep("",d[1]*d[3])
      for (i in 1:d[3]) {
          fl <- (i-1)*d[1] + 1 #first line
          ll <- i*d[1] #last line
          r[fl:ll,] <- cbind(OR[i,],ci[,,i],p[i,])
          rownames(r)[fl:ll] <- paste0(rownames(OR)[i],"/",colnames(OR))
      }
    }
    r[,1:3] <- round(r[,1:3],digits=digits)
    colnames(r) <- c("OR",dimnames(ci)[[2]],"p")
    stats::printCoefmat(r,signif.stars=TRUE,has.Pvalue=TRUE)
}

#' @rdname odds.ratio
#' @aliases odds.ratio.factor
#' @param y a second factor object
#' @return
#' For 2x2 \code{table} or \code{factor} objects, \code{odds.ratio}
#' is a wrapper for \code{fisher.test}.
#' @examples
#' odds.ratio(hdv2003$sport, hdv2003$cuisine)
#' @export

`odds.ratio.factor` <- 
function(x, y, level=0.95, ...) {
    if (!inherits(x, "factor")) stop("x must be of class 'factor'.")
    stats::fisher.test(x, y, conf.level=level)
}

#' @rdname odds.ratio
#' @aliases odds.ratio.table
#' @examples
#' odds.ratio(table(hdv2003$sport, hdv2003$cuisine))
#' @seealso 
#' \code{\link{fisher.test}} in the \link{stats} package.
#' @export

`odds.ratio.table` <- 
function(x, level=0.95, ...) {
    if (!inherits(x, "table")) stop("x must be of class 'table'.")
    stats::fisher.test(x, conf.level=level)
}
