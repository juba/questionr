#' Odds Ratios for logistic regression
#' 
#' Computes odds ratios (the exponential of model coefficients), 
#' their confidence intervals and tests if they differ from 1.
#' 
#' @param reg an object of class \code{"glm"} (with \code{family=binomial}) or \code{"multinom"}
#' @param level the confidence level required
#' @param digits number of decimal to display
#' @details
#' Odds ratio could also be obtained with \code{exp(coef(reg))} and confidence
#' intervals with \code{exp(confint(reg))}.
#' 
#' For models calculated with \code{glm}, p-value are the same as
#' \code{summary(reg)$coefficients[,4]}. For models calculated with
#' \code{multinom} (nnet), p-value are calculated according to
#' \url{http://www.ats.ucla.edu/stat/r/dae/mlogit.htm}.
#' @examples
#' data(hdv2003)
#' reg <- glm(cinema ~ sexe + age, data=hdv2003, family=binomial)
#' odds.ratio(reg)
#' @seealso 
#' \code{\link{glm}} in the \link{stats} package and
#'  \code{\link[nnet]{multinom}} in the \link[nnet]{nnet} package.
#' @author Joseph Larmarange <joseph@larmarange.net>
#' @export odds.ratio

odds.ratio <- function(reg, level=0.95, digits=3) {
	if ("glm" %in% class(reg)) {
		if(reg$family$family == "binomial"){
			r <- cbind(exp(coef(reg)),exp(confint(reg, level=level)),summary(reg)$coefficients[,4])
			r[,1:3] <- round(r[,1:3],digits=digits)
			colnames(r)[1] <- "OR"
			colnames(r)[4] <- "p"
			printCoefmat(r,signif.stars=TRUE,has.Pvalue=TRUE)
		} else {
			stop('reg should be a glm with family=binomial or the result of multinom.')
		}
	} else if ("multinom" %in% class(reg)) {
		coef <- summary(reg)$coefficients
		ci <- confint(reg,level=level)
		# From http://www.ats.ucla.edu/stat/r/dae/mlogit.htm
		z <- summary(reg)$coefficients/summary(reg)$standard.errors
		p <- p <- (1 - pnorm(abs(z), 0, 1)) * 2
		d <- dim(ci)
		r <- array(NA,c(d[1]*d[3],d[2]+2))
		dimnames(r)[[1]]<-rep("",d[1]*d[3])
		for (i in 1:d[3]) {
			fl <- (i-1)*d[1] + 1 #first line
			ll <- i*d[1] #last line
			r[fl:ll,] <- cbind(coef[i,],ci[,,i],p[i,])
			rownames(r)[fl:ll] <- paste0(rownames(coef)[i],"/",colnames(coef))
		}
		r[,1:3] <- round(r[,1:3],digits=digits)
		colnames(r) <- c("OR",dimnames(ci)[[2]],"p")
		printCoefmat(r,signif.stars=TRUE,has.Pvalue=TRUE)
	}
	else 
		stop('reg should be a glm with family=binomial or the result of multinom.')
}