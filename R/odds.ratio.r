#' Odds Ratio
#' 
#' S3 method for odds ratio
#' 
#' @aliases odds.ratio odds.ratio.glm odds.ratio.multinom
#' @param x object from whom odds ratio will be computed
#' @param ... additional parameters
#' @author Joseph Larmarange <joseph@@larmarange.net>
#' @export odds.ratio


odds.ratio <-
	function (x, ...) {
		UseMethod("odds.ratio")
	}

#' @rdname odds.ratio
#' @method odds.ratio glm
#' @S3method odds.ratio glm
#' @aliases odds.ratio.glm
#' @param level the confidence level required
#' @param digits number of decimal to display
#' @details
#' Odds ratio could also be obtained with \code{exp(coef(x))} and confidence
#' intervals with \code{exp(confint(x))}.
#' 
#' For models calculated with \code{glm}, p-value are the same as
#' \code{summary(x)$coefficients[,4]}.
#' @return
#' For \code{glm} or \code{multinom} objects, returns odds ratios, 
#' their confidence interval and tests if they differ from 1.
#' @examples
#' data(hdv2003)
#' reg <- glm(cinema ~ sexe + age, data=hdv2003, family=binomial)
#' odds.ratio(reg)
#' @seealso 
#' \code{\link{glm}} in the \link{stats} package.
#' @export odds.ratio.glm

odds.ratio.glm <- 
  function(x, level=0.95, digits=3) {
	if (!inherits(x, "glm")) stop("x must be of class 'glm'.")
  	if(x$family$family != "binomial")
  		stop('x should be a glm with family=binomial.')
  	r <- cbind(exp(coef(x)),exp(confint(x, level=level)),summary(x)$coefficients[,4])
  	r[,1:3] <- round(r[,1:3],digits=digits)
  	colnames(r)[1] <- "OR"
  	colnames(r)[4] <- "p"
  	printCoefmat(r,signif.stars=TRUE,has.Pvalue=TRUE)
  }


#' @rdname odds.ratio
#' @method odds.ratio multinom
#' @S3method odds.ratio multinom
#' @aliases odds.ratio.multinom
#' @details
#' For models calculated with \code{multinom} (nnet),
#' p-value are calculated according to
#' \url{http://www.ats.ucla.edu/stat/r/dae/mlogit.htm}.
#' @seealso 
#' \code{\link[nnet]{multinom}} in the \link[nnet]{nnet} package.
#' @export odds.ratio.glm

odds.ratio.multinom <- 
  function(x, level=0.95, digits=3) {
  	if (!inherits(x, "multinom")) stop("x must be of class 'multinom'.")
  	coef <- summary(x)$coefficients
  	ci <- confint(x,level=level)
  	# From http://www.ats.ucla.edu/stat/r/dae/mlogit.htm
  	z <- summary(x)$coefficients/summary(x)$standard.errors
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