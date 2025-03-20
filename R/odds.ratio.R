#' Odds Ratio
#'
#' S3 method for odds ratio
#'
#' @param x object from whom odds ratio will be computed
#' @param ... further arguments passed to or from other methods
#' @author Joseph Larmarange <joseph@@larmarange.net>
#' @export

`odds.ratio` <-
  function(x, ...) {
    UseMethod("odds.ratio")
  }

#' @rdname odds.ratio
#' @aliases odds.ratio.glm
#' @param level the confidence level required
#' @details
#' For models calculated with \code{glm}, \code{x} should have
#' been calculated with \code{family=binomial}.
#' p-value are the same as \code{summary(x)$coefficients[,4]}.
#' Odds ratio could also be obtained with \code{exp(coef(x))} and
#' confidence intervals with \code{exp(confint(x))}.
#' @return
#' Returns a data.frame of class \code{odds.ratio} with odds ratios,
#' their confidence interval and p-values.
#' @examples
#' data(hdv2003)
#' reg <- glm(cinema ~ sexe + age, data = hdv2003, family = binomial)
#' odds.ratio(reg)
#' @seealso
#' \code{\link{glm}} in the \link{stats} package.
#' @export

`odds.ratio.glm` <-
  function(x, level = 0.95, ...) {
    if (!inherits(x, "glm")) stop("x must be of class 'glm'.")
    if (x$family$family != "binomial" && x$family$family != "quasibinomial") {
      stop("x should be a glm with family=binomial or family=quasibinomial.")
    }
    r <- cbind(exp(stats::coef(x)), exp(stats::confint(x, level = level)), summary(x)$coefficients[, 4])
    colnames(r)[1] <- "OR"
    colnames(r)[4] <- "p"
    r <- as.data.frame(r)
    class(r) <- c("odds.ratio", "data.frame")
    r
  }

#' @rdname odds.ratio
#' @aliases odds.ratio.multinom
#' @details
#' For models calculated with \code{multinom} (nnet),
#' p-value are calculated according to
#' \url{https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/}.
#' @seealso
#' \code{\link[nnet]{multinom}} in the \link[nnet]{nnet} package.
#' @export

`odds.ratio.multinom` <-
  function(x, level = 0.95, ...) {
    if (!inherits(x, "multinom")) stop("x must be of class 'multinom'.")
    OR <- exp(summary(x)$coefficients)
    ci <- exp(stats::confint(x, level = level))
    ## From https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
    s <- summary(x)
    z <- s$coefficients / s$standard.errors
    p <- p <- (1 - stats::pnorm(abs(z), 0, 1)) * 2
    d <- dim(ci)
    if (is.na(d[3])) { # If only 2 dimensions
      r <- cbind(OR, ci, p)
    } else {
      r <- array(NA, c(d[1] * d[3], d[2] + 2))
      dimnames(r)[[1]] <- rep("", d[1] * d[3])
      for (i in 1:d[3]) {
        fl <- (i - 1) * d[1] + 1 # first line
        ll <- i * d[1] # last line
        r[fl:ll, ] <- cbind(OR[i, ], ci[, , i], p[i, ])
        rownames(r)[fl:ll] <- paste0(rownames(OR)[i], "/", colnames(OR))
      }
    }
    colnames(r) <- c("OR", dimnames(ci)[[2]], "p")
    r <- as.data.frame(r)
    class(r) <- c("odds.ratio", "data.frame")
    r
  }

#' @rdname odds.ratio
#' @aliases odds.ratio.factor
#' @param fac a second factor object
#' @details
#' For 2x2 \code{table}, \code{factor} or \code{matrix}, \code{odds.ratio}
#' uses \code{\link[stats]{fisher.test}} to compute the odds ratio.
#' @examples
#' odds.ratio(hdv2003$sport, hdv2003$cuisine)
#' @export

`odds.ratio.factor` <-
  function(x, fac, level = 0.95, ...) {
    if (!inherits(x, "factor")) stop("x must be of class 'factor'.")
    ft <- stats::fisher.test(x, fac, conf.level = level)
    r <- data.frame(OR = ft$estimate, lower = ft$conf.int[1], upper = ft$conf.int[2], p = ft$p.value)
    names(r)[2] <- paste(100 * (1 - level) / 2, "%")
    names(r)[3] <- paste(100 * (1 - (1 - level) / 2), "%")
    rownames(r) <- "Fisher's test"
    class(r) <- c("odds.ratio", "data.frame")
    r
  }

#' @rdname odds.ratio
#' @aliases odds.ratio.table
#' @examples
#' odds.ratio(table(hdv2003$sport, hdv2003$cuisine))
#' @seealso
#' \code{\link[stats]{fisher.test}} in the \link{stats} package.
#' @export
#' @importFrom stats fisher.test

`odds.ratio.table` <-
  function(x, level = 0.95, ...) {
    if (!inherits(x, "table")) stop("x must be of class 'table'.")
    ft <- stats::fisher.test(x, conf.level = level)
    r <- data.frame(OR = ft$estimate, lower = ft$conf.int[1], upper = ft$conf.int[2], p = ft$p.value)
    names(r)[2] <- paste(100 * (1 - level) / 2, "%")
    names(r)[3] <- paste(100 * (1 - (1 - level) / 2), "%")
    rownames(r) <- "Fisher's test"
    class(r) <- c("odds.ratio", "data.frame")
    r
  }

#' @rdname odds.ratio
#' @aliases odds.ratio.matrix
#' @examples
#' M <- matrix(c(759, 360, 518, 363), ncol = 2)
#' odds.ratio(M)
#' @export
#' @importFrom stats fisher.test

`odds.ratio.matrix` <-
  function(x, level = 0.95, ...) {
    if (!inherits(x, "matrix")) stop("x must be of class 'matrix'.")
    ft <- stats::fisher.test(x, conf.level = level)
    r <- data.frame(OR = ft$estimate, lower = ft$conf.int[1], upper = ft$conf.int[2], p = ft$p.value)
    names(r)[2] <- paste(100 * (1 - level) / 2, "%")
    names(r)[3] <- paste(100 * (1 - (1 - level) / 2), "%")
    rownames(r) <- "Fisher's test"
    class(r) <- c("odds.ratio", "data.frame")
    r
  }

#' @rdname odds.ratio
#' @aliases odds.ratio.numeric
#' @param y a second numeric object
#' @return
#' If \code{x} and \code{y} are proportions, \code{odds.ratio} simply
#' returns the value of the odds ratio, with no confidence interval.
#' @examples
#' odds.ratio(0.26, 0.42)
#' @export

`odds.ratio.numeric` <-
  function(x, y, level = 0.95, ...) {
    if (!inherits(x, "numeric")) stop("x must be numeric.")
    if (length(x) > 1) stop("x should be a simple value.")
    if (x <= 0 || x >= 1) stop("x should be between 0 and 1")
    if (!inherits(y, "numeric")) stop("y must be numeric.")
    if (length(y) > 1) stop("y should be a simple value.")
    if (y <= 0 || y >= 1) stop("y should be between 0 and 1")
    return(x * (1 - y) / (y * (1 - x)))
  }

#' @rdname odds.ratio
#' @param signif.stars logical; if \code{TRUE}, p-values are encoded visually as 'significance stars'
#' @seealso
#' \code{\link[stats]{printCoefmat}} in the \link{stats} package.
#' @export

`print.odds.ratio` <-
  function(x, signif.stars = TRUE, ...) {
    if (!inherits(x, "odds.ratio")) stop("x must be of class 'odds.ratio'")
    stats::printCoefmat(x, signif.stars = signif.stars, has.Pvalue = TRUE, ...)
  }
