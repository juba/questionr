`cprop` <-
function (tab, digits = 1, total = TRUE, percent=FALSE) {
  dn <- names(dimnames(tab))
  if (total) tab <- cbind(tab, Ensemble=apply(tab,1,sum))
  tab <- prop.table(tab,2)*100
  if (total) tab <- rbind(tab,Total=apply(tab,2,sum))
  result <- as.table(tab)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  return(result)
}

`format.proptab` <-
function (x, digits=NULL, percent=NULL, justify="right", ...) {
  if (!inherits(x, "proptab")) stop("Le tableau n'est pas de classe proptab")
  if (is.null(digits)) digits <- attr(x, "digits")
  if (is.null(percent)) percent <- attr(x, "percent")
  if (percent) {
    fmt <- paste("%.",digits,"f%%",sep="")
    x[,] <- sprintf(x, fmt=fmt)
    result <- format.default(x,justify=justify, ...)
  }
  else
    result <- format.default(round(x,digits), ...)
  return(result)
}

`freq` <-
function (x, digits=1, cum=FALSE, total=FALSE, exclude=NULL, sort="") {
  if (is.factor(x)) x <- factor(x, exclude=exclude)
  if (is.table(x)) tab <- x
  else tab <- table(x, exclude=exclude)
  effectifs <- as.vector(tab)
  pourc <- as.vector(effectifs/sum(effectifs)*100)
  result <- data.frame(n=effectifs, pourc=pourc)
  rownames(result) <- ifelse(is.na(names(tab)),"NA",names(tab))
  if (sort=="inc") result <- result[order(result$n),]
  if (sort=="dec") result <- result[order(result$n, decreasing=TRUE),]
  if (total) result <- rbind(result, Total=apply(result,2,sum))
  if (cum) {
    pourc.cum <- cumsum(result$pourc)
    if (total) pourc.cum[length(pourc.cum)] <- 100
    result <- cbind(result, pourc.cum)
  }
  names(result)[which(names(result)=="pourc")] <- "%"
  names(result)[which(names(result)=="pourc.cum")] <- "%cum"
  round(result, digits=digits)
}

`lprop` <-
function(tab, digits=1, total=TRUE, percent=FALSE) {
  dn <- names(dimnames(tab))
  if (total) tab <- rbind(tab, Ensemble=apply(tab,2,sum))
  tab <- prop.table(tab,1)*100
  if (total) tab <- cbind(tab, Total=apply(tab,1,sum))
  result <- as.table(tab)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  return(result)
}

`prop` <-
function (tab, digits=1, total=TRUE, percent=FALSE) {
  dn <- names(dimnames(tab))
  tmp <- tab/sum(tab)*100
  if (total) {
    tmp <- rbind(tmp,Total=apply(tmp,2,sum))
    tmp <- cbind(tmp,Total=apply(tmp,1,sum))
  }
  result <- as.table(tmp)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  return(result)
}

`residus` <-
function (tab, digits=2) {
  round(chisq.test(tab)$residuals, digits)
}

`theff` <-
function (tab, digits=2) {
  round(chisq.test(tab)$expected, digits)
}

`thprop` <-
function (tab, digits=1, percent=FALSE) {
  dn <- names(dimnames(tab))
  tmp <- as.vector(apply(tab,1,sum)/sum(tab))%*%t(as.vector(apply(tab,2,sum)/sum(tab)))
  colnames(tmp) <- colnames(tab)
  rownames(tmp) <- rownames(tab)
  result <- as.table(tmp*100)
  names(dimnames(result)) <- dn
  class(result) <- c("proptab", class(result))
  attr(result, "percent") <- percent
  attr(result, "digits") <- digits
  return(result)
}

`wtd.table` <-
function (x, y = NULL, weights = NULL, normwt = FALSE, na.rm = TRUE) 
{
  if (is.null(weights)) weights <- rep(1, length(x))  
  if (length(x) != length(weights)) stop("x et weights doivent etre de meme longueur")
  if (!is.null(y) & (length(x) != length(y))) stop("x et y doivent etre de meme longueur")
  if (na.rm) {
     s <- !is.na(x) & !is.na(weights)
     if (!is.null(y)) s <- s & !is.na(y)
     x <- x[s, drop = FALSE]
     if (!is.null(y)) y <- y[s, drop = FALSE]
     weights <- weights[s]
  }
  if (normwt) {
    weights <- weights * length(x)/sum(weights)
  }
  if (is.null(y)) {
    result <- tapply(weights, x, sum, simplify=TRUE)
  }
  else {
    result <- tapply(weights, list(x,y), sum, simplify=TRUE)
  }
  result[is.na(result)] <- 0
  as.table(result)
}

