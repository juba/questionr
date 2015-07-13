## Internal function
## wrapping to sjmisc if available
## doing a minimum overwise (haven compatibility only)

.get_var_labels <- function(x) {
  if (requireNamespace("sjmisc", quietly = TRUE))
    return(sjmisc::get_var_labels(x))
  else
    return(attr(x, "label"))
}

.get_val_labels <- function(x) {
  if (requireNamespace("sjmisc", quietly = TRUE)) {
    tmp <- sjmisc::get_val_labels(x, include.values = "n")
    res <- names(tmp)
    names(res) <- tmp
    return(res)
  }
    
  else
    return(attr(x, "labels"))
}

.as_factor <- function(x, levels = c("prefixed", "labels", "values")) {
  levels <- match.arg(levels)
  if (is.factor(x)) return(x)
  if (!is.atomic(x)) stop("x should be a vector.")
  
  v <- unique(x[!is.na(x)])
  if (is.null(.get_val_labels(x)))
    l <- data.frame(values = v, labels = v)
  else if (levels == "values") 
    l <- data.frame(values = v, labels = v)
  else 
    l <- data.frame(values = .get_val_labels(x), labels = names(.get_val_labels(x)))
  
  if (levels == "prefixed")
    l$labels <- paste0("[", l$values, "] ", l$labels)
  
  v <- data.frame(values = v)
  v$values <- as.character(v$values)
  l$values <- as.character(l$values)
  l$labels <- as.character(l$labels)
  v <- merge(l, v, by = "values", all.x = TRUE, all.y = TRUE)
  v[is.na(v$labels), "labels"] <- v[is.na(v$labels), "values"] 
  
  res <- factor(x, levels = v$values, labels = v$labels)
  attr(res, "label") <- .get_var_labels(x)
  return(res)
}