## Internal function
## wrapping to labelled package if available
## doing a minimum overwise (haven compatibility only)

.get_var_label <- function(x) {
  if (requireNamespace("labelled", quietly = TRUE))
    return(labelled::var_label(x))
  else
    if (is.data.frame(x))
      return(lapply(x, .get_var_label))
    else
      return(attr(x, "label", exact = TRUE))
}

.get_val_labels <- function(x) {
  if (requireNamespace("labelled", quietly = TRUE)) {
    return(labelled::val_labels(x))
  }
  else
    return(attr(x, "labels", exact = TRUE))
}

.to_factor <- function(x, levels = c("labels", "values", "prefixed")) {
  if (requireNamespace("labelled", quietly = TRUE)) {
    levels <- match.arg(levels)
    if (inherits(x, "labelled"))
      return(labelled::to_factor(x, levels = levels))
    else 
      return(labelled::to_factor(x))
  } else {
    return(as.factor(x))
  }
}

