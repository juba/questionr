## Internal function
## wrapping to sjmisc if available
## doing a minimum overwise (haven compatibility only)

.get_var_label <- function(x) {
  if (requireNamespace("labelled", quietly = TRUE))
    return(labelled::var_label(x))
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

.as_factor <- function(x, levels = c("prefixed", "labels", "values")) {
  if (requireNamespace("labelled", quietly = TRUE)) {
    return(labelled::as_factor(x, levels))
  } else {
    return(as.factor(x))
  }
}