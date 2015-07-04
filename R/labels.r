#' @title Retrieve value labels of a data frame or variable
#' @name get_val_labels
#'
#' @description This function retrieves the value labels of an imported
#'                SPSS, SAS or STATA data set (with \pkg{haven} or \pkg{foreign}) and
#'                \itemize{
#'                  \item if \code{x} is a data frame or list of variables, returns the all variable's value labels as \code{\link{list}}
#'                  \item or, if \code{x} is a vector, returns the label as string.
#'                  }
#' @param x a \code{data.frame} with variables that have attached value labels; a variable
#'          (vector) with attached value labels; or a \code{list} of variables
#'          with attached values labels. See 'Examples'.
#' @param include.values string, indicating whether the values associated with the
#'          value labels are returned as well. If \code{include.values = "as.name"}
#'          (or \code{include.values = "n"}), values are set as \code{\link{names}}
#'          attribute of the returned object. If \code{include.values = "as.prefix"}
#'          (or \code{include.values = "p"}), values are includes as prefix
#'          to each label. See 'Examples'.
#' @return Either a list with all value labels from all variables if \code{x}
#'           is a \code{data.frame} or \code{list}; a string with the value
#'           labels, if \code{x} is a variable;
#'           or \code{NULL} if no value label attribute was found.
#'
#' @source \url{https://github.com/sjPlot/sjmisc}
#' 
#' @seealso \code{\link{get_var_labels}}
#'
#' @examples
#' \dontrun{
#' load(url("http://larmarange.github.io/analyse-R/data/enquete_menages.Rdata"))
#' get_val_labels(femmes)
#' get_val_labels(femmes$matri)
#' get_val_labels(femmes$matri, "n")
#' get_val_labels(femmes$matri, "p")
#' }
#' 
#' @export
get_val_labels <- function(x, include.values = NULL) {
  # .Deprecated("get_labels")
  if (is.data.frame(x) || is.matrix(x) || is.list(x)) {
    a <- lapply(x, FUN = sji.getValueLabel, include.values)
  } else {
    a <- sji.getValueLabel(x, include.values)
  }
  return(a)
}


#' @name get_labels
#' @rdname get_val_labels
#' @export
get_labels <- function(x, include.values = NULL) {
  return(get_val_labels(x, include.values))
}


sji.getValueLabel <- function(x, include.values = NULL) {
  labels <- NULL
  # haven or sjPlot?
  attr.string <- getValLabelAttribute(x)
  # nothing found? then leave...
  if (is.null(attr.string)) return(NULL)
  # retrieve named labels
  lab <- attr(x, attr.string, exact = T)
  # check if we have anything
  if (!is.null(lab)) {
    # retrieve values associated with labels
    values <- as.numeric(unname(attr(x, attr.string, exact = T)))
    # retrieve order of value labels
    reihenfolge <- order(values)
    # retrieve label values in correct order
    labels <- names(lab)[reihenfolge]
    # include associated values?
    if (!is.null(include.values)) {
      # for backwards compatibility, we also accept "TRUE"
      # here we set values as names-attribute
      if ((is.logical(include.values) && include.values == TRUE) ||
          include.values == "as.name" ||
          include.values == "n") {
        names(labels) <- values[reihenfolge]
      }
      # here we include values as prefix of labels
      if (include.values == "as.prefix" || include.values == "p") {
        labels <- sprintf("[%i] %s", values[reihenfolge], labels)
      }
    }
  }
  # return them
  return(labels)
}


#' @title Retrieve values of labelled variables
#' @name get_values
#'
#' @description This function retrieves the values associated with value labels
#'                of an imported SPSS, SAS or STATA data set (with \pkg{haven} or \pkg{foreign}).
#'
#' @seealso \code{\link{get_labels}}
#'
#' @param x a variable (vector) with attached value labels.
#' @param sort.val logical, if \code{TRUE} (default), values of associated value labels
#'          are sorted.
#' @return The values associated with value labels from \code{x}
#'
#' @details Labelled vectors are numeric by default and have variable and value labels attached.
#'            The value labels are associated with those values from the labelled vector.
#'            This function returns the values associated with the vector's value labels,
#'            which may differ from actual values in the vector (e.g. due to missings)
#'            or are not represented in sorted order.
#'            
#' @source \url{https://github.com/sjPlot/sjmisc}
#'
#' @export
get_values <- function(x, sort.val = TRUE) {
  return(sji.getValueLabelValues(x, sort.val))
}


sji.getValueLabelValues <- function(x, sort.val = TRUE) {
  # haven or sjPlot?
  attr.string <- getValLabelAttribute(x)
  # nothing found? then leave...
  if (is.null(attr.string)) return(NULL)
  # get values
  val.sort <- as.numeric(unname(attr(x, attr.string, exact = T)))
  # sort values
  if (sort.val) val.sort <- sort(val.sort)
  # return sorted
  return(val.sort)
}



#' @title Retrieve variable label(s) of a data frame or variable
#' @name get_var_labels
#'
#' @description This function retrieves the variable labels of an imported
#'                SPSS, SAS or STATA data set (via (with \pkg{haven} or \pkg{foreign})) and
#'                \itemize{
#'                  \item if \code{x} is a data frame or a list of variables, returns the all variable labels as names character vector of length \code{ncol(x)}.
#'                  \item or, if \code{x} is a vector, returns the variable label as string.
#'                  }
#'
#' @param x a \code{data.frame} with variables that have attached variable labels; a variable
#'          (vector) with attached variable label; or a \code{list} of variables
#'          with attached variable labels. See 'Examples'.
#'
#' @return A named char vector with all variable labels from the data frame or list;
#'           or a simple char vector (of length 1) with the variable label, if \code{x} is a variable.
#'
#' @seealso \code{\link{get_val_labels}}
#' 
#' @source \url{https://github.com/sjPlot/sjmisc}
#'
#' @examples
#' \dontrun{
#' load(url("http://larmarange.github.io/analyse-R/data/enquete_menages.Rdata"))
#' get_var_labels(femmes)
#' }
#'
#' @export
get_var_labels <- function(x) {
  # .Deprecated("get_label")
  # ----------------------------
  # auto-detect variable label attribute
  # ----------------------------
  attr.string <- getVarLabelAttribute(x)
  # do we have a df?
  if (is.data.frame(x) || is.matrix(x)) {
    # if yes, check if we have attached label table
    # from foreign import
    labels <- attr(x, "variable.labels", exact = T)
    # if not, get labels from each single vector
    if (is.null(labels) && !is.null(attr.string)) {
      # return value
      all.labels <- c()
      # iterate df
      for (i in 1:ncol(x)) {
        # get label
        label <- attr(x[[i]], attr.string, exact = T)
        # any label?
        if (!is.null(label)) {
          # name label
          names(label) <- colnames(x)[i]
          # append to return result
          all.labels <- c(all.labels, label)
        } else {
          all.labels <- c(all.labels, "")
        }
      }
      return(all.labels)
    } else {
      return(attr(x, "variable.labels", exact = T))
    }
  } else if (is.list(x)) {
    # nothing found? then leave...
    if (is.null(attr.string)) return(NULL)
    # return attribute of all variables
    return(unlist(lapply(x, attr, attr.string, exact = T)))
  } else {
    # nothing found? then leave...
    if (is.null(attr.string)) return(NULL)
    # else return attribute
    return(attr(x, attr.string, exact = T))
  }
}


#' @name get_label
#' @rdname get_var_labels
#' @export
get_label <- function(x) {
  return(get_var_labels(x))
}


# auto-detect attribute style for variable labels.
# either haven style ("label") or foreign style
# ("variable.label")
getVarLabelAttribute <- function(x) {
  attr.string <- NULL
  # check if x is data frame. if yes, retrieve one "example" variable
  if (is.data.frame(x) || is.list(x)) {
    # define length for loop
    if (is.data.frame(x))
      counter <- ncol(x)
    else
      counter <- length(x)
    # we need to check all variables until first variable
    # that has any attributes at all - SPSS variables without
    # labels would return NULL, so if -e.g.- first variable
    # of data set has no label attribute, but second had, this
    # function would stop after first attribute and return NULL
    for (i in 1:counter) {
      # retrieve attribute names
      an <- names(attributes(x[[i]]))
      # check for label attributes
      if (any(an == "label") || any(an == "variable.label")) {
        x <- x[[i]]
        break
      }
    }
  }
  # check if vector has label attribute
  if (!is.null(attr(x, "label", exact = T))) attr.string <- "label"
  # check if vector has variable label attribute
  if (!is.null(attr(x, "variable.label", exact = T))) attr.string <- "variable.label"
  # not found any label yet?
  if (is.null(attr.string)) {
    # ----------------------------
    # check value_labels option
    # ----------------------------
    opt <- getOption("value_labels")
    if (!is.null(opt)) attr.string <- ifelse(opt == "haven", "label", "variable.label")
  }
  return(attr.string)
}


# auto-detect attribute style for value labels.
# either haven style ("labels") or foreign style
# ("value.labels")
getValLabelAttribute <- function(x) {
  attr.string <- NULL
  # check if x is data frame. if yes, just retrieve one "example" variable
  if (is.data.frame(x)) {
    # find first variable with labels or value.labels attribute
    for (i in 1:ncol(x)) {
      # has any attribute?
      if (!is.null(attr(x[[i]], "labels", exact = T))) {
        attr.string <- "labels"
        break
      } else if (!is.null(attr(x[[i]], "value.labels", exact = T))) {
        attr.string <- "value.labels"
        break
      }
    }
  } else {
    # check if vector has labels attribute
    if (!is.null(attr(x, "labels", exact = T))) attr.string <- "labels"
    # check if vector has value.labels attribute
    if (!is.null(attr(x, "value.labels", exact = T))) attr.string <- "value.labels"
  }
  # not found any label yet?
  if (is.null(attr.string)) {
    # ----------------------------
    # check value_labels option
    # ----------------------------
    opt <- getOption("value_labels")
    if (!is.null(opt)) attr.string <- ifelse(opt == "haven", "label", "variable.label")
  }
  return(attr.string)
}

