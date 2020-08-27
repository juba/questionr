#' Describe the variables of a data.frame
#'
#' This function describes the variables of a vector or a dataset that might
#' include labels imported with \pkg{haven} packages.
#' 
#' @param x object to describe
#' @param ... further arguments passed to or from other methods, see details
#' @return an object of class \code{description}.
#' @author Joseph Larmarange <joseph@@larmarange.net>
#' @export

`describe` <-
  function (x, ...) {
    UseMethod("describe")
  }

#' @rdname describe
#' @aliases describe.factor
#' @param n number of first values to display
#' @param show.length display length of the vector?
#' @param freq.n.max display a frequency table if the number of unique values is less than this value, 0 to hide
#' @examples
#' data(hdv2003)
#' describe(hdv2003$sexe)
#' @export
#' @importFrom utils head

`describe.factor` <- 
  function(x, n = 10, show.length = TRUE, freq.n.max = 10, ...) {
    res <- ""
    if (show.length)
      res <- paste0("[", length(x), " obs.] ")
    res <- paste0(res, labelled::var_label(x), "\n")
    
    if (is.ordered(x))
      res <- paste0(res, "ordinal factor: ")
    else
      res <- paste0(res, "nominal factor: ")
    
    quotes <- rep("\"", times = n)
    quotes[is.na(head(x, n = n))] <- ""
    obs <- paste0(quotes, head(x, n = n), quotes, collapse = " ")
    if (length(x) > n) obs <- paste(obs, "...")
    res <- paste0(res, obs, "\n")
    
    if (is.ordered(x))
      res <- paste0(res, nlevels(x), " levels: ", paste(levels(x), collapse = " < "), "\n")
    else
      res <- paste0(res, nlevels(x), " levels: ", paste(levels(x), collapse = " | "), "\n")
    
    nNA <- sum(is.na(x))
    res <- paste0(res, "NAs: ", nNA, " (", round((nNA / length(x)) * 100, digits = 1), "%)")
    
    if (length(unique(x)) < freq.n.max)
      res <- paste0(
        res, "\n\n", 
        paste0(utils::capture.output(freq(x, total = TRUE, exclude = .excludeNA_if_zero(nNA))), collapse = "\n")
      )
    
    class(res) <- "description"
    return(res)
  }

.excludeNA_if_zero <- function(nNA) {
  if (nNA == 0) return(NA)
  else return(NULL)
}

#' @rdname describe
#' @aliases describe.numeric
#' @examples
#' describe(hdv2003$age)
#' @export
#' @importFrom utils head 

`describe.numeric` <- 
  function(x, n = 10, show.length = TRUE, freq.n.max = 10, ...) {
    res <- ""
    if (show.length)
      res <- paste0("[", length(x), " obs.] ")
    res <- paste0(res, labelled::var_label(x), "\n")
    
    if (inherits(x, "haven_labelled_spss")) 
      cl <- paste("labelled_spss", typeof(x))
    else if (inherits(x, "haven_labelled")) 
      cl <- paste("labelled", typeof(x))
    else 
      cl <- class(x)
    res <- paste0(res, cl, ": ")
    
    obs <- paste0(head(x, n = n), collapse = " ")
    if (length(x) > n) obs <- paste(obs, "...")
    res <- paste0(res, obs, "\n")
    
    res <- paste0(
      res, 
      "min: ", min(x, na.rm = T),
      " - max: ", max(x, na.rm = T), 
      " - "
    )
    nNA <- sum(is.na(x))
    res <- paste0(res, "NAs: ", nNA, " (", round((nNA / length(x)) * 100, digits = 1), "%)")
    res <- paste0(res, " - ", length(unique(x)), " unique values")
    
    if (length(unique(x)) < freq.n.max)
      res <- paste0(
        res, "\n\n", 
        paste0(utils::capture.output(freq(x, total = TRUE, exclude = .excludeNA_if_zero(nNA))), collapse = "\n")
      )
    
    class(res) <- "description"
    return(res)
  }

#' @rdname describe
#' @aliases describe.character
#' @export
#' @importFrom utils head

`describe.character` <- 
  function(x, n = 10, show.length = TRUE, freq.n.max = 10, ...) {
    res <- ""
    if (show.length)
      res <- paste0("[", length(x), " obs.] ")
    res <- paste0(res, labelled::var_label(x), "\n")
    
    if (inherits(x, "haven_labelled_spss")) 
      cl <- paste("labelled_spss", typeof(x))
    else if (inherits(x, "haven_labelled")) 
      cl <- paste("labelled", typeof(x))
    else 
      cl <- class(x)
    res <- paste0(res, cl, ": ")
    
    quotes <- rep("\"", times = n)
    quotes[is.na(head(x, n = n))] <- ""
    obs <- paste0(quotes, head(x, n = n), quotes, collapse = " ")
    if (length(x) > n) obs <- paste(obs, "...")
    res <- paste0(res, obs, "\n")
    
    nNA <- sum(is.na(x))
    res <- paste0(res, "NAs: ", nNA, " (", round((nNA / length(x)) * 100, digits = 1), "%)")
    res <- paste0(res, " - ", length(unique(x)), " unique values")
    
    if (length(unique(x)) < freq.n.max)
      res <- paste0(
        res, "\n\n", 
        paste0(utils::capture.output(freq(x, total = TRUE, exclude = .excludeNA_if_zero(nNA))), collapse = "\n")
      )
    
    class(res) <- "description"
    return(res)
  }


#' @rdname describe
#' @aliases describe.default
#' @export
#' @importFrom utils head

`describe.default` <- 
  function(x, n = 10, show.length = TRUE, freq.n.max = 10, ...) {
    if (!is.atomic(x)) stop("no method specified for this kind of object.")
    res <- ""
    if (show.length)
      res <- paste0("[", length(x), " obs.] ")
    res <- paste0(res, labelled::var_label(x), "\n")
    
    res <- paste0(res, class(x), ": ")
    
    obs <- paste0(format(head(x, n = n), trim = TRUE), collapse = " ")
    if (length(x) > n) obs <- paste(obs, "...")
    res <- paste0(res, obs, "\n")
    
    res <- paste0(res, "min: ", min(x, na.rm = T), " - max: ", max(x, na.rm = T), " - ")
    nNA <- sum(is.na(x))
    res <- paste0(res, "NAs: ", nNA, " (", round((nNA / length(x)) * 100, digits = 1), "%)")
    res <- paste0(res, " - ", length(unique(x)), " unique values")
    
    if (length(unique(x)) < freq.n.max)
      res <- paste0(
        res, "\n\n", 
        paste0(utils::capture.output(freq(x, total = TRUE, exclude = .excludeNA_if_zero(nNA))), collapse = "\n")
      )
    
    class(res) <- "description"
    return(res)
  }


#' @rdname describe
#' @aliases describe.haven_labelled
#'
#' @export
`describe.haven_labelled` <- 
  function(x, n = 10, show.length = TRUE, freq.n.max = 10, ...) {
    if (is.numeric(x)) {
      res <- describe.numeric(x, n = n, show.length = show.length, freq.n.max = 0, ...)
    }
    else if (is.character(x)) {
      res <- describe.character(x, n = n, show.length = show.length,  freq.n.max = 0, ...)
    }
    else {
      res <- describe.default(x, n = n, show.length = show.length,  freq.n.max = 0, ...)
    }
    if (!is.null(labelled::val_labels(x)))
      res <- paste0(
        res, "\n", 
        length(labelled::val_labels(x)), " value labels: ", 
        paste0("[", labelled::val_labels(x), "] ", names(labelled::val_labels(x)), collapse = " ")
      )
    
    if (!is.null(labelled::na_values(x)))
      res <- paste0(res, "\nuser-defined na values: ", paste0(labelled::na_values(x), collapse = ", "))
    if (!is.null(labelled::na_range(x)))
      res <- paste0(res, "\nuser-defined na range: ", paste0(labelled::na_range(x), collapse = "-"))

    if (length(unique(x)) < freq.n.max)
      res <- paste0(
        res, "\n\n", 
        paste0(utils::capture.output(freq(x, total = TRUE, exclude = .excludeNA_if_zero(sum(is.na(x))))), collapse = "\n")
      )
    
    class(res) <- "description"
    return(res)
  }

#' @rdname describe
#' @aliases describe.data.frame
#' @details When describing a data.frame, you can provide variable names as character strings. 
#' Using the "*" or "|" wildcards in a variable name will search for it using a regex match.
#' The search will also take into account variable labels, if any.
#' See examples.
#' @seealso \code{\link{lookfor}}
#' @examples
#' describe(hdv2003)
#' describe(hdv2003, "cuisine", "heures.tv")
#' describe(hdv2003, "trav*")
#' describe(hdv2003, "trav|lecture")
#' describe(hdv2003, "trav", "lecture")
#' 
#' data(fertility)
#' describe(women$residency)
#' describe(women)
#' describe(women, "id")
#' @export

`describe.data.frame` <- 
  function(x, ..., n = 10, freq.n.max = 0) {
    # applying to_labelled if available
    x <- labelled::to_labelled(x)
    
    # select variables
    s <- lookfor(x, ..., details = FALSE)$variable
    
    if (is.null(s)) 
      return(NULL)
    
    # subsetting
    # using [] to keep labels and attributes not preserved by subset
    # specifying `[.data.frame` for compatibility with data.table objects
    y <- `[.data.frame`(x, , s, drop = FALSE) 
    
    res <- paste0("[", nrow(x), " obs. x ", ncol(x), " variables] ", paste(class(x), collapse = " "))
    
    for (v in names(y)) {
      res <- paste0(res, "\n\n$", v, ": ", describe(y[[v]], n = n, show.length = FALSE, freq.n.max = freq.n.max))  
    }
    
    class(res) <- "description"
    return(res)
  }

#' @rdname describe
#' @export

`print.description` <- 
  function(x, ...) {
    cat(x)
    invisible(x)
  }
