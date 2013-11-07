#' Transform an object into HTML and copy it for export
#'
#' This function transforms its argument to HTML and then copy it to the
#' clipboard or to a file for later use in an external application.
#' 
#' @aliases copie copie.default clipcopy.default
#' @param obj object to be copied
#' @param ... arguments passed to \code{R2HTML::HTML}
#' @details
#' Under linux, this function requires that \code{xclip} is
#' installed on the system to copy to the clipboard.
#' @examples
#' data(iris)
#' tab <- table(cut(iris$Sepal.Length,8),cut(iris$Sepal.Width,4))
#' \dontrun{copie(tab)}
#' ptab <- rprop(tab, percent=TRUE)
#' \dontrun{clipcopy(ptab)}
#' @seealso \code{\link[R2HTML]{HTML}}, \code{\link[questionr]{format.proptab}}
#' @keywords connection 
#' @export clipcopy clipcopy.default copie copie.default 

`clipcopy` <-
function (obj, ...) {
  UseMethod("clipcopy")
}
copie <- clipcopy

#' @return \code{NULL}
#'
#' @rdname clipcopy
#' @method clipcopy default
#' @S3method clipcopy default
#' @aliases copie.proptab
#' @param append if TRUE, append to the file instead of replacing it
#' @param file if TRUE, export to a file instead of the clipboard
#' @param filename name of the file to export to
#' @param clipboard.size under Windows, size of the clipboard in kB

`clipcopy.default` <-
function (obj, append=FALSE, file=FALSE, filename="temp.html", clipboard.size=4096, ...) {
  require(R2HTML)
  if (file) {
    conn <- file(filename, "w", encoding="Latin1")
    HTML(obj, file=conn, append=append)
    close(conn)
    return
  }
  if (Sys.info()["sysname"] == "Windows") {
    connection.name <- paste("clipboard", format(clipboard.size, scientific=1000), sep="-")
    conn <- file(connection.name, "w", encoding="Latin1")
  } 
  if (Sys.info()["sysname"] == "Darwin") conn <- pipe("pbcopy", "w", encoding="Latin1")
  if (Sys.info()["sysname"] == "Linux") conn <- pipe("xclip -i", "w", encoding="Latin1")
  R2HTML::HTML(obj, file = conn, append = append, ...)
  close(conn)
}

copie.default <- clipcopy.default


#' @return \code{NULL}
#'
#' @rdname clipcopy
#' @method clipcopy proptab
#' @S3method clipcopy proptab
#' @aliases copie.proptab
#' @param percent whether to add a percent sign in each cell
#' @param digits number of digits to display
#' @param justify justification
#' @seealso \code{\link[questionr]{clipcopy}}, \code{\link[questionr]{format.proptab}}
#' @export clipcopy.proptab copie.proptab

`clipcopy.proptab` <-
function (obj, percent=NULL, digits=NULL, justify="right", ...) {
  if (!inherits(obj, "proptab")) stop("Object is not of class proptab")
  obj <- format.proptab(obj, digits=digits, percent=percent, justify=justify)
  copie.default(obj, ...)
}
copie.proptab <- clipcopy.proptab

#' Rename a data frame column
#'
#' 
#' @aliases renomme.variable
#' @param df data frame
#' @param old old name
#' @param new new name
#' @keywords manip
#' @return A data frame with the column named "old" renamed as "new"
#' @examples
#' data(iris)
#' str(iris)
#' iris <- rename.variable(iris, "Species", "especes")
#' str(iris)
#' @export renomme.variable rename.variable

`rename.variable` <-
function (df, old, new) {
  names(df)[which(names(df)==old)] <- new
  df
}
renomme.variable <- rename.variable
