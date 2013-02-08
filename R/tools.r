`copie` <-
function (obj, ...) {
  UseMethod("copie")
}

`copie.default` <-
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

`copie.proptab` <-
function (obj, percent=NULL, digits=NULL, justify="right", ...) {
  if (!inherits(obj, "proptab")) stop("Le tableau n'est pas de classe proptab")
  obj <- format.proptab(obj, digits=digits, percent=percent, justify=justify)
  copie.default(obj, ...)
}

`renomme.variable` <-
function (df, old, new) {
  names(df)[which(names(df)==old)] <- new
  df
}
