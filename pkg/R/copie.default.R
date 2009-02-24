`copie.default` <-
function (obj, append=FALSE, file=FALSE, filename="temp.html", ...) {
  require(R2HTML)
  if (file) {
    HTML(obj, file=filename, append=append)
    return
  }
  if (Sys.info()["sysname"] == "Windows") conn <- file("clipboard", "w")
  if (Sys.info()["sysname"] == "Darwin") conn <- pipe("pbcopy", "w")
  if (Sys.info()["sysname"] == "Linux") conn <- pipe("xclip -i", "w")
  R2HTML::HTML(obj, file = conn, append = append, ...)
  close(conn)
}

