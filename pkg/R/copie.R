`copie` <-
function (obj, append=FALSE, ...) {
  if (Sys.info()["sysname"] == "Windows") conn <- file("clipboard", "w")
  if (Sys.info()["sysname"] == "Darwin") conn <- pipe("pbcopy", "w")
  if (Sys.info()["sysname"] == "Linux") conn <- pipe("xclip -i", "w")
  R2HTML::HTML(obj, file = conn, append = append, ...)
  close(conn)
}

