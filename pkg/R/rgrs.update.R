`rgrs.update` <-
function() {
  installed.rgrs.version <- installed.packages()["rgrs","Version"]
  cat("Version de rgrs installee sur le systeme :", installed.rgrs.version, "\n")
  remote.rgrs.version <- available.packages(contrib.url("http://r-forge.r-project.org/"))["rgrs","Version"]
  cat("Version de rgrs disponible sur r-forge :", remote.rgrs.version, "\n")
  if (installed.rgrs.version == remote.rgrs.version) cat("Pas de nouvelle version a installer\n")
  else {
    install.packages("rgrs",repos="http://r-forge.r-project.org")
    cat("Mise a jour effectuee\n")
    detach(package:rgrs)
    library(rgrs)
  }
}
