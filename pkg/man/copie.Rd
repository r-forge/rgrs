\encoding{UTF-8}
\name{copie}
\alias{copie}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Export d'un objet vers un tableur }
\description{
  Cette fonction transforme l'objet passé en argument en HTML via
  R2HTML, puis le place dans le presse-papier.
}
\usage{
copie(obj, append=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ nom de l'objet à exporter }
  \item{append}{ si FALSE (par défaut), remplace le contenu du
    presse-papier par le résultat. Si TRUE, ajoute le résultat à la suite
    du contenu du presse-papier }
  \item{...}{ arguments passés à la fonction HTML() }
}
\details{
  ATTENTION, pour l'instant cette fonction ne fonctionne que sous
  Windows. Sous Linux elle nécessite la présence du programme
  xclip. Elle n'a pas pu être testée sous Mac OS X. 
}
\value{
  Après exécution, le presse-papier contient une copie de l'objet
  formaté en HTML. On peut alors facilement coller le résultat
  directement sous Microsoft Excel, ou sous OpenOffice Calc via un
  collage spécial HTML.
}
\references{ Pas pour l'instant... }
\seealso{  \code{\link[R2HTML]{HTML}} }
\author{ Julien Barnier <julien.barnier@ens-lsh.fr>}
\examples{
data(iris)
tab <- table(cut(iris$Sepal.Length,8),cut(iris$Sepal.Width,4))
copie(tab)
}
\keyword{connection}