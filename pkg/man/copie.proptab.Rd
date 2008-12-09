\encoding{UTF-8}
\name{copie.proptab}
\alias{copie.proptab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Export d'un objet proptab au format HTML }
\description{
  Applique la fonction générique \code{copie} à un tableau de classe \code{proptab}.
}
\usage{
\method{copie}{proptab}(obj, percent=NULL, digits=NULL, justify="right", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ nom de l'objet à exporter }
  \item{percent}{ affichage du symbole pourcentage dans les cellules du
    tableau }
  \item{digits}{ nombre de décimales à afficher }
  \item{justify}{ justification du contenu des cellules
    (\code{"left"}, \code{"right"} ou \code{"centre"}) }
  \item{...}{ arguments passés à la fonction copie() }
}
\details{
  Pour plus d'informations sur les arguments et les résultats de la
  fonction, se référer à l'aide des fonctions \code{format.proptab} et
  \code{copie}.
}
\seealso{  \code{\link[rgrs]{copie}}, \code{\link[rgrs]{format.proptab}} }
\author{ Julien Barnier <julien.barnier@ens-lsh.fr>}
\examples{
data(iris)
tab <- table(cut(iris$Sepal.Length,8),cut(iris$Sepal.Width,4))
ptab <- lprop(tab, percent=TRUE)
copie(ptab)
}
\keyword{connection}