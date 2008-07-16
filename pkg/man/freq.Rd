\encoding{UTF-8}
\name{freq}
\alias{freq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Retourne le tri à plat d'une variable }
\description{
  Cette fonction retourne et affiche le tri à plat d'une variable (vecteur).
}
\usage{
freq(var, digits=1, cum=FALSE, exclude=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{ vecteur pour lequel on souhaite obtenir le tri à plat }
  \item{digits}{ nombre de chiffres à conserver après la virgule }
  \item{cum}{ si TRUE, affiche les pourcentages cumulés }
  \item{exclude}{ valeurs à exclure du tri à plat (aucune par défaut) }
}
\details{
  Le tableau croisé passé en argument est un objet de type table.
}
\value{
  Un data frame dont les noms de lignes sont les modalités de la
  variables, et dont les colonnes sont les effectifs, le pourcentage et
  (si demandé) le pourcentage cumulé de ces modalités.
}
\references{ Pas pour l'instant... }
\seealso{  \code{\link{table}}, \code{\link{prop}} }
\author{ Julien Barnier <julien.barnier@ens-lsh.fr>}
\examples{
v <- c(round(runif(230,1,5)), NA)
freq(v)
freq(v, cum=TRUE)
freq(v, exclude=NA)
freq(v, exclude=c(1,2,NA))
freq(v, digits=3)
}
\keyword{univar}