\encoding{UTF-8}
\name{freq}
\alias{freq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Retourne le tri à plat d'une variable }
\description{
  Cette fonction affiche le tri à plat d'une variable (vecteur).
}
\usage{ 
freq(x, digits=1, eff=TRUE, cum=FALSE, total=FALSE, exclude=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ vecteur pour lequel on souhaite obtenir le tri à plat ou
    tableau de dimension 1}
  \item{digits}{ nombre de chiffres à conserver après la virgule }
  \item{eff}{ si TRUE, affiche les effectifs }
  \item{cum}{ si TRUE, affiche les pourcentages cumulés }
  \item{total}{ si TRUE, affiche le total des effectifs }
  \item{exclude}{ valeurs à exclure du tri à plat (aucune par défaut) }
}
\details{
  L'objet x est soit un vecteur, dans ce cas le tri à plat est calculé à
  l'aide de la fonction table, soit déjà un tri à plat, c'est-à-dire une
  table à une dimension, dans ce cas c'est cette table qui est utilisée
  telle quelle.
}
\value{
  Un data frame dont les noms de lignes sont les modalités de la
  variables, et dont les colonnes sont les effectifs, le pourcentage et
  (si demandé) le pourcentage cumulé de ces modalités.
}
\seealso{  \code{\link{table}}, \code{\link{prop}} }
\author{ Julien Barnier <julien.barnier@ens-lsh.fr>}
\examples{
v <- c(round(runif(230,1,5)), NA)
freq(v)
freq(v, cum=TRUE)
freq(v, exclude=NA)
freq(v, exclude=c(1,2,NA))
freq(v, digits=3)
freq(v, eff=FALSE)
freq(v, total=TRUE)
tab <- table(v)
freq(tab)
}
\keyword{univar}