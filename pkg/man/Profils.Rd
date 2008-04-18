\encoding{UTF-8}
\name{Profils}
\alias{lprop}
\alias{cprop}
\alias{prop}
\alias{thprop}
\alias{theff}
\alias{residus}
\title{ Fonctions de calcul de pourcentages à partir d'un tableau croisé }
\description{
  Fonctions calculant différents pourcentages d'un tableau croisé
}
\usage{
       cprop(tab, digits = 1, add.tot = TRUE)
       lprop(tab, digits = 1, add.tot = TRUE)
       prop(tab, digits = 1, add.tot = TRUE)
       thprop(tab, digits = 1)
       theff(tab, digits = 2)
       residus(tab, digits = 2)
}
\arguments{
  \item{tab}{ Tableau croisé (objet de type table) }
  \item{digits}{ Nombre de chiffres après la virgule à conserver }
  \item{add.tot}{ Ajouter des lignes/colonnes pour les marges du tableau }
}
\details{
  Ces fonctions calculent les profils lignes ou colonnes d'un tableau croisé
}
\value{
  \code{cprop} retourne un tableau contenant les pourcentages colonnes,
  \code{lprop} renvoit un tableau contenant les poiurcentages lignes,
  \code{prop} renvoit un tableau contenant les pourcentages globaux,
  \code{thprop} renvoit un tableau de pourcentages théoriques sous
  l'hypothèse d'indépendance, \code{theff} renvoit un tableau
  d'effectifs théoriques sous l'hypothèse d'indépendance, et
  \code{residus} renvoit le tableau des résidus de Pearson.
}
\author{ Julien Barnier <julien.barnier@ens-lsh.fr> }
\seealso{ \code{\link{table}}, \code{\link{prop.table}},
  \code{\link{sweep}}, \code{\link{chisq.test}} }
\examples{
v1 <- factor(round(runif(500,1,4)))
v2 <- factor(round(runif(500,1,3)))

tab <- table(v1,v2)
tab
lprop(tab,digits=5)
cprop(tab,digits=2)
prop(tab)
thprop(tab)
theff(tab)
residus(tab)
}
\keyword{univar}