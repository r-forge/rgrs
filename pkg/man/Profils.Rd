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
       cprop(tab, digits = 1, total = TRUE, percent = FALSE)
       lprop(tab, digits = 1, total = TRUE, percent = FALSE)
       prop(tab, digits = 1, total = TRUE, percent = FALSE)
       thprop(tab, digits = 1, percent = FALSE)
       theff(tab, digits = 2)
       residus(tab, digits = 2)
}
\arguments{
  \item{tab}{ Tableau croisé (objet de type table) }
  \item{digits}{ Nombre de chiffres après la virgule à conserver à l'affichage }
  \item{total}{ Ajouter des lignes/colonnes pour les marges du tableau }
  \item{percent}{ Ajout du symbole \% dans chaque case lors de l'affichage du tableau }
}
\details{
  Dans le cas des tableaux contenant des proportions (\code{cprop},
  \code{lprop}, \code{prop}, \code{thprop}), les options \code{digits}
  et \code{percent} sont des attributs du tableau résultant qui
  contrôlent l'affichage du tableau avec \code{print} ou \code{copie}. On peut modifier
  ponctuellement ces options en les passant directement à
  \code{\link[rgrs]{print.proptab}} \code{\link[rgrs]{copie.proptab}}. Les données numériques stockées
  conservent l'intégralité des valeurs décimales.
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
  \code{\link{sweep}}, \code{\link{chisq.test}},
  \code{\link[rgrs]{print.proptab}}, \code{\link[rgrs]{copie.proptab}}}
\examples{
v1 <- factor(round(runif(500,1,4)))
v2 <- factor(round(runif(500,1,3)))

tab <- table(v1,v2)
tab
lprop(tab,digits=5)
cprop(tab,digits=2)
prop(tab)
thprop(tab, percent=TRUE)
theff(tab)
residus(tab)
}
\keyword{univar}