\encoding{UTF-8}
\name{format.proptab}
\alias{format.proptab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Formate le contenu d'un tableau contenant des proportions }
\description{
  Cette fonction formate un tableau contenant des pourcentages en
  contrôlant leur présentation. Cette fonction est prévue pour une
  utilisation interne, et ne devrait pas être utilisée directement.
}
\usage{
   \method{format}{proptab}(x, digits=NULL, percent=NULL, justify="right", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ tableau à formater }
  \item{digits}{ indique le nombre de décimales à conserver pour
    l'affichage. Si \code{NULL}, on utilise l'attribut \code{digits} de \code{x} }
  \item{percent}{ indique si on doit afficher (\code{TRUE}) ou non
    (\code{FALSE}) le symbole \% dans chaque case du tableau. Si
    \code{NULL}, on utilise l'attribut \code{percent} de \code{x} }
  \item{justify}{ justification du contenu des cellules
    (\code{"left"}, \code{"right"} ou \code{"centre"}) }
  \item{...}{ arguments passés à la fonction format() }
}
\seealso{  \code{\link[rgrs]{copie.proptab}}, \code{\link[rgrs]{print.proptab}} }
\author{ Julien Barnier <julien.barnier@ens-lsh.fr>}
\keyword{print}
