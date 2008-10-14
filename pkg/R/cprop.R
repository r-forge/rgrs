`cprop` <-
function (tab, digits = 1, total = TRUE) {
  dn <- names(dimnames(tab))
  if (total) tab <- cbind(tab, Ensemble=apply(tab,1,sum))
  tab <- prop.table(tab,2)*100
  if (total) tab <- rbind(tab,Total=apply(tab,2,sum))
  result <- as.table(round(tab,digits))
  names(dimnames(result)) <- dn
  return(result)
}

