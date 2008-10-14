`lprop` <-
function(tab,digits=1, total=TRUE) {
  dn <- names(dimnames(tab))
  if (total) tab <- rbind(tab, Ensemble=apply(tab,2,sum))
  tab <- prop.table(tab,1)*100
  if (total) tab <- cbind(tab, Total=apply(tab,1,sum))
  result <- as.table(round(tab,digits))
  names(dimnames(result)) <- dn
  return(result)
}

