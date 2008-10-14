`prop` <-
function (tab, digits=1, total=TRUE) {
  dn <- names(dimnames(tab))
  tmp <- tab/sum(tab)*100
  if (total) {
    tmp <- rbind(tmp,Total=apply(tmp,2,sum))
    tmp <- cbind(tmp,Total=apply(tmp,1,sum))
  }
  result <- as.table(round(tmp,digits))
  names(dimnames(result)) <- dn
  return(result)
  
}

