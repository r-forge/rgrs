`thprop` <-
function (tab, digits=1) {
  dn <- names(dimnames(tab))
  tmp <- as.vector(apply(tab,1,sum)/sum(tab))%*%t(as.vector(apply(tab,2,sum)/sum(tab)))
  colnames(tmp) <- colnames(tab)
  rownames(tmp) <- rownames(tab)
  result <- as.table(round(tmp*100,digits))
  names(dimnames(result)) <- dn
  return(result)
}

