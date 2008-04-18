`prop` <-
function (tab, digits=1, add.tot=TRUE) {
  tmp <- tab/sum(tab)*100
  if (add.tot) {
    tmp <- rbind(tmp,Total=apply(tmp,2,sum))
    tmp <- cbind(tmp,Total=apply(tmp,1,sum))
  }
  round(tmp,digits)
}

