`lprop` <-
function(tab,digits=1, add.tot=TRUE) {
  if (add.tot) tmp <- rbind(tab, Total=apply(tab,2,sum))
  tmp <- prop.table(tmp,1)*100
  if (add.tot) tmp <- cbind(tmp,Ensemble=apply(tmp,1,sum))
  round(tmp,digits)
}

