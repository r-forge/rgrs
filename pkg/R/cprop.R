`cprop` <-
function (tab, digits = 1, add.tot = TRUE) {
  if (add.tot) tmp <- cbind(tab, Ensemble=apply(tab,1,sum))
  tmp <- prop.table(tmp,2)*100
  if (add.tot) tmp <- rbind(tmp,Total=apply(tmp,2,sum))
  round(tmp,digits)
}

