`freq` <-
function (var, digits=1, cum=FALSE, exclude=NULL) {
  tab <- table(var, exclude=exclude)
  eff <- as.vector(tab)
  pourc <- as.vector(eff/sum(eff)*100)
  result <- data.frame(n=eff, pourc=pourc)
  if (cum) {
    pourc.cum <- cumsum(pourc)
    result <- cbind(result, pourc.cum)
  }
  rownames(result) <- ifelse(is.na(names(tab)),"NA",names(tab))
  round(result, digits=digits)
}

