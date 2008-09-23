`freq` <-
function (var, digits=1, eff=TRUE, cum=FALSE, total=FALSE, exclude=NULL) {
  if (is.factor(var)) var <- factor(var, exclude=exclude)
  tab <- table(var, exclude=exclude)
  effectifs <- as.vector(tab)
  pourc <- as.vector(effectifs/sum(effectifs)*100)
  if (eff) result <- data.frame(n=effectifs, pourc=pourc)
  else result <- data.frame(pourc=pourc)
  rownames(result) <- ifelse(is.na(names(tab)),"NA",names(tab))
  if (total) result <- rbind(result, Total=apply(result,2,sum))
  if (cum) {
    pourc.cum <- cumsum(pourc)
    if (total) pourc.cum <- c(pourc.cum, 100)
    result <- cbind(result, pourc.cum)
  }
  round(result, digits=digits)
}

