`carte.prop` <-
function (sp, data, varname, sp.key="id", data.key="id", diverg=FALSE, nbcuts=6, at=NULL, at.lim=FALSE, main="", sub=NULL, posleg="topleft", palette.pos="Reds", palette.neg="Blues", palette=NULL, ...) {
  tmp <- data[,c(data.key, varname)]
  require(sp)
  require(RColorBrewer)
  sp@data <- merge(sp@data, tmp, by.x=sp.key, by.y=data.key, all.x=TRUE, all.y=FALSE)
  tmp.var <- na.omit(sp@data[,varname])
  if (is.null(at)) at <- pretty(tmp.var,n=nbcuts)
  if (!is.null(at) && at.lim) {
    vmax <- max(tmp[,varname], na.rm=TRUE)
    if (max(at) < vmax) at <- c(at,round(vmax,2))
    vmin <- min(tmp[,varname], na.rm=TRUE)
    if (vmin < min(at)) at <- c(round(vmin,2),at)
  }
  value <- findInterval(sp@data[,varname],at,all.inside=TRUE)
  if (is.null(palette)) {
    palette <- brewer.pal(length(at)-1,palette.pos)
    if (diverg) {
      nb.pos <- sum(at>0)
      if (nb.pos<3) palpos <-  brewer.pal(3,palette.pos)[1:nb.pos]
      else palpos <- brewer.pal(nb.pos,palette.pos)
      nb.neg <- sum(at<0)  
      if (nb.neg<3) palneg <- brewer.pal(3,palette.neg)[1:nb.neg]
      else palneg <- brewer.pal(nb.neg,palette.neg)
      palette <- c(rev(palneg),palpos)
    }
  }
  cols <- palette[value]
  plot(sp, col=cols, ...)
  na.values <- is.na(sp@data[,varname]) | is.nan(sp@data[,varname])
  na.leg <- FALSE
  if (sum(na.values)>0) {
    plot(sp[na.values,], density=30, angle=45, add=TRUE)
    na.leg <- TRUE
  }    
  title(main,sub,line=1)
  box()
  carte.prop.legende(posleg=posleg, at, palette, na.leg=na.leg)
}
