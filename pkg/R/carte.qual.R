`carte.qual` <-
function (sp, data, varname, sp.key="id", data.key="id", main="", sub=NULL, posleg="topleft", palette.qual="Set3", palette=NULL, ...) {
  require(sp)
  require(RColorBrewer)
  tmp <- data[,c(data.key, varname)]
  sp@data <- merge(sp@data, tmp, by.x=sp.key, by.y=data.key, all.x=TRUE, all.y=FALSE, sort=FALSE)
  tmp.var <- na.omit(sp@data[,varname])
  qual.names <- sort(unique(data[,varname]))
  qual.nb <- length(qual.names)
  value <- match(sp@data[,varname],qual.names)
  if (is.null(palette)) palette <- brewer.pal(qual.nb,palette.qual)
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
  if (posleg != "none" && !is.null(posleg))
    carte.qual.legende(posleg=posleg, qual.names, palette, na.leg=na.leg)
}
