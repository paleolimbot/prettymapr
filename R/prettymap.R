#plot maps without space for axes


prettymap <- function(plotexpression, oma=c(0, 0, 0, 0),
                      mai=c(0.1, 0.1, 0.1, 0.1), drawbox=TRUE, box.lwd=1,
                      drawscale=TRUE, scale.pos="bottomleft", scale.htin=0.1,
                      scale.widthhint=0.25, scale.unitcategory="metric", scale.style="bar",
                      scale.bar.cols=c("black", "white"), scale.lwd=1, scale.linecol="black",
                      scale.padin=c(0.15, 0.15), scale.labelpadin=0.08, scale.label.cex=0.8,
                      scale.label.col="black", scale.plotunit=NULL) {
  prevpars <- par(oma=oma, mai=mai)
  force(plotexpression)
  if(drawbox) box(lwd=box.lwd)
  if(drawscale) scalebar(plotunit=scale.plotunit, pos=scale.pos, htin=scale.htin,
                         widthhint=scale.widthhint, unitcategory=scale.unitcategory, style=scale.style,
                         bar.cols=scale.bar.cols, lwd=scale.lwd, linecol=scale.linecol,
                         padin=scale.padin, labelpadin=scale.labelpadin, label.cex=scale.label.cex,
                         label.col=scale.label.col)
  par(prevpars)
}
