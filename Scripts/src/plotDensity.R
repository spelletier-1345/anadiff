###############################################
# AnaDiff Agilent
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

.plotDens <- function(l2Tab, nbCol, state, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("plotDens")}
  dmat<- matrix(nrow = 2*nbCol, ncol=2)
  mode(dmat)="list"
  dmax <- 1
  for (z in seq(1, 2*nbCol)) {
    d <- density(l2Tab[,z+3])
    dmat[[z]] <- list(x=d$x, y=d$y)
    dmax <- ifelse(dmax<max(d$y), max(d$y), dmax)
  }
  if (state == "RG") {
    mainState="Densité RG"
  } else {
    mainState="Densité MA"
  }
  plot(0, type = "n", ylim = c(0, dmax), xlim = c(4, 16), bty="n", xaxt="n", yaxt="n",
       main = mainState, cex.main=0.9)
  for (indNbCol in seq(1, nbCol)) {
    lines(dmat[[indNbCol]], col="red", lwd=1.5)
    lines(dmat[[indNbCol+nbCol]], col="green", lwd=1.5)
  }
  legend("topright", box.lty=1,  box.col="white", legend=c("Cy3", "Cy5"), lty = 1,
         col=c("green", "red"), lwd = 2, cex=0.7)
  par(mgp=c(0,0.1,0))
  axis(side=1, at=seq(4,16,2),lwd=1,lwd.ticks=1,pos=0, cex.axis=0.7, xlab="", ylab="")
  mtext(side=1,text="Intensités moyennes par sonde (log2)",line=0.5, cex=0.65)
  par(mgp=c(0,0.5,0))
  a <- ifelse(dmax<=1,0.5,round(dmax/2))
  axis(side=2, at=seq(0,dmax+1,a),lwd=1,lwd.ticks=1,pos=3.95, cex.axis=0.6, las=1)
  mtext(side=2,text="Densité des sondes",line=0.5, cex=0.65)
}
