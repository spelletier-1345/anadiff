###############################################
# AnaDiff Agilent
# Le 23 janvier 2019 - Sandra PELLETIER
###############################################

.plotSpikes <- function(spikesTab, indNbCol, nbCol, minSpikes, maxSpikes, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("plotSpikes")}
  # Graph Spikes
  plot(spikesTab$expect, spikesTab[,indNbCol*2+1], xlim=c(-4,round(maxSpikes+0.5)),
       ylim=c(round(minSpikes-0.5),round(maxSpikes+0.5)), pch = "*", col = "purple",
       main = "Correction des spikes par loess", cex.main=0.9, bty="n", xaxt="n", yaxt="n")
  par(mgp=c(0,0.1,0))
  axis(side=1, at=seq(-4,round(maxSpikes+0.5),2),lwd=1,lwd.ticks=1,pos=round(minSpikes-0.5),
       cex.axis=0.7, xlab="", ylab="")
  mtext(side=1,text="Valeurs attendues",line=0.5, cex=0.65)
  par(mgp=c(0,0.5,0))
  axis(side=2, at=seq(round(minSpikes-0.5),round(maxSpikes+0.5),2),lwd=1,lwd.ticks=1,pos=-4,
       cex.axis=0.6, las=1)
  mtext(side=2,text="Valeurs observées",line=0.5, cex=0.65)
  lines(lowess(spikesTab$expect, spikesTab[,indNbCol*2+1],f=0.3),col = "purple")
  points(spikesTab$expect, spikesTab[,indNbCol*2+2], pch = "*", col = "darkorange")
  lines(lowess(spikesTab$expect, spikesTab[,indNbCol*2+2],f=0.3),col = "darkorange")
  lines(-4:4,-4:4, col = "gray40", lty = 2)
  text(-2.7, 3, labels = "RG", cex = 0.8)
  text(-2.7, 2.3, labels = "MA", cex = 0.8)
  lines(x = c(-3.7,-3.2), y = c(3,3), col = "purple")
  lines(x = c(-3.7,-3.2), y = c(2.3,2.3), col = "darkorange")
  points(-3.4,3.02, pch = "*", col = "purple", cex = 1.25)
  points(-3.4,2.32, pch = "*", col = "darkorange", cex = 1.25)
  return(NULL)
}
