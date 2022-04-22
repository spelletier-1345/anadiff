#!/usr/bin/env Rscript

# Copyright Sandra PELLETIER ([2022-04-20])
# 
# [sandra.pelletier@inrae.fr]
# 
# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software.  You can  use, 
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info". 
# 
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.

###############################################
# AnaDiff Agilent
# Le 23 janvier 2019 - Sandra PELLETIER
###############################################

.plotRGMA <- function(RGMAtab, indNbCol, nbCol, state, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("plotRGMA")}
  indNbCol <- indNbCol+3
  if (state == "RG") {
    mainState="Graphe RG avant normalisation par loess"
    colState = "purple"
  } else {
    mainState="Graphe MA après normalisation par loess"
    colState = "darkorange"
  }
  smoothScatter(RGMAtab[,indNbCol+nbCol], RGMAtab[,indNbCol], main=mainState, cex.main=0.9,
                xlab="",ylab="", xlim = c(4.5,16), ylim = c(-6, 6), cex = 1.5,
                colramp=colorRampPalette(c(rep("white", 2), rep("lightblue", 1),
                                           rep("blue", 1), rep("darkblue", 10))),
                bty="n", xaxt="n", yaxt="n")
  abline(h = 0, col = "gray40", lty = 2)
  lines(lowess(RGMAtab[,indNbCol+nbCol], RGMAtab[,indNbCol],f=0.3), col=colState, lwd=1.5)
  par(mgp=c(0,-1.2,0))
  axis(side=1, at=seq(4,16,2),lwd=0,lwd.ticks=1,pos=-6.5, cex.axis=0.6)
  mtext(side=1,text="Intensités moyennes par sonde (log2)",line=0, cex=0.7)
  par(mgp=c(0,0.5,0))
  axis(side=2, at=seq(-6,6,6),lwd=0,lwd.ticks=1,pos=3.95, cex.axis=0.6, las=1)
  mtext(side=2,text="Ratio par sonde (log2)",line=1.2, cex=0.7)
}
