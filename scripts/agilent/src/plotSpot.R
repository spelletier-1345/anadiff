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

.plotSpot <- function(RGlogTab, indNbCol, nbCol, state, maxRowCol, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("plotSpot")}
  indNbCol <- indNbCol+3
  if (state == "Cy3") {
    val <- round(RGlogTab[,indNbCol+nbCol]-min(RGlogTab[,indNbCol+nbCol])+1)
    paletteState <- colorRampPalette(c("#FFFFFF", "#069C00", "#058700", "#035700"))(13)
    x <- RGlogTab$RG.genes.Row
    y <- RGlogTab$RG.genes.Col
    mainState = "Répartition des cibles Cy3"
  } else if (state == "Cy5") { # vert
    val <- round(RGlogTab[,indNbCol]-min(RGlogTab[,indNbCol])+1)
    paletteState <- colorRampPalette(c("#FFFFFF", "#FF0000", "#B80000", "#960000"))(13)
    x <- RGlogTab$RG.genes.Row
    y <- RGlogTab$RG.genes.Col
    mainState = "Répartition des cibles Cy5"
  } else {
    val <- abs(round(RGlogTab[,indNbCol]))
    val[which(val>=1)] <- 2
    val[which(val<1)] <- 1
    paletteState <- c("white", "blue2")
    x <- RGlogTab$MA.genes.Row
    y <- RGlogTab$MA.genes.Col
    mainState = "Différences de marquage"
  }
  plot(x, y, col=paletteState[val], pch="18", cex=0.4, xaxt="n", yaxt="n",
       xlab = "", ylab = "", xlim=c(0,maxRowCol[1]), ylim=c(0,maxRowCol[2]), main=mainState, cex.main=0.9)
}
