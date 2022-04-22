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

.exportQCArray <- function(chip, smoothScatterRG, smoothScatterMA, histRG, histMA,
                           spikesPlot, spikesTab, greenLoc, redLoc, diffLoc, labelling, dirName, dataTest=conf$dataTest) {
  graphics.off()
  if (!is.null(dataTest)) {print("exportQCArray")}
  if(!is.null(dataTest)) {chip <- gsub(dataTest, "", chip)}
  pngName <- paste(dirName, "qualityControl_geoSubmission/", gsub(".txt", ".png", chip), sep="")
  png(pngName, width=768, height=1086, res=120)
  par(oma=c(0,0,3,0), mar=c(1.5,2.5,1,1), tcl=0.2, bg="white", cex=0.8)
  if (labelling=="direct") {
    mat <- matrix(c(0.00, 0.66, 0.70, 0.95,
                    0.00, 0.66, 0.45, 0.70,
                    0.66, 1.00, 0.70, 0.94,
                    0.66, 1.00, 0.45, 0.69,
                    0.00, 0.50, 0.20, 0.42,
                    0.50, 1.00, 0.20, 0.42,
                    0.00, 0.33, 0.02, 0.17,
                    0.33, 0.66, 0.02, 0.17,
                    0.66, 1.00, 0.02, 0.17),nrow=9,byrow=TRUE)
    split.screen(figs=mat)
    screen(1)
    smoothScatterRG
    screen(2)
    smoothScatterMA
    screen(3)
    histRG
    screen(4)
    histMA
    screen(5)
    spikesPlot
    screen(6)
    spikesTab
    screen(7)
    greenLoc
    screen(8)
    redLoc
    screen(9)
    diffLoc
  } else {
    mat <- matrix(c(0.00, 0.66, 0.70, 0.95,
                    0.00, 0.66, 0.45, 0.70,
                    0.66, 1.00, 0.70, 0.94,
                    0.66, 1.00, 0.45, 0.69,
                    0.00, 0.33, 0.20, 0.35,
                    0.33, 0.66, 0.20, 0.35,
                    0.66, 1.00, 0.20, 0.35),nrow=7,byrow=TRUE)
    split.screen(figs=mat)
    screen(1)
    smoothScatterRG
    screen(2)
    smoothScatterMA
    screen(3)
    histRG
    screen(4)
    histMA
    screen(5)
    greenLoc
    screen(6)
    redLoc
    screen(7)
    diffLoc
  }
  close.screen(all = TRUE)
  mtext(chip, side = 3, line = 1, outer = TRUE, cex = 0.8, font = 4, col="darkblue")
  dev.off()
}
