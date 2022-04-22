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

.tabSpikes <- function(spikesTab, indNbCol, nbCol, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("tabSpikes")}
  spikesTab <- spikesTab[order(spikesTab$expect, decreasing = T),]
  par(mar=c(0.5, 0, 0.5, 0))
  plot(0, type = "n", ylim = c(0.1, 4), xlim = c(0.1, 4), axes = F, ann = F)
  ## Background
  rect(0, 0, 4, 4, col = "snow2", border = par()$col.axis)
  ## Texte de la legende
  text(2, 3.6, labels = "*****                  *****", font = 2, cex = 1.2)
  text(2, 3.7, labels = "spikes", font = 2, cex = 1.1)
  lines(x = c(0.2, 3.8), y = c(3.4, 3.4))
  text(0.6, 3.14, labels = "ID", pos = 4, cex = 0.7, font = 2)
  text(1.7, 3.14, labels = "Expect", pos = 4, cex = 0.7, font = 2)
  text(2.6, 3.14, labels = "RG", pos = 4, cex = 0.7, font = 2, col = "purple")
  text(3.4, 3.14, labels = "MA", pos = 4, cex = 0.7, font = 2, col = "darkorange")
  lines(x = c(1.6, 1.6), y = c(0.2, 3.2), col = "slategrey", lty = 2)
  lines(x = c(2.4, 2.4), y = c(0.2, 3.2), col = "slategrey", lty = 2)
  lines(x = c(3.2, 3.2), y = c(0.2, 3.2), col = "slategrey", lty = 2)
  ## Ajout des données
  for (sp in seq(1,nrow(spikesTab))) {
    text(0.8, (sp/3.5), labels = spikesTab$spikes[sp], cex = 0.7)
    text(2, (sp/3.5), labels = round(spikesTab$expect[sp],2), cex = 0.7)
    text(2.8, (sp/3.5), labels = round(spikesTab[sp, indNbCol*2+1],2), cex = 0.7)
    text(3.6, (sp/3.5), labels = round(spikesTab[sp, indNbCol*2+2],2), cex = 0.7)
  }
  return(NULL)
}
