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

.tabRG <- function(swap, arrays, dataTest=conf$dataTest) {
  # Lecture des donnees
  # Args:
  #   swap : nom du swap
  # CeQuIlFait:
  #   ouvre les fichiers de donnees definies par arrays
  #   met tous les bruits de fond à 0
  # Returns:
  #   large liste contenant l'ensemble des infos des donnees du swap
  if (!is.null(dataTest)) {print("tabRG")}
  target <- arrays[arrays$Swaps==swap,]   # identification des lames à analyser ensemble
  if (!is.null(dataTest)) {target$FileName <- paste(dirname(dataTest), target$FileName, sep="/")}
  spots  <- list(R="rMedianSignal",G="gMedianSignal",Rb="rBGMedianSignal",Gb="gBGMedianSignal")
  sondes <- c("ProbeName", "Row", "Col")
  cat("\nLecture des donnees brutes...\n")
  RG     <- read.maimages(target,columns=spots,annotation=sondes,verbose=F,encoding="utf-8")   # Lecture des lames
  RG$Rb[,]  <- 0 ; RG$Gb[,] <- 0
  RG$R[RG$R==0] <- 0.5
  RG$G[RG$G==0] <- 0.5
  return(RG)
}
