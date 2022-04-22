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
# Le 30 janvier 2019 - Sandra PELLETIER
###############################################

.mergeAnnot <- function(tabResult, annot, sens, dataTest=conf$dataTest) {
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  # Returns:
  if (!is.null(dataTest)) {print("mergeAnnot")}
  tabAnnot     <- read.csv(file = annot, sep="\t", header=T, encoding="utf-8", check.names = F, as.is = T)
  if (sens=="antisens") {colnames(tabAnnot)[1:2] <- c("probe_rev", "probe_id")}
  tabFinal     <- merge(tabResult,tabAnnot,by="probe_id",all.y=T)
  colnames(tabAnnot)[1:2] <- c("probe.x", "probe.y")
  assign("annot", tabAnnot[,1:2], envir=globalenv())
  return(tabFinal)
}
