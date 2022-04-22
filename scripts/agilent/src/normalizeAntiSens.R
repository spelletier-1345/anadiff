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

# Normalisation de l'ensemble puis séparation des antisens
.normalizeAntiSense <- function (RGtmp, probeList, fileOut, compare, probe, dataTest = conf$dataTest) {
  if (!is.null(dataTest)) {print("normalizeAntiSense")}
  MA <- normalizeWithinArrays(RGtmp,method = "loess",bc.method = "none")
  res <- .statAnaDiff(MA, fileOut, compare)
  tabResult <- res$tabFit
  tabResult <- .selectProbes(tabResult, probe = probe)
  tabResult <- tabResult[tabResult$Agilent_id %in% probeList$V1,]
  return(list(tabResult = tabResult, MA = MA, var = res$variance))
}
