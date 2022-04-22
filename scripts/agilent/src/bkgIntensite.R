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
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.bkgIntensite <- function(norm_intensities, nbg, expName, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("bkgIntensite")}
  # soustraie le bruit de fond pour chaque donnee d'intensite normalisee et les exporte
  # Args:
  #   norm_intensite : le tableau d'intensites normalisee
  #   nbg : nombre de sonde pour le calcul
  #   expName : le nom d'export du swap et du sens
  # Returns:
  #   rien
  cat("soustraction du bruit de fond...\n")
  ssbkg <- function(intensite, nbg, dataTest=conf$dataTest) {
    if (!is.null(dataTest)) {print("ssbkg")}
    bkg_intensities <- intensite - .calcBkg(intensite, nbg)$background
    bkg_intensities[which(bkg_intensities<0)] <- 0
    return(bkg_intensities)
  }
  bkg_intensities      <- norm_intensities
  bkg_intensities[,-1] <- mapply(ssbkg, norm_intensities[,-1], nbg)
  bkg_intensities <- bkg_intensities[order(bkg_intensities$probe_id), ]
  write.table(bkg_intensities, expName, quote=F, sep="\t", row.names=F, dec=".")
  return(NULL)
}
