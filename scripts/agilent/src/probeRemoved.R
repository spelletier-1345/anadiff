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

.probeRemoved <- function(tab, norm_intensities, expName, fileOut, dataTest=conf$dataTest) {
  # identifie les sondes ecartees de l'analyse, ajoute les valeur d'intensite et exporte
  # Args:
  #   tab : le tableau de donnee analysees
  #   norm_intensities : le tableau d'intensites normalisees
  #   swap : le nom du swap
  #   dirName : dossier d'export pour .writeLineOut
  # CeQuIlFait:
  #   verifie quelles sondes correspondent a un ratio sup a 1 dont la pval est sup a 0.01
  #   colle derriere les intensites de chaque echantillon pour chaque sonde selectionnee
  # Returns:
  #   rien
  if (!is.null(dataTest)) {print("probeRemoved")}
  cat("verification des sondes retirees de l'analyse...\n")
  probes_removed <- tab[which(abs(tab$ratio) > 1 & tab$pvalue > 0.01),]
  probes_removed <- merge(probes_removed,norm_intensities[,],by="probe_id")
  .writeLineOut(paste("\nNombre de sondes retirees de l'analyse :", nrow(probes_removed)), fileOut)
  .writeLineOut("   (sondes ayant un ratio absolu superieur a 1 et une pval superieure a 1%)", fileOut)
  probes_removed <- probes_removed[order(probes_removed$probe_id),]
  write.table(probes_removed, expName, quote=F, sep="\t", row.names=F, dec=".")
  return(nrow(probes_removed))
}