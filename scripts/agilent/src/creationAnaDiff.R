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
# Le 9 janvier 2019 - Sandra PELLETIER
###############################################

.creationAnaDiff <- function(designPuce, swap, labelling) {
  # Creation du dossier AnaDiff et du fichier out
  # Args:
  #   rien
  # CeQuIlFait:
  #   verifie si le dossier AnaDiff existe deja
  #   si oui : invite a le deplacer
  #   si non : cree le dossier
  #   defini le dossier de travail
  # Returns:
  #   rien
  dirName <- paste("AnaDiff_", swap, sep="")
  if (is.element(dirName,dir())) {
    cat("Creation du dossier AnaDiff...\n")
    numDir <- 1
    dirName <- paste("AnaDiff_", swap, "(", numDir, ")", sep="")
    while (is.element(dirName,dir())) {
      numDir <- numDir+1
      dirName <- paste("AnaDiff_", swap, "(", numDir, ")", sep="")
    }
  }
  cat("... Dossier de travail : ",dirName,"\n")
  dir.create(dirName,showWarnings=F)
  dir.create(paste(dirName, "antisens", sep="/"),showWarnings=F)
  dir.create(paste(dirName, "sens", sep="/"),showWarnings=F)
  dir.create(paste(dirName, "qualityControl_geoSubmission", sep="/"),showWarnings=F)
  dirName <- paste(dirName,"/",sep="")
  return(dirName)
}
