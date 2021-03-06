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

.creationFileOut <- function(dirName, swap, dataTest=conf$dataTest) {
  # Cree le fichier out dans le dossier dirName
  # Export les infos relatives a la date et au dossier de travail
  # Args:
  #   dirName : dossier d'export pour .writeLineOut
  # Returns:
  #   rien
  if (!is.null(dataTest)) {print("creationFileOut")}
  if (!is.null(dataTest)) {
    fileOut <- paste(dirName, "qualityControl_geoSubmission/AnaDiff_", swap, "_out.txt", sep="")
    file.create(fileOut)
  } else {
    fileOut <- paste(dirName, "qualityControl_geoSubmission/AnaDiff_", swap, "_", Sys.Date(), "_out.txt", sep="")
    file.create(fileOut)
    .writeLineOut("\n###  Analyses microarrays ###\n", fileOut)
    .writeLineOut(paste("Le",format(Sys.Date(), "%a %d %b %Y")), fileOut)
    .writeLineOut(paste("Repertoire de travail :",getwd()), fileOut)
    .writeLineOut(paste("Dossier d'export :      ",getwd(),"/",dirName,sep=""), fileOut)
  }
  return(fileOut)
}
