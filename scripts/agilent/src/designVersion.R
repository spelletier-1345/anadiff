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

.designVersion <- function(designPuce, adresse, dataTest=conf$dataTest) {
  # importe les attributs correspondant au design de la puce
  # Args:
  #   designPuce : design de la puce utilisee
  # Returns:
  #   designInfo : dataframe comportant les differentes infos relatives au design utilise
  if (!is.null(dataTest)) {print(".designVersion")}
  if (!is.null(dataTest)) {
    # Utilisation d'un fichier designSpecificity.txt de test
    # Mettre le nom du fichier à tester
    designInfo <- read.table(paste(adresse,"designSpecificity.txt",sep=""),sep="\t",header=T)
  } else {
    designInfo <- read.table(paste(adresse,"designSpecificity.txt",sep=""),sep="\t",header=T)
  }
  designInfo <- droplevels(designInfo[which(designInfo$ID==designPuce),])
  designList <- list()
  for (column in colnames(designInfo)) {
    if (class(designInfo[,column])=="factor") {
      designInfo[,column] <- as.character(designInfo[,column])
    }
    designList[[column]] <- unique(designInfo[,column])
  }
  return(designList)
}
