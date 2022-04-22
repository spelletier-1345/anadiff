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

.mkdirAnaDiff <- function(folderSummary) {
  # Creation du dossier AnaDiff et du fichier out
  # Args:
  #   rien
  # CeQuIlFait:
  #   verifie si le dossier AnaDiff existe deja
  #   si non : cree le dossier
  #   si oui : incremente le nom et cree dossier
  #   defini le dossier de travail
  # Returns:
  #   rien
  cat("Creation du dossier AnaDiff...\n")
  pathFolder <- folderSummary[[1]]
  folderName <- tools::file_path_sans_ext(folderSummary[[2]])
  dirName <- paste(pathFolder, folderName, sep="")
  if(file.exists(dirName)) {
    i <- 1
    dirNameId <- paste(dirName,i,sep=".")
    while (file.exists(dirNameId)) {
      i <- i+1
      dirNameId <- paste(dirName,i,sep=".")
    }
  } else {dirNameId <- dirName}
  cat("...Dossier de travail : ",dirNameId,"\n")
  dir.create(dirNameId,showWarnings=F)
  dirNameId <- paste(dirNameId,"/",sep="")
  return(dirNameId)
}
