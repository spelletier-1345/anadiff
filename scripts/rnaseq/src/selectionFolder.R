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

.selectionFolder <- function() {
  # Selection du dossier de travail
  # Args: 
  #   rien
  # CeQuIlFait:
  #   demande ou est le fichier arrays.txt
  #   defini le dossier de travail
  # Returns:
  #   rien
  readLines(n=1)
  e <- simpleError("Oups")
  fileSummary <- tryCatch(file.choose(), error=function(e) e)
  if (class(fileSummary)!="character") {
    return("file.choose() : choix de fichier annule")
    stop(call.=F)
  }
  dataFolder <- try(paste(dirname(fileSummary),"/",sep=""), silent=TRUE) 
  cat("Dossier de travail :\n",dataFolder,"\n\n",sep="")
  setwd(dataFolder)
  fileSummary <- basename(fileSummary)
  return(list(dataFolder,fileSummary))
}
