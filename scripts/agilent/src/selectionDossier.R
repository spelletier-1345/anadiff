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

.selectionDossier <- function(conf, inviteDossier=TRUE) {
  if (conf$dataTest) {print("selectionDossier")} else {inviteDossier=FALSE}
  if (inviteDossier) {
    fileArray <- conf$dataFolder
  } else {
    cat(conf$message)
    readLines(n=1)
    e <- simpleError("Oups")
    fileArray <- tryCatch(file.choose(), error=function(e) e)
    if (class(fileArray)!="character") {
      return("file.choose() : opening of the folder canceled")
      stop(call.=F)
    }
  }
  dataFolder <- try(paste(dirname(fileArray),"/",sep=""), silent=TRUE)
  if (!is.null(inviteDossier)) {
    setwd(dataFolder)
    cat("Dossier de travail :\n",dataFolder,"\n\n",sep="")
  }
  fileArray <- basename(fileArray)
  
  return(list(dataFolder,fileArray))
}
