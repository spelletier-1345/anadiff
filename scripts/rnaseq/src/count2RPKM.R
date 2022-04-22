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

source("./src/selectionFolder.R")
count2rpkm <- function(countCol, lengthCol, header=F) {
  localOpt <- options()
  options(warn=-1)
  # Téléchargement du fichier à modifier
  theFile <- .selectionFolder()
  if (class(theFile)=="character") {return("file.choose() : choix de fichier annule") ; stop(call.=F)}
  count = read.csv(paste(theFile[[1]], theFile[[2]], sep=""), sep="\t", header = header)
  # Calcul des RPKM
  t <- as.double(sum(count[,countCol]))
  count$RPKM <- round((1e9*count[,countCol])/(t*count[,lengthCol]), 2)
  # Enregistrement du fichier
  extension <- substr(theFile[[2]], max(which(strsplit(theFile[[2]], "")[[1]]=="."))+1, nchar(theFile[[2]]))
  nameFile <- gsub(extension, "rpkm", theFile[[2]])
  nbCol <- ncol(count)
  colNames <- colnames(count)
  if (!header) {
    colNames[countCol] <- "count"
    colNames[lengthCol] <- "length"
  }
  write.table(count,paste(theFile[[1]], nameFile, sep=""), sep="\t", row.names = F,col.names = colNames , quote = F)
  cat("Fichier", nameFile, "enregistré dans le dossier de travail\n")
  options(localOpt)
}
