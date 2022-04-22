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

source("./src/writeLineOut.R")

.readSummary <- function(fileSummary, fileOut, pathFolder) {
  # Ouverture et modif du fichier arrays.txt
  # Args: 
  #   dirName : dossier d'export pour .writeLineOut
  # CeQuIlFait:
  #   lit la liste des fichiers de donnees brutes et le fichier arrays.txt
  #   change la colonne fichier en fonction des nom des fichiers brutes
  #   exporte le tableau arrays dans le fichier Out
  # Returns:
  #   le tableau arrays
  cat("Lecture du fichier",fileSummary,"...\n")
  myData <- read.table(fileSummary,header=T,sep="\t",check.names=F)
  cat("Vérification de la présence des fichiers...\n")
  errorMsg <- FALSE
  for (countFile in myData$File) {
    pathFile <- paste(pathFolder, countFile, sep="")
    cat(pathFile)
    if(file.exists(pathFile)) {
      cat(" : OK\n")
    } else {
      errorMsg <- TRUE
      cat("\n   !!! MISSING FILE !!!\n")
    }
  }
  if (errorMsg) {return("error")}
  cat("Ecriture du tableau summary dans le fichier de sortie...\n")
  tab <- rbind(myData[1,],myData[,])
  for (indice in seq(1,4)) {
    tab[,indice] <- c(matrix(colnames(myData),nrow=1)[,indice],as.character(myData[,indice]))
  }
  tab <- apply(tab, 2, format)
  .writeLineOut(paste("\nFichier summary utilise par le script :",fileSummary,"\n"),fileOut)
  write.table(tab ,fileOut,append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
  
  cat("Analyses de chaque comparaison...\n\n")
  cat("########################################################\n")
  cat("                 Analyse differentielle\n")
  cat("########################################################\n\n")
  .writeLineOut("\n#################################################################",fileOut)
  .writeLineOut("                 Analyse differentielle",fileOut)
  .writeLineOut("#################################################################\n",fileOut)
  return(myData)
}
