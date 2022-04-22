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
xlsTransform <- function() {
  localOpt <- options() ; options(warn=-1)
  cat("Selection du repertoire de travail...\n")
  cat("\n------------ MESSAGE -------------\n")
  cat("\nAppuyez sur \"Entree\" et selectionnez le fichier texte de données compilées.\n")
  selectXls <- .selectionFolder()
  if (class(selectXls)=="character") {
    return("file.choose() : choix de fichier annule")
    options(localOpt)
    stop(call.=F)}
  filexls <- read.csv(selectXls[[2]], sep = "\t", check.names = F, row.names = NULL)
  header <- colnames(filexls)
  len <- length(header)
  lenGene <- header[6]=="4_length"
  for (i in seq(1, len)) {
    title <- header[i]
    nc <- as.numeric(nchar(title))
    if (nc > 5 & substr(title, nc-4, nc)=="count") {
      if (lenGene) {
        count <- filexls[,c(1,4,i,i)]
        count[,4] <- 0
      } else {
        count <- filexls[,c(1,i,i,i)]
        count[,c(2,4)] <- 0
      }
      title <- substr(title, 1, nc-6)
      listeBefore <- unlist(strsplit(title, "-"))
      listeAfter  <- c()
      for (l in listeBefore) {
        listeAfter <- c(listeAfter, gsub("[[:punct:]]", "_", l))
      }
      fileName = paste(paste(listeAfter, collapse="-"), "count", sep=".")
      i <- 1
      while (file.exists(fileName)) {
        fileName <- paste(paste(listeAfter, collapse="-"), i, "count", sep=".")
        i <- i+1
      }
      write.table(count, fileName, quote=F, sep="\t", row.names = F, col.names = F)
      write("*\t0\t0\t0", fileName, append = T)
    }
  }
}
