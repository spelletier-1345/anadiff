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

.writeLineOut <- function(lineOut, fileOut, dataTest=conf$dataTest) { #valide
  # Ecrit une ligne dans le fichier out
  # Args:
  #   lineOut : texte a ecrire
  #   dirName : dossier d'export pour .writeLineOut
  # Dependance:
  #   .nameFileOut(dirName)
  # Returns:
  #   rien
  if (!is.null(dataTest)) {print("writeLineOut")}
  cat(paste(lineOut, "\n", sep=""))
  if (fileOut!="noOut") {
    write.table(lineOut, fileOut, quote=F, append=T, row.names=F, col.names=F)
  }
  return(NULL)
}
