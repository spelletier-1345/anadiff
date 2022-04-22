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

.numPuce <- function(fichier, dataTest=conf$dataTest) {
  # recupere le numero de la puce depuis le nom de fichier
  # Args:
  #   fichier : nom du fichier
  # Returns:
  #   le numero de la puce
  if (!is.null(dataTest)) {print("numPuce")}
  puce <- substr(fichier,(nchar(fichier)-6),(nchar(fichier)-4))
  return(puce)
}
