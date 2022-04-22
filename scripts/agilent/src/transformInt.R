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
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.transformInt <- function(int, dataTest=conf$dataTest,
                          gammeJaune=.gammeCouleurs()$jaune,
                          valeurZero=.gammeCouleurs()$noire) {
  # renvoi le code couleur en fonction de la valeur de l'intensite
  # Args:
  #   int : intensite a transformer
  #   gammeJaune : vecteur de 9 couleurs dans le jaune
  #   valeurZero : valeur pour une intensite nulle
  # CeQuIlFait:
  #   la valeur est arrondie et bornee entre 0 et 9
  # Returns:
  #   le code couleur hexadécimal correspondant
  if (is.na(int)) {cInt <- "#a0a0a0"}
  else {
    int <- round(int)
    int[which(int<0)] <- 0 ; int[which(int>9)] <- 9
    cInt <- ""
    ifelse (int==0, cInt <- valeurZero , cInt <- gammeJaune[int])
  }
  return(cInt)
}
