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

.tabColorHexa <- function(tab, statBH, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("tabColorHexa")}
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  #   rien
  # Returns:
  #   rien
  gamme <- .gammeCouleurs()
  tabColor <- tab[,2:6]
  tabColor[,1]    <- sapply(tab[,2],    .transformInt,  gammeJaune=gamme$jaune, valeurZero=gamme$noire)
  tabColor[,2]    <- sapply(tab[,3],    .transformInt,  gammeJaune=gamme$jaune, valeurZero=gamme$noire)
  tabColor$ratio  <- sapply(tab$ratio,  .transformRat)
  tabColor$pvalue <- sapply(tab$pvalue, .transformPval, gammeBleue=gamme$bleue, valeurZero=gamme$noire)
  tabColor[[statBH]]<- sapply(tab[[statBH]], .transformBh, gammeBleue=gamme$bleue, valeurZero=gamme$noire)
  return(tabColor)
}
