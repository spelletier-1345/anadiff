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

.tabColorWithoutData <- function(tabColor, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("tabColorWithoutData")}
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  #   tabColor : tableau de couleur apres transformation de tabResult
  # Returns:
  #   rien
  element1 <- "<tr><td>"
  element2 <- "</td><td align=\"center\" bgcolor=\""
  element3 <- "\">"
  element4 <- "</td><td>"
  element5 <- "</td></tr>"
  
  matColor <- matrix(nrow=nrow(tabColor)+3, ncol=1)
  matColor[1,1] <- "<table>"
  matColor[2,1] <- "<tr></tr>"
  matColor[3:(nrow(tabColor)+2),] <- matrix(paste(element1,
                                                  element2,tabColor[,1],element3,
                                                  element2,tabColor[,2],element3,
                                                  element2,tabColor[,3],element3,
                                                  element2,tabColor[,4],element3,
                                                  element2,tabColor[,5],element3,
                                                  element5,
                                                  sep=""),ncol=1)
  matColor[(nrow(tabColor)+3),1] <- "</table>"
  return(matColor)
}