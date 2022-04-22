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

.transformPval <- function(pval, dataTest=conf$dataTest,
                           gammeBleue=.gammeCouleurs()$bleue,
                           valeurZero=.gammeCouleurs()$noire) {
  # renvoi le code couleur en fonction de la valeur de la pvalue
  # Args:
  #   pval : valeur a transformer
  #   gammeBleue : vecteur de 4 couleurs dans le bleu
  #   valeurZero : valeur pour une intensite nulle
  # Returns:
  #   le code couleur hexadécimal correspondant
  if      (is.na(pval))    {cPval <- "#a0a0a0"}
  else if (pval < 0.00001) {cPval <- gammeBleue[4]}
  else if (pval < 0.001)   {cPval <- gammeBleue[3]}
  else if (pval < 0.01)    {cPval <- gammeBleue[2]}
  else if (pval < 0.05)    {cPval <- gammeBleue[1]}
  else                     {cPval <- valeurZero}
  return(cPval)
}
