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

.transformBh <- function(bh, dataTest=conf$dataTest, 
                         gammeBleue=.gammeCouleurs()$bleue, 
                         valeurZero=.gammeCouleurs()$noire) {
  # renvoi le code couleur en fonction de la valeur du BH
  # Args:
  #   bh : valeur a transformer
  #   gammeBleue : vecteur de 4 couleurs dans le bleu
  #   valeurZero : valeur pour une intensite nulle
  # Returns:
  #   le code couleur hexadécimal correspondant
  if      (is.na(bh))    {cBh <- "#a0a0a0"}
  else if (bh < 0.00001) {cBh <- gammeBleue[4]}
  else if (bh < 0.01)    {cBh <- gammeBleue[3]}
  else if (bh < 0.05)    {cBh <- gammeBleue[2]}
  else if (bh < 0.1)     {cBh <- gammeBleue[1]}
  else                   {cBh <- valeurZero}
  return(cBh)
}
