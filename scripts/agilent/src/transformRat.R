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

.transformRat <- function(rat, dataTest=conf$dataTest) {
  # renvoi le code couleur en fonction de la valeur du ratio
  # les ratios s'interpretent sur une gamme logarithmique, la correspondance n'est pas lineaire
  # Args:
  #   rat : valeur du ratio a transformer
  # Returns:
  #   le code couleur hexadécimal correspondant
  if      (is.na(rat))   {cRat <- "#a0a0a0"}
  else if (rat < (-3))   {cRat <- "#00ff00"}
  else if (rat < (-1.5)) {cRat <- "#00aa00"}
  else if (rat < (-1))   {cRat <- "#006600"}
  else if (rat < (-0.5)) {cRat <- "#003300"}
  else if (rat < 0)      {cRat <- "#001100"}
  else if (rat == 0)     {cRat <- "#000000"}
  else if (rat < 0.5)    {cRat <- "#110000"}
  else if (rat < 1)      {cRat <- "#330000"}
  else if (rat < 1.5)    {cRat <- "#770000"}
  else if (rat < 3)      {cRat <- "#aa0000"}
  else                   {cRat <- "#ff0000"}
  return(cRat)
}