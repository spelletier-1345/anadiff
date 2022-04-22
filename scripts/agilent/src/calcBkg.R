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
# Le 30 janvier 2019 - Sandra PELLETIER
###############################################

.calcBkg <- function(intensite, nbg, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("calcBkg")}
  # calcul du bruit de fond en fonction de nbg
  # Args:
  #   intensite : liste des intensitees
  #   nbg : nombre de sonde pour le calcul
  # Returns:
  #   bkg : liste d'info : moyenne, ecart-type, bruit de fond calcule
  Abg   <- intensite[intensite<=sort(intensite)[nbg]]      # Abg = Tri des nbg plus petites valeurs
  mAbg  <- mean(Abg,na.rm=T)       # mAbg est la moyenne de ces nbg plus petites valeurs
  sdAbg <- sd(Abg,na.rm=T)         # sdAbg est l'ecart type de ces nbg plus petites valeurs
  bg    <- mAbg+2*sdAbg            # Calcul du bruit de fond
  return(bkg=list(moyenne=mAbg,ecarttype=sdAbg,background=bg))   # sortie : liste des elements moyenne, sd et bg
}
