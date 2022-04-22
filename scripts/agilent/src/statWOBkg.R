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

.statWoBkg <- function(tab, targets, dataTest=conf$dataTest) {
  # calcul des intensite par echantillon et soustraction du bruit de fond
  # tri le tableau par probe_id et l'enregistre
  # Args:
  #   tab : tableau de donnees avec probe_id, Amean et ratio
  #   nbg : nombre de sonde pour le calcul
  #   dirName : dossier d'export pour .writeLineOut
  #   targets : sous-tableau de array correspondant au swap, provenant de RG$targets
  # Returns:
  #   tab : tableau de donnees avec intensite soustraite du bruit de fond par echantillon
  if (!is.null(dataTest)) {print("statWoBkg")}
  cat("calcul de l'intensite de l'echantillon controle...\n")
  IGreen    <- round(tab$Amean - tab$ratio/2, 2)
  cat("calcul de l'intensite de l'echantillon traitement\n")
  IRed   <- round(tab$Amean + tab$ratio/2, 2)
  tab <- data.frame(tab[,1],IGreen,IRed,round(tab[,3:4],4))
  names(tab)[1:3] <- c("probe_id", targets$CtrName[1], targets$TtmtName[1])
  return(tab)
}
