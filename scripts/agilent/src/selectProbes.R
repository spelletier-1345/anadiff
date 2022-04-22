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
# Le 23 janvier 2019 - Sandra PELLETIER
###############################################

.selectProbes <- function(tab, probe, dataTest=conf$dataTest) {
  # selection des sondes et homogeneisation des probe_id des duplicats
  # Args:
  #   designPuce : type de puce utilisee
  #   tab : tableau de donnees avec Agilent_id
  # Returns:
  #   tab : tableau de donnees sans les sondes interne Agilent et avec les probe_id definitifs
  if (!is.null(dataTest)) {print("selectProbes")}
  cat("\nSelection des sondes...\n")
  tmp <- tab[0,]
  for (indProbe in probe) {
    l <- nchar(indProbe)
    tmp <- rbind(tmp,tab[which(substr(tab$Agilent_id,1,l)==indProbe),]) # remplacement de 3 par l
  }
  return(tmp)
}
