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

.defineComparisons <- function(myData) {
  # Recupère une liste unique de nom de swap
  # Args: 
  #   arrays : tableau de meta-donnee sur design de la manip
  # Returns:
  #   liste de comparaison uniques
  cat("Definition de la comparaison...\n")
  comparisons <- paste(unique(myData$Comparison))
  return(comparisons)
}
