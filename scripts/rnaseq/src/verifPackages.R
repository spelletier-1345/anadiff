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

.verifPackages <- function(vPackages) {
  # Verification de l'installation des packages
  # Args: 
  #   vPackages : vecteur des packages à tester
  # CeQuIlFait:
  #   si non installe : arrêt du script et rm de la memoire
  #   si oui : chargement des packages
  # Returns:
  #   rien
  cat("\n-------------------------\n\n")
  cat("Verification des packages...\n")
  for (package in vPackages) {
    if (is.element(package,installed.packages()[,1])) {} 
    else {
      message <- paste("\n#########################################\n\nVous devez telecharger le package ", package, "\n",
                       "pour cela, coller les lignes suivantes :\n\nsource(\"http://bioconductor.org/biocLite.R\")\n",
                       "biocLite(\"", package, "\")\n\n#########################################\n\n", sep="")
      cat("\n-------------------------\n\n")
      cat(message)
      cat("\n-------------------------\n\n")
      rm(list=ls(all=T))
      stop("Script arrete",call.=F)
    }
  }
  for (package in vPackages) {
    library(package,character.only=TRUE)
    cat("chargement de la librairie",package,"\n")
  }
  return(NULL)
}
