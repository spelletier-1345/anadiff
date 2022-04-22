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

.nomExportAD <- function(export, sens, dirName, swap, adresse, dataTest=conf$dataTest) {  # doc !!!
  # cree une liste de nom pour l'export des fichiers couleur en fonction des formats
  if (!is.null(dataTest)) {print("nomExportAD")}
  exportSens=paste(export, sens, sep="_")
  dirNameSens <- paste(dirName, sens, "/", sep="")
  htmlC <- .nomFichier("_AnaDiff_Couleur.html", dirNameSens, swap, exportSens)
  texte <- .nomFichier("_AnaDiff.txt", dirNameSens, swap, exportSens)
  txtWb <- .nomFichier("_AnaDiff_wb.txt", dirNameSens, swap, exportSens)
  htmlD <- .nomFichier("_AnaDiff.html", dirNameSens, swap, exportSens)
  bkgI  <- .nomFichier("_bkg_intensities.txt", dirNameSens, swap, exportSens)
  normI <- .nomFichier("_norm_intensities.txt", dirNameSens, swap, exportSens)
  data  <- .nomFichier("_AnaDiff_Data.txt", dirNameSens, swap, exportSens)

  dirNameQC <- paste(dirName, "qualityControl_geoSubmission/", sep="")
  removed <- .nomFichier(paste("_", sens, "_probes_removed.txt", sep=""),
                         paste(dirName, "qualityControl_geoSubmission/", sep=""), swap, export)
  tabDble <- .nomFichier("_AnaDiff.txt", dirName, swap, export)
  colDble <- .nomFichier("_AnaDiff.html", dirName, swap, export)
  return(list(htmlC=htmlC, texte=texte, htmlD=htmlD, bkgI=bkgI, normI=normI, txtWb=txtWb,
              removed=removed, data=data, tabDble=tabDble, colDble=colDble))
}
