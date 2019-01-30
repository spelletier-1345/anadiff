###############################################
# AnaDiff Agilent
# Le 30 janvier 2018 - Sandra PELLETIER
###############################################

.nomExportAD <- function(export, sens, dirName, swap, adresse, dataTest=conf$dataTest) {  # doc !!!
  # cree une liste de nom pour l'export des fichiers couleur en fonction des formats
  if (!is.null(dataTest)) {print("nomExportAD")}
  exportS=paste(export, sens, sep="_")
  dirNameS <- paste(dirName, sens, "/", sep="")
  htmlC <- .nomFichier("_AnaDiff_Couleur.html", dirNameS, swap, exportS)
  texte <- .nomFichier("_AnaDiff.txt", dirNameS, swap, exportS)
  txtWb <- .nomFichier("_AnaDiff_wb.txt", dirNameS, swap, exportS)
  htmlD <- .nomFichier("_AnaDiff.html", dirNameS, swap, exportS)
  bkgI  <- .nomFichier("_bkg_intensities.txt", dirNameS, swap, exportS)
  normI <- .nomFichier("_norm_intensities.txt", dirNameS, swap, exportS)
  data  <- .nomFichier("_AnaDiff_Data.txt", dirNameS, swap, exportS)

  dirNameQC <- paste(dirName, "qualityControl_geoSubmission/", sep="")
  removed <- .nomFichier(paste("_", sens, "_probes_removed.txt", sep=""),
                         paste(dirName, "qualityControl_geoSubmission/", sep=""), swap, export)
  tabDble <- .nomFichier("_AnaDiff.txt", dirName, swap, export)
  colDble <- .nomFichier("_AnaDiff.html", dirName, swap, export)
  return(list(htmlC=htmlC, texte=texte, htmlD=htmlD, bkgI=bkgI, normI=normI, txtWb=txtWb,
              removed=removed, data=data, tabDble=tabDble, colDble=colDble))
}
