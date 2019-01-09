###############################################
# AnaDiff Agilent
# Le 9 janvier 2018 - Sandra PELLETIER
###############################################

.creationFileOut <- function(dirName, swap, dataTest=NULL) {
  # Cree le fichier out dans le dossier dirName
  # Export les infos relatives a la date et au dossier de travail
  # Args:
  #   dirName : dossier d'export pour .writeLineOut
  # Returns:
  #   rien
  if (!is.null(dataTest)) {
    fileOut <- paste(dirName, "qualityControl_geoSubmission/AnaDiff_", swap, "_out.txt", sep="")
    file.create(fileOut)
  } else {
    fileOut <- paste(dirName, "qualityControl_geoSubmission/AnaDiff_", swap, "_", Sys.Date(), "_out.txt", sep="")
    file.create(fileOut)
    .writeLineOut("\n###  Analyses microarrays ###\n", fileOut)
    .writeLineOut(paste("Le",format(Sys.Date(), "%a %d %b %Y")), fileOut)
    .writeLineOut(paste("Repertoire de travail :",getwd()), fileOut)
    .writeLineOut(paste("Dossier d'export :      ",getwd(),"/",dirName,sep=""), fileOut)
  }
  return(fileOut)
}
