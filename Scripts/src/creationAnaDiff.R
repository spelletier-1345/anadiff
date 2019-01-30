###############################################
# AnaDiff Agilent
# Le 9 janvier 2018 - Sandra PELLETIER
###############################################

.creationAnaDiff <- function(dataTest=NULL, designPuce, swap, labelling) { #valide
  # Creation du dossier AnaDiff et du fichier out
  # Args:
  #   rien
  # CeQuIlFait:
  #   verifie si le dossier AnaDiff existe deja
  #   si oui : invite a le deplacer
  #   si non : cree le dossier
  #   defini le dossier de travail
  # Returns:
  #   rien
  if (is.null(dataTest)) {print("")}
  dirName <- paste("AnaDiff_", swap, sep="")
  if (!is.null(dataTest)) {
    dirName <- paste(paste(dataTest, "AnaDiff", sep=""),designPuce,labelling,sep="_")
  } else if (is.element(dirName,dir())) {
    cat("Creation du dossier AnaDiff...\n")
    numDir <- 1
    dirName <- paste("AnaDiff_", swap, "(", numDir, ")", sep="")
    while (is.element(dirName,dir())) {
      numDir <- numDir+1
      dirName <- paste("AnaDiff_", swap, "(", numDir, ")", sep="")
    }
  }
  cat("... Dossier de travail : ",dirName,"\n")
  dir.create(dirName,showWarnings=F)
  dir.create(paste(dirName, "antisens", sep="/"),showWarnings=F)
  dir.create(paste(dirName, "sens", sep="/"),showWarnings=F)
  dir.create(paste(dirName, "qualityControl_geoSubmission", sep="/"),showWarnings=F)
  dirName <- paste(dirName,"/",sep="")
  return(dirName)
}