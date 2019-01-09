###############################################
# AnaDiff Agilent
# Le 9 janvier 2018 - Sandra PELLETIER
###############################################

.selectionDossier <- function(inviteDossier=NULL) {
  # Selection du dossier de travail
  # Args:
  #   rien
  # CeQuIlFait:
  #   demande ou est le fichier arrays.txt
  #   defini le dossier de travail
  # Returns:
  #   rien
  if (is.null(inviteDossier)) {
    fileArray <- "/home/spelletier/Documents/Projets/Agilent/Data/summary.txt"
} else {
    cat(inviteDossier)
    readLines(n=1)
    e <- simpleError("Oups")
    fileArray <- tryCatch(file.choose(), error=function(e) e)
    if (class(fileArray)!="character") {
      return("file.choose() : choix de fichier annule")
      stop(call.=F)
    }
  }
  
  dataFolder <- try(paste(dirname(fileArray),"/",sep=""), silent=TRUE)
  if (!is.null(inviteDossier)) {
    setwd(dataFolder)
    cat("Dossier de travail :\n",dataFolder,"\n\n",sep="")
  }
  fileArray <- basename(fileArray)
  
  return(list(dataFolder,fileArray))
}
