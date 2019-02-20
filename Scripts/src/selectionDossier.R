###############################################
# AnaDiff Agilent
# Le 9 janvier 2019 - Sandra PELLETIER
###############################################

.selectionDossier <- function(inviteDossier=NULL, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print(".selectionDossier")}
  if (is.null(inviteDossier)) {
    fileArray <- conf$dataTest
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
