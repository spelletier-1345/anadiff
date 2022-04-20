###############################################
# AnaDiff Agilent
# Le 9 janvier 2019 - Sandra PELLETIER
###############################################

.selectionDossier <- function(conf, inviteDossier=TRUE) {
  if (conf$dataTest) {print("selectionDossier")} else {inviteDossier=FALSE}
  if (inviteDossier) {
    fileArray <- conf$dataFolder
  } else {
    cat(conf$message)
    readLines(n=1)
    e <- simpleError("Oups")
    fileArray <- tryCatch(file.choose(), error=function(e) e)
    if (class(fileArray)!="character") {
      return("file.choose() : opening of the folder canceled")
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
