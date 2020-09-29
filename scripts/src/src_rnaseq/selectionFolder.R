.selectionFolder <- function() {
  # Selection du dossier de travail
  # Args: 
  #   rien
  # CeQuIlFait:
  #   demande ou est le fichier arrays.txt
  #   defini le dossier de travail
  # Returns:
  #   rien
  readLines(n=1)
  e <- simpleError("Oups")
  fileSummary <- tryCatch(file.choose(), error=function(e) e)
  if (class(fileSummary)!="character") {
    return("file.choose() : choix de fichier annule")
    stop(call.=F)
  }
  dataFolder <- try(paste(dirname(fileSummary),"/",sep=""), silent=TRUE) 
  cat("Dossier de travail :\n",dataFolder,"\n\n",sep="")
  setwd(dataFolder)
  fileSummary <- basename(fileSummary)
  return(list(dataFolder,fileSummary))
}