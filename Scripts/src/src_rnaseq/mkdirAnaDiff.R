.mkdirAnaDiff <- function(folderSummary) {
  # Creation du dossier AnaDiff et du fichier out
  # Args:
  #   rien
  # CeQuIlFait:
  #   verifie si le dossier AnaDiff existe deja
  #   si non : cree le dossier
  #   si oui : incremente le nom et cree dossier
  #   defini le dossier de travail
  # Returns:
  #   rien
  cat("Creation du dossier AnaDiff...\n")
  pathFolder <- folderSummary[[1]]
  folderName <- tools::file_path_sans_ext(folderSummary[[2]])
  dirName <- paste(pathFolder, folderName, sep="")
  if(file.exists(dirName)) {
    i <- 1
    dirNameId <- paste(dirName,i,sep=".")
    while (file.exists(dirNameId)) {
      i <- i+1
      dirNameId <- paste(dirName,i,sep=".")
    }
  } else {dirNameId <- dirName}
  cat("...Dossier de travail : ",dirNameId,"\n")
  dir.create(dirNameId,showWarnings=F)
  dirNameId <- paste(dirNameId,"/",sep="")
  return(dirNameId)
}
