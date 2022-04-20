source("./src/writeLineOut.R")

.readSummary <- function(fileSummary, fileOut, pathFolder) {
  # Ouverture et modif du fichier arrays.txt
  # Args: 
  #   dirName : dossier d'export pour .writeLineOut
  # CeQuIlFait:
  #   lit la liste des fichiers de donnees brutes et le fichier arrays.txt
  #   change la colonne fichier en fonction des nom des fichiers brutes
  #   exporte le tableau arrays dans le fichier Out
  # Returns:
  #   le tableau arrays
  cat("Lecture du fichier",fileSummary,"...\n")
  myData <- read.table(fileSummary,header=T,sep="\t",check.names=F)
  cat("Vérification de la présence des fichiers...\n")
  errorMsg <- FALSE
  for (countFile in myData$File) {
    pathFile <- paste(pathFolder, countFile, sep="")
    cat(pathFile)
    if(file.exists(pathFile)) {
      cat(" : OK\n")
    } else {
      errorMsg <- TRUE
      cat("\n   !!! MISSING FILE !!!\n")
    }
  }
  if (errorMsg) {return("error")}
  cat("Ecriture du tableau summary dans le fichier de sortie...\n")
  tab <- rbind(myData[1,],myData[,])
  for (indice in seq(1,4)) {
    tab[,indice] <- c(matrix(colnames(myData),nrow=1)[,indice],as.character(myData[,indice]))
  }
  tab <- apply(tab, 2, format)
  .writeLineOut(paste("\nFichier summary utilise par le script :",fileSummary,"\n"),fileOut)
  write.table(tab ,fileOut,append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
  
  cat("Analyses de chaque comparaison...\n\n")
  cat("########################################################\n")
  cat("                 Analyse differentielle\n")
  cat("########################################################\n\n")
  .writeLineOut("\n#################################################################",fileOut)
  .writeLineOut("                 Analyse differentielle",fileOut)
  .writeLineOut("#################################################################\n",fileOut)
  return(myData)
}