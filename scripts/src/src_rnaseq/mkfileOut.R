source("/home/spelletier/dev/RNAseq/Scripts/AnaDiffRNAseq/src/writeLineOut.R")

.mkfileOut <- function(dirName) {
  # Cree le fichier out dans le dossier dirName
  # Export les infos relatives a la date et au dossier de travail
  # Args: 
  #   dirName : dossier d'export pour .writeLineOut
  # Returns:
  #   rien
  fileOut <- paste(dirName,"AnaDiff_",Sys.Date(),"_out.txt",sep="")
  file.create(fileOut)
  .writeLineOut("\n###  Analyses differentielles RNAseq ###\n", fileOut)
  .writeLineOut(paste("Le",format(Sys.Date(), "%a %d %b %Y")), fileOut)
  .writeLineOut("Script Anadiff_RNAseq.R mis a jour le 17/09/2018", fileOut)
  .writeLineOut("Script Functions_RNAseq.R mis a jour le 17/11/2014", fileOut)
  .writeLineOut("Package R utilises : edgeR, DESeq2, geneplotter", fileOut)
  .writeLineOut(paste("Repertoire de travail :",getwd()), fileOut)
  .writeLineOut(paste("Dossier d'export : ", dirName, sep=""), fileOut)
  return(fileOut)
} 