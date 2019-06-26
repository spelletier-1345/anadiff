.writeLineOut <- function(ligneOut, fileOut) {
  # Ecrit une ligne dans le fichier out
  # Args: 
  #   ligneOut : texte a ecrire
  #   dirName : dossier d'export pour .writeLineOut
  # Dependance:
  #   .nameFileOut(dirName)
  # Returns:
  #   rien  
  write.table(ligneOut, fileOut, quote=F, append=T, row.names=F, col.names=F)
  return(NULL)
}