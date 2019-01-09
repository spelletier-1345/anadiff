###############################################
# AnaDiff Agilent
# Le 9 janvier 2018 - Sandra PELLETIER
###############################################

.writeLineOut <- function(lineOut, fileOut) { #valide
  # Ecrit une ligne dans le fichier out
  # Args:
  #   lineOut : texte a ecrire
  #   dirName : dossier d'export pour .writeLineOut
  # Dependance:
  #   .nameFileOut(dirName)
  # Returns:
  #   rien
  if (fileOut=="noOut") {
    cat(paste(lineOut, "\n", sep=""))
  } else {
    cat(paste(lineOut, "\n", sep=""))
    write.table(lineOut, fileOut, quote=F, append=T, row.names=F, col.names=F)
  }
  return(NULL)
}
