###############################################
# AnaDiff Agilent
# Le 9 janvier 2018 - Sandra PELLETIER
###############################################

.defineArrays <- function(dirName, fileOut, fileArray, dataTest=NULL) {
  # Ouverture et modif du fichier arrays.txt
  # Args:
  #   dirName : dossier d'export pour .writeLineOut
  # CeQuIlFait:
  #   lit la liste des fichiers de donnees brutes et le fichier arrays.txt
  #   change la colonne fichier en fonction des nom des fichiers brutes
  #   exporte le tableau arrays dans le fichier Out
  # Returns:
  #   le tableau arrays
  path <- ifelse(!is.null(dataTest), dataTest, ".")
  fileListe <- list.files(path = path, pattern = ("_[[:digit:]]+_[[:digit:]]+.txt$"))
  arrays <- read.table(paste(path, fileArray, sep="/"), header=T, encoding="utf-8")
  arrays$FileName <- as.character(arrays$FileName)
  compteur <- 0
  for (ligneArrays in arrays$FileName) {
    compteur <- compteur+1
    # lame_arrays : recupere le numéro de la lame dans arrays
    lame_arrays <- substr(ligneArrays,1,12) #
    # file_lame : liste des fichiers dans repertoire avec meme num de lame que dans arrays
    file_lame   <- fileListe[which(substr(fileListe,1,12)==lame_arrays)]
    
    
    if (length(file_lame)==0) {
      # si pas de fichier dans le repertoire -> arret du script
      message <- paste("\n#########################################\n\nIl n'y a pas de puce ",puce_arrays , " pour la lame ", lame_arrays, "\n",
                       "Verifiez les codes barre de vos lames dans le fichier arrays.txt\n",
                       "Verifier que les fichiers de donnees brutes soient dans le meme repertoire que le fichier arrays.txt",
                       "\n\n#########################################\n\n", sep="")
      cat(message)
      rm(list=ls(all=T))
      stop("Script arrete",call.=F)
      
    } else {
      # puce_arrays : recupere le num de puce dans arrays
      puce_arrays <- substr(ligneArrays,(nchar(ligneArrays)-2),(nchar(ligneArrays)))
      # file_arrays : recupere dans la sous-liste le nom du fichier avec le meme num de puce
      file_arrays <- file_lame[which(.numPuce(fileListe)==puce_arrays)]
      
      if (length(file_arrays)==0) {
        # si pas de fichier dans le repertoire -> arret du script
        message <- paste("\n#########################################\n\nIl n'y a pas de puce ",puce_arrays , " pour la lame ", lame_arrays, "\n",
                         "Verifiez les numeros de puce de vos lames dans le fichier arrays.txt\n",
                         "Verifier que les fichiers de donnees brutes soient dans le meme repertoire que le fichier arrays.txt",
                         "\n\n#########################################\n\n", sep="")
        cat(message)
        rm(list=ls(all=T))
        stop("Script arrete",call.=F)
        
      } else {
        arrays$FileName[compteur] <- file_arrays # copie le nom du fichier dans arrays
      }
    }
  }
  return(arrays)
}
