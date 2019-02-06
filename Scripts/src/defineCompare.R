###############################################
# AnaDiff Agilent
# Le 23 janvier 2019 - Sandra PELLETIER
###############################################

.defineCompare <- function(swap, targets, fileOut, dataTest=conf$dataTest) {
  # definie le vecteur de comparaison pour limma
  # Args:
  #   swap : nom du swap
  #   targets : sous-tableau de array correspondant au swap, provenant de RG$targets
  #   fileOut : dossier d'export pour .writeLineOut
  # CeQuIlFait:
  #   verifie si dans la colonne "Control" il y a la valeur 3 ou 5
  # Returns:
  #   compare : vecteur de comparaison (liste de 1 et -1)
  if (!is.null(dataTest)) {print("defineCompare")}
  cat("Definition du vecteur de comparaisons...\n\n")
  compare <- c() ; y <- 0
  for (dye in targets$Control) {  # Une boucle pour chaque lame du dye-switch
    y <- y+1
    if(dye==3)
    {
      ctr <- 1  # puce <- substr(fichier,(nchar(fichier)-6),(nchar(fichier)-4))
      .writeLineOut(lineOut=paste("lame", substr(targets$FileName[y],1,12), "- puce", .numPuce(targets$FileName[y]),
                                  ": l'echantillon controle est :", as.character(targets$Cy3[y]),sep=" "), fileOut)
    } else {
      ctr <- (-1)
      .writeLineOut(lineOut=paste("lame", substr(targets$FileName[y],1,12), "- puce", .numPuce(targets$FileName[y]),
                                  ": l'echantillon controle est :", as.character(targets$Cy5[y]),sep=" "), fileOut)
    }
    compare <- c(compare,ctr)
  }
  return(compare)
}
