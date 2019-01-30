###############################################
# AnaDiff Agilent
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

.nomFichier <- function(texte, dirName, swap, export, dataTest=NULL) {
  if (is.null(dataTest)) {print("nomFichier")}
  t <- paste("_", export, texte, sep="")
  nom <- paste(dirName, swap, t, sep="")
  return(nom)
}
