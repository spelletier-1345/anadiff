###############################################
# AnaDiff Agilent
# Le 9 janvier 2018 - Sandra PELLETIER
###############################################

.numPuce <- function(fichier, dataTest=conf$dataTest) {
  # recupere le numero de la puce depuis le nom de fichier
  # Args:
  #   fichier : nom du fichier
  # Returns:
  #   le numero de la puce
  if (!is.null(dataTest)) {print("numPuce")}
  puce <- substr(fichier,(nchar(fichier)-6),(nchar(fichier)-4))
  return(puce)
}
