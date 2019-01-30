###############################################
# AnaDiff Agilent
# Le 9 janvier 2018 - Sandra PELLETIER
###############################################

# Les données de test et les données de conf (annot, ...) sont dans le mnt/projects

.adresse <- function(dataTest = NULL) {
  if (is.null(dataTest)) {print(".adresse")}
  if (is.null(dataTest)) {
    adresse <- "http://pegasus-bioinfo.angers-nantes.inra.fr/rscripts/Tools/Agilent/" # TODO
  } else {
    # adresse <- "http://147.99.112.52/rscripts/Tools/Agilent/"
    adresse <- dataTest
  }
  return(adresse)
}
