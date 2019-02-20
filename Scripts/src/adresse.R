###############################################
# AnaDiff Agilent
# Le 9 janvier 2019 - Sandra PELLETIER
###############################################

# Les données de test et les données de conf (annot, ...) sont dans le mnt/projects

.adresse <- function(dataTest = conf$dataTest) {
  if (!is.null(dataTest)) {print(".adresse")}
  if (is.null(dataTest)) {
    source("./conf.R")
  } else {
    adresse <- paste(dirname(conf$dataTest), "/", sep="")
  }
  return(adresse)
}
