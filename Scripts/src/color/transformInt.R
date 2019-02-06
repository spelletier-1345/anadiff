###############################################
# AnaDiff Agilent
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.transformInt <- function(int, dataTest=conf$dataTest,
                          gammeJaune=.gammeCouleurs()$jaune,
                          valeurZero=.gammeCouleurs()$noire) {
  # renvoi le code couleur en fonction de la valeur de l'intensite
  # Args:
  #   int : intensite a transformer
  #   gammeJaune : vecteur de 9 couleurs dans le jaune
  #   valeurZero : valeur pour une intensite nulle
  # CeQuIlFait:
  #   la valeur est arrondie et bornee entre 0 et 9
  # Returns:
  #   le code couleur hexadécimal correspondant
  if (is.na(int)) {cInt <- "#a0a0a0"}
  else {
    int <- round(int)
    int[which(int<0)] <- 0 ; int[which(int>9)] <- 9
    cInt <- ""
    ifelse (int==0, cInt <- valeurZero , cInt <- gammeJaune[int])
  }
  return(cInt)
}
