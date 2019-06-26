###############################################
# AnaDiff Agilent
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.tabColorHexa <- function(tab, statBH, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("tabColorHexa")}
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  #   rien
  # Returns:
  #   rien
  gamme <- .gammeCouleurs()
  tabColor <- tab[,2:6]
  tabColor[,1]    <- sapply(tab[,2],    .transformInt,  gammeJaune=gamme$jaune, valeurZero=gamme$noire)
  tabColor[,2]    <- sapply(tab[,3],    .transformInt,  gammeJaune=gamme$jaune, valeurZero=gamme$noire)
  tabColor$ratio  <- sapply(tab$ratio,  .transformRat)
  tabColor$pvalue <- sapply(tab$pvalue, .transformPval, gammeBleue=gamme$bleue, valeurZero=gamme$noire)
  tabColor[[statBH]]<- sapply(tab[[statBH]], .transformBh, gammeBleue=gamme$bleue, valeurZero=gamme$noire)
  return(tabColor)
}
