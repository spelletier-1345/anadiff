###############################################
# AnaDiff Agilent
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.transformPval <- function(pval, dataTest=conf$dataTest,
                           gammeBleue=.gammeCouleurs()$bleue,
                           valeurZero=.gammeCouleurs()$noire) {
  # renvoi le code couleur en fonction de la valeur de la pvalue
  # Args:
  #   pval : valeur a transformer
  #   gammeBleue : vecteur de 4 couleurs dans le bleu
  #   valeurZero : valeur pour une intensite nulle
  # Returns:
  #   le code couleur hexadécimal correspondant
  if      (is.na(pval))    {cPval <- "#a0a0a0"}
  else if (pval < 0.00001) {cPval <- gammeBleue[4]}
  else if (pval < 0.001)   {cPval <- gammeBleue[3]}
  else if (pval < 0.01)    {cPval <- gammeBleue[2]}
  else if (pval < 0.05)    {cPval <- gammeBleue[1]}
  else                     {cPval <- valeurZero}
  return(cPval)
}
