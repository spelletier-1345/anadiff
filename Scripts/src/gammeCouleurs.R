###############################################
# AnaDiff Agilent
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.gammeCouleurs <- function(dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("gammeCouleurs")}
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  #   rien
  # Returns:
  #   liste de vecteurs :
  #   - bleu  :  4 couleurs
  #   - jaune :  9 couleurs
  #   - vert  : 10 couleurs
  #   - rouge : 10 couleurs
  #   - noir  :  1 couleur
  paletteBleue <- colorRampPalette(c("#0000FF","#79F8F8"))(4)
  paletteJaune <- colorRampPalette(c("#3A3A00","#C5C500","#E6E600","#FFFF66"))(9)
  paletteVerte <- colorRampPalette(c("#11391E","#00FF00"))(10)
  paletteRouge <- colorRampPalette(c("#430000","#FE0000"))(10)
  paletteNoire <- "#000000"
  return(list(bleue=paletteBleue, jaune=paletteJaune, verte=paletteVerte, rouge=paletteRouge, noire=paletteNoire))
}
