###############################################
# AnaDiff Agilent
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.look <- function(ratio, stats, seuil, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("look")}
  # donne des informations stats : recherche du nombre de bonne stats et ratio associé à la moins bonne
  # Args:
  #   liste numerique des ratios
  #   liste numerique des stats
  # Returns:
  #   liste : ratio minimum et nombre de genes diff
  valmini  <- stats[stats<=seuil]                   # liste des valeurs de stats inferieures au seuil
  valmax   <- max(valmini)                          # valeur de stats exact immediatement inferieure au seuil
  valcible <- ratio[which(stats<=valmax)]           # liste des ratios dont stats est inferieures au seuil
  nombre   <- length(valcible)                      # nombre de sondes dont stats est inferieures au seuil
  valeur   <- min(abs(valcible))                    # valeur du plus petit ratio (en absolu) correspondant au seuil
  return(list(valeur_minimum=valeur,nombre_de_sondes=nombre))
}
