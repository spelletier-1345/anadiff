###############################################
# AnaDiff Agilent
# Le 30 janvier 2018 - Sandra PELLETIER
###############################################

.calcBkg <- function(intensite, nbg, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("calcBkg")}
  # calcul du bruit de fond en fonction de nbg
  # Args:
  #   intensite : liste des intensitees
  #   nbg : nombre de sonde pour le calcul
  # Returns:
  #   bkg : liste d'info : moyenne, ecart-type, bruit de fond calcule
  Abg   <- intensite[intensite<=sort(intensite)[nbg]]      # Abg = Tri des nbg plus petites valeurs
  mAbg  <- mean(Abg,na.rm=T)       # mAbg est la moyenne de ces nbg plus petites valeurs
  sdAbg <- sd(Abg,na.rm=T)         # sdAbg est l'ecart type de ces nbg plus petites valeurs
  bg    <- mAbg+2*sdAbg            # Calcul du bruit de fond
  return(bkg=list(moyenne=mAbg,ecarttype=sdAbg,background=bg))   # sortie : liste des elements moyenne, sd et bg
}
