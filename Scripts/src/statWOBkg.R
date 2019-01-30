###############################################
# AnaDiff Agilent
# Le 30 janvier 2018 - Sandra PELLETIER
###############################################

.statWoBkg <- function(tab, targets, dataTest=conf$dataTest) {
  # calcul des intensite par echantillon et soustraction du bruit de fond
  # tri le tableau par probe_id et l'enregistre
  # Args:
  #   tab : tableau de donnees avec probe_id, Amean et ratio
  #   nbg : nombre de sonde pour le calcul
  #   dirName : dossier d'export pour .writeLineOut
  #   targets : sous-tableau de array correspondant au swap, provenant de RG$targets
  # Returns:
  #   tab : tableau de donnees avec intensite soustraite du bruit de fond par echantillon
  if (!is.null(dataTest)) {print("statWoBkg")}
  cat("calcul de l'intensite de l'echantillon controle...\n")
  IGreen    <- round(tab$Amean - tab$ratio/2, 2)
  cat("calcul de l'intensite de l'echantillon traitement\n")
  IRed   <- round(tab$Amean + tab$ratio/2, 2)
  tab <- data.frame(tab[,1],IGreen,IRed,round(tab[,3:4],4))
  names(tab)[1:3] <- c("probe_id", targets$CtrName[1], targets$TtmtName[1])
  return(tab)
}
