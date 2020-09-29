###############################################
# AnaDiff Agilent
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.bkgIntensite <- function(norm_intensities, nbg, expName, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("bkgIntensite")}
  # soustraie le bruit de fond pour chaque donnee d'intensite normalisee et les exporte
  # Args:
  #   norm_intensite : le tableau d'intensites normalisee
  #   nbg : nombre de sonde pour le calcul
  #   expName : le nom d'export du swap et du sens
  # Returns:
  #   rien
  cat("soustraction du bruit de fond...\n")
  ssbkg <- function(intensite, nbg, dataTest=conf$dataTest) {
    if (!is.null(dataTest)) {print("ssbkg")}
    bkg_intensities <- intensite - .calcBkg(intensite, nbg)$background
    bkg_intensities[which(bkg_intensities<0)] <- 0
    return(bkg_intensities)
  }
  bkg_intensities      <- norm_intensities
  bkg_intensities[,-1] <- mapply(ssbkg, norm_intensities[,-1], nbg)
  bkg_intensities <- bkg_intensities[order(bkg_intensities$probe_id), ]
  write.table(bkg_intensities, expName, quote=F, sep="\t", row.names=F, dec=".")
  return(NULL)
}
