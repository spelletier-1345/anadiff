###############################################
# AnaDiff Agilent
# Le 30 janvier 2018 - Sandra PELLETIER
###############################################

# Normalisation de l'ensemble puis séparation des antisens
.normalizeAntiSense <- function (RGtmp, probeList, fileOut, compare, probe, dataTest = conf$dataTest) {
  if (!is.null(dataTest)) {print("normalizeAntiSense")}
  MA <- normalizeWithinArrays(RGtmp,method = "loess",bc.method = "none")
  res <- .statAnaDiff(MA, fileOut, compare)
  tabResult <- res$tabFit
  tabResult <- .selectProbes(tabResult, probe = probe)
  tabResult <- tabResult[tabResult$Agilent_id %in% probeList$V1,]
  return(list(tabResult = tabResult, MA = MA, var = res$variance))
}