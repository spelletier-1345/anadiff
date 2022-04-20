###############################################
# AnaDiff Agilent
# Le 30 janvier 2019 - Sandra PELLETIER
###############################################

# Séparation des sens puis normalisation des sens uniquement
.normalizeSense <- function (RGtmp, probeList, fileOut, compare, dataTest = conf$dataTest) {
  if (!is.null(dataTest)) {print("normalizeSense")}
  RGsens <- RGtmp[RGtmp$genes$ProbeName %in% probeList$V1,]
  MA <- normalizeWithinArrays(RGsens,method = "loess",bc.method = "none")
  res <- .statAnaDiff(MA, fileOut, compare)
  tabResult <- res$tabFit
  return(list(tabResult = tabResult, MA = MA, var = res$variance))
}
