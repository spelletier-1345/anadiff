###############################################
# AnaDiff Agilent
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

# Séparation des sens puis normalisation des sens uniquement
.normalizeSense <- function (RGtmp, probeList, swap, export, fileOut, compare, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("normalizeSense")}
  RGsens <- RGtmp[RGtmp$genes$ProbeName %in% probeList$V1,]
  MA <- normalizeWithinArrays(RGsens,method="loess",bc.method="none")
  res <- .statAnaDiff(MA, swap, export, fileOut, compare)
  tabResult <- res$tabFit
  return(tabResult)
}
