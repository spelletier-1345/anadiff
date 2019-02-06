###############################################
# AnaDiff Agilent
# Le 23 janvier 2019 - Sandra PELLETIER
###############################################

.selectRGProbes <- function(RGlist, probe, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("selectRGProbes")}
  RGgenes <- data.frame(RGlist$genes, probeRow = rownames(RGlist$genes))
  RGgenes$Agilent_id <- RGgenes$ProbeName
  RGprobe <- .selectProbes(tab = RGgenes, probe)
  RGtmp <- RGlist[RGlist$genes$ProbeName %in% RGprobe$ProbeName,]
  return(RGtmp)
}
