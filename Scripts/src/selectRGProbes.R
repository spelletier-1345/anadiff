###############################################
# AnaDiff Agilent
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

.selectRGProbes <- function(RGlist, probe) {
  RGgenes <- data.frame(RGlist$genes, probeRow = rownames(RGlist$genes))
  RGgenes$Agilent_id <- RGgenes$ProbeName
  RGprobe <- .selectProbes(tab = RGgenes, probe)
  RGtmp <- RGlist[RGlist$genes$ProbeName %in% RGprobe$ProbeName,]
  return(RGtmp)
}
