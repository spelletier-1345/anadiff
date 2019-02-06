###############################################
# AnaDiff Agilent
# Le 30 janvier 2019 - Sandra PELLETIER
###############################################

.mergeAnnot <- function(tabResult, annot, sens, dataTest=conf$dataTest) {
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  # Returns:
  if (!is.null(dataTest)) {print("mergeAnnot")}
  tabAnnot     <- read.csv(file = annot, sep="\t", header=T, encoding="utf-8", check.names = F, as.is = T)
  if (sens=="antisens") {colnames(tabAnnot)[1:2] <- c("probe_rev", "probe_id")}
  tabFinal     <- merge(tabResult,tabAnnot,by="probe_id",all.y=T)
  colnames(tabAnnot)[1:2] <- c("probe.x", "probe.y")
  assign("annot", tabAnnot[,1:2], envir=globalenv())
  return(tabFinal)
}
