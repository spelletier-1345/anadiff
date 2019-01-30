###############################################
# AnaDiff Agilent
# Données d'un sens
# Le 30 janvier 2018 - Sandra PELLETIER
###############################################

sens <- c()

sens$sens <- c("sens", "antisens")

# for (s in sens$sens) {
s <- sens$sens[1]

sens$sensLabel <- ifelse(data$labelling=="indirect", ifelse(s=="sens", "antisens", "sens"), s)
sens$expName <- .nomExportAD(export$export, sens$sensLabel, data$dirName, swap$swap, adresse=conf$adresse)
if (s=="sens") {
  sens$probeList <- data.frame(V1=export$probeList[,1])
} else {
  sens$probeList <- data.frame(V1=export$probeList[,2])
}
cat("\nnormalisation des donnees par Lowess...")

