###############################################
# AnaDiff Agilent
# Données d'un sens
# Le 30 janvier 2018 - Sandra PELLETIER
###############################################

sens <- c()

sens$sens <- c("sens", "antisens")

# for (s in sens$sens) {
se <- sens$sens[1]

sens$sensLabel <- ifelse(data$labelling=="indirect", ifelse(se=="sens", "antisens", "sens"), se)
.writeLineOut(paste("\n===>>>   Analyse des sondes", se), swap$fileOut)
sens$expName <- .nomExportAD(export$export, sens$sensLabel, data$dirName, swap$swap, adresse=conf$adresse)
if (se=="sens") {
  sens$probeList <- data.frame(V1=export$probeList[,1])
} else {
  sens$probeList <- data.frame(V1=export$probeList[,2])
}
cat("\nnormalisation des donnees par Lowess...\n")

if (sens$sensLabel=="sens") { # Séparation des sens puis normalisation des sens uniquement
  sens$tabResult <- .normalizeSense(export$RGtmp, sens$probeList, swap$fileOut, data$compare)
} else { # Normalisation de l'ensemble puis séparation des antisens
  sens$tabResult <- .normalizeAntiSense(export$RGtmp, sens$probeList, swap$fileOut, data$compare, export$probe)
}
