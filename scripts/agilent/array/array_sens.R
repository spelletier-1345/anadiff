###############################################
# AnaDiff Agilent
# Données d'un sens
# Le 30 janvier 2019 - Sandra PELLETIER
###############################################

sens <- c()

sens$sensLabel <- ifelse(data$labelling=="indirect", ifelse(se=="sens", "antisens", "sens"), se)
.writeLineOut(paste("\n===>>>   Analyse des sondes", se), swap$fileOut)
sens$expName <- .nomExportAD(ex, sens$sensLabel, swap$dirName, sw, adresse=conf$adresse)
if (se=="sens") {
  sens$probeList <- data.frame(V1=export$probeList[,1])
} else {
  sens$probeList <- data.frame(V1=export$probeList[,2])
}
cat("\nnormalisation des donnees par Lowess...\n")

if (sens$sensLabel=="sens") { # Séparation des sens puis normalisation des sens uniquement
  sens$normalize <- .normalizeSense(export$RGtmp, sens$probeList, swap$fileOut, data$compare)
} else { # Normalisation de l'ensemble puis séparation des antisens
  sens$normalize <- .normalizeAntiSense(export$RGtmp, sens$probeList, swap$fileOut, data$compare, export$probe)
}
sens$normalize$tabResult$probe_id <- sens$normalize$tabResult$Agilent_id
sens$tabResultComplet <- sens$normalize$tabResult
