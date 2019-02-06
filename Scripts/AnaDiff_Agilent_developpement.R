###############################################
# Script de développement pour AnaDiff Agilent
# Le 9 janvier 2018 - Sandra PELLETIER
#
# - Chargement des fonctions
# - Chargement des données de configuration
# - Chargement des données de test
#
###############################################

rm(list=ls(all.names = TRUE))

# source des fonctions précédentes
# source("./Functions_Agilent_v4.1.R")

# source des fonctions (mises à jour)
source("./src/adresse.R")
source("./src/verifPackages.R")
source("./src/selectionDossier.R")
# Possibilité de modifier le fichier de designSpeificities.txt
# pendant le développement (dataTest=TRUE) : 
# Créer un nouveau fichier et le renseigner dans le source suivant
source("./src/designVersion.R")
source("./src/creationAnaDiff.R")
source("./src/writeLineOut.R")
source("./src/creationFileOut.R")
source("./src/numPuce.R")
source("./src/defineArrays.R")
source("./src/tabRG.R")
source("./src/defineCompare.R")
source("./src/RG4Graph.R")
source("./src/exportQCArray.R")
source("./src/plotRGMA.R")
source("./src/plotDensity.R")
source("./src/plotSpot.R")
source("./src/plotSpikes.R")
source("./src/tabSpikes.R")
source("./src/nomFichier.R")
source("./src/normIntensite.R")
source("./src/selectRGProbes.R")
source("./src/selectProbes.R")
source("./src/RGmean.R")
source("./src/nomExportAD.R")
source("./src/statAnaDiff.R")
source("./src/normalizeSens.R")
source("./src/normalizeAntiSens.R")
source("./src/statBH.R")
source("./src/kerfdr.R")
source("./src/calcBkg.R")
source("./src/statBkg.R")
source("./src/statWOBkg.R")
source("./src/mergeAnnot.R")
source("./src/bkgIntensite.R")
source("./src/probeRemoved.R")
source("./src/exportAnaDiff.R")
source("./src/color/tabColorHexa.R")
source("./src/color/gammeCouleurs.R")
source("./src/color/transformBh.R")
source("./src/color/transformInt.R")
source("./src/color/transformPval.R")
source("./src/color/transformRat.R")
source("./src/color/tabColorWithoutData.R")
source("./src/look.R")

# source des données de configuration
# Vérifier le chemin des dataTest
source("./array_conf.R")

# source des données de travail
# Modification possibles : 
# - numéro de design
# - labelling
source("./array_dataTest.R")

# Analyse des données

# swap
# for (swap in swaps) {
sw <- data$swaps[1]
source("./array_swap.R")
if (conf$graph) {source("./array_graph-GEO.R")}

# export
# for (export in exports) {
ex <- data$exports[1]
source("./array_export.R")

# sens
for (se in c("sens", "antisens")) {
  # se <- "sens"
  source("./array_sens.R")
  source("./array_stats.R")
  if (conf$db) {source("./array_json4db.R")}
  source("./array_json4db.R")
}


print("done")
