###############################################
# AnaDiff IRHS
# Le 9 janvier 2019 - Sandra PELLETIER
#
# Chargement des scripts pour analyses Agilent
# - Chargement des fonctions
# - Chargement des données de configuration
#
###############################################

# source des fonctions (mises à jour)
source("./src/adresse.R")
source("./src/verifPackages.R")
source("./src/selectionDossier.R")
# Possibilité de modifier le fichier de designSpecificities.txt
#   pendant le développement (dataTest=TRUE) :
# Pour cela, créer un nouveau fichier et le renseigner dans le source suivant
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
source("./src/tabColorHexa.R")
source("./src/gammeCouleurs.R")
source("./src/transformBh.R")
source("./src/transformInt.R")
source("./src/transformPval.R")
source("./src/transformRat.R")
source("./src/tabColorWithoutData.R")
source("./src/look.R")
source("./src/tabSAS.R")
source("./src/alertes.R")
