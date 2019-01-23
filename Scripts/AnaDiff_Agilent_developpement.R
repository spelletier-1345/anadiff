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

# source des données de configuration
# Vérifier le chemin des dataTest
source("./data_conf.R")

# source des données de travail
# Modification possibles : 
# - numéro de design
# - labelling
source("./data_test.R")

# Analyse des données

# En verssion standard
# source("./anaDiff_standard.R")

