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
source("./Functions_Agilent_v4.1.R")

# source des fonctions modifiées
source("./src/adresse.R")
source("./src/verifPackages.R")
source("./src/selectionDossier.R")

# source des données de configuration
source("./data_conf.R")

# source des données de travail