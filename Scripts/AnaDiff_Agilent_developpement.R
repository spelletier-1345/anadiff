###############################################
# Script de développement pour AnaDiff IRHS
# Le 9 janvier 2019 - Sandra PELLETIER
#
# - Chargement des fonctions
# - Chargement des données de configuration
# - Chargement des données de test
# - Analyse par swap
# - Analyse par génome (export)
# - Analyse par sens de sonde (sens/antisens)
# - Gestion des exceptions
#
###############################################

rm(list=ls(all.names = TRUE))
designPuce <- "dataTest" # TODO : effacer

source("./array_anaDiff.R")
AnaDiff_Agilent("dataTest")

print("ok 20/02/2019")
print("done")
