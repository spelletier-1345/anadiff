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
# - Affichage des alertes et enregistrement dans la base
#
###############################################

### Nettoyage de la mémoire
rm(list=ls(all.names = TRUE))
### Dossier de travail
setwd("/home/spelletier/Documents/Projets/anadiff/")
### Source des fonctions
listFiles <- list.files(path = "Scripts/src/", pattern = (".R$"))
for (files in listFiles) {source(paste("Scripts/src/", files, sep = ""))}
rm(files, listFiles)
ls(all.names = TRUE)

### conf dataTest
# dataTest <- FALSE ; anaDiff_Agilent("84550")
dataTest <- TRUE
designPuce <- "84550"

conf <- c()
conf$designPuce <- "84550"
conf$adresse <- "/home/spelletier/Documents/Projets/anadiff/Data/Agilent"
conf$verifPackage <- .verifPackages(conf$pck)!=0
# conf$dataTest <- dataTest
conf$dataFolder <- "/home/spelletier/Documents/Projets/anadiff/Data/anaDiff_Agilent/summary.txt"
conf$folderArray <- list(
  dataFolder <- "/home/spelletier/Documents/Projets/anadiff/Data/anaDiff_Agilent/",
  fileArray <- "summary.txt"
  )

anaDiff_Agilent("dataTest")
# print(conf)
# cat("\n") ; print("all is ok for designPuce")
# 
# AnaDiff_Agilent("dataTest")
# print(conf)
# cat("\n") ; print("all is ok for dataTest")
# 
# cat("\n")
# print("all is ok today")
# print("done")
