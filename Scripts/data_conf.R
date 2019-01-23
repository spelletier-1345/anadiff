###############################################
# AnaDiff Agilent
# Données de configuration
# Le 9 janvier 2018 - Sandra PELLETIER
###############################################

conf  <- c()
conf0 <- c()

conf$dataTest <- "/home/spelletier/Documents/Projets/agilent-irhs/Data/"

conf$version <- "AnaDiff_Script_functions_v4.1.R" # TODO
conf$adresse <- .adresse(conf$dataTest)
conf0$localOpt <- options() ; options(warn=-1)
conf$pck <- c("limma", "httr", "jsonlite")
conf$verifPackage <- .verifPackages(conf$pck)!=0
conf$folderArray <- .selectionDossier()

