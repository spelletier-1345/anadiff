###############################################
# AnaDiff Agilent
# Données de configuration
# Le 9 janvier 2019 - Sandra PELLETIER
###############################################

if (!exists("conf")) {conf  <- c()}

conf$version <- "AnaDiff_Agilent_v5.R" # TODO
conf$adresse <- .adresse(conf$dataTest)
conf$localOpt <- options() ; options(warn=-1)
conf$pck <- c("limma", "httr", "jsonlite")
conf$verifPackage <- .verifPackages(conf$pck)!=0
conf$folderArray <- .selectionDossier(dataTest = conf$dataTest)
