###############################################
# AnaDiff IRHS
# Le 20 février 2019 - Sandra PELLETIER
#
# Mes données de test
#
###############################################

# Dossier de travail
conf <- c()
conf$pck <- c("limma", "httr", "jsonlite")
conf$folderArray[[1]] <- "/home/spelletier/Documents/Projets/anadiff/Data/"
conf$folderArray[[2]] <- "summary.txt"
conf$dataTest <- paste(conf$folderArray, collapse="")
# conf$dataTest <- paste(conf$folderArray, collapse="")
conf$graph <- FALSE
conf$db <- FALSE

# Données du design pour AnaDiff_Agilent()
data <- c()
data$designPuce <- "84550" # Carrot v1
data$labelling <- "direct"
data$popBH <- "alternate"
data$statBH <- "BH"
data$bkgCalculation <- TRUE
data$dec <- "."

adresse = "/home/spelletier/Documents/Projets/anadiff/Data/"
# adresse = "http://147.99.112.52/rscripts/Tools/Agilent/"
