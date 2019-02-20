###############################################
# AnaDiff Agilent
# Données de configuration
# Le 20 février 2019 - Sandra PELLETIER
###############################################

# source des scripts pour Agilent
source("./arrays_sources.R") # TODO : makefile

AnaDiff_Agilent <- function(designPuce, labelling="direct", popBH="alternate", statBH="BH",
                             dataTest=conf$dataTest, bkgCalculation = TRUE, dec = ".") {
  if (designPuce=="dataTest") {
    print(">>> dataTest.R")
    source("./dataTest.R")
  }
  
  # source des données de configuration
  # Vérifier le chemin des dataTest
  # TODO : fichier de configuration
  source("./array_conf.R")
  print(">>> array_conf.R")
  if (!designPuce=="dataTest") {
    data$designPuce <- designPuce
    data$labelling <- labelling
    data$popBH <- popBH
    data$statBH <- statBH
    data$bkgCalculation <- bkgCalculation
    data$dec <- dec
  }
  
  # source des données de travail
  # Modification possibles :
  # - numéro de design
  # - labelling
  print(">>> array_data.R")
  source("./array_data.R")
  
  # Analyse des données
  #####################
  
  # swap
  for (sw in data$swaps) {
    # sw <- data$swaps[1]
    print(">>> array_swap.R")
    source("./array_swap.R")
    if (conf$graph) {
      print(">>> array_graph-GEO.R")
      source("./array_graph-GEO.R")
    }
    
    # export
    for (ex in data$exports) {
      # ex <- data$exports[1]
      print(">>> array_export.R")
      source("./array_export.R")
      
      # sens
      for (se in c("sens", "antisens")) {
        # se <- "sens"
        print(">>> array_sens.R")
        source("./array_sens.R")
        print(">>> array_stats.R")
        source("./array_stats.R")
        if (conf$db) {
          print(">>> array_json4db.R")
          source("./array_json4db.R")
        }
        # print(">>> array_alerte.R")
        # source("./array_alerte.R")
      }
      print(">>> array_exceptions.R")
      source("./array_exceptions.R")
      print(">>> array_tableDoubleSAS.R")
      source("./array_tableDoubleSAS.R")
    }
    print(">>> array_alerte-req.R")
    source("./array_alerte-req.R")
  }
}
