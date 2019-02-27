###############################################
# AnaDiff Agilent
# Données de configuration
# Le 20 février 2019 - Sandra PELLETIER
###############################################

# source des scripts pour Agilent
source("./array/array_sources.R") # TODO : makefile

AnaDiff_Agilent <- function(designPuce, labelling="direct", popBH="alternate", statBH="BH",
                             dataTest=conf$dataTest, bkgCalculation = TRUE, dec = ".") {
  if (designPuce=="dataTest") {
    cat("\n");cat(">>> dataTest.R\n")
    source("./array/array_dataTest.R")
  }

  # source des données de configuration
  # Vérifier le chemin des dataTest
  # TODO : fichier de configuration
  cat("\n");cat(">>> array_conf.R\n")
  source("./array/array_conf.R")
  if (!"data" %in% ls()) {data <- c()}
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
  cat("\n");cat(">>> array_data.R\n")
  source("./array/array_data.R", TRUE)
  
  # Analyse des données
  #####################
  
  # swap
  for (sw in data$swaps) {
    # sw <- data$swaps[1]
    cat("\n");cat(">>> array_swap.R\n")
    source("./array/array_swap.R", TRUE)
    if (conf$graph) {
      cat("\n");cat(">>> array_graph-GEO.R\n")
      source("./array/array_graph-GEO.R")
    }
    
    # export
    for (ex in data$exports) {
      # ex <- data$exports[1]
      cat("\n");cat(">>> array_export.R\n")
      source("./array/array_export.R", TRUE)
      
      # sens
      for (se in c("sens", "antisens")) {
        # se <- "sens"
        cat("\n");cat(">>> array_sens.R\n")
        source("./array/array_sens.R", TRUE)
        cat("\n");cat(">>> array_stats.R\n")
        source("./array/array_stats.R", TRUE)
        if (conf$db) {
          cat("\n");cat(">>> array_json4db.R\n")
          source("./array/array_json4db.R")
        }
        # cat("\n");cat(">>> array_alerte.R\n")
        # source("./array/array_alerte.R")
      }
      cat("\n");cat(">>> array_exceptions.R\n")
      source("./array/array_exceptions.R")
      cat("\n");cat(">>> array_tableDoubleSAS.R\n")
      source("./array/array_tableDoubleSAS.R", TRUE)
    }
    cat("\n");cat(">>> array_alerte-req.R\n")
    source("./array/array_alerte-req.R", TRUE)
  }
  cat("\n===================================\n")
  cat("====== Analyses terminées =========")
  cat("\n===================================\n")
  cat("\nRécapitulation des alertes :\n")
  .alertes(data$alertes, "noOut")
  
  rm(annot, color_antisens, color_sens, designPuce, tab_antisens, tab_sens, envir=globalenv())
  options(conf$localOpt)
  
  if (designPuce=="dataTest") {
    assign("swap", swap, envir = globalenv())
  }
}
