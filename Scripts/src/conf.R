#!/usr/bin/env Rscript
###############################################
# AnaDiff IRHS
# Le 20 février 2019 - Sandra PELLETIER
#
# Les données de configuration
#
###############################################

# Configuration
.conf <- function(dataTest){
  conf <- c()
  conf$localOpt <- options() ; options(warn=-1)
  conf$version <- "AnaDiff_Script_functions_v5.0.R"
  conf$pck <- c("limma", "httr", "jsonlite")
  conf$message <- "Selection of the working directory ... \n
  ------------ MESSAGE -------------
  Press \"Entree\" and select the file describing your experiences (arrays.txt)
  The \".txt \" raw data files must be in this same folder \n\n "
}
