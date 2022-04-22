#!/usr/bin/env Rscript

# Copyright Sandra PELLETIER ([2022-04-20])
# 
# [sandra.pelletier@inrae.fr]
# 
# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software.  You can  use, 
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info". 
# 
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.

#!/usr/bin/env Rscript
###############################################
# AnaDiff IRHS
# Le 20 février 2019 - Sandra PELLETIER
#
# Les données de configuration
#
###############################################

# Configuration
.conf <- function(conf){
  if (is.null(conf)) {conf <- c()}
  conf$localOpt <- options() ; options(warn=-1)
  conf$version <- "AnaDiff_Script_functions_v5.0.R"
  conf$pck <- c("limma", "httr", "jsonlite")
  conf$message <- "Selection of the working directory ... \n
  ------------ MESSAGE -------------
  Press \"Entree\" and select the file describing your experiences (arrays.txt)
  The \".txt \" raw data files must be in this same folder \n\n "
  return(conf)
}
