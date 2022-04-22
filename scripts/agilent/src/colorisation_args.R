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

# ###############################################
# AnaDiff 
# Le 17 avril 2019 - Sandra PELLETIER
###############################################

# .arguments(command="", argument, listArgs)
#
# Mise en forme des arguments pour la fonction colorisation.R
# Est appelée par la fonction .readArgs
# S'utilise dans une boucle où le résultat est dépendant de la sortie précédente.
# 
# command : option choisie
# argument : valeur de l'option
# listArgs : liste d'arguments enrichie à chaque boucle
# return : la liste des arguments mis en forme

.arguments <- function(command, argument, listArgs) {
  if (command=="f") {
    listArgs$file2color <- argument
  } else if (command=="i") {
    listArgs$intensity <- argument
  } else if (command=="r") {
    listArgs$ratio <- argument
  } else if (command=="p") {
    listArgs$pval <- argument
  } else if (command=="b") {
    listArgs$bh <- argument
  }
  return(listArgs)
}
