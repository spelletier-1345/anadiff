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

# .readArgs(listArgs, help, args)
#
# Liste des arguments pour une fonction données
# Vérification du nombre d'arguments, et création d'une liste
# Si pas d'argument ou si appel de l'aide : affichage de l'aide
# 
# listArgs : liste des arguments en entrée
# help : source de l'aide pour la fonction appelée
# args : source pour les arguments attendus par la fonction appelée
# return : la liste des arguments mis en forme si pas d'appel d'aide

.readArgs <- function(listArgs, help, args) {
  # Vérification non nul ou aide
  if (length(listArgs[[1]])==0 || 
      (length(listArgs[[1]])==1 && listArgs[[1]] %in% c("", "-h", "--help"))) {
    source(help)
  } else { # Si tout est ok, alors :
    # Lecture des arguments
    cat("\n--- Lecture des arguments\n")
    source(args)
    # Boucle sur les arguments : l'argument est
    #   soit la valeur de la commande précédente
    #   soit la commande pour la valeur suivante
    # La première commande étant nulle
    command <- ""
    for (argument in listArgs[[1]]) {
      listArgs <- .arguments(command, argument, listArgs)
      if (substr(argument,1,1)=="-") {
        command <- substr(argument,2,2)
      } else {
        command <- ""
      }
    }
    return(listArgs)
  }
}
