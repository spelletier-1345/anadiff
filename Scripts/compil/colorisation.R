#!/usr/bin/env Rscript

# Liste des arguments (fichier et entêtes de colonnes)
cat("\n--- Lecture des arguments\n")
listArgs <- list(commandArgs(trailingOnly=TRUE))[]
print(listArgs)

cat("\n--- Arguments\n")

command <- ""
arguments <- function(command, argument, listArgs){
  if (command=="f") {
    listArgs$file2color <- argument
  } else if (command=="r") {
    listArgs$ratio <- argument
  } else if (command=="p") {
    listArgs$pval <- argument
  } else if (command=="b") {
    listArgs$bh <- argument
  } else if (command=="s") {
    listArgs$seb <- argument
  }
  return(listArgs)
}

for (argument in listArgs[[1]]) {
  listArgs <- arguments(command, argument, listArgs)
  if (substr(argument,1,1)=="-") {
    command <- substr(argument,2,2)
  } else {
    command <- ""
  }
}
print(listArgs)

# Lecture du fichier
cat("\n--- Lecture du fichier\n")
tab <- read.table(listArgs$file2color, header=T, encoding="utf-8")
print(nrow(tab))
print(colnames(tab))
