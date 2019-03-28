#!/usr/bin/env Rscript
source("/home/spelletier/mnt/irhs001_projects/PROJETS_COLLABORATIFS/ANADIFF/PRODUCTION/AnaDiff_IRHS/Scripts/compil/colorFunctions.R")
# Liste des arguments (fichier et entêtes de colonnes)
cat("\n--- Lecture des arguments\n")
listArgs <- list("arguments" = commandArgs(trailingOnly=TRUE))

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
cat("--- Lecture du fichier\n")
tab <- read.table(listArgs$file2color, header=T, encoding="utf-8")
nbcol <- ncol(tab)
nbrow <- nrow(tab)
cat(paste("Nombre de lignes :", nbrow))
cat(paste("\nNombre de colonnes :", nbcol))
cat("\nEntêtes des colonnes :")
cat(paste("\n  ", colnames(tab)))
cat("\n")

# transformation du tableau
cat("\n--- Transformation du fichier\n")
typeOfColor <- function(listArgs, ent) {
  for (i in listArgs[3:length(listArgs)]) {
    if (length(grep(i, ent)==1)) {return(names(which(listArgs[3:length(listArgs)]==i)))}
  }
}

matColor <- matrix(nrow=nbrow+3, ncol=1)
matColor[1,1] <- "<table>"
matColor[2,1] <- "<tr></tr>"
matColor[3:nrow(matColor)-1,1] <- "<tr>"
matColor[nrow(matColor),1] <- "</table>"

temp <- matColor[3:nrow(matColor)-1,1]

for (i in seq(1:ncol(tab))){
  data <- list("ent"=colnames(tab)[i], val=list(tab[,i])[[1]])
  toc <- typeOfColor(listArgs, data$ent)
  print(toc)
  print(data$val)
  if (is.null(toc)) {
    temp <- paste(temp, "<tr></tr>", sep="")
  } else if (toc=="ratio") {
    val <- sapply(data$val, .transformRat)
    val <- sapply(val, .htmlTag)
    temp <- paste(temp, val, sep="")
  }
}
matColor[3:nrow(matColor)-1,1] <- temp

cat("\n--- Résultat matColor\n")
print(data$val)
print(sapply(data$val, .transformRat))
print(.htmlTag("#a0a0a0"))
for (row in matColor[-c(10:80),]) {
  print(row)
}
