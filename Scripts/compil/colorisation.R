#!/usr/bin/env Rscript
# @ags-1345-209:PRODUCTION$ ./AnaDiff_IRHS/Scripts/compil/colorisation.R -f ./DataTest/RNAseq_seb/compil.txt -m BH_* -p pval_* -r *_logFC
source("./Scripts/src/conf.R")
source("./Scripts/compil/colorFunctions.R")

# Liste des arguments (fichier et entêtes de colonnes)
listArgs <- list("arguments" = commandArgs(trailingOnly=TRUE))
# Vérification non nul ou aide
if (length(listArgs)==1) {
  if (length(listArgs[[1]])==0) {
    source("./Scripts/help/colorisation_help.R")
    quit()
  } else if (listArgs[[1]] %in% c("", "-h", "--help")) {
    source("./Scripts/help/colorisation_help.R")
    quit()
  }
}

cat("\n--- Lecture des arguments\n")
command <- ""
arguments <- function(command, argument, listArgs){
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
  } else if (command=="m") {
    listArgs$minimize <- argument
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
tab <- read.table(listArgs$file2color, header=T, sep="\t", encoding="utf-8")
nbcol <- ncol(tab)
nbrow <- nrow(tab)
cat(paste("Nombre de lignes :", nbrow))
cat(paste("\nNombre de colonnes :", nbcol))
cat("\nEntêtes des colonnes :")
cat(paste("\n  ", colnames(tab)))
cat("\n")

# transformation du tableau
typeOfColor <- function(listArgs, ent) {
  for (i in listArgs[3:length(listArgs)]) {
    if (length(grep(i, ent)==1)) {return(names(which(listArgs[3:length(listArgs)]==i)))}
  }
}

matColor <- matrix(nrow=nbrow+3, ncol=1)
matColor[1,1] <- "<table>"
matColor[2,1] <- "<tr></tr>"
matColor[3:(nrow(matColor)-1),1] <- "<tr>"
matColor[nrow(matColor),1] <- "</table>"
temp <- matColor[3:(nrow(matColor)-1),1]

for (i in seq(1:ncol(tab))){
  data <- list("ent"=colnames(tab)[i], val=list(tab[,i])[[1]])
  toc <- typeOfColor(listArgs, data$ent)
  if (is.null(toc)) {
    temp <- paste(temp, "<td></td>", sep="")
  } else if (toc=="ratio") {
    val <- sapply(data$val, .transformRat)
    val <- sapply(val, .htmlTag)
    temp <- paste(temp, val, sep="")
  } else if (toc=="pval") {
    val <- sapply(data$val, .transformPval)
    val <- sapply(val, .htmlTag)
    temp <- paste(temp, val, sep="")
  } else if (toc=="bh") {
    val <- sapply(data$val, .transformBh)
    val <- sapply(val, .htmlTag)
    temp <- paste(temp, val, sep="")
  } else if (toc=="intensity") {
    val <- sapply(data$val, .transformInt)
    val <- sapply(val, .htmlTag)
    temp <- paste(temp, val, sep="")
  } else if (toc=="minimize") {
    val <- "<td align=\"center\" sdnum=\"4108;0;0.00\"></td>"
    temp <- paste(temp, val, sep="")
  }
}
temp <- paste(temp, "</tr>", sep="")
matColor[3:(nrow(matColor)-1),1] <- temp

fileName <- paste(tools::file_path_sans_ext(listArgs$file2color), ".html", sep="")
write.table(matColor, fileName, row.names=F, col.names=F, quote=F)
