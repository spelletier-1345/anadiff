#!/usr/bin/env Rscript
# @ags-1345-209:PRODUCTION$ ./AnaDiff_IRHS/Scripts/compil/colorisation.R -f ./DataTest/RNAseq_seb/compil.txt -m BH_* -p pval_* -r *_logFC
# setwd("~/Documents/AnaDiff_IRHS") # Mac

source("./Scripts/src/conf.R")
source("./Scripts/compil/colorFunctions.R")
source("./Scripts/src/colorisation_args.R")
source("./Scripts/src/readArgs.R")

# Liste des arguments (fichier et entêtes de colonnes)
listArgs <- list("arguments" = commandArgs(trailingOnly=TRUE))
listArgs <- list("arguments" = c(
  "-f", "../Compil/RNAseq/result_anaDiff_compil.txt",
  "-r", "*_FC",
  "-p", "*_pval",
  "-b", "*_BH"))

listArgs <- .readArgs(listArgs,
                      "./Scripts/help/colorisation_help.R",
                      "./Scripts/src/colorisation_args.R")
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

.matColor <- matrix(nrow=nbrow+3, ncol=1)
.matColor[1,1] <- "<table>"
.matColor[2,1] <- "<tr></tr>"
.matColor[3:(nrow(.matColor)-1),1] <- "<tr>"
.matColor[nrow(.matColor),1] <- "</table>"
temp <- .matColor[3:(nrow(.matColor)-1),1]

for (i in seq(1:ncol(tab))){
  data <- list("ent"=colnames(tab)[i], val=list(tab[,i])[[1]])
  toc <- .typeOfColor(listArgs, data$ent)
  if (is.null(toc)) {
    temp <- paste(temp, "<td></td>", sep="")
  } else {
    if (toc=="ratio") {
      val <- sapply(data$val, .transformRat)
    } else if (toc=="pval") {
      val <- sapply(data$val, .transformPval)
    } else if (toc=="bh") {
      val <- sapply(data$val, .transformBh)
    } else if (toc=="intensity") {
      val <- sapply(data$val, .transformInt)
    }
    val <- sapply(val, .htmlTag)
    temp <- paste(temp, val, sep="")
  }
}
.matColor[3:(nrow(.matColor)-1),1] <- paste(temp, "</tr>", sep="")
rm(i, toc, val, temp, nbcol, nbrow, data, tab)

outFileName <- paste(tools::file_path_sans_ext(listArgs$file2color), ".html", sep="")
write.table(.matColor, outFileName, row.names=F, col.names=F, quote=F)
