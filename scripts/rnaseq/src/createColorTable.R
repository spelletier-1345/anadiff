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

source("./src/gammeRatio.R")
source("./src/gammeInt.R")
source("./src/gammePval.R")
source("./src/gammeBH.R")

.createColorTable <- function(tab, pathResults, comparison, fileHTML) {
  # Mise en forme des donnees
  tabColor  <- tab
  ratio     <- which(colnames(tab)=="logFC")
  tabColor[,1]   <- paste("<tr><td></td>",sep="")
  for (colonne in seq(2,ratio-3)) {tabColor[,colonne] <- paste("<td></td>",sep="")}
  if (colnames(tab)[ratio+1]=="pval_DESeq2") {
    tab[,ratio-2][is.na(tab[,ratio-2])] <- 0
    tab[,ratio-1][is.na(tab[,ratio-1])] <- 0
    tab[,ratio][is.na(tab[,ratio])] <- 0
    tab[,ratio+1][is.na(tab[,ratio+1])] <- 1
    tab[,ratio+2][is.na(tab[,ratio+2])] <- 1
  }
  tabColor[,ratio]   <- paste("<td align=\"center\" bgcolor=\"#",sapply(as.numeric(tab[,ratio]),.gammeRatio),"\"></td>",sep="")
  tabColor[,ratio-1] <- paste("<td align=\"center\" bgcolor=\"#",sapply(as.numeric(tab[,ratio-1]),.gammeInt),"\"></td>",sep="")
  tabColor[,ratio-2] <- paste("<td align=\"center\" bgcolor=\"#",sapply(as.numeric(tab[,ratio-2]),.gammeInt),"\"></td>",sep="")
  tabColor[,ratio+1] <- paste("<td align=\"center\" bgcolor=\"#",sapply(as.numeric(tab[,ratio+1]),.gammePval),"\"></td>",sep="")
  tabColor[,ratio+2] <- paste("<td align=\"center\" bgcolor=\"#",sapply(as.numeric(tab[,ratio+2]),.gammeBH),"\"></td></tr>",sep="")
  tabColor$Color <- do.call(paste,tabColor)
  Color <- matrix(nrow=nrow(tab)+3, ncol=1)
  Color[1,1] <- "<table>"
  Color[2,1] <- paste("<tr><td></td></tr>",sep="")
  Color[3:(nrow(tab)+2),1] <- matrix(tabColor$Color,ncol=1)
  Color[(nrow(tab)+3),1]    <- "</table>"
  write.table(Color,paste(pathResults,comparison,fileHTML,sep=""),row.names=F,col.names=F,quote=F)
  return(NULL)
}
