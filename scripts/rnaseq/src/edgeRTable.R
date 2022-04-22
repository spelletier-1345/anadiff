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

.edgeRTable <- function(countTable, re, group) {
  re.tab    <- re$table
  re.tab$id <- rownames(re.tab)
  re.tab    <- merge(countTable,re.tab,by = "row.names")
  ratio     <- which(colnames(re.tab) == "logFC")
  nbcol     <- length(colnames(re.tab))
  re.tab$logCPM_Ctrl <- re.tab$logCPM - re.tab$logFC/2
  re.tab$logCPM_Ttmt <- re.tab$logCPM + re.tab$logFC/2
  re.tab    <- re.tab[,c(nbcol,2:(ratio-1),(nbcol+2),(nbcol+1),ratio,(nbcol-2),(nbcol-1))]
  colnames(re.tab)[ratio]   <- paste(colnames(re.tab[ratio]),group[length(group)],sep="_")
  colnames(re.tab)[ratio+1] <- paste(colnames(re.tab[ratio+1]),group[1],sep="_")
  colnames(re.tab)[(nbcol-1):nbcol] <- paste(colnames(re.tab[(nbcol-1):nbcol]),"edgeR",sep="_")
  re.tab    <- re.tab[order(re.tab$id),]
  return(re.tab)
}
