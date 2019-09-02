.edgeRTable <- function(countTable, re, group) {
  re.tab    <- re$table
  re.tab$id <- rownames(re.tab)
  re.tab    <- merge(countTable,re.tab,by="row.names")
  ratio     <- which(colnames(re.tab)=="logFC")
  nbcol     <- length(colnames(re.tab))
  re.tab$logCPM_Ctrl <- re.tab$logCPM - re.tab$logFC/2
  re.tab$logCPM_Ttmt <- re.tab$logCPM + re.tab$logFC/2
  re.tab[,c((ratio):(ratio+3),(ratio+5):(ratio+6))] <- round(re.tab[,c((ratio):(ratio+3),(ratio+5):(ratio+6))],5)
  re.tab    <- re.tab[,c(nbcol,2:(ratio-1),(nbcol+1),(nbcol+2),ratio,(nbcol-2),(nbcol-1))]
  colnames(re.tab)[ratio]   <- paste(colnames(re.tab[ratio]),group[1],sep="_")
  colnames(re.tab)[ratio+1] <- paste(colnames(re.tab[ratio+1]),group[length(group)],sep="_")
  colnames(re.tab)[(nbcol-1):nbcol] <- paste(colnames(re.tab[(nbcol-1):nbcol]),"edgeR",sep="_")
  re.tab    <- re.tab[order(re.tab$id),]
  return(re.tab)
}