source("/home/spelletier/dev/RNAseq/Scripts/AnaDiffRNAseq/src/gammeRatio.R")
source("/home/spelletier/dev/RNAseq/Scripts/AnaDiffRNAseq/src/gammeInt.R")
source("/home/spelletier/dev/RNAseq/Scripts/AnaDiffRNAseq/src/gammePval.R")
source("/home/spelletier/dev/RNAseq/Scripts/AnaDiffRNAseq/src/gammeBH.R")

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