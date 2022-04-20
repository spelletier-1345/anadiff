source("./src/writeLineOut.R")
source("./src/createColorTable.R")

.functionDESeq2 <- function(myCompare, countTable, comparison, fileOut, pathResults) {
  rownames(myCompare) <- myCompare$File
  dds <- DESeqDataSetFromMatrix(countTable,colData = myCompare, design = ~ Group)
  dds <- DESeq(dds)
  res <- results(dds, contrast=c("Group","Ttmt","Control"))

  tabres <- as.data.frame.array(res[,1:2])
  tabres$significance <- as.logical(lapply(res$padj, function(x) ifelse(is.na(x),1,x)<0.05))

  pngName <- paste(pathResults, comparison, "_DESeq2.png", sep="")
  graphics.off()
  png(pngName, width=1086, height=1086, res=120)
  plotMA(tabres, main=paste("DESeq2\n", comparison), ylim=c(-4,4))
  dev.off()

  # Recuperation des donnees
  res$baseMean <- log2(res$baseMean)
  deseqTab    <- as.data.frame.array(res[,c(1,2,5,6)])
  deseqTab$logCPM_Ctrl <- deseqTab$baseMean - deseqTab$log2FoldChange/2
  deseqTab$logCPM_Ttmt <- deseqTab$baseMean + deseqTab$log2FoldChange/2
  deseqTab <- deseqTab[,c(5,6,2,3,4)]
  Ctrl <- as.character(unique(myCompare[which(myCompare$Group=="Control"),"Name"]))
  Ttmt <- as.character(unique(myCompare[which(myCompare$Group=="Ttmt"),"Name"]))
  colnames(deseqTab)[1] <- paste("logCPM_Ctrl_",Ctrl,sep="")
  colnames(deseqTab)[2] <- paste("logCPM_Ttmt_",Ttmt,sep="")
  colnames(deseqTab)[3:5] <- c("logFC","pval_DESeq2","BH_DESeq2")
  deseqTab    <- merge(countTable,deseqTab,by="row.names")
  colnames(deseqTab)[1] <- "id"
  write.table(deseqTab, paste(pathResults, comparison, "_DESeq2.txt", sep=""),
              sep="\t",row.names=F,quote=F, dec=".")

  # Resume
  pval     <- sum(deseqTab$pval_DESeq2 < 0.05, na.rm=TRUE )
  BH       <- sum(deseqTab$BH_DESeq2 < 0.1, na.rm=TRUE )
  downpval <- nrow(deseqTab[which(deseqTab$logFC<0 & deseqTab$pval_DESeq2<(0.05)),])
  uppval   <- nrow(deseqTab[which(deseqTab$logFC>0 & deseqTab$pval_DESeq2<(0.05)),])
  downBH   <- nrow(deseqTab[which(deseqTab$logFC<0 & deseqTab$BH_DESeq2<(0.1)),])
  upBH     <- nrow(deseqTab[which(deseqTab$logFC>0 & deseqTab$BH_DESeq2<(0.1)),])
  noDEpval <- nrow(deseqTab) - (downpval+uppval)
  noDEBH   <- nrow(deseqTab) - (downBH+upBH)
  .writeLineOut(paste("### DESeq2 analysis summary ###\n"), fileOut)
  .writeLineOut(paste("number of genes with pval less than 5% :",pval), fileOut)
  .writeLineOut(paste("genes down with pval 5%                :",downpval), fileOut)
  .writeLineOut(paste("genes up with pval 5%                  :",uppval), fileOut)
  .writeLineOut(paste("number of genes no diff with pval      :",noDEpval,"\n"), fileOut)
  .writeLineOut(paste("number of genes with BH less than 10%  :",BH), fileOut)
  .writeLineOut(paste("genes down with BH 10%                 :",downBH), fileOut)
  .writeLineOut(paste("genes up with BH 10%                   :",upBH), fileOut)
  .writeLineOut(paste("number of genes no diff with BH        :",noDEBH,"\n"), fileOut)

  cat(paste("\nDESeq2 analysis summary\n\n"))
  cat(paste("number of genes with pval less than 5% :",pval,"\n"))
  cat(paste("genes down with pval 5%                :",downpval,"\n"))
  cat(paste("genes up with pval 5%                  :",uppval,"\n"))
  cat(paste("number of genes no diff with pval      :",noDEpval,"\n\n"))
  cat(paste("number of genes with BH less than 10%  :",BH,"\n"))
  cat(paste("genes down with BH 10%                 :",downBH,"\n"))
  cat(paste("genes up with BH 10%                   :",upBH,"\n"))
  cat(paste("number of genes no diff with BH        :",noDEBH,"\n\n"))

  # Mise en forme des donnees
  .createColorTable(deseqTab, pathResults, comparison, fileHTML="_DESeq2.html")
  return(NULL)
}
