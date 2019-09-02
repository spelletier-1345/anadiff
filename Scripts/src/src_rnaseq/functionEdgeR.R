source("/home/spelletier/dev/RNAseq/Scripts/AnaDiffRNAseq/src/writeLineOut.R")
source("/home/spelletier/dev/RNAseq/Scripts/AnaDiffRNAseq/src/edgeRTable.R")
source("/home/spelletier/dev/RNAseq/Scripts/AnaDiffRNAseq/src/createColorTable.R")

.functionEdgeR <- function(repet, countTable, group, comparison, fileOut, pathResults) {

  if (sum(repet==1)!=0) {
    bcv <- 0.05 # si l'un des échantillon est en une seule repet bio
    y  <- DGEList(counts=countTable,group=group)  # Creates a DGEList object from a table of counts
    et <- exactTest(y,dispersion=bcv^2,pair=c(as.vector(group)[1],as.vector(group)[length(group)])) # Compute genewise exact tests for differences in the means between two groups of negative-binomially distributed counts.
    de <- decideTestsDGE(et, p=0.05, adjust="BH") #Classify a series of related differential expression statistics as up, down or not significant.
  } else {
    y  <- DGEList(counts=countTable,group=group)  # Creates a DGEList object from a table of counts
    y  <- calcNormFactors(y)                      # Calculate normalization factors to scale the raw library sizes.
    y  <- estimateCommonDisp(y)                   # Maximizes the negative binomial conditional common likelihood to give the estimate of the common dispersion across all tags.
    y  <- estimateTagwiseDisp(y)                  # Estimates tagwise dispersion values by an empirical Bayes method based on weighted conditional maximum likelihood.
    et <- exactTest(y, pair=c(as.vector(group)[1],as.vector(group)[length(group)]))   # Compute genewise exact tests for differences in the means between two groups of negative-binomially distributed counts.
    de <- decideTestsDGE(et, p=0.05, adjust="BH") #Classify a series of related differential expression statistics as up, down or not significant.
    cat(paste("common dispersion :",round(y$common.dispersion,digits=3),"\n"))
    .writeLineOut(paste("common dispersion with edgR :",round(y$common.dispersion,digits=3),"\n"), fileOut)
  }

  # Plots
  pngName <- paste(pathResults, comparison, "_edgeR.png", sep="")
  graphics.off()
  png(pngName, width=1086, height=1086, res=120)
  de.tags <- rownames(topTags(et, n=500)$table)
  plotSmear(y, de.tags=de.tags, lowess=T,main=paste("edgeR\n",comparison), ylim=c(-4,4)) # highlighting the top 500 most DE tags
  dev.off()

  tag <- topTags(et,n=1)
  re <- topTags(et,nrow(et$table))

  # Recuperation des donnees
  edgeRTab <- .edgeRTable(countTable,re,group)
  write.table(edgeRTab, paste(pathResults,comparison,"_edgeR.txt",sep=""),sep="\t",row.names=F,quote=F, dec=".")

  # Resume
  pval     <- sum(edgeRTab$PValue_edgeR < 0.05, na.rm=TRUE )
  BH       <- sum(edgeRTab$FDR_edgeR < 0.1, na.rm=TRUE )
  downpval <- nrow(edgeRTab[which(edgeRTab$logFC<0 & edgeRTab$PValue_edgeR<(0.05)),])
  uppval   <- nrow(edgeRTab[which(edgeRTab$logFC>0 & edgeRTab$PValue_edgeR<(0.05)),])
  downBH   <- nrow(edgeRTab[which(edgeRTab$logFC<0 & edgeRTab$FDR_edgeR<(0.1)),])
  upBH     <- nrow(edgeRTab[which(edgeRTab$logFC>0 & edgeRTab$FDR_edgeR<(0.1)),])
  noDEpval <- nrow(edgeRTab) - (downpval+uppval)
  noDEBH   <- nrow(edgeRTab) - (downBH+upBH)
  .writeLineOut(paste("\n### edgeR analysis summary ###\n"), fileOut)
  .writeLineOut(paste("number of genes with pval less than 5% :",pval), fileOut)
  .writeLineOut(paste("genes down with pval 5%                :",downpval), fileOut)
  .writeLineOut(paste("genes up with pval 5%                  :",uppval), fileOut)
  .writeLineOut(paste("number of genes no diff with pval      :",noDEpval,"\n"), fileOut)
  .writeLineOut(paste("number of genes with BH less than 10%  :",BH), fileOut)
  .writeLineOut(paste("genes down with BH 10%                 :",downBH), fileOut)
  .writeLineOut(paste("genes up with BH 10%                   :",upBH), fileOut)
  .writeLineOut(paste("number of genes no diff with BH        :",noDEBH,"\n"), fileOut)

  cat(paste("\nedgeR analysis summary\n\n"))
  cat(paste("number of genes with pval less than 5% :",pval,"\n"))
  cat(paste("genes down with pval 5%                :",downpval,"\n"))
  cat(paste("genes up with pval 5%                  :",uppval,"\n"))
  cat(paste("number of genes no diff with pval      :",noDEpval,"\n\n"))
  cat(paste("number of genes with BH less than 10%  :",BH,"\n"))
  cat(paste("genes down with BH 10%                 :",downBH,"\n"))
  cat(paste("genes up with BH 10%                   :",upBH,"\n"))
  cat(paste("number of genes no diff with BH        :",noDEBH,"\n\n"))

  # Mise en forme des donnees
  .createColorTable(edgeRTab, pathResults, comparison, fileHTML = "_edgeR.html")
  return(NULL)
}
