
# AnaDiff_RNAseq.R
# Script d'entrée

anaDiff_RNAseq <- function() {
  R.home()
  localOpt <- options()
  options(warn=-1)
  tryCatch(anaDiff <- .anaDiffRNAseq(),
           error = function(e) {
             options(localOpt); warning("anaDiffRNAseq error")
             print("An error has occurred. Please check your summary file.")
             print("If you use reads count files produced by a tool that does not come from the IRHS BioInfo team, please contact the team by email (<contact-bioinfo-irhs@inra.fr>) specifying the error next :");
             print("")
             print(e);
           })
  if (anaDiff == "error") {
    cat("\n\n#################################
        \n###   Analyses interrompues   ###
        \n#################################
        \nAu moins un fichier est manquant.\nMerci de vérifier votre fichier summary.")
  } else {
    cat("\n\n################################\n ###   Analyses terminees   ###\n################################\n\n")
  }
  options(localOpt)
  return("done")
}
count2rpkm <- function(countCol, lengthCol, header=F) {
  localOpt <- options()
  options(warn=-1)
  # Téléchargement du fichier à modifier
  theFile <- .selectionFolder()
  if (class(theFile)=="character") {return("file.choose() : choix de fichier annule") ; stop(call.=F)}
  count = read.csv(paste(theFile[[1]], theFile[[2]], sep=""), sep="\t", header = header)
  # Calcul des RPKM
  t <- as.double(sum(count[,countCol]))
  count$RPKM <- round((1e9*count[,countCol])/(t*count[,lengthCol]), 2)
  # Enregistrement du fichier
  extension <- substr(theFile[[2]], max(which(strsplit(theFile[[2]], "")[[1]]=="."))+1, nchar(theFile[[2]]))
  nameFile <- gsub(extension, "rpkm", theFile[[2]])
  nbCol <- ncol(count)
  colNames <- colnames(count)
  if (!header) {
    colNames[countCol] <- "count"
    colNames[lengthCol] <- "length"
  }
  write.table(count,paste(theFile[[1]], nameFile, sep=""), sep="\t", row.names = F,col.names = colNames , quote = F)
  cat("Fichier", nameFile, "enregistré dans le dossier de travail\n")
  options(localOpt)
}

.countTable <- function(nameFiles) {
  # Fonction de base : 
  #   On enlève la ligne qui contient x
  #   On récupère les colonnes 1 et y
  cT <- function(counTable, x,y) {
    countTable <- countTable[-which(countTable$V1==x),c(1,y)]
    for (nameFile in nameFiles[-1]) {
      dataTable  <- read.table(nameFile,sep="\t")[,c(1,y)]
      countTable <- merge(countTable,dataTable,by="V1",all.x=T)
    }
    rm(dataTable)
    return(countTable)
  }
  # Lecture du premier fichier
  countTable <- read.table(nameFiles[1],sep="\t")
  # Si données count (SamTools), alors fin * à la dernière ligne et 4 colonnes.
  if (sum(countTable=="*")==1) {
    countTable <- countTable[-which(countTable$V1=="*"),c(1,3)]
    for (nameFile in nameFiles[-1]) {
      dataTable  <- read.table(nameFile,sep="\t")[,c(1,3)]
      countTable <- merge(countTable,dataTable,by="V1",all.x=T)
    }
    rm(dataTable)
  }
  # Si données sf (Salmon), alors entête ("Name") et 5 colonnes.
  if (sum(countTable=="Name")==1) {
    countTable <- read.table(nameFiles[1],sep="\t", header=T, 
                             colClasses=c("character", rep("numeric", 4)))[,c(1,5)]
    for (nameFile in nameFiles[-1]) {
      dataTable  <- read.table(nameFile,sep="\t", header=T, 
                               colClasses=c("character", rep("numeric", 4)))[,c(1,5)]
      countTable <- merge(countTable,dataTable,by="Name",all.x=T)
    }
    rm(dataTable)
    countTable[,-1] <- apply(countTable[,-1], 2, round) 
  }
  colnames(countTable) <- c("id", nameFiles)
  rownames(countTable) <- countTable$id # Identification des noms de ligne
  countTable <- countTable[,-1]
  return(countTable)
}

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
.defineComparisons <- function(myData) {
  # Recupère une liste unique de nom de swap
  # Args: 
  #   arrays : tableau de meta-donnee sur design de la manip
  # Returns:
  #   liste de comparaison uniques
  cat("Definition de la comparaison...\n")
  comparisons <- paste(unique(myData$Comparison))
  return(comparisons)
}
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

.anaDiffRNAseq <- function(){
  .verifPackages(c("edgeR", "DESeq2", "geneplotter"))
  cat("Selection du repertoire de travail...\n")
  cat("\n------------ MESSAGE -------------\n")
  cat("\nAppuyez sur \"Entree\" et selectionnez le fichier summary.txt\n")
  cat("Les fichiers de donnees brutes (.count ou .fs) doivent etre dans ce meme dossier\n")
  folderSummary <- .selectionFolder()
  if (class(folderSummary)=="character") {return("file.choose() : choix de fichier annule") ; stop(call.=F)}
  pathFolder <- folderSummary[[1]]
  fileSummary <- paste(pathFolder,folderSummary[[2]],sep="")
  pathResults <- .mkdirAnaDiff(folderSummary)
  fileOut <- .mkfileOut(pathResults)
  myData  <- as.data.frame(.readSummary(fileSummary, fileOut, pathFolder))
  if (myData[1,1]=="error"){return("error")}
  comparisons <- .defineComparisons(myData)

  for (comparison in comparisons) {
    cat("\n","########### ",comparison," ###########","\n\n")
    .writeLineOut(paste("\n","########### ",comparison," ###########","\n",sep=""), fileOut)
    myCompare  <- myData[myData$Comparison==comparison,] # Sous tableau de comparaison
    nameFiles  <- as.vector(myCompare$File)
    countTable <- .countTable(nameFiles)
    repet      <- c(sum(myCompare$Group==("Control")),sum(myCompare$Group==("Ttmt")))
    group      <- factor(myCompare$Name[order(myCompare$Group)])
    cat(paste("Control : ",group[1]),"\n")
    .writeLineOut(paste("Control : ",group[1]), fileOut)
    cat(paste("Ttmt    : ",group[length(group)],"\n"))
    .writeLineOut(paste("Ttmt    : ",group[length(group)]), fileOut)
    .functionEdgeR(repet, countTable, group,comparison, fileOut, pathResults)
    .functionDESeq2(myCompare, countTable, comparison, fileOut, pathResults)
  }
  return("OK")
}


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

.gammeBH <- function(x) { # fonction pour BH
  if      (x < 0.00001) {color <- "00ffff"}
  else if (x < 0.01)    {color <- "0099ff"}
  else if (x < 0.05)    {color <- "0000ff"}
  else if (x < 0.1)     {color <- "222222"}
  else                  {color <- "000000"}
  return(color)
}
.gammeInt <- function(x) { # fonction pour intensite
  if      (x > 9)   {color <- "ffff99"}
  else if (x > 8)   {color <- "ffff66"}
  else if (x > 7)   {color <- "ffff33"}
  else if (x > 6)   {color <- "eeee00"}
  else if (x > 5)   {color <- "dddd00"}
  else if (x > 4)   {color <- "bbbb00"}
  else if (x > 3)   {color <- "999900"}
  else if (x > 2)   {color <- "777700"}
  else if (x > 1)   {color <- "555500"}
  else if (x > 0.5) {color <- "333300"}
  else if (x > 0)   {color <- "222200"}
  else              {color <- "000000"}
  return(color)
}
.gammePval <- function(x) { # fonction pour pvalue
  if      (x < 0.00001) {color <- "00ffff"}
  else if (x < 0.001)   {color <- "0099ff"}
  else if (x < 0.01)    {color <- "0000ff"}
  else if (x < 0.05)    {color <- "222222"}
  else                  {color <- "000000"}
  return(color)
}
.gammeRatio <- function(x) { # fonction pour ratio
  if      (x < (-3))   {color <- "00ff00"}
  else if (x < (-1.5)) {color <- "00aa00"}
  else if (x < (-1))   {color <- "006600"}
  else if (x < (-0.5)) {color <- "003300"}
  else if (x < 0)      {color <- "001100"}
  else if (x==0 | x=="NaN" | x=="-Inf" | x=="Inf") {color <- "000000"}
  else if (x < 0.5)  {color <- "110000"}
  else if (x < 1)    {color <- "330000"}
  else if (x < 1.5)  {color <- "770000"}
  else if (x < 3)    {color <- "aa0000"}
  else               {color <- "ff0000"}
  return(color)
}
.mkdirAnaDiff <- function(folderSummary) {
  # Creation du dossier AnaDiff et du fichier out
  # Args:
  #   rien
  # CeQuIlFait:
  #   verifie si le dossier AnaDiff existe deja
  #   si non : cree le dossier
  #   si oui : incremente le nom et cree dossier
  #   defini le dossier de travail
  # Returns:
  #   rien
  cat("Creation du dossier AnaDiff...\n")
  pathFolder <- folderSummary[[1]]
  folderName <- tools::file_path_sans_ext(folderSummary[[2]])
  dirName <- paste(pathFolder, folderName, sep="")
  if(file.exists(dirName)) {
    i <- 1
    dirNameId <- paste(dirName,i,sep=".")
    while (file.exists(dirNameId)) {
      i <- i+1
      dirNameId <- paste(dirName,i,sep=".")
    }
  } else {dirNameId <- dirName}
  cat("...Dossier de travail : ",dirNameId,"\n")
  dir.create(dirNameId,showWarnings=F)
  dirNameId <- paste(dirNameId,"/",sep="")
  return(dirNameId)
}


.mkfileOut <- function(dirName) {
  # Cree le fichier out dans le dossier dirName
  # Export les infos relatives a la date et au dossier de travail
  # Args: 
  #   dirName : dossier d'export pour .writeLineOut
  # Returns:
  #   rien
  fileOut <- paste(dirName,"AnaDiff_",Sys.Date(),"_out.txt",sep="")
  file.create(fileOut)
  .writeLineOut("\n###  Analyses differentielles RNAseq ###\n", fileOut)
  .writeLineOut(paste("Le",format(Sys.Date(), "%a %d %b %Y")), fileOut)
  .writeLineOut("Script Anadiff_RNAseq.R mis a jour le 17/09/2018", fileOut)
  .writeLineOut("Script Functions_RNAseq.R mis a jour le 17/11/2014", fileOut)
  .writeLineOut("Package R utilises : edgeR, DESeq2, geneplotter", fileOut)
  .writeLineOut(paste("Repertoire de travail :",getwd()), fileOut)
  .writeLineOut(paste("Dossier d'export : ", dirName, sep=""), fileOut)
  return(fileOut)
} 

.readSummary <- function(fileSummary, fileOut, pathFolder) {
  # Ouverture et modif du fichier arrays.txt
  # Args: 
  #   dirName : dossier d'export pour .writeLineOut
  # CeQuIlFait:
  #   lit la liste des fichiers de donnees brutes et le fichier arrays.txt
  #   change la colonne fichier en fonction des nom des fichiers brutes
  #   exporte le tableau arrays dans le fichier Out
  # Returns:
  #   le tableau arrays
  cat("Lecture du fichier",fileSummary,"...\n")
  myData <- read.table(fileSummary,header=T,sep="\t",check.names=F)
  cat("Vérification de la présence des fichiers...\n")
  errorMsg <- FALSE
  for (countFile in myData$File) {
    pathFile <- paste(pathFolder, countFile, sep="")
    cat(pathFile)
    if(file.exists(pathFile)) {
      cat(" : OK\n")
    } else {
      errorMsg <- TRUE
      cat("\n   !!! MISSING FILE !!!\n")
    }
  }
  if (errorMsg) {return("error")}
  cat("Ecriture du tableau summary dans le fichier de sortie...\n")
  tab <- rbind(myData[1,],myData[,])
  for (indice in seq(1,4)) {
    tab[,indice] <- c(matrix(colnames(myData),nrow=1)[,indice],as.character(myData[,indice]))
  }
  tab <- apply(tab, 2, format)
  .writeLineOut(paste("\nFichier summary utilise par le script :",fileSummary,"\n"),fileOut)
  write.table(tab ,fileOut,append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
  
  cat("Analyses de chaque comparaison...\n\n")
  cat("########################################################\n")
  cat("                 Analyse differentielle\n")
  cat("########################################################\n\n")
  .writeLineOut("\n#################################################################",fileOut)
  .writeLineOut("                 Analyse differentielle",fileOut)
  .writeLineOut("#################################################################\n",fileOut)
  return(myData)
}
.selectionFolder <- function() {
  # Selection du dossier de travail
  # Args: 
  #   rien
  # CeQuIlFait:
  #   demande ou est le fichier arrays.txt
  #   defini le dossier de travail
  # Returns:
  #   rien
  readLines(n=1)
  e <- simpleError("Oups")
  fileSummary <- tryCatch(file.choose(), error=function(e) e)
  if (class(fileSummary)!="character") {
    return("file.choose() : choix de fichier annule")
    stop(call.=F)
  }
  dataFolder <- try(paste(dirname(fileSummary),"/",sep=""), silent=TRUE) 
  cat("Dossier de travail :\n",dataFolder,"\n\n",sep="")
  setwd(dataFolder)
  fileSummary <- basename(fileSummary)
  return(list(dataFolder,fileSummary))
}
.verifPackages <- function(vPackages) {
  # Verification de l'installation des packages
  # Args: 
  #   vPackages : vecteur des packages à tester
  # CeQuIlFait:
  #   si non installe : arrêt du script et rm de la memoire
  #   si oui : chargement des packages
  # Returns:
  #   rien
  cat("\n-------------------------\n\n")
  cat("Verification des packages...\n")
  for (package in vPackages) {
    if (is.element(package,installed.packages()[,1])) {} 
    else {
      message <- paste("\n#########################################\n\nVous devez telecharger le package ", package, "\n",
                       "pour cela, coller les lignes suivantes :\n\nsource(\"http://bioconductor.org/biocLite.R\")\n",
                       "biocLite(\"", package, "\")\n\n#########################################\n\n", sep="")
      cat("\n-------------------------\n\n")
      cat(message)
      cat("\n-------------------------\n\n")
      rm(list=ls(all=T))
      stop("Script arrete",call.=F)
    }
  }
  for (package in vPackages) {
    library(package,character.only=TRUE)
    cat("chargement de la librairie",package,"\n")
  }
  
  return(NULL)
}
.writeLineOut <- function(ligneOut, fileOut) {
  # Ecrit une ligne dans le fichier out
  # Args: 
  #   ligneOut : texte a ecrire
  #   dirName : dossier d'export pour .writeLineOut
  # Dependance:
  #   .nameFileOut(dirName)
  # Returns:
  #   rien  
  write.table(ligneOut, fileOut, quote=F, append=T, row.names=F, col.names=F)
  return(NULL)
}
xlsTransform <- function() {
  localOpt <- options() ; options(warn=-1)
  cat("Selection du repertoire de travail...\n")
  cat("\n------------ MESSAGE -------------\n")
  cat("\nAppuyez sur \"Entree\" et selectionnez le fichier texte de données compilées.\n")
  selectXls <- .selectionFolder()
  if (class(selectXls)=="character") {
    return("file.choose() : choix de fichier annule")
    options(localOpt)
    stop(call.=F)}
  filexls <- read.csv(selectXls[[2]], sep = "\t", check.names = F, row.names = NULL)
  header <- colnames(filexls)
  len <- length(header)
  lenGene <- header[6]=="4_length"
  for (i in seq(1, len)) {
    title <- header[i]
    nc <- as.numeric(nchar(title))
    if (nc > 5 & substr(title, nc-4, nc)=="count") {
      if (lenGene) {
        count <- filexls[,c(1,4,i,i)]
        count[,4] <- 0
      } else {
        count <- filexls[,c(1,i,i,i)]
        count[,c(2,4)] <- 0
      }
      title <- substr(title, 1, nc-6)
      listeBefore <- unlist(strsplit(title, "-"))
      listeAfter  <- c()
      for (l in listeBefore) {
        listeAfter <- c(listeAfter, gsub("[[:punct:]]", "_", l))
      }
      fileName = paste(paste(listeAfter, collapse="-"), "count", sep=".")
      i <- 1
      while (file.exists(fileName)) {
        fileName <- paste(paste(listeAfter, collapse="-"), i, "count", sep=".")
        i <- i+1
      }
      write.table(count, fileName, quote=F, sep="\t", row.names = F, col.names = F)
      write("*\t0\t0\t0", fileName, append = T)
    }
  }
}