source("./src/verifPackages.R")
source("./src/selectionFolder.R")
source("./src/mkdirAnaDiff.R")
source("./src/mkfileOut.R")
source("./src/readSummary.R")
source("./src/defineComparisons.R")
source("./src/writeLineOut.R")
source("./src/countTable.R")
source("./src/functionEdgeR.R")
source("./src/functionDEseq2.R")

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
    countTable <- countTable[,names(countTable)[order(myCompare$Group, decreasing = T)]]
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
