###############################################
# AnaDiff Agilent
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.exportAnaDiff <- function(annot, tabResult, adresse, expNameTexte, expNameHtmlC, fileOut, 
                           sensStep, sensLabel, statBH, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("exportAnaDiff")}
  # Création d'une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # et export des données
  #   tabFinal : tableau de resultats merger avec les annotations
  #   tabColor : tableau de resultats transforme en couleur
  #   export de ces tableaux
  
  tabResult$sense <- sensLabel
  tabFinal <- .mergeAnnot(tabResult, annot, sensStep)
  if (nrow(tabFinal)!=nrow(tabResult) & sum((is.na(tabFinal$ratio)))!=0) {
    tabFinal <- tabFinal[-which(is.na(tabFinal$ratio)),]
    test <- tabFinal[-which(is.na(tabFinal$ratio)),]
  }
  tabFinal <- tabFinal[order(tabFinal$probe_id),]
  write.table(tabFinal, expNameTexte, row.names=F, col.names=T, quote=F, sep="\t", dec=".")
  name <- paste("tab", sensLabel, sep="_")
  assign(name, tabFinal, envir=globalenv())
  
  tabColor <- .tabColorHexa(tabFinal, statBH)
  tabColorExport <- .tabColorWithoutData(tabColor) # .tabColorWithData voir v3
  write.table(tabColorExport, expNameHtmlC, row.names=F, col.names=F, quote=F)
  name <- paste("color", sensLabel, sep="_")
  assign(name, tabColorExport, envir=globalenv())
  
  ### Quelques stats ###
  
  stat <- list()
  stat[["pval"]] <- .look(tabFinal$ratio,tabFinal$pvalue,0.01)
  stat[["BH"]] <- .look(tabFinal$ratio,tabFinal$BH,0.05)
  .writeLineOut("\nNombre de sondes significativement differentiellement exprimées a :",fileOut)
  .writeLineOut(paste("     1%  par p.value   :",stat$pval[[2]]),fileOut)
  .writeLineOut(paste("     5% par BH :",stat$BH[[2]]),fileOut)
  .writeLineOut(paste("\nValeur du ratio avec pval 1%   : ",stat$pval[[1]]),fileOut)
  .writeLineOut(paste("Valeur du ratio avec BH 5%  : ",stat$BH[[1]],"\n"),fileOut)
  
  express <- nrow(tabFinal[which(tabFinal[,2]>=0.5|tabFinal[,3]>=0.5),])
  .writeLineOut(paste("Nombre de sondes exprimées avec Int > 0.5 :",express),fileOut)
  pourcent <- round(express*100/nrow(tabFinal),0)
  .writeLineOut(paste("     soit :",pourcent,"% des sondes présentes (total :",nrow(tabFinal),"sondes)\n"),fileOut)
  stat[["express"]][["intSup05"]] <- list(express=express, pourcent=pourcent)
  
  express <- nrow(tabFinal[which(tabFinal[,2]>=1|tabFinal[,3]>=1),])
  .writeLineOut(paste("Nombre de sondes exprimées avec Int > 1 :",express),fileOut)
  pourcent <- round(express*100/nrow(tabFinal),0)
  .writeLineOut(paste("     soit :",pourcent,"% des sondes présentes (total :",nrow(tabFinal),"sondes)"),fileOut)
  stat[["express"]][["intSup1"]] <- list(express=express, pourcent=pourcent)
  stat[["express"]][["total"]] <- nrow(tabFinal)
  
  resume <- summary(tabFinal[,2:3])[c(1,3,4,6),]
  resume <- rbind(colnames(resume),resume)
  resume <- apply(resume, 2, format)
  .writeLineOut("\nIntensite des echantillons dans ce swap pour ce genome :",fileOut)
  write.table(resume,fileOut,append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
  print(resume[-1,],quote=F)
  return(stat)
}
