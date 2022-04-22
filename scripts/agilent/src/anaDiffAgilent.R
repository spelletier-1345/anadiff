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

###############################################
# AnaDiff Agilent
# Sandra PELLETIER
# Version révisée le 26 juin 2019
###############################################

anaDiff_Agilent <- function(
  designPuce,
  labelling = "direct",
  popBH = "alternate",
  statBH = "BH",
  bkgCalculation = TRUE,
  plot = TRUE,
  adresse = "http://pegasus-bioinfo.angers-nantes.inra.fr/rscripts/Tools/Agilent/",
  dec = "."
) {
  #####
  #' Differential analysis for Agilent array
  #'
  #' @description Comparison analysis using limma.
  #'
  #' This fonction read the summary file and execute the several comparison of it.
  #' Library used are limma, httr and jsonlite.
  #
  #' @param designPuce character or integer. The Agilent number of the design
  #' @param labelling character. The default is "direct" for a direct labelling of target.
  #' For an indirect labelling, use "indirect".
  #' @param popBH character. The default is "alternate" : For the BH calculation, all the 
  #' probe non expressed and non differentiel will exclude. The other choice is "complete"
  #' for the BH calculation on all the probes.
  #' @param statBH character. The default is "BH". Other choice are "Bonferroni", 
  #' "Bonferroni.with.m0" and "qvalue".
  #' @param bkgCalculation logical. Default is TRUE. For calculation and substraction of
  #' the background. For the background calculation, a subset of the 10 percent less expressed
  #' probes is used for mean and standard deviation. The background is this mean plus twice
  #' standard deviation.
  #' @param plot logical. Default is TRUE, for generate the png quality plot.
  #' @param adresse character. 
  #' The default is "http://pegasus-bioinfo.angers-nantes.inra.fr/rscripts/Tools/Agilent/",
  #' used for the specification of the design and the read of annotations file.
  #' @param dataTest logical. Default is FALSE.
  #' @param dec character. Default is "." as decimal separator.
  #' @usage anaDiff_Agilent(designPuce)
  #' @return Summary of quality appreciations. Files are wrote on a new directory using the
  #' summary file name.
  #' @details 
  #' @note For more explanation or problems, please contact the IRHS BIDeFI team : 
  #' contact-bioinfo-irhs@inra.fr
  #' @section Prerequisites and summary file:
  #' The summary file must be in the same directory than raw data. It is a tabular text 
  #' file with 7 columns and one line by array.
  #' \subsection{FileName}{
  #' The summary file name of the array with the barcode of the slide and the position of the 
  #' array on the slide. ei: 258455010001_1_1
  #' }
  #' \subsection{Cy3}{
  #' Name of the Cy3 sample. ei: NaCl_repet1
  #' }
  #' \subsection{Cy5}{
  #' Name of the Cy5 sample. ei. H2O_2
  #' }
  #' \subsection{Control}{
  #' 3 or 5: the dye of the control sample (denominator for comparison)
  #' }
  #' \subsection{Swaps}{
  #' Name of the comparison. Must be the same for all the array (line) composing the 
  #' comparison. ei: NaCl_vs_H2O
  #' }
  #' \subsection{CtrName}{
  #' Name of the control sample (denominator) after compilation (mean) of biological or 
  #' thechnical repets.
  #' }
  #' \subsection{TtmtName}{
  #' Name of the treated sample (numerator) after compilation (mean) of biological or 
  #' thechnical repets.
  #' }
  #' @references 
  #' @author Sandra Pelletier
  #' 
  #' @examples
  #' anaDiff_Agilent(85550)
  #' anaDiff_Agilent("85550")
  #' anaDiff_Agilent("85550", labelling = "indirect", statBH = "Bonferroni")
  #' @export database: files
  #' @importFrom pegasus IRHS site.
  
  ##### 1- data configuration #####
  cat("Check START\n")
  if(!exists("dataTest")){
    conf <- .conf(NULL)
    conf$designPuce <- designPuce
    conf$adresse <- adresse
    conf$dataTest <- dataTest
    print(conf$pck)
    conf$verifPackage <- .verifPackages(conf$pck)!=0
    if (conf$verifPackage) {return("stopped script") ; stop(call.=F)}
    conf$folderArray <- .selectionDossier(conf)
  }
  
  ##### 2- array data
  cat("Check 26/06/2019\n")
  
  array <- c()
  array$arraysTxt <- read.table(paste(conf$folderArray, collapse = ""), header=T, encoding="utf-8")
  cat("Definition du swap...\n")
  array$swaps <- paste(unique(array$arraysTxt$Swaps))
  array$designList <- .designVersion(conf$designPuce, conf$adresse, conf$dataTest)
  exports = designList$export
  alertes <- data.frame(matrix(vector(),nrow=0, ncol=6,
                               dimnames=list(c(),c("swap", "export", "sens", "variance", "gRemoved", "gExpress"))))
  for (swap in swaps) {
    dirName <- .creationAnaDiff(dataTest, designPuce, swap, labelling)
    fileOut <- .creationFileOut(dirName, swap, dataTest)
    .writeLineOut(paste("Version du script d'analyse :", version), fileOut)
    .writeLineOut(paste("Fichier arrays utilisÃ© :", folderArray[[2]]), fileOut)
    .writeLineOut(paste("Le marquage des cibles est :", labelling),fileOut)
    .writeLineOut(paste("Le design de la puce utilisee est :", designPuce),fileOut)
    .writeLineOut(paste("Les calculs de bruit de fond sont faits avec :"),fileOut)
    .writeLineOut(paste(" - les", designList$nbg,
                        "sondes les moins exprimees pour", designList$export),fileOut)
    ligne <- paste(rep("=",nchar(paste("    ->>     ",swap,"     <<-    "))),collapse = "")
    .writeLineOut(lineOut=paste("\n====",ligne,"====",sep=""), fileOut)
    .writeLineOut(lineOut=paste("====    ->>     ",swap,"     <<-    ===="), fileOut)
    .writeLineOut(lineOut=paste("====",ligne,"====",sep=""), fileOut)
    arrays <- as.data.frame(.defineArrays(dirName, fileOut, folderArray[[2]]))
    tab <- read.table(folderArray[[2]], header=F, encoding="utf-8")
    tab[-1,1] <- arrays[,1]
    tab <- apply(tab, 2, format)
    .writeLineOut("\nDescription du swap dans le fichier arrays :\n", fileOut)
    write.table(tab,fileOut,append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
    print(arrays,quote=F)

    RG <- .tabRG(swap, arrays)
    if (designPuce=="64677") {RG$genes$ProbeName <- substr(RG$genes$ProbeName,1,18)}
    .writeLineOut("", fileOut)
    compare <- .defineCompare(swap, RG$targets, fileOut)
    alerte <- data.frame(matrix(vector(),nrow=0, ncol=6,
                                 dimnames=list(c(),c("swap", "export", "sens", "variance", "gRemoved", "gExpress"))))
    rg4graph <- .RG4Graph(RG=RG, labelling=labelling, dirName=dirName)
    expInt <- .nomFichier("geoSubmission.txt", paste(dirName, "qualityControl_geoSubmission/", sep=""), swap, export="")
    geo <- .normIntensite(rg4graph, "", expInt, "")
    rm(rg4graph, expInt, geo)
    json <- list(swap=list(name=swap, design=designPuce, labelling=labelling, exports=exports,
                               fileArray=arrays[,1:4], ctrlName=arrays[1,6], ttmtName=arrays[1,7]))

    for (export in exports) {
      .writeLineOut(paste("\n... genome :",export, "..."),fileOut)
      probe <- designList$probe[which(designList$export==export)]
      genome  <- designList$annotation[which(exports==export)]
      nbg  <- designList$nbg[which(exports==export)]
      RGtmp <- .selectRGProbes(RG, probe)
      RGtmp <- .RGmean(RGtmp)
      sensTxt <- paste(adresse, genome, sep="")
      probeListe <- read.csv(file=sensTxt,sep="\t",header=T,encoding="utf-8", check.names=F, as.is=T)[,1:2]

      for (senseStep in c("sens", "antisens")) {
      #for (senseStep in c("antisens")) {
        sense <- ifelse(labelling=="indirect", ifelse(senseStep=="sens", "antisens", "sens"), senseStep)
        .writeLineOut(paste("\n===>>>   Analyse des sondes", sense), fileOut)
        expName <- .nomExportAD(export, sense, dirName, swap=swap, adresse=adresse)
        if (senseStep=="sens") {
          pl <- data.frame(V1=probeListe[,1])
        } else {
          pl <- data.frame(V1=probeListe[,2])
        }
        cat("\nnormalisation des donnees par Lowess...")
        if (sense=="sens") { # SÃ©paration des sens puis normalisation des sens uniquement
          RGsens <- RGtmp[RGtmp$genes$ProbeName %in% pl$V1,]
          MA <- normalizeWithinArrays(RGsens,method="loess",bc.method="none")
          res <- .statAnaDiff(MA, swap, export, fileOut, compare)
          tabResult <- res$tabFit
        } else { # Normalisation de l'ensemble puis sÃ©paration des antisens
          MA <- normalizeWithinArrays(RGtmp,method="loess",bc.method="none")
          res <- .statAnaDiff(MA, swap, export, fileOut, compare)
          tabResult <- res$tabFit
          tabResult <- .selectProbes(tabResult, probe=probe)
          tabResult <- tabResult[tabResult$Agilent_id %in% pl$V1,]
        }
        tabResult$probe_id <- tabResult$Agilent_id
        tabResultComplet <- tabResult
        statBkg <- .statBkg(tabResult, nbg = 8000, dirNameOut,
                              fileOut, targets = MA$targets)
        tabResult <- statBkg$tab
        tabResult <- .statBh(tabResult, popBH, statBH, fileOut)
        if (bkgCalculation==F) {
          tabWBkg <- .statWoBkg(tabResultComplet, targets = MA$targets)
          tabResultWBkg <- tabResult
          tabResultWBkg[,2:3] <- tabWBkg[,2,3]
          tabResultWBkg$sense <- sense
          tabFinalWBkg <- .mergeAnnot(annotations = genome, tabResult = tabResultWBkg, adresse = adresse)
          tabFinalWBkg <- tabFinalWBkg[order(tabFinalWBkg$probe_id),]
          write.table(tabFinalWBkg,expName$txtWb,row.names=F,col.names=T,quote=F,sep="\t",dec=dec)
        }
        normIntensities <- .normIntensite(MA, tab = tabResultComplet, expName$normI, probe=probe)
        .bkgIntensite(normIntensities, nbg = as.numeric(nbg), expName)
        pr <- .probeRemoved(tab = tabResult, normIntensities, expName, fileOut)
        stat <- .exportAnaDiff(export = export, genome = genome, probe = probe,
                       tabResult = tabResult, adresse = adresse, expName = expName,
                       fileOut, swap = swap, dec = dec, designPuce, senseStep, sense, statBH) # !! senseStep ET sense ?
        json[[export]][[sense]] <- list(var = res$variance, remove=pr, stat=stat, bkg=statBkg$stat)
        json[[export]][[sense]][["Int"]] <- list(ctrl=list(min=min(tabResult[,2]),
                                                       mean=mean(tabResult[,2]),
                                                       med=median(tabResult[,2]),
                                                       max=max(tabResult[,2])),
                                             ttmt=list(min=min(tabResult[,3]),
                                                       mean=mean(tabResult[,3]),
                                                       med=median(tabResult[,3]),
                                                       max=max(tabResult[,3])))
        alerte[(nrow(alerte)+1),] <- c(swap, export, sense, res$variance, pr, stat$pval[[2]])
      }
      if (designPuce == "70158" | designPuce == "70465") { # Cas particulier pour AryANE_v2
        tabAryANE <- .tabSASayane(annot, probeName=RG$genes$ProbeName, tab_sens, tab_antisens,
                                  color_sens, color_antisens, statBH, labelling)
        annot <- tabAryANE$annot
        tab_sens <- tabAryANE$tab_sens
        tab_antisens <- tabAryANE$tab_antisens
        color_sens <- tabAryANE$color_sens
        color_antisens <- tabAryANE$color_antisens
      }
      tabDouble <- .tabSAS(annot, tab_sens, tab_antisens, color_sens, color_antisens, sense, expName)
      name <- paste(swap, export, sep="_")
      assign(name, tabDouble, envir=globalenv())
    }
    alertes <- rbind(alertes, alerte)
    req <- list(service="ServiceRscripts", method="addAnalyticsRscriptsValue", id=1,
                params=c('anaDiff_Agilent', toJSON(json)))
    res <- POST("http://147.99.112.52/cgi-bin/elvis/ServiceRscripts.py", body=req, encode = "json")
    .alertes(alerte, fileOut)
    .writeLineOut(paste("\n===  Analyses du swap", swap, "terminÃ©es  ==="), fileOut)
  }
  cat("\n===================================\n")
  cat("====== Analyses terminÃ©es =========")
  cat("\n===================================\n")
  cat("\nRÃ©capitulation des alertes :\n")
  .alertes(alertes, "noOut")
  rm(annot, color_antisens, color_sens, tab_antisens, tab_sens, envir=globalenv())
  options(conf$localOpt)
}
