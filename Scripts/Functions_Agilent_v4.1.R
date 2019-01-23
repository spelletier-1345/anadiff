###############################################
# Version revisee le 21/03/2017
# - simplification du code
# - suppression de Geneplotter
# - Sens des sondes
###############################################

# Note : Version ordi portable :
#         - Changer la fonction adresse.
#         - Remettre le POST vers DB

anaDiffAgilent <- function(designPuce, labelling, dec = ".", popBH="alternate", statBH="BH",
                           dataTest=FALSE, bkgCalculation = TRUE) {

  # Note : labelling = c("direct", "indirect")
  # Note : statBH = c("BH", "Bonferroni", "Bonferroni.with.m0", "qvalue")

  version <- "AnaDiff_Script_functions_v4.0.R" # TODO
  adresse <- .adresse(dataTest)
  print(designPuce)
  designPuce <- ifelse(substr(designPuce,1,1)==0, substr(designPuce,2,nchar(designPuce)), designPuce)
  print(designPuce)
  localOpt <- options() ; options(warn=-1)
  pck <- c("limma", "httr", "jsonlite")
  verifPackage <- .verifPackages(pck)!=0
  if (verifPackage) {return("script arrêté") ; stop(call.=F)}
  message <- "Selection du repertoire de travail...\n
  ------------ MESSAGE -------------
  Appuyez sur \"Entree\" et selectionnez le fichier arrays.txt
  Les fichiers \".txt\" de donnees brutes doivent etre dans ce meme dossier\n\n"
  folderArray <- .selectionDossier(message)
  if (class(folderArray)=="character") {return("file.choose() : choix de fichier annule") ; stop(call.=F)}
  arraysTxt <- read.table(folderArray[[2]], header=T, encoding="utf-8")
  cat("Definition du swap...\n")
  swaps <- paste(unique(arraysTxt$Swaps))
  designList <- .designVersion(designPuce, adresse, dataTest)
  print(designPuce)
  print(designList)
  exports = designList$export
  alertes <- data.frame(matrix(vector(),nrow=0, ncol=6,
                               dimnames=list(c(),c("swap", "export", "sens", "variance", "gRemoved", "gExpress"))))
  for (swap in swaps) {
    dirName <- .creationAnaDiff(dataTest, designPuce, swap, labelling)
    fileOut <- .creationFileOut(dirName, swap, dataTest)
    .writeLineOut(paste("Version du script d'analyse :", version), fileOut)
    .writeLineOut(paste("Fichier arrays utilisé :", folderArray[[2]]), fileOut)
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
    # rg4graph <- .RG4Graph(RG=RG, labelling=labelling, dirName=dirName)
    # expInt <- .nomFichier("geoSubmission.txt", paste(dirName, "qualityControl_geoSubmission/", sep=""), swap, export="")
    # geo <- .normIntensite(rg4graph, "", expInt, "")
    # rm(rg4graph, expInt, geo)
    json <- list(swap=list(name=swap, design=designPuce, labelling=labelling, exports=exports,
                               fileArray=arrays[,1:4], ctrlName=arrays[1,6], ttmtName=arrays[1,7]))

    print(exports)
    for (export in exports) {
      print("OK 1")
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
        if (sense=="sens") { # Séparation des sens puis normalisation des sens uniquement
          RGsens <- RGtmp[RGtmp$genes$ProbeName %in% pl$V1,]
          MA <- normalizeWithinArrays(RGsens,method="loess",bc.method="none")
          res <- .statAnaDiff(MA, swap, export, fileOut, compare)
          tabResult <- res$tabFit
        } else { # Normalisation de l'ensemble puis séparation des antisens
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
    .writeLineOut(paste("\n===  Analyses du swap", swap, "terminées  ==="), fileOut)
  }
  cat("\n===================================\n")
  cat("====== Analyses terminées =========")
  cat("\n===================================\n")
  cat("\nRécapitulation des alertes :\n")
  .alertes(alertes, "noOut")
  rm(annot, color_antisens, color_sens, tab_antisens, tab_sens, envir=globalenv())
  options(localOpt)
}


.alertes <- function(alertes, fileOut) {
  al <- c()
  attach(alertes, warn.conflicts = F)
  for (alRow in seq(1:nrow(alertes))) {
    if (as.numeric(variance[alRow])>0.09) {
      ai <- paste("\nswap ", swap[alRow], ", genome ", export[alRow], ", sondes ", sens[alRow],
                  "\n\n   Variance = ", variance[alRow], "  (Attendue : <0.06)",
                  " : \n   ATTENTION, la variance ne correspond pas au critère souhaité.",
                  "\n   =>> Prenez contact avec le PTM ANAN.", sep="")
      al <- c(al, ai)
    } else if (as.numeric(variance[alRow])>0.06) {
      ai <- paste("\nswap ", swap[alRow], ", genome ", export[alRow], ", sondes ", sens[alRow],
                  "\n\n   Variance = ", variance[alRow], "  (Attendue : <0.06)",
                  " : \n   Attention, la variance est élevée.", sep="")
      al <- c(al, ai)
    }

    if (as.numeric(gRemoved[alRow])>as.numeric(gExpress[alRow])) {
      ai <- paste("\nswap ", swap[alRow], ", genome ", export[alRow], ", sondes ", sens[alRow],
                  "\n\n   ", gRemoved[alRow], " gènes enlevés pour ", gExpress[alRow], " gènes différentiellement exprimés",
                  " : \n   ATTENTION, trop de sondes sont retirées de l'analyse.",
                  "\n   =>> Prenez contact avec le PTM ANAN.", sep="")
      al <- c(al, ai)
    } else if (as.numeric(gRemoved[alRow])>(0.5*as.numeric(gExpress[alRow]))) {
      ai <- paste("\nswap ", swap[alRow], ", genome ", export[alRow], ", sondes ", sens[alRow],
                  "\n\n   ", gRemoved[alRow], " gènes enlevés pour ", gExpress[alRow], " gènes différentiellement exprimés",
                  " : \n   Attention, quantité très importante de sondes retirées.", sep="")
      al <- c(al, ai)
    }
  }
  detach(alertes)
  if (length(al)==0) {
    .writeLineOut("\nBravo, tous les indicateurs sont aux verts, aucune alerte à signaler. :)", fileOut)
  } else {
    .writeLineOut("\n--------------------", fileOut)
    .writeLineOut("  !!! ALERTES !!!", fileOut)
    .writeLineOut("--------------------", fileOut)
    for (i2 in al) {
      .writeLineOut(i2, fileOut)
    }
    .writeLineOut("\n--------------------\n", fileOut)
  }
}

.bkgIntensite <- function(norm_intensities, nbg, expName) {
  # soustraie le bruit de fond pour chaque donnee d'intensite normalisee et les exporte
  # Args:
  #   norm_intensite : le tableau d'intensites normalisee
  #   swap : le nom du swap
  #   nbg : nombre de sonde pour le calcul
  # Returns:
  #   rien
  cat("soustraction du bruit de fond...\n")
  ssbkg <- function(intensite, nbg) {
    bkg_intensities <- intensite - .calcBkg(intensite,nbg)$background
    bkg_intensities[which(bkg_intensities<0)] <- 0
    return(bkg_intensities)
  }
  bkg_intensities      <- norm_intensities
  bkg_intensities[,-1] <- mapply(ssbkg,norm_intensities[,-1],nbg)
  bkg_intensities <- bkg_intensities[order(bkg_intensities$probe_id),]
  write.table(bkg_intensities,expName$bkgI,quote=F,sep="\t",row.names=F,dec=".")
  return(NULL)
}

.calcBkg <- function(intensite, nbg) {
  # calcul du bruit de fond en fonction de nbg
  # Args:
  #   intensite : liste des intensitees
  #   nbg : nombre de sonde pour le calcul
  # Returns:
  #   bkg : liste d'info : moyenne, ecart-type, bruit de fond calcule
  Abg   <- intensite[intensite<=sort(intensite)[nbg]]      # Abg = Tri des nbg plus petites valeurs
  mAbg  <- mean(Abg,na.rm=T)       # mAbg est la moyenne de ces nbg plus petites valeurs
  sdAbg <- sd(Abg,na.rm=T)         # sdAbg est l'ecart type de ces nbg plus petites valeurs
  bg    <- mAbg+2*sdAbg            # Calcul du bruit de fond
  return(bkg=list(moyenne=mAbg,ecarttype=sdAbg,background=bg))   # sortie : liste des elements moyenne, sd et bg
}





.exportAnaDiff <- function(export, genome, probe, tabResult, adresse, expName,
                           fileOut, swap, dec=".", designPuce, senseStep, sense, statBH) {
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  #   exports : liste des noms a mettre dans le nom de fichier a exporter
  #   genomes : liste des noms de fichier de reference du genome a ouvrir et ayant les annotations
  #   tabResult : tableau de resultat a merger avec le tableau d'annotation, a colorer et exporter
  # CeQuIlFait :
  #   tabFinal : tableau de resultats merger avec les annotations
  #   tabColor : tableau de resultats transforme en couleur
  #   export de ces tableaux

  tabResult$sense <- sense
  tabFinal <- .mergeAnnot(annotations = genome, tabResult = tabResult, adresse = adresse, senseStep)
  if (nrow(tabFinal)!=nrow(tabResult) & sum((is.na(tabFinal$ratio)))!=0) {
    tabFinal <- tabFinal[-which(is.na(tabFinal$ratio)),]
    test <- tabFinal[-which(is.na(tabFinal$ratio)),]
  }
  tabFinal <- tabFinal[order(tabFinal$probe_id),]
  write.table(tabFinal,expName$texte,row.names=F,col.names=T,quote=F,sep="\t",dec=dec)
  name <- paste("tab", sense, sep="_")
  assign(name, tabFinal, envir=globalenv())

  tabColor <- .tabColorHexa(tabFinal, statBH)
  tabColorExport <- .tabColorWithoutData(tabColor) # .tabColorWithData voir v3
  write.table(tabColorExport,expName$htmlC,row.names=F,col.names=F,quote=F)
  name <- paste("color", sense, sep="_")
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

.gammeCouleurs <- function() {
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  #   rien
  # Returns:
  #   liste de vecteurs :
  #   - bleu  :  4 couleurs
  #   - jaune :  9 couleurs
  #   - vert  : 10 couleurs
  #   - rouge : 10 couleurs
  #   - noir  :  1 couleur
  paletteBleue <- colorRampPalette(c("#0000FF","#79F8F8"))(4)
  paletteJaune <- colorRampPalette(c("#3A3A00","#C5C500","#E6E600","#FFFF66"))(9)
  paletteVerte <- colorRampPalette(c("#11391E","#00FF00"))(10)
  paletteRouge <- colorRampPalette(c("#430000","#FE0000"))(10)
  paletteNoire <- "#000000"
  return(list(bleue=paletteBleue, jaune=paletteJaune, verte=paletteVerte, rouge=paletteRouge, noire=paletteNoire))
}

inopsisFileDivision <- function() {
  cat("\nDivision of the inopsis file for the anaDiffAgilent function\n")
  message <- "Selection du repertoire de travail...\n
  ------------ MESSAGE -------------
  Appuyez sur \"Entree\" et selectionnez le fichier a diviser\n\n"
  folderArray <- .selectionDossier(message)
  inopsisClass <- c("integer", "integer", "integer", rep("NULL", 5), "numeric", rep("NULL", 5),
                    "numeric", rep("NULL", 13), "numeric", rep("NULL", 5), "numeric",
                    rep("NULL", 25), "character", rep("NULL", 3), "integer", rep("NULL", 4))
  cat("\nReading the file", folderArray[[2]], "\n")
  inopsis <- read.table(folderArray[[2]], skip=31, header=T, colClass=inopsisClass)
  inopsis <- inopsis[,c(1,9,3,2,8,4,5,6,7)]
  colnames(inopsis) <- c("Block", "FeatureNum", "Row", "Col", "ProbeName",
                         "rMedianSignal", "rBGMedianSignal", "gMedianSignal", "gBGMedianSignal")
  file1 <- inopsis[which(inopsis$Block==4),-1]
  file2 <- inopsis[which(inopsis$Block==3),-1]
  file3 <- inopsis[which(inopsis$Block==2),-1]
  file4 <- inopsis[which(inopsis$Block==1),-1]
  files <- list("file1"=file1, "file2"=file2, "file3"=file3, "file4"=file4)
  ent <- matrix(c("integer", "integer", "integer", "text", "float", "float", "float", "float",
                  "FeatureNum", "Row", "Col", "ProbeName", "rMedianSignal", "rBGMedianSignal",
                  "gMedianSignal", "gBGMedianSignal"),byrow = T, nrow = 2)
  barcode <- substr(as.character(read.table(folderArray[[2]], skip=15, nrows=1)[1,1]), 9, 20)
  usertext <- substr(folderArray[[2]], 34, 48)
  userdate <- substr(folderArray[[2]], 1, 11)
  for (i in seq(1:4)) {
    fileName <- paste(barcode, "_", userdate, usertext, "_1_", i, ".txt", sep="")
    cat("\nSaving the file", fileName, "\n")
    write.table(ent, fileName, quote=F, sep="\t", row.names=F, col.names=F, dec=".") # Entête
    write.table(files[[i]], fileName, quote=F, sep="\t", row.names=F, col.names=F, dec=".", append=T) # Valeurs
  }
  cat("\nJob is done", "\n")
}

.kerfdr<-function (pv, fileOut, lambda = seq(0, 0.9, 0.05),
                   cuts = c(1e-04, 0.001, 0.01, 0.025, 0.05, 0.1)) {
  ## script IPS2 (Véronique Brunaud)
  ## modif de la methode storey
  ## inclusion du etape de smoothing
  pi0.lambda<-apply(as.matrix(lambda), 1, FUN = function(x) mean(pv >= x)/(1 - x))
  pi1<- 1-pi0.lambda
  if(length(lambda)!=1) {
    pi0.spline <- smooth.spline(lambda, pi0.lambda, df = 3)
    pi0 <- max(0, min(predict(pi0.spline, x = max(lambda))$y,1))
    pi1<-1-pi0
  }
  if (pi1 < 0) {
    warning(paste("estimated pi1 =", round(pi1, digit = 4), "set to 0.0"))
    pi1 = 0
  }
  if (pi1 > 1) {
    warning(paste("estimated pi1 =", round(pi1, digit = 4), "set to 1.0"))
    pi1 = 1
  }
  pi0 = 1 - pi1
  ## modification pour tenir compte de la correction par pi0
  bon.pi0 = (pv *pi0 ) * length(pv)
  bon.pi0[which(bon.pi0 > 1)] = 1
  ## modification pour tenir compte de la correction par pi0
  bh.pi0 = (pv*pi0) * length(pv)/rank(pv)
  bh.pi0[which(bh.pi0 > 1)] = 1
  ## modification pour tenir compte de la correction par pi0
  bon = (pv *1 ) * length(pv)
  bon[which(bon > 1)] = 1
  ## modification pour tenir compte de la correction par pi0
  bh = (pv*1) * length(pv)/rank(pv)
  bh[which(bh > 1)] = 1
  counts <- sapply(cuts,
                   function(x) c(`p-value` = sum(pv <= x),
                                 `FWER (Bonf.)` = sum(bon <= x),
                                 `FWER (Bonf. with m0)` = sum(bon.pi0 <= x),
                                 `FDR (BH)` = sum(bh <= x),
                                 `qvalue` = sum(bh.pi0 <= x)))
  colnames(counts) <- paste("<", cuts, sep = "")
  #cat("\n") ; print(counts) ;cat("\n")
  tab <- rbind(colnames(counts),counts)
  tab <- cbind(rownames(tab),tab)
  tab <- apply(tab, 2, format)
  .writeLineOut("\nContrôle des faux positifs :\n",fileOut)
  write.table(tab, fileOut, append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
  print(tab[-1,-1],quote=F) ; cat("\n")
  results = list(pv = pv, pi0 = pi0, pi1 = pi1, BH=bh, Bonferroni=bon,Bonferroni.with.m0=bon.pi0,
                 qvalue=bh.pi0,summary = counts)
  return(results)
}

.look <- function(ratio, stats, seuil) {
  # donne des informations stats : recherche du nombre de bonne stats et ratio associé à la moins bonne
  # Args:
  #   liste numerique des ratios
  #   liste numerique des stats
  # Returns:
  #   liste : ratio minimum et nombre de genes diff
  valmini  <- stats[stats<=seuil]                   # liste des valeurs de stats inferieures au seuil
  valmax   <- max(valmini)                          # valeur de stats exact immediatement inferieure au seuil
  valcible <- ratio[which(stats<=valmax)]           # liste des ratios dont stats est inferieures au seuil
  nombre   <- length(valcible)                      # nombre de sondes dont stats est inferieures au seuil
  valeur   <- min(abs(valcible))                    # valeur du plus petit ratio (en absolu) correspondant au seuil
  return(list(valeur_minimum=valeur,nombre_de_sondes=nombre))
}

.mergeAnnot <- function(annotations, tabResult, adresse, senseStep) {
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  # Returns:
  adresseAnnot <- paste(adresse,annotations,sep="")
  tabAnnot     <- read.csv(file=adresseAnnot,sep="\t",header=T,encoding="utf-8", check.names=F, as.is=T)
  if (senseStep=="antisens") {colnames(tabAnnot)[1:2] <- c("probe_rev", "probe_id")}
  tabFinal     <- merge(tabResult,tabAnnot,by="probe_id",all.y=T)
  colnames(tabAnnot)[1:2] <- c("probe.x", "probe.y")
  assign("annot", tabAnnot[,1:2], envir=globalenv())
  return(tabFinal)
}

.nameFileOut <- function(dirName, swap, dataTest) {
  # Genere le nom du fichier out
  # Args:
  #   dirName : dossier d'export pour .writeLineOut
  # Returns:
  #   fileOut : le nom du fichier out
  if (dataTest == TRUE) {
    fileOut <- paste(dirName, "AnaDiff", swap, "out.txt", sep="_")
  } else {
    fileOut <- paste(dirName, "AnaDiff", swap, Sys.Date(), "out.txt", sep="_")
  }
  return(fileOut)
}

.nomExportAD <- function(export, sense, dirName, swap, adresse) {  # doc !!!
  # cree une liste de nom pour l'export des fichiers couleur en fonction des formats
  # Args:
  #   rien
  # Returns:
  #   rien
  exportS=paste(export, sense, sep="_")
  dirNameS <- paste(dirName, sense, "/", sep="")
  htmlC <- .nomFichier("_AnaDiff_Couleur.html", dirNameS, swap, exportS)
  texte <- .nomFichier("_AnaDiff.txt", dirNameS, swap, exportS)
  txtWb <- .nomFichier("_AnaDiff_wb.txt", dirNameS, swap, exportS)
  htmlD <- .nomFichier("_AnaDiff.html", dirNameS, swap, exportS)
  bkgI  <- .nomFichier("_bkg_intensities.txt", dirNameS, swap, exportS)
  normI <- .nomFichier("_norm_intensities.txt", dirNameS, swap, exportS)
  data  <- .nomFichier("_AnaDiff_Data.txt", dirNameS, swap, exportS)

  dirNameQC <- paste(dirName, "qualityControl_geoSubmission/", sep="")
  removed <- .nomFichier(paste("_", sense, "_probes_removed.txt", sep=""),
                         paste(dirName, "qualityControl_geoSubmission/", sep=""), swap, export)
  tabDble <- .nomFichier("_AnaDiff.txt", dirName, swap, export)
  colDble <- .nomFichier("_AnaDiff.html", dirName, swap, export)
  return(list(htmlC=htmlC, texte=texte, htmlD=htmlD, bkgI=bkgI, normI=normI, txtWb=txtWb,
              removed=removed, data=data, tabDble=tabDble, colDble=colDble))
}

.nomFichier <- function(texte, dirName, swap, export) {
  t <- paste("_",export,texte, sep="")
  nom <- paste(dirName,swap,t,sep="")
  return(nom)
}

.normIntensite <- function(MA, tab, expName, probe) {
  # Recupere les intensites normalisees par echantillon hybride et les exporte
  # Args:
  #   MA : Le tableau general après normalisation
  #   tab : les probes triees et nettoyées
  #   swap : le nom du swap
  # Returns:
  #   le tableau des intensites normalisees
  print("OK")
  cat("recuperation des intensites normalisees individuelles...\n")
  RGnorm <- RG.MA(MA)    # Retransformer les donnees MA en RG (non log2)
  norm_intensities <- data.frame (RGnorm$genes$ProbeName,log2(RGnorm$R),log2(RGnorm$G))
  name_sample <- c()
  dye         <- c() # pour faire le tri des colonnes ensuite : ctl d'abord puis ttmt apres
  cat("definition des dyes utilises pour chaque echantillon hybride...\n")
  for (maRow in seq(1,nrow(MA$targets))) {
    name_sample <- c(name_sample, paste(MA$targets[maRow,3], "Cy5", sep="_"))
    dye <- c(dye, ifelse(MA$targets[maRow,4]==3, "z", "y"))
  }
  for (maRow in seq(1,nrow(MA$targets))) {
    name_sample <- c(name_sample, paste(MA$targets[maRow,2], "Cy3", sep="_"))
    dye <- c(dye, ifelse(MA$targets[maRow,4]==3, "y", "z"))
  }
  colnames(norm_intensities) <- c("Agilent_id",name_sample)
  dye <- c("Agilent_id",dye)
  norm_intensities <- norm_intensities[,order(dye)]
  if (probe!="") {
    norm_intensities <- .selectProbes(norm_intensities, probe=probe)
    norm_intensities <- merge(tab[,1:2],norm_intensities,by="Agilent_id")
    norm_intensities$mean <- apply(norm_intensities[,-c(1,2)],1,mean)
    norm_intensities <- norm_intensities[which(round(norm_intensities$Amean,5)==round(norm_intensities$mean,5)),]
    norm_intensities <- norm_intensities[!duplicated(norm_intensities[,c('Agilent_id','Amean')]),]
    norm_intensities <- norm_intensities[,-c(2, ncol(norm_intensities))]
    colnames(norm_intensities)[which(colnames(norm_intensities)=="Agilent_id")] <- "probe_id"
  } else {
    norm_intensities <- cbind(MA$genes, norm_intensities[,-1])
  }
  write.table(norm_intensities,expName, quote=F,sep="\t",row.names=F,dec=".")
  return(norm_intensities)
}


.probeRemoved <- function(tab, norm_intensities, expName, fileOut){
  # identifie les sondes ecartees de l'analyse, ajoute les valeur d'intensite et exporte
  # Args:
  #   tab : le tableau de donnee analysees
  #   norm_intensities : le tableau d'intensites normalisees
  #   swap : le nom du swap
  #   dirName : dossier d'export pour .writeLineOut
  # CeQuIlFait:
  #   verifie quelles sondes correspondent a un ratio sup a 1 dont la pval est sup a 0.01
  #   colle derriere les intensites de chaque echantillon pour chaque sonde selectionnee
  # Returns:
  #   rien
  cat("verification des sondes retirees de l'analyse...\n")
  probes_removed <- tab[which(abs(tab$ratio) > 1 & tab$pvalue > 0.01),]
  probes_removed <- merge(probes_removed,norm_intensities[,],by="probe_id")
  .writeLineOut(paste("\nNombre de sondes retirees de l'analyse :", nrow(probes_removed)), fileOut)
  .writeLineOut("   (sondes ayant un ratio absolu superieur a 1 et une pval superieure a 1%)", fileOut)
  probes_removed <- probes_removed[order(probes_removed$probe_id),]
  write.table(probes_removed,expName$removed,quote=F,sep="\t",row.names=F,dec=".")
  return(nrow(probes_removed))
}


.RGmean <- function(RG) {
  RGmean <- new("RGList")
  p <- aggregate(RG$Rb, by=list(RG$genes$ProbeName) ,mean)
  RGmean$R <- as.matrix(aggregate(RG$R, by=list(RG$genes$ProbeName) ,mean)[,-1])
  RGmean$G <- as.matrix(aggregate(RG$G, by=list(RG$genes$ProbeName) ,mean)[,-1])
  RGmean$Rb <- as.matrix(p[,-1])
  RGmean$Gb <- as.matrix(p[,-1])
  RGmean$targets <- RG$targets
  ProbeName <- p[,1]
  RGmean$genes <- as.data.frame(ProbeName)
  return(RGmean)
}

.selectProbes <- function(tab, probe) {
  # selection des sondes et homogeneisation des probe_id des duplicats
  # Args:
  #   designPuce : type de puce utilisee
  #   tab : tableau de donnees avec Agilent_id
  # Returns:
  #   tab : tableau de donnees sans les sondes interne Agilent et avec les probe_id definitifs
  cat("\nSelection des sondes...\n")
  tmp <- tab[0,]
  for (indProbe in probe) {
    l <- nchar(indProbe)
    tmp <- rbind(tmp,tab[which(substr(tab$Agilent_id,1,l)==indProbe),]) # remplacement de 3 par l
  }
  return(tmp)
}

.selectRGProbes <- function(RGlist, probe) {
  RGgenes <- data.frame(RGlist$genes, probeRow = rownames(RGlist$genes))
  RGgenes$Agilent_id <- RGgenes$ProbeName
  RGprobe <- .selectProbes(tab = RGgenes, probe)
  RGtmp <- RGlist[RGlist$genes$ProbeName %in% RGprobe$ProbeName,]
  return(RGtmp)
}

singleTiffCompilation <- function() {
  cat("\nCompilation of raw data files from singleTiff to one multiTiff file\n")

  # setwd("~/Bureau/Matthieu/T2/")
  folder <- try(paste(dirname(file.choose()),"/",sep=""), silent=TRUE)
  testFolder <- FALSE
  if (class(folder)=="character") {
    setwd(folder)
  } else {
    # si pas de sélection du dossier
    testFolder <- TRUE
    message <- paste("\n#########################################\n\n",
                     "You have not selected a folder.\n",
                     "To start the compilation, choose at least one of the files to compile.",
                     "\n\n#########################################\n\n", sep="")
    cat(message)
  }
  if (testFolder) {return("Script stopped") ; stop(call.=F)}
  # Enlever les fichiers qui ne finnissent pas par "GE2_SingleTiff_[1-2]_[1-4]_FEATURES.txt"
  files <- list.files(path=folder, pattern=".txt")
  for (f in files) {
    if (length(grep("GE2_SingleTiff_[1-2]_[1-4]_FEATURES.txt", f))==0) {
      files <- files[-which(files==f)]
    } else {print(f)}
  }

  # Protocole = GE2-SingleTiff_1_4_FEATURES.txt       : 31 caracteres (fileName : len1-30)
  # Protocole = GE2-NonAT_test_1_4_FEATURES.txt       : 31 caracteres (fileName : len1-30)
  # Protocole = GE2-NonAT_SingleTiff_1_4_FEATURES.txt : 37 caracteres (fileName : len1-36)
  # Protocole = GE1_1105_Oct12_1_4.txt                : 22 caracteres (fileName : len1-21) TODO ?

  # 1) faire un sous-groupe par lame
  step1 <- unique(as.character(lapply(files, substr, 1, 12))) # liste des n° de lames

  for (lame in step1) {
    cat("\n------ slide", lame, "------\n")
    dir.create(lame,showWarnings=F)
    setwd(lame)
    group1 <- sort(files[which(substr(files, 1, 12)==lame)]) # liste des fichiers de la lame

    # 2) faire un sous-groupe par puce
    len <- length(group1) # nombre de fichiers pour la lame
    if (len%%2!=0) {len<-len+1} # gestion des nombre impaires

    lpuce <- c() # liste complète des puces par lames
    for (g in group1) {
      lpuce <- c(lpuce,substr(g, (nchar(g)-15), (nchar(g)-13)))
    }
    puces <- unique(sort(lpuce)) # liste des puces unique
    # s'il n'y a pas deux lames par puces, on supprime la puce de la liste
    for (puce in puces) {
      if (sum(lpuce %in% puce) != 2) {puces <- puces[-which(puces==puce)]}
    }

    # entête du fichier de sortie
    ent <- matrix(c("integer", "integer", "integer", "text", "float", "float", "float", "float",
                    "FeatureNum", "Row", "Col", "ProbeName", "rMedianSignal", "rBGMedianSignal",
                    "gMedianSignal", "gBGMedianSignal"),byrow = T, nrow = 2)

    len1 <- nchar(group1[1]) # nombre de caracteres des fichiers dans une couleur
    len2 <- nchar(group1[len/2 + 1]) # nombre de caracteres des fichiers dans l'autre couleur

    for (puce in puces) {
      if (len1==len2) { # si même longueur (ex : cy3/cy5)
        group2 <- group1[which(substr(group1, (len1-15), (len1-13))==puce)] # nom des deux fichiers pour une puce
        puce1 <- group2[1]
        puce2 <- group2[2]
      } else { # sinon (ex : vert/rouge)
        puce1 <- group1[which(substr(group1, (len1-15), (len1-13))==puce)] # nom du fichier dans une couleur
        puce2 <- group1[which(substr(group1, (len2-15), (len2-13))==puce)] # nom du fichier associé dans l'autre couleur
      }
      cat("\n>>> chip", puce, "\n", puce1, "\n", puce2)

      # si problème on passe à la puce suivante
      if (length(puce1)==0 | length(puce2)==0) { # Si l'un des deux nom de fichier est absent (rare)
        cat("missing file\n")
        next
      } else if (puce1==puce2) { # si les deux fichiers ont le même nom
        cat("missing file\n")
        next
      }

      # lecture du premier fichier
      tab1 <- read.table(paste("..", puce1, sep="/"), skip=1, sep="\t", header=T)
      # tab1 pour le rouge et tab2 pour le vert, pour pouvoir mettre les bons noms de colonne
      if ("rMedianSignal" %in% colnames(tab1)) { # tab1 pour le rouge et tab2 pour le vert
        tab2 <- read.table(paste("..", puce2, sep="/"), skip=1, sep="\t", header=T)
      } else if("gMedianSignal" %in% colnames(tab1)) {
        tab2 <- tab1
        tab1 <- read.table(paste("..", puce2, sep="/"), skip=1, sep="\t", header=T)
      } else {
        # si ce n'est pas l'entête de colonne attendu
        message <- paste("\n#########################################\n\n",
                         "Le fichier de données brutes n'a pas le format attendu",
                         "\n\n#########################################\n\n", sep="")
        cat(message)
        stop("Script arrete",call.=F)
      }

      # Construction du tableau de compilation
      tab <- data.frame("FeatureNum"=tab1$FeatureNum,
                        "Row"=tab1$Row,
                        "Col"=tab1$Col,
                        "ProbeName"=tab1$ProbeName,
                        "rMedianSignal"=tab1$rMedianSignal,
                        "rBGMedianSignal"=tab1$rBGMedianSignal,
                        "gMedianSignal"=tab2$gMedianSignal,
                        "gBGMedianSignal"=tab2$gBGMedianSignal)
      colnames(tab) <- c("FeatureNum", "Row", "Col", "ProbeName", "rMedianSignal", "rBGMedianSignal", "gMedianSignal", "gBGMedianSignal")

      # Récupération des parties communes des noms de fichiers
      p1 <- unlist(strsplit(puce1, "")) ; p1
      p2 <- unlist(strsplit(puce2, "")) ; p2
      j <- 0
      x <- T
      while (x & (j!=len1 & j!=len2)) { # recherche de la position où se situe la 1ère différence
        j <- j+1
        x <- p1[j] == p2[j]
      }
      cy1 <- c("y", "Y")
      cy2 <- c("c", "C")
      ifelse(p1[j-1] %in% cy1 & p2[j-1] %in% cy1 & p1[j-2] %in% cy2 & p2[j-2] %in% cy2, j <- j-2, j) # si cy3/cy5, on recule de deux pas
      # Nom du fichier de compilation du type "257046510049_aaaa-mm-jj_xxx_GE2-SingleTiff_1_1.txt"
      fileName <- paste(substr(puce1, 1, j-1), substr(puce1, len1-30, len1-13), substr(puce1, len1-3, len1), sep="")

      # Ecriture du fichier
      write.table(ent, fileName, quote=F, sep="\t", row.names=F, col.names=F, dec=".") # Entête
      write.table(tab, fileName, quote=F, sep="\t", row.names=F, col.names=F, dec=".", append=T) # Valeurs
      cat("\nSaving the file", fileName, "\n")
    }
    # Retour au dossier parent pour passer à la lame suivante
    setwd("..")
  }
  cat("\n The job is done :)\nThanks for using our tools and citing Sandra Pelletier for this job\n")
}

.statAnaDiff <- function(MA, swap, export, fileOut, compare) {
  # Traitement statistique des donnees
  # Args:
  #   MA : donnees normalisees
  #   swap : nom du swap
  #   dirName : dossier d'export pour .writeLineOut
  # Returns:
  #   tableau de donnees analysees avec pval
  cat("\najustement inter-puce en fonction des controles...\n")
  fit    <- lmFit(MA,compare)
  cat("test Bayesien : analyse differentielle...\n")
  fiteB  <- eBayes(fit)
  variance <- round(fiteB$s2.prior,3)
  .writeLineOut(paste("\nVariance du swap : ", variance, "\n"), fileOut)
  tabFit <- data.frame(fiteB$genes,fiteB$Amean,round(fiteB$coefficients,2),fiteB$p.value)
  colnames(tabFit)  <- c("Agilent_id","Amean","ratio","pvalue")
  return(list(tabFit=tabFit, variance=variance))
}

.statBh <- function(tab, popBH, statBH, fileOut) {
  # selection des meilleurs pval et calcul du BH
  # Args:
  #   tab : tableau de donnees avec pvalue
  # Returns:
  #   tab : tableau de donnees analysees avec BH
  if (popBH=="complete") {
    cat("test BH : traitement des faux-positifs, methode \"complete\"...\n")
    tab[[statBH]] <- .kerfdr(tab$pvalue, fileOut)[[statBH]]
  } else {
    cat("test BH : traitement des faux-positifs, methode \"alternate\"...\n")
    tmp <- tab[tab[,2]>=0.5 | tab[,3]>=0.5,]
    tmp[[statBH]] <- .kerfdr(tmp$pvalue, fileOut)[[statBH]]
    tab <- merge(tab,tmp[,c(1,6)], by="probe_id", all.x=T)
    tab[[statBH]][which(is.na(tab[[statBH]]))] <- 1
  }
  return(tab)
}

.statBkg <- function(tab, nbg, dirName, fileOut, targets) {
  # calcul des intensite par echantillon et soustraction du bruit de fond
  # tri le tableau par probe_id et l'enregistre
  # Args:
  #   tab : tableau de donnees avec probe_id, Amean et ratio
  #   nbg : nombre de sonde pour le calcul
  #   dirName : dossier d'export pour .writeLineOut
  #   targets : sous-tableau de array correspondant au swap, provenant de RG$targets
  # Returns:
  #   tab : tableau de donnees avec intensite soustraite du bruit de fond par echantillon
  cat("calcul de l'intensite de l'echantillon controle...\n")
  IGreen    <- tab$Amean - tab$ratio/2
  bkgGreen  <- .calcBkg(IGreen,nbg)
  IGreenBkg <- round(IGreen - bkgGreen$background,2)
  cat("calcul de l'intensite de l'echantillon traitement\n")
  IRed   <- tab$Amean + tab$ratio/2
  bkgRed  <- .calcBkg(IRed,nbg)
  IRedBkg <- round(IRed - bkgRed$background,2)

  stat <- list(ctrl=list(mean=round(bkgGreen$moyenne,4),
                         dev=round(bkgGreen$ecarttype,4),
                         bkg=round(bkgGreen$background,4)),
               ttmt=list(mean=round(bkgRed$moyenne,4),
                         dev=round(bkgRed$ecarttype,4),
                         bkg=round(bkgRed$background,4)))
  .writeLineOut("Valeurs du bruit de fond du swap :\n", fileOut)
  .writeLineOut("          \tControle\tTraitement", fileOut)
  .writeLineOut(paste("moyenne   \t",round(bkgGreen$moyenne,4),"\t",round(bkgRed$moyenne,4)), fileOut)
  .writeLineOut(paste("ecart-type\t",round(bkgGreen$ecarttype,4),"\t",round(bkgRed$ecarttype,4)), fileOut)
  .writeLineOut(paste("background\t",round(bkgGreen$background,4),"\t",round(bkgRed$background,4)), fileOut)

  cat("\nsoustraction du bruit de fond dans le tableau de donnees...\n")
  tab <- data.frame(tab[,1],IGreenBkg,IRedBkg,round(tab[,3:4],4))
  names(tab)[1:3] <- c("probe_id", paste(targets$CtrName[1],".bg",sep=""), paste(targets$TtmtName[1],".bg",sep=""))
  return(list(tab=tab, stat=stat))
}

.statWoBkg <- function(tab, targets) {
  # calcul des intensite par echantillon et soustraction du bruit de fond
  # tri le tableau par probe_id et l'enregistre
  # Args:
  #   tab : tableau de donnees avec probe_id, Amean et ratio
  #   nbg : nombre de sonde pour le calcul
  #   dirName : dossier d'export pour .writeLineOut
  #   targets : sous-tableau de array correspondant au swap, provenant de RG$targets
  # Returns:
  #   tab : tableau de donnees avec intensite soustraite du bruit de fond par echantillon
  cat("calcul de l'intensite de l'echantillon controle...\n")
  IGreen    <- round(tab$Amean - tab$ratio/2, 2)
  cat("calcul de l'intensite de l'echantillon traitement\n")
  IRed   <- round(tab$Amean + tab$ratio/2, 2)
  tab <- data.frame(tab[,1],IGreen,IRed,round(tab[,3:4],4))
  names(tab)[1:3] <- c("probe_id", targets$CtrName[1], targets$TtmtName[1])
  return(tab)
}

.tabColorHexa <- function(tab, statBH) {
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  #   rien
  # Returns:
  #   rien
  gamme <- .gammeCouleurs()
  tabColor <- tab[,2:6]
  tabColor[,1]    <- sapply(tab[,2],    .transformInt,  gammeJaune=gamme$jaune, valeurZero=gamme$noire)
  tabColor[,2]    <- sapply(tab[,3],    .transformInt,  gammeJaune=gamme$jaune, valeurZero=gamme$noire)
  tabColor$ratio  <- sapply(tab$ratio,  .transformRat)
  tabColor$pvalue <- sapply(tab$pvalue, .transformPval, gammeBleue=gamme$bleue, valeurZero=gamme$noire)
  tabColor[[statBH]]<- sapply(tab[[statBH]], .transformBh, gammeBleue=gamme$bleue, valeurZero=gamme$noire)
  return(tabColor)
}

.tabColorWithoutData <- function(tabColor) {
  # cree une liste de vecteur de code couleur hexadecimal pour chaque gamme
  # Args:
  #   tabColor : tableau de couleur apres transformation de tabResult
  # Returns:
  #   rien
  element1 <- "<tr><td>"
  element2 <- "</td><td align=\"center\" bgcolor=\""
  element3 <- "\">"
  element4 <- "</td><td>"
  element5 <- "</td></tr>"

  matColor <- matrix(nrow=nrow(tabColor)+3, ncol=1)
  matColor[1,1] <- "<table>"
  matColor[2,1] <- "<tr></tr>"
  matColor[3:(nrow(tabColor)+2),] <- matrix(paste(element1,
                                                  element2,tabColor[,1],element3,
                                                  element2,tabColor[,2],element3,
                                                  element2,tabColor[,3],element3,
                                                  element2,tabColor[,4],element3,
                                                  element2,tabColor[,5],element3,
                                                  element5,
                                                  sep=""),ncol=1)
  matColor[(nrow(tabColor)+3),1] <- "</table>"
  return(matColor)
}

.tabSAS <- function(annot, tab_sens, tab_antisens, color_sens, color_antisens, sense, expName) {
  byY <- ifelse(sense=="sens", "probe.y", "probe.x")
  byX <- ifelse(sense=="sens", "probe.x", "probe.y")
  tab <- merge(tab_sens[,1:6], annot, by.x="probe_id", by.y=byY, all.y=T)
  tab <- merge(tab, tab_antisens, by.x=byX, by.y="probe_id", all.x=T)
  tab <- tab[,c(2:7,1,8:12,15:ncol(tab))]
  if(sum(duplicated(tab))!=0) {tab <- tab[-which(duplicated(tab)),]}
  ent <- colnames(tab)
  ent[1] <- "transcript.sens"
  ent[7] <- "transcript.antisens"
  ent[2:6] <- paste(substr(ent[2:6], 1, nchar(ent[2:6])-1), "sens", sep="")
  ent[8:12] <- paste(substr(ent[8:12], 1, nchar(ent[8:12])-1), "antisens", sep="")
  colnames(tab) <- ent
  tab <- tab[order(tab$transcript.sens),]
  write.table(tab, expName$tabDble, row.names=F, col.names=T, quote=F, sep="\t")

  tc <- data.frame(ts=color_sens[3:(length(color_sens)-1)], ta=color_antisens[3:(length(color_antisens)-1)])
  lchar <- nchar(as.character(tc[1,1]))
  tc$tc <- paste(substr(tc$ts,1, lchar-5), substr(tc$ta,14, lchar), sep="<td></td>")
  t1 <- data.frame(tf=color_antisens[1:2,1])
  t2 <- data.frame(tf=tc$tc)
  t3 <- data.frame(tf=color_antisens[nrow(color_antisens),1])
  tf <- rbind(t1, t2, t3)
  write.table(tf, expName$colDble, row.names=F, col.names=F, quote=F)
  return(tab)
}

.tabSASayane <- function(annot, probeName, tab_sens, tab_antisens, color_sens, color_antisens, statBH, labelling) {
  probeName <- data.frame("ProbeName"=probeName)
  annot$x <- annot$probe.x %in% probeName$ProbeName
  annot$y <- annot$probe.y %in% probeName$ProbeName
  annot$xy <- annot$x + annot$y
  annot <- annot[which(annot$xy==2), 1:2]

  if (labelling=="direct") {
    tab_sens <- tab_sens[which(tab_sens$probe_id %in% annot$probe.x),]
    tab_antisens <- tab_antisens[which(tab_antisens$probe_id %in% annot$probe.y),]
  } else {
    tab_sens <- tab_sens[which(tab_sens$probe_id %in% annot$probe.y),]
    tab_antisens <- tab_antisens[which(tab_antisens$probe_id %in% annot$probe.x),]
  }

  color_sens <- .tabColorWithoutData(.tabColorHexa(tab_sens, statBH))
  color_antisens <- .tabColorWithoutData(.tabColorHexa(tab_antisens, statBH))

  return(list(annot=annot, color_antisens=color_antisens, color_sens=color_sens, tab_antisens=tab_antisens, tab_sens=tab_sens))
}

.transformBh <- function(bh, gammeBleue=.gammeCouleurs()$bleue, valeurZero=.gammeCouleurs()$noire) {
  # renvoi le code couleur en fonction de la valeur du BH
  # Args:
  #   bh : valeur a transformer
  #   gammeBleue : vecteur de 4 couleurs dans le bleu
  #   valeurZero : valeur pour une intensite nulle
  # Returns:
  #   le code couleur hexadécimal correspondant
  if      (is.na(bh))    {cBh <- "#a0a0a0"}
  else if (bh < 0.00001) {cBh <- gammeBleue[4]}
  else if (bh < 0.01)    {cBh <- gammeBleue[3]}
  else if (bh < 0.05)    {cBh <- gammeBleue[2]}
  else if (bh < 0.1)     {cBh <- gammeBleue[1]}
  else                   {cBh <- valeurZero}
  return(cBh)
}

.transformInt <- function(int, gammeJaune=.gammeCouleurs()$jaune, valeurZero=.gammeCouleurs()$noire) {
  # renvoi le code couleur en fonction de la valeur de l'intensite
  # Args:
  #   int : intensite a transformer
  #   gammeJaune : vecteur de 9 couleurs dans le jaune
  #   valeurZero : valeur pour une intensite nulle
  # CeQuIlFait:
  #   la valeur est arrondie et bornee entre 0 et 9
  # Returns:
  #   le code couleur hexadécimal correspondant
  if (is.na(int)) {cInt <- "#a0a0a0"}
  else {
    int <- round(int)
    int[which(int<0)] <- 0 ; int[which(int>9)] <- 9
    cInt <- ""
    ifelse (int==0, cInt <- valeurZero , cInt <- gammeJaune[int])
  }
  return(cInt)
}

.transformPval <- function(pval, gammeBleue=.gammeCouleurs()$bleue, valeurZero=.gammeCouleurs()$noire) {
  # renvoi le code couleur en fonction de la valeur de la pvalue
  # Args:
  #   pval : valeur a transformer
  #   gammeBleue : vecteur de 4 couleurs dans le bleu
  #   valeurZero : valeur pour une intensite nulle
  # Returns:
  #   le code couleur hexadécimal correspondant
  if      (is.na(pval))    {cPval <- "#a0a0a0"}
  else if (pval < 0.00001) {cPval <- gammeBleue[4]}
  else if (pval < 0.001)   {cPval <- gammeBleue[3]}
  else if (pval < 0.01)    {cPval <- gammeBleue[2]}
  else if (pval < 0.05)    {cPval <- gammeBleue[1]}
  else                     {cPval <- valeurZero}
  return(cPval)
}

.transformRat <- function(rat) {
  # renvoi le code couleur en fonction de la valeur du ratio
  # les ratios s'interpretent sur une gamme logarithmique, la correspondance n'est pas lineaire
  # Args:
  #   rat : valeur du ratio a transformer
  # Returns:
  #   le code couleur hexadécimal correspondant
  if      (is.na(rat))   {cRat <- "#a0a0a0"}
  else if (rat < (-3))   {cRat <- "#00ff00"}
  else if (rat < (-1.5)) {cRat <- "#00aa00"}
  else if (rat < (-1))   {cRat <- "#006600"}
  else if (rat < (-0.5)) {cRat <- "#003300"}
  else if (rat < 0)      {cRat <- "#001100"}
  else if (rat == 0)     {cRat <- "#000000"}
  else if (rat < 0.5)    {cRat <- "#110000"}
  else if (rat < 1)      {cRat <- "#330000"}
  else if (rat < 1.5)    {cRat <- "#770000"}
  else if (rat < 3)      {cRat <- "#aa0000"}
  else                   {cRat <- "#ff0000"}
  return(cRat)
}


