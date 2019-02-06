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
                           dataTest=conf$dataTest, bkgCalculation = TRUE) {
  if (!is.null(dataTest)) {print("anaDiffAgilent")}

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


.alertes <- function(alertes, fileOut, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("alertes")}
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

inopsisFileDivision <- function(dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("inopsisFileDivision")}
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

.nameFileOut <- function(dirName, swap, dataTest, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("nameFileOut")}
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




singleTiffCompilation <- function(dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("singleTiffCompilation")}
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



.tabSAS <- function(annot, tab_sens, tab_antisens, color_sens, color_antisens, 
                    sense, expName, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("tabSAS")}
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

.tabSASayane <- function(annot, probeName, tab_sens, tab_antisens, color_sens, color_antisens,
                         statBH, labelling, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("tabSASayane")}
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

  return(list(annot=annot, color_antisens=color_antisens, color_sens=color_sens,
              tab_antisens=tab_antisens, tab_sens=tab_sens))
}



