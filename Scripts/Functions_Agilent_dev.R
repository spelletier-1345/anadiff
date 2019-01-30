
# Vérification des scripts d'analyses anaDiffAgilent après modif
# utilise les données dans le dossier data de Rscript
#  - ne génere pas de pdf 
#  - dossier anaDiff avec n° de design dans le dossier data


# Vérifications designs
anaDiffAgilent("62001", "direct")   # Venise v1
anaDiffAgilent("62001", "indirect") # Venise v1
anaDiffAgilent("64677", "direct", dataTest=TRUE)   # Aryane v1
anaDiffAgilent("64677", "indirect", dataTest=TRUE) # Aryane v1
anaDiffAgilent("69670", "direct", dataTest=TRUE)   # Orobanche
anaDiffAgilent("69670", "indirect", dataTest=TRUE) # Orobanche
anaDiffAgilent("70158", "direct", dataTest=TRUE)   # Aryane v2 sans Venise v1
anaDiffAgilent("70158", "indirect", dataTest=TRUE) # Aryane v2 sans venise v1
anaDiffAgilent("70465", "direct", dataTest=TRUE)   # Aryane v2
anaDiffAgilent("70465", "indirect", dataTest=TRUE) # Aryane v2
anaDiffAgilent("71114", "direct", dataTest=TRUE)   # CatmaIRHS v1
anaDiffAgilent("71114", "indirect", dataTest=TRUE) # CatmaIRHS v1
anaDiffAgilent("73146", "direct", dataTest=TRUE)   # CatmaIRHS v1 + AltBr v1.2
anaDiffAgilent("73146", "indirect", dataTest=TRUE) # CatmaIRHS v1 + AltBr v1.2
anaDiffAgilent("78635", "direct", dataTest=TRUE)   # Pyrus v1
anaDiffAgilent("78635", "indirect", dataTest=TRUE) # Pyrus v1
anaDiffAgilent("84550", "direct", dataTest=TRUE)   # Carrot v1
anaDiffAgilent("84550", "indirect", dataTest=TRUE) # Carrot v1
anaDiffAgilent("85275", "direct", dataTest=TRUE)   # Malus v1

#design <- commandArgs(trailingOnly=TRUE)

#### Sources ####

source("http://147.99.112.52/rscripts/Tools/Agilent/Functions/Functions_Agilent_v4.1.R")
source("http://pegasus-bioinfo.angers-nantes.inra.fr/rscripts/AnaDiff_Agilent.R")

designPuce <- "64677" # Aryane v1
designPuce <- "69670" # Orobanche
designPuce <- "70158" # Aryane v2 sans venise v1
designPuce <- "70465" # Aryane v2
designPuce <- "71114" # CatmaIRHS v1
designPuce <- "73146" # CatmaIRHS v1 + AltBr v1.2
designPuce <- "78635" # Pyrus v1
designPuce <- "84550" # Carrot v1
designPuce <- "85275" # Malus v1
designPuce <- "85372" # Medicago v1

#### Pas à pas ####

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


###################

verifPackage <- .verifPackages(c("limma", "httr"))!=0

labelling="direct"
dataTest=FALSE
adresse <- .adresse(dev=TRUE)
adresse <- "http://147.99.112.52/rscripts/Tools/Agilent/"
message <- "Selection du repertoire de travail"
folderArray <- .selectionDossier(message)

arraysTxt <- read.table(folderArray[[2]], header=T, encoding="utf-8")
swaps <- paste(unique(arraysTxt$Swaps))
designList <- .designVersion(designPuce, adresse, dataTest)
swap=swaps[1]
dirName <- .creationAnaDiff(dataTest, designPuce, swap)
fileOut <- .creationFileOut(dirName, swap, dataTest)
arrays <- as.data.frame(.defineArrays(dirName, fileOut, folderArray[[2]]))
RG <- .tabRG(swap, arrays)
compare <- .defineCompare(swap, RG$targets, fileOut)

exports = designList$export
export=exports[1]

probe <- designList$probe[which(designList$export==export)]
genome  <- designList$annotation[which(exports==export)]
nbg  <- designList$nbg[which(exports==export)]
RGtmp <- .selectRGProbes(RG, probe)
RGtmp1 <- RGtmp
RGtmp <- .RGmean(RGtmp)
sensTxt <- paste(adresse, genome, sep="")
probeListe <- read.csv(file=sensTxt,sep="\t",header=T,encoding="utf-8", check.names=F, as.is=T)[,1:2]

senseStep="sens"
dec="."
popBH="alternate"
statBH="BH"
bkgCalculation=T

# alerte <- data.frame(matrix(vector(),nrow=0, ncol=6, 
#                             dimnames=list(c(),c("swap", "export", "sens", "variance", "gRemoved", "gExpress"))))
# json <- list(swap=list(name=swap, design=designPuce, labelling=labelling, exports=exports,
#                        fileArray=arrays[,1:4], ctrlName=arrays[1,6], ttmtName=arrays[1,7]))

#### Soustraction d'un tableau par un autre ####

test <- !(tab_antisens$probe_rev %in% tab_sens$probe_id)
t <- tab_antisens[which(test),]

#### Nouveau scanner ####

setwd("~/irhs001-sites/rscripts/Tools/Agilent/Data/InopsisScanner/CarottA-lame2-H1I2")
inopsis <- read.table("2018-03-20_10h33m17_258455010007_H1I2-48h-I_0005.txt", skip = 31, header=T)

#### Fonctions de test ####

.folderArrayTest <- function(design) {
  #dataFolder <- "/home/spelletier/mnt/irhs001-var/sites/rscripts/Agilent/data/"
  dataFolder <- "/home/spelletier/Bureau/Clementine/"
  #fileArray <- paste("arrays_", design, ".txt", sep="")
  fileArray <- paste("arrays.txt", sep="")
  return(list(dataFolder,fileArray))
}

.loadPackage <- function(vPackages) {
  for (package in vPackages) { msg.trap <- capture.output(suppressMessages(library(package, character.only = TRUE))) }
  return(NULL)
}