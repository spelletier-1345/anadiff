
# Vérification des scripts d'analyses anaDiffAgilent après modif
# utilise les données dans le dossier data de Rscript
#  - ne génere pas de pdf 
#  - dossier anaDiff avec n° de design dans le dossier data


# Vérifications designs
# anaDiffAgilent("62001", "direct")   # Venise v1
# anaDiffAgilent("62001", "indirect") # Venise v1
anaDiffAgilent("64677", "direct")   # Aryane v1
anaDiffAgilent("64677", "indirect") # Aryane v1
anaDiffAgilent("69670", "direct")   # Orobanche
anaDiffAgilent("69670", "indirect") # Orobanche
# anaDiffAgilent("70158", "direct")   # Aryane v2 sans Venise v1
# anaDiffAgilent("70158", "indirect") # Aryane v2 sans venise v1
anaDiffAgilent("70465", "direct")   # Aryane v2
anaDiffAgilent("70465", "indirect") # Aryane v2
anaDiffAgilent("71114", "direct")   # CatmaIRHS v1
anaDiffAgilent("71114", "indirect") # CatmaIRHS v1
anaDiffAgilent("73146", "direct")   # CatmaIRHS v1 + AltBr v1.2
anaDiffAgilent("73146", "indirect") # CatmaIRHS v1 + AltBr v1.2
anaDiffAgilent("78635", "direct")   # Pyrus v1
anaDiffAgilent("78635", "indirect") # Pyrus v1
anaDiffAgilent("84550", "direct")   # Carrot v1
anaDiffAgilent("84550", "indirect") # Carrot v1
anaDiffAgilent("85275", "direct")   # Malus v1

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