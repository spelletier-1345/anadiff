###############################################
# AnaDiff Agilent
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

.tabRG <- function(swap, arrays, dataTest=NULL) {
  # Lecture des donnees
  # Args:
  #   swap : nom du swap
  # CeQuIlFait:
  #   ouvre les fichiers de donnees definies par arrays
  #   met tous les bruits de fond à 0
  # Returns:
  #   large liste contenant l'ensemble des infos des donnees du swap
  if (is.null(dataTest)) {print("tabRG")}
  target <- arrays[arrays$Swaps==swap,]   # identification des lames à analyser ensemble
  if (!is.null(dataTest)) {target$FileName <- paste(dataTest, target$FileName, sep="")}
  spots  <- list(R="rMedianSignal",G="gMedianSignal",Rb="rBGMedianSignal",Gb="gBGMedianSignal")
  sondes <- c("ProbeName", "Row", "Col")
  cat("\nLecture des donnees brutes...\n")
  RG     <- read.maimages(target,columns=spots,annotation=sondes,verbose=F,encoding="utf-8")   # Lecture des lames
  RG$Rb[,]  <- 0 ; RG$Gb[,] <- 0
  RG$R[RG$R==0] <- 0.5
  RG$G[RG$G==0] <- 0.5
  return(RG)
}
