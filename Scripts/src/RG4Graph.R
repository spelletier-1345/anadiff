###############################################
# AnaDiff Agilent
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

.RG4Graph <- function(RG, labelling, dirName, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("RG4Graph")}
  # Transformation de RG en MA
  MA <- normalizeWithinArrays(RG,method="loess",bc.method="none")
  # Récupération des données de RG et MA en data.frame
  RGdf <- data.frame(RG$genes$ProbeName, RG$genes$Row, RG$genes$Col, RG$R, RG$G)
  MAdf <- data.frame(MA$genes$ProbeName, MA$genes$Row, MA$genes$Col, MA$M, MA$A)
  maxRowCol <- c(max(RG$genes$Row), max(RG$genes$Col))

  # Transformation en log2 des données RG
  RGl2 <- RGdf
  RGl2[,-c(1:3)] <- log2(RGdf[,-c(1:3)])
  # Calcul de M et A pour les RG
  RGMA <- RGdf
  nbCol <- (ncol(RGdf)-3)/2 # Nombre de lames
  for (indNbCol in seq(4,nbCol+3)) {
    RGMA[,indNbCol] <- (RGl2[,indNbCol]-RGl2[,indNbCol+nbCol]) #M
    RGMA[,indNbCol+nbCol] <- (RGl2[,indNbCol]+RGl2[,indNbCol+nbCol])/2 #A
  }
  RGnorm <- RG.MA(MA)    # Retransformer les donnees MA en RG (non log2)
  RGMAdf <- data.frame(RGnorm$genes$ProbeName, RGnorm$genes$Row, RGnorm$genes$Col, RGnorm$R, RGnorm$G)
  MAl2 <- RGMAdf
  MAl2[,-c(1:3)] <- log2(RGMAdf[,-c(1:3)])
  # Récupération des sondes "spikes"
  if (labelling=="direct") {
    RGspikes <- RGMA[which(substr(RGMA$RG.genes.ProbeName,1,3)=="(+)"),]
    spikesRG <- aggregate(RGspikes[,4:(nbCol+3)], by=list(RGspikes$RG.genes.ProbeName), mean)
    MAspikes <- MAdf[which(substr(MAdf$MA.genes.ProbeName,1,3)=="(+)"),]
    spikesMA <- aggregate(MAspikes[,4:(nbCol+3)], by=list(MAspikes$MA.genes.ProbeName), mean)
    cn <- function(tab) {
      p1 <- substring(tab, 10, 13)
      p2 <- substring(tab, nchar(tab)-2, nchar(tab))
      p  <- paste(p1, p2, sep="-")
      return(p)
    }
    spikes <- data.frame(spikes=c("(+)E1A_r60_n9",
                                  "(+)E1A_r60_a135",
                                  "(+)E1A_r60_a107",
                                  "(+)E1A_r60_n11",
                                  "(+)E1A_r60_1",
                                  "(+)E1A_r60_a20",
                                  "(+)E1A_r60_a97",
                                  "(+)E1A_r60_3",
                                  "(+)E1A_r60_a104",
                                  "(+)E1A_r60_a22"),
                         expect=c(-3.322,-1.585,-1.585,-1.585,0,0,1.585,1.585,1.585,3.322))

    for (indColSpikes in seq(2,(ncol(spikesRG)))) {
      spikes <- merge(spikes, spikesRG[,c(1,indColSpikes)], by.x="spikes", by.y="Group.1")
      spikes <- merge(spikes, spikesMA[,c(1,indColSpikes)], by.x="spikes", by.y="Group.1")
      colnames(spikes)[(2*indColSpikes-1)] <- paste(cn(colnames(spikesRG[indColSpikes])), "RG", sep="-")
      colnames(spikes)[(2*indColSpikes)]   <- paste(cn(colnames(spikesMA[indColSpikes])), "MA", sep="-")
    }
    minSpikes = min(spikes[,-1]) ; minSpikes <- ifelse(minSpikes<(-4),minSpikes,-4)
    maxSpikes = max(spikes[,-1]) ; maxSpikes <- ifelse(maxSpikes > 4, maxSpikes, 4)
  } else {
    spikes <- NULL
    minSpikes <- NULL
    maxSpikes <- NULL
  }
  rg4graph=list(RGMA=RGMA, MAdf=MAdf, RGl2=RGl2, MAl2=MAl2, spikes=spikes, nbCol=nbCol, minSpikes=minSpikes, maxSpikes=maxSpikes)
  for (indNbCol in seq(1,nbCol)) {
    .exportQCArray(chip=RG$targets$FileName[indNbCol],
                   smoothScatterRG=.plotRGMA(rg4graph$RGMA, indNbCol, rg4graph$nbCol, state="RG"),
                   smoothScatterMA=.plotRGMA(rg4graph$MAdf, indNbCol, rg4graph$nbCol, state="MA"),
                   histRG=.plotDens(rg4graph$RGl2, rg4graph$nbCol, state="RG"),
                   histMA=.plotDens(rg4graph$MAl2, rg4graph$nbCol, state="MA"),
                   spikesPlot=.plotSpikes(rg4graph$spikes, indNbCol, rg4graph$nbCol, rg4graph$minSpikes, rg4graph$maxSpikes),
                   spikesTab=.tabSpikes(rg4graph$spikes, indNbCol, rg4graph$nbCol),
                   greenLoc=.plotSpot(rg4graph$RGl2, indNbCol, rg4graph$nbCol, "Cy3", maxRowCol),
                   redLoc=.plotSpot(rg4graph$RGl2, indNbCol, rg4graph$nbCol, "Cy5", maxRowCol),
                   diffLoc=.plotSpot(rg4graph$MAdf, indNbCol, rg4graph$nbCol, "diff", maxRowCol),
                   labelling=labelling, dirName=dirName, dataTest=dataTest)
  }
  return(MA)
}
