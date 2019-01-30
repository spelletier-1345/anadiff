###############################################
# AnaDiff Agilent
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

.RGmean <- function(RG, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("RGmean")}
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
