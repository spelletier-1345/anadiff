###############################################
# AnaDiff Agilent
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

.normIntensite <- function(MA, tab, expName, probe, dataTest=NULL) {
  # Recupere les intensites normalisees par echantillon hybride et les exporte
  # Args:
  #   MA : Le tableau general après normalisation
  #   tab : les probes triees et nettoyées
  #   swap : le nom du swap
  # Returns:
  #   le tableau des intensites normalisees
  if (is.null(dataTest)) {print("normIntensite")}
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
