###############################################
# AnaDiff Agilent
# Données d'un export
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

export <- c()

# Sélection des sondes
export$probe  <- data$designList$probe[which(data$designList$export==ex)]
export$nbg    <- data$designList$nbg[which(data$exports==ex)]
export$RGtmp  <- .selectRGProbes(swap$RG, export$probe)
export$RGtmp  <- .RGmean(export$RGtmp)
export$annot <- data$designList$annotation[which(data$exports==ex)]
export$annot <- paste(conf$adresse, export$annot, sep="")
export$probeList <- read.csv(file=export$annot, sep="\t", header=T, encoding="utf-8", check.names=F, as.is=T)[,1:2]