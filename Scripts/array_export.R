###############################################
# AnaDiff Agilent
# Données d'un export
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

export <- c()

export$export <- data$exports

# Sélection des sondes
export$probe  <- data$designList$probe[which(data$designList$export==export)]
export$genome <- data$designList$annotation[which(data$exports==export)]
export$nbg    <- data$designList$nbg[which(data$exports==export)]
export$RGtmp  <- .selectRGProbes(swap$RG, export$probe)
export$RGtmp  <- .RGmean(export$RGtmp)
export$sensTxt <- paste(conf$adresse, export$genome, sep="")
export$probeList <- read.csv(file=export$sensTxt, sep="\t", header=T, encoding="utf-8", check.names=F, as.is=T)[,1:2]


