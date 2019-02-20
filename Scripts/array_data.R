###############################################
# AnaDiff Agilent
# Données de test
# Le 9 janvier 2019 - Sandra PELLETIER
###############################################

data$arraysTxt <- read.table(paste(conf$folderArray, collapse = ""), header=T, encoding="utf-8")
data$swaps <- paste(unique(data$arraysTxt$Swaps))
data$designList <- .designVersion(data$designPuce, conf$adresse)
data$exports = data$designList$export
data$alertes <- data.frame(matrix(vector(),nrow=0, ncol=6,
                                  dimnames=list(c(), c("swap", "export", "sens", "variance", "gRemoved", "gExpress"))))

