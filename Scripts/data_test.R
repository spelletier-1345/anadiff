###############################################
# AnaDiff Agilent
# Données de test
# Le 9 janvier 2018 - Sandra PELLETIER
###############################################

data <- c()

data$designPuce <- "84550" # Carrot v1
data$labelling <- "direct"

data$arraysTxt <- read.table(paste(conf$folderArray, collapse = ""), header=T, encoding="utf-8")
data$swaps <- paste(unique(data$arraysTxt$Swaps))
data$designList <- .designVersion(data$designPuce, conf$adresse, conf$dataTest)
data$exports = data$designList$export
data$alertes <- data.frame(matrix(vector(),nrow=0, ncol=6,
                                  dimnames=list(c(),c("swap", "export", "sens", "variance", "gRemoved", "gExpress"))))
# Swap
data$swap <- data$swaps[1]
data$dirName <- .creationAnaDiff(conf$dataTest, data$designPuce, data$swap, data$labelling)
data$fileOut <- .creationFileOut(data$dirName, data$swap, conf$dataTest)
data$arrays <- as.data.frame(.defineArrays(data$dirName, data$fileOut, conf$folderArray[[2]], conf$dataTest))

data$RG <- .tabRG(data$swap, data$arrays, conf$dataTest)
if (data$designPuce=="64677") {data$RG$genes$ProbeName <- substr(data$RG$genes$ProbeName,1,18)} # TODO : test 64677
data$compare <- .defineCompare(data$swap, data$RG$targets, data$fileOut)
data$alerte <- data.frame(matrix(vector(),nrow=0, ncol=6,
                            dimnames=list(c(),c("swap", "export", "sens", "variance", "gRemoved", "gExpress"))))
data$rg4graph <- .RG4Graph(RG=data$RG, labelling=data$labelling, dirName=data$dirName, dataTest=conf$dataTest)

print("ok")