###############################################
# AnaDiff Agilent
# Données d'un swap
# Le 9 janvier 2018 - Sandra PELLETIER
###############################################

swap <- c()

swap$swap <- data$swaps[1]

swap$dirName <- .creationAnaDiff(conf$dataTest, data$designPuce, swap$swap, data$labelling)
swap$fileOut <- .creationFileOut(swap$dirName, swap$swap, conf$dataTest)
swap$arrays <- as.data.frame(.defineArrays(swap$dirName, swap$fileOut, conf$folderArray[[2]], conf$dataTest))

swap$RG <- .tabRG(swap$swap, swap$arrays, conf$dataTest)
if (data$designPuce=="64677") {swap$RG$genes$ProbeName <- substr(swap$RG$genes$ProbeName,1,18)} # TODO : test 64677
swap$compare <- .defineCompare(swap$swap, swap$RG$targets, swap$fileOut)
swap$alerte <- data.frame(matrix(vector(),nrow=0, ncol=6,
                                 dimnames=list(c(),c("swap", "export", "sens", "variance", "gRemoved", "gExpress"))))

# Graph, et soumission Geo
rg4graph <- .RG4Graph(RG=swap$RG, labelling=data$labelling, dirName=swap$dirName, dataTest=conf$dataTest)
expInt <- .nomFichier("geoSubmission.txt", paste(swap$dirName, "qualityControl_geoSubmission/", sep=""),
                      swap$swap, export="")
geo <- .normIntensite(rg4graph, "", expInt, "")
rm(rg4graph, expInt, geo)

# JSON
swap$json <- list(swap=list(
  name=swap$swap,
  design=data$designPuce,
  labelling=data$labelling,
  exports=data$exports,
  fileArray=swap$arrays[,1:4],
  ctrlName=swap$arrays$CtrName[1],
  ttmtName=swap$arrays$TtmtName[1]))
