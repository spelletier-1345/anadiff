###############################################
# AnaDiff Agilent
# Données d'un swap
# Le 23 janvier 2019 - Sandra PELLETIER
###############################################
swap <- c()
swap$dirName <- .creationAnaDiff(conf$dataTest, data$designPuce, sw, data$labelling)
swap$fileOut <- .creationFileOut(swap$dirName, sw, conf$dataTest)
swap$arrays <- as.data.frame(.defineArrays(swap$dirName, swap$fileOut, conf$folderArray[[2]], conf$dataTest))

swap$RG <- .tabRG(sw, swap$arrays, conf$dataTest)
if (data$designPuce=="64677") {swap$RG$genes$ProbeName <- substr(swap$RG$genes$ProbeName,1,18)} # TODO : test 64677
swap$compare <- .defineCompare(sw, swap$RG$targets, swap$fileOut)

# JSON
swap$json <- list(swap=list(
  name=swap$swap,
  design=data$designPuce,
  labelling=data$labelling,
  exports=data$exports,
  fileArray=swap$arrays[,1:4],
  ctrlName=swap$arrays$CtrName[1],
  ttmtName=swap$arrays$TtmtName[1]))

# Alertes du swap
swap$alerte <-data.frame(matrix(vector(),nrow=0, ncol=6,
                         dimnames=list(c(), c("swap", "export", "sens", "variance", "gRemoved", "gExpress"))))
