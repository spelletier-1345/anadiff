###############################################
# AnaDiff Agilent
# Données d'un swap
# Le 30 janvier 2018 - Sandra PELLETIER
###############################################

# JSON
swap$json <- list(swap=list(
  name=swap$swap,
  design=data$designPuce,
  labelling=data$labelling,
  exports=data$exports,
  fileArray=swap$arrays[,1:4],
  ctrlName=swap$arrays$CtrName[1],
  ttmtName=swap$arrays$TtmtName[1]))
