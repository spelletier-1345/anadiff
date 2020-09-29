.countTable <- function(nameFiles) {
  # Fonction de base : 
  #   On enlève la ligne qui contient x
  #   On récupère les colonnes 1 et y
  cT <- function(counTable, x,y) {
    countTable <- countTable[-which(countTable$V1==x),c(1,y)]
    for (nameFile in nameFiles[-1]) {
      dataTable  <- read.table(nameFile,sep="\t")[,c(1,y)]
      countTable <- merge(countTable,dataTable,by="V1",all.x=T)
    }
    rm(dataTable)
    return(countTable)
  }
  # Lecture du premier fichier
  countTable <- read.table(nameFiles[1],sep="\t")
  # Si données count (SamTools), alors fin * à la dernière ligne et 4 colonnes.
  if (sum(countTable=="*")==1) {
    countTable <- countTable[-which(countTable$V1=="*"),c(1,3)]
    for (nameFile in nameFiles[-1]) {
      dataTable  <- read.table(nameFile,sep="\t")[,c(1,3)]
      countTable <- merge(countTable,dataTable,by="V1",all.x=T)
    }
    rm(dataTable)
  }
  # Si données sf (Salmon), alors entête ("Name") et 5 colonnes.
  if (sum(countTable=="Name")==1) {
    countTable <- read.table(nameFiles[1],sep="\t", header=T, 
                             colClasses=c("character", rep("numeric", 4)))[,c(1,5)]
    for (nameFile in nameFiles[-1]) {
      dataTable  <- read.table(nameFile,sep="\t", header=T, 
                               colClasses=c("character", rep("numeric", 4)))[,c(1,5)]
      countTable <- merge(countTable,dataTable,by="Name",all.x=T)
    }
    rm(dataTable)
    countTable[,-1] <- apply(countTable[,-1], 2, round) 
  }
  colnames(countTable) <- c("id", nameFiles)
  rownames(countTable) <- countTable$id # Identification des noms de ligne
  countTable <- countTable[,-1]
  return(countTable)
}