###############################################
# AnaDiff Agilent
# Le 9 janvier 2019 - Sandra PELLETIER
###############################################

.verifPackages <- function(vPackages) {
  verif <- c()
  for (package in vPackages) {
    if (is.element(package,installed.packages())) {
      verif <- verif
    } else {
      verif <- c(verif, package)
    }
  }
  if (length(verif)!=0) {
    for (package in vPackages) {
      if (is.element(package,installed.packages()[,1])) {}
      else {
        message <- paste("\nVous devez telecharger le package ", package, "\n",
                         "pour cela, collez les lignes souivantes :\n\nsource(\"http://bioconductor.org/biocLite.R\")\n",
                         "biocLite(\"", package, "\")\n\n", sep="")
        cat(message)
        rm(list=ls(all=T))
      }
    }
    return("verifPackages annule")
    stop(call.=F)
  } else {
    for (package in vPackages) {
      library(package,character.only=TRUE)
      cat("chargement de la librairie",package,"\n")
    }
  }
  return(length(verif))
}
