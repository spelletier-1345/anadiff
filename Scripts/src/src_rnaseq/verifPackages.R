.verifPackages <- function(vPackages) {
  # Verification de l'installation des packages
  # Args: 
  #   vPackages : vecteur des packages à tester
  # CeQuIlFait:
  #   si non installe : arrêt du script et rm de la memoire
  #   si oui : chargement des packages
  # Returns:
  #   rien
  cat("\n-------------------------\n\n")
  cat("Verification des packages...\n")
  for (package in vPackages) {
    if (is.element(package,installed.packages()[,1])) {} 
    else {
      message <- paste("\n#########################################\n\nVous devez telecharger le package ", package, "\n",
                       "pour cela, coller les lignes suivantes :\n\nsource(\"http://bioconductor.org/biocLite.R\")\n",
                       "biocLite(\"", package, "\")\n\n#########################################\n\n", sep="")
      cat("\n-------------------------\n\n")
      cat(message)
      cat("\n-------------------------\n\n")
      rm(list=ls(all=T))
      stop("Script arrete",call.=F)
    }
  }
  for (package in vPackages) {
    library(package,character.only=TRUE)
    cat("chargement de la librairie",package,"\n")
  }
  
  return(NULL)
}