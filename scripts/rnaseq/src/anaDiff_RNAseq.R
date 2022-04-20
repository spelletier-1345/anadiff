# AnaDiff_RNAseq.R
# Script d'entrée
# Mise à jour le 20-04-2022
# Auteure : Sandra PELLETIER - BIDefI team - IRHS unit

source("./src/functionAnaDiffRNAseq.R")

anaDiff_RNAseq <- function() {
  R.home()
  localOpt <- options()
  options(warn=-1)
  tryCatch(anaDiff <- .anaDiffRNAseq(),
           error = function(e) {
             options(localOpt); warning("anaDiffRNAseq error")
             print("An error has occurred. Please check your summary file.")
             print("If you use reads count files produced by a tool that does not come from the IRHS BioInfo team, please contact the team by email (<contact-bioinfo-irhs@inra.fr>) specifying the error next :");
             print("")
             print(e);
           })
  if (anaDiff == "error") {
    cat("\n\n#################################
        \n###   Analyses interrompues   ###
        \n#################################
        \nAu moins un fichier est manquant.\nMerci de vérifier votre fichier summary.")
  } else {
    cat("\n\n################################\n ###   Analyses terminees   ###\n################################\n\n")
  }
  options(localOpt)
  return("done")
}
