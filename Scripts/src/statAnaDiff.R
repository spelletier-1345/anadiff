###############################################
# AnaDiff Agilent
# Le 30 janvier 2018 - Sandra PELLETIER
###############################################

.statAnaDiff <- function(MA, swap, export, fileOut, compare, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("statAnaDiff")}
  # Traitement statistique des donnees
  # Args:
  #   MA : donnees normalisees
  #   swap : nom du swap
  #   dirName : dossier d'export pour .writeLineOut
  # Returns:
  #   tableau de donnees analysees avec pval
  cat("\najustement inter-puce en fonction des controles...\n")
  fit    <- lmFit(MA,compare)
  cat("test Bayesien : analyse differentielle...\n")
  fiteB  <- eBayes(fit)
  variance <- round(fiteB$s2.prior,3)
  .writeLineOut(paste("\nVariance du swap : ", variance, "\n"), fileOut)
  tabFit <- data.frame(fiteB$genes,fiteB$Amean,round(fiteB$coefficients,2),fiteB$p.value)
  colnames(tabFit)  <- c("Agilent_id","Amean","ratio","pvalue")
  return(list(tabFit=tabFit, variance=variance))
}
