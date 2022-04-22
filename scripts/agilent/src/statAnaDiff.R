#!/usr/bin/env Rscript

# Copyright Sandra PELLETIER ([2022-04-20])
# 
# [sandra.pelletier@inrae.fr]
# 
# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software.  You can  use, 
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info". 
# 
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.

###############################################
# AnaDiff Agilent
# Le 30 janvier 2019 - Sandra PELLETIER
###############################################

.statAnaDiff <- function(MA, fileOut, compare, dataTest=conf$dataTest) {
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
