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

.kerfdr<-function (pv, fileOut, lambda = seq(0, 0.9, 0.05), dataTest=conf$dataTest,
                   cuts = c(1e-04, 0.001, 0.01, 0.025, 0.05, 0.1)) {
  if (!is.null(dataTest)) {print("kerfdr")}
  ## script IPS2 (Véronique Brunaud)
  ## modif de la methode storey
  ## inclusion du etape de smoothing
  pi0.lambda<-apply(as.matrix(lambda), 1, FUN = function(x) mean(pv >= x)/(1 - x))
  pi1<- 1-pi0.lambda
  if(length(lambda)!=1) {
    pi0.spline <- smooth.spline(lambda, pi0.lambda, df = 3)
    pi0 <- max(0, min(predict(pi0.spline, x = max(lambda))$y,1))
    pi1<-1-pi0
  }
  if (pi1 < 0) {
    warning(paste("estimated pi1 =", round(pi1, digit = 4), "set to 0.0"))
    pi1 = 0
  }
  if (pi1 > 1) {
    warning(paste("estimated pi1 =", round(pi1, digit = 4), "set to 1.0"))
    pi1 = 1
  }
  pi0 = 1 - pi1
  ## modification pour tenir compte de la correction par pi0
  bon.pi0 = (pv *pi0 ) * length(pv)
  bon.pi0[which(bon.pi0 > 1)] = 1
  ## modification pour tenir compte de la correction par pi0
  bh.pi0 = (pv*pi0) * length(pv)/rank(pv)
  bh.pi0[which(bh.pi0 > 1)] = 1
  ## modification pour tenir compte de la correction par pi0
  bon = (pv *1 ) * length(pv)
  bon[which(bon > 1)] = 1
  ## modification pour tenir compte de la correction par pi0
  bh = (pv*1) * length(pv)/rank(pv)
  bh[which(bh > 1)] = 1
  counts <- sapply(cuts,
                   function(x) c(`p-value` = sum(pv <= x),
                                 `FWER (Bonf.)` = sum(bon <= x),
                                 `FWER (Bonf. with m0)` = sum(bon.pi0 <= x),
                                 `FDR (BH)` = sum(bh <= x),
                                 `qvalue` = sum(bh.pi0 <= x)))
  colnames(counts) <- paste("<=", cuts, sep = "")
  #cat("\n") ; print(counts) ;cat("\n")
  tab <- rbind(colnames(counts),counts)
  tab <- cbind(rownames(tab),tab)
  tab <- apply(tab, 2, format)
  .writeLineOut("\nContrôle des faux positifs :\n",fileOut)
  write.table(tab, fileOut, append=TRUE, quote=FALSE, row.names=FALSE, col.names=FALSE)
  print(tab[-1,-1],quote=F) ; cat("\n")
  results = list(pv = pv, pi0 = pi0, pi1 = pi1, BH=bh, Bonferroni=bon,Bonferroni.with.m0=bon.pi0,
                 qvalue=bh.pi0,summary = counts)
  return(results)
}
