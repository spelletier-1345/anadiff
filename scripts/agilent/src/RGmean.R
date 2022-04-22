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
# Le 23 janvier 2019 - Sandra PELLETIER
###############################################

.RGmean <- function(RG, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("RGmean")}
  RGmean <- new("RGList")
  p <- aggregate(RG$Rb, by=list(RG$genes$ProbeName) ,mean)
  RGmean$R <- as.matrix(aggregate(RG$R, by=list(RG$genes$ProbeName) ,mean)[,-1])
  RGmean$G <- as.matrix(aggregate(RG$G, by=list(RG$genes$ProbeName) ,mean)[,-1])
  RGmean$Rb <- as.matrix(p[,-1])
  RGmean$Gb <- as.matrix(p[,-1])
  RGmean$targets <- RG$targets
  ProbeName <- p[,1]
  RGmean$genes <- as.data.frame(ProbeName)
  return(RGmean)
}
