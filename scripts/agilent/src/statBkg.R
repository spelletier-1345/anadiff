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

.statBkg <- function(tab, nbg, fileOut, targets, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("statBkg")}
  # calcul des intensite par echantillon et soustraction du bruit de fond
  # tri le tableau par probe_id et l'enregistre
  # Args:
  #   tab : tableau de donnees avec probe_id, Amean et ratio
  #   nbg : nombre de sonde pour le calcul
  #   targets : sous-tableau de array correspondant au swap, provenant de RG$targets
  # Returns:
  #   tab : tableau de donnees avec intensite soustraite du bruit de fond par echantillon
  cat("calcul de l'intensite de l'echantillon controle...\n")
  IGreen    <- tab$Amean - tab$ratio/2
  bkgGreen  <- .calcBkg(IGreen,nbg)
  IGreenBkg <- round(IGreen - bkgGreen$background,2)
  cat("calcul de l'intensite de l'echantillon traitement\n")
  IRed   <- tab$Amean + tab$ratio/2
  bkgRed  <- .calcBkg(IRed,nbg)
  IRedBkg <- round(IRed - bkgRed$background,2)

  stat <- list(ctrl=list(mean=round(bkgGreen$moyenne,4),
                         dev=round(bkgGreen$ecarttype,4),
                         bkg=round(bkgGreen$background,4)),
               ttmt=list(mean=round(bkgRed$moyenne,4),
                         dev=round(bkgRed$ecarttype,4),
                         bkg=round(bkgRed$background,4)))
  .writeLineOut("Valeurs du bruit de fond du swap :\n", fileOut)
  .writeLineOut("          \tControle\tTraitement", fileOut)
  .writeLineOut(paste("moyenne   \t",round(bkgGreen$moyenne,4),"\t",round(bkgRed$moyenne,4)), fileOut)
  .writeLineOut(paste("ecart-type\t",round(bkgGreen$ecarttype,4),"\t",round(bkgRed$ecarttype,4)), fileOut)
  .writeLineOut(paste("background\t",round(bkgGreen$background,4),"\t",round(bkgRed$background,4)), fileOut)

  cat("\nsoustraction du bruit de fond dans le tableau de donnees...\n")
  tab <- data.frame(tab[,1],IGreenBkg,IRedBkg,round(tab[,3:4],4))
  names(tab)[1:3] <- c("probe_id", paste(targets$CtrName[1],".bg",sep=""), paste(targets$TtmtName[1],".bg",sep=""))
  return(list(tab=tab, stat=stat))
}
