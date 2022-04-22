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
# Le 20 février 2019 - Sandra PELLETIER
###############################################

.alertes <- function(alertes, fileOut, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("alertes")}
  al <- c()
  attach(alertes, warn.conflicts = F)
  for (alRow in seq(1:nrow(alertes))) {
    if (as.numeric(variance[alRow])>0.09) {
      ai <- paste("\nswap ", swap[alRow], ", genome ", export[alRow], ", sondes ", sens[alRow],
                  "\n\n   Variance = ", variance[alRow], "  (Attendue : <0.06)",
                  " : \n   ATTENTION, la variance ne correspond pas au critère souhaité.",
                  "\n   =>> Prenez contact avec le PTM ANAN.", sep="")
      al <- c(al, ai)
    } else if (as.numeric(variance[alRow])>0.06) {
      ai <- paste("\nswap ", swap[alRow], ", genome ", export[alRow], ", sondes ", sens[alRow],
                  "\n\n   Variance = ", variance[alRow], "  (Attendue : <0.06)",
                  " : \n   Attention, la variance est élevée.", sep="")
      al <- c(al, ai)
    }
    
    if (as.numeric(gRemoved[alRow])>as.numeric(gExpress[alRow])) {
      ai <- paste("\nswap ", swap[alRow], ", genome ", export[alRow], ", sondes ", sens[alRow],
                  "\n\n   ", gRemoved[alRow], " gènes enlevés pour ", gExpress[alRow], " gènes différentiellement exprimés",
                  " : \n   ATTENTION, trop de sondes sont retirées de l'analyse.",
                  "\n   =>> Prenez contact avec le PTM ANAN.", sep="")
      al <- c(al, ai)
    } else if (as.numeric(gRemoved[alRow])>(0.5*as.numeric(gExpress[alRow]))) {
      ai <- paste("\nswap ", swap[alRow], ", genome ", export[alRow], ", sondes ", sens[alRow],
                  "\n\n   ", gRemoved[alRow], " gènes enlevés pour ", gExpress[alRow], " gènes différentiellement exprimés",
                  " : \n   Attention, quantité très importante de sondes retirées.", sep="")
      al <- c(al, ai)
    }
  }
  detach(alertes)
  if (length(al)==0) {
    .writeLineOut("\nBravo, tous les indicateurs sont aux verts, aucune alerte à signaler. :)", fileOut)
  } else {
    .writeLineOut("\n--------------------", fileOut)
    .writeLineOut("  !!! ALERTES !!!", fileOut)
    .writeLineOut("--------------------", fileOut)
    for (i2 in al) {
      .writeLineOut(i2, fileOut)
    }
    .writeLineOut("\n--------------------\n", fileOut)
  }
}
