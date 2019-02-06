###############################################
# AnaDiff Agilent
# stats bkg, fdr et normalisation pour un sens
# Le 30 janvier 2019 - Sandra PELLETIER
###############################################

sens$statBkg <- .statBkg(sens$normalize$tabResult, export$nbg, swap$fileOut, targets = sens$normalize$MA$targets)
sens$normalize$tabResult <- sens$statBkg$tab
sens$normalize$tabResult <- .statBh(sens$normalize$tabResult, data$popBH, data$statBH, swap$fileOut)

if (!data$bkgCalculation) {
  tabWoBkg <- .statWoBkg(sens$tabResultComplet, sens$normalize$MA$targets)
  tabResultWoBkg <- sens$normalize$tabResult
  tabResultWoBkg[,2:3] <- tabWoBkg[,2,3]
  tabResultWoBkg$sens <- sens$sensLabel
  tabFinalWoBkg <- .mergeAnnot(tabResultWoBkg, export$annot, se)
  tabFinalWoBkg <- tabFinalWoBkg[order(tabFinalWoBkg$probe_id),]
  write.table(tabFinalWoBkg, sens$expName$txtWb, row.names=F, col.names=T, quote=F, sep="\t", dec=data$dec)
  rm(tabWoBkg, tabFinalWoBkg, tabResultWoBkg)
}

sens$normIntensities <- .normIntensite(sens$normalize$MA, sens$tabResultComplet, sens$expName$normI, export$probe)
.bkgIntensite(sens$normIntensities, as.numeric(export$nbg), sens$expName$bkgI)
sens$probesRemoved <- .probeRemoved(sens$normalize$tabResult, sens$normIntensities, sens$expName$removed, swap$fileOut)

sens$stat <- .exportAnaDiff(export$annot, sens$normalize$tabResult, conf$adresse, sens$expName$texte, sens$expName$htmlC,
                       swap$fileOut, se, sens$sensLabel, data$statBH) # !! senseStep ET sense

data$alertes[(nrow(data$alertes)+1),] <- c(sw, ex, se, sens$normalize$var, sens$probesRemoved, sens$stat$pval$nombre_de_sondes)


print("ok 06/02/2019")
