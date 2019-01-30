###############################################
# AnaDiff Agilent
# Le 30 janvier 2018 - Sandra PELLETIER
###############################################

.statBh <- function(tab, popBH, statBH, fileOut, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("statBh")}
  # selection des meilleurs pval et calcul du BH
  # Args:
  #   tab : tableau de donnees avec pvalue
  # Returns:
  #   tab : tableau de donnees analysees avec BH
  if (popBH=="complete") {
    cat("test BH : traitement des faux-positifs, methode \"complete\"...\n")
    tab[[statBH]] <- .kerfdr(tab$pvalue, fileOut)[[statBH]]
  } else {
    cat("test BH : traitement des faux-positifs, methode \"alternate\"...\n")
    tmp <- tab[tab[,2]>=0.5 | tab[,3]>=0.5,]
    tmp[[statBH]] <- .kerfdr(tmp$pvalue, fileOut)[[statBH]]
    tab <- merge(tab,tmp[,c(1,6)], by="probe_id", all.x=T)
    tab[[statBH]][which(is.na(tab[[statBH]]))] <- 1
  }
  return(tab)
}
