###############################################
# AnaDiff Agilent
# Le 23 janvier 2018 - Sandra PELLETIER
###############################################

.selectProbes <- function(tab, probe, dataTest=NULL) {
  # selection des sondes et homogeneisation des probe_id des duplicats
  # Args:
  #   designPuce : type de puce utilisee
  #   tab : tableau de donnees avec Agilent_id
  # Returns:
  #   tab : tableau de donnees sans les sondes interne Agilent et avec les probe_id definitifs
  if (is.null(dataTest)) {print("selectProbes")}
  cat("\nSelection des sondes...\n")
  tmp <- tab[0,]
  for (indProbe in probe) {
    l <- nchar(indProbe)
    tmp <- rbind(tmp,tab[which(substr(tab$Agilent_id,1,l)==indProbe),]) # remplacement de 3 par l
  }
  return(tmp)
}
