.defineComparisons <- function(myData) {
  # Recupère une liste unique de nom de swap
  # Args: 
  #   arrays : tableau de meta-donnee sur design de la manip
  # Returns:
  #   liste de comparaison uniques
  cat("Definition de la comparaison...\n")
  comparisons <- paste(unique(myData$Comparison))
  return(comparisons)
}