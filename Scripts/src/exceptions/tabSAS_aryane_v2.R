###############################################
# AnaDiff Agilent
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.tabSASayane <- function(annot, probeName, tab_sens, tab_antisens, color_sens, color_antisens,
                         statBH, labelling, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("tabSASayane")}
  probeName <- data.frame("ProbeName"=probeName)
  annot$x <- annot$probe.x %in% probeName$ProbeName
  annot$y <- annot$probe.y %in% probeName$ProbeName
  annot$xy <- annot$x + annot$y
  annot <- annot[which(annot$xy==2), 1:2]
  
  if (labelling=="direct") {
    tab_sens <- tab_sens[which(tab_sens$probe_id %in% annot$probe.x),]
    tab_antisens <- tab_antisens[which(tab_antisens$probe_id %in% annot$probe.y),]
  } else {
    tab_sens <- tab_sens[which(tab_sens$probe_id %in% annot$probe.y),]
    tab_antisens <- tab_antisens[which(tab_antisens$probe_id %in% annot$probe.x),]
  }
  color_sens <- .tabColorWithoutData(.tabColorHexa(tab_sens, statBH))
  color_antisens <- .tabColorWithoutData(.tabColorHexa(tab_antisens, statBH))
  
  assign("tab_sens", tab_sens, envir=globalenv())
  assign("tab_antisens", tab_antisens, envir=globalenv())
  assign("color_sens", color_sens, envir=globalenv())
  assign("color_antisens", color_antisens, envir=globalenv())
  
  return(NULL)
}
