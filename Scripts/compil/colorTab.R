.tabColorHexa <- function(tab, statBH, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("tabColorHexa")}
  gamme <- .gammeCouleurs()
  tabColor <- tab[,2:6]
  tabColor[,1]    <- sapply(tab[,2],    .transformInt,  gammeJaune=gamme$jaune, valeurZero=gamme$noire)
  tabColor[,2]    <- sapply(tab[,3],    .transformInt,  gammeJaune=gamme$jaune, valeurZero=gamme$noire)
  tabColor$ratio  <- sapply(tab$ratio,  .transformRat)
  tabColor$pvalue <- sapply(tab$pvalue, .transformPval, gammeBleue=gamme$bleue, valeurZero=gamme$noire)
  tabColor[[statBH]]<- sapply(tab[[statBH]], .transformBh, gammeBleue=gamme$bleue, valeurZero=gamme$noire)
  return(tabColor)
}
.tabColorWithoutData <- function(tabColor, dataTest=conf$dataTest) {
  if (!is.null(dataTest)) {print("tabColorWithoutData")}
  element1 <- "<tr><td>"
  element2 <- "</td><td align=\"center\" bgcolor=\""
  element3 <- "\">"
  element4 <- "</td><td>"
  element5 <- "</td></tr>"
  matColor <- matrix(nrow=nrow(tabColor)+3, ncol=1)
  matColor[1,1] <- "<table>"
  matColor[2,1] <- "<tr></tr>"
  matColor[3:(nrow(tabColor)+2),] <- matrix(paste(element1,
                                                  element2,tabColor[,1],element3,
                                                  element2,tabColor[,2],element3,
                                                  element2,tabColor[,3],element3,
                                                  element2,tabColor[,4],element3,
                                                  element2,tabColor[,5],element3,
                                                  element5,
                                                  sep=""),ncol=1)
  matColor[(nrow(tabColor)+3),1] <- "</table>"
  return(matColor)
}
