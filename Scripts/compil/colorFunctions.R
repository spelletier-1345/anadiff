.gammeCouleurs <- function() {
  paletteBleue <- colorRampPalette(c("#0000FF","#79F8F8"))(4)
  paletteJaune <- colorRampPalette(c("#3A3A00","#C5C500","#E6E600","#FFFF66"))(9)
  paletteVerte <- colorRampPalette(c("#11391E","#00FF00"))(10)
  paletteRouge <- colorRampPalette(c("#430000","#FE0000"))(10)
  paletteNoire <- "#000000"
  return(list(bleue=paletteBleue, jaune=paletteJaune, verte=paletteVerte, rouge=paletteRouge, noire=paletteNoire))
}
.transformBh <- function(bh,
                         gammeBleue=.gammeCouleurs()$bleue,
                         valeurZero=.gammeCouleurs()$noire) {
  if      (is.na(bh))    {cBh <- "#a0a0a0"}
  else if (bh < 0.00001) {cBh <- gammeBleue[4]}
  else if (bh < 0.01)    {cBh <- gammeBleue[3]}
  else if (bh < 0.05)    {cBh <- gammeBleue[2]}
  else if (bh < 0.1)     {cBh <- gammeBleue[1]}
  else                   {cBh <- valeurZero}
  return(cBh)
}
.transformInt <- function(int,
                          gammeJaune=.gammeCouleurs()$jaune,
                          valeurZero=.gammeCouleurs()$noire) {
  if (is.na(int)) {cInt <- "#a0a0a0"}
  else {
    int <- round(int)
    int[which(int<0)] <- 0 ; int[which(int>9)] <- 9
    cInt <- ""
    ifelse (int==0, cInt <- valeurZero , cInt <- gammeJaune[int])
  }
  return(cInt)
}
.transformPval <- function(pval,
                           gammeBleue=.gammeCouleurs()$bleue,
                           valeurZero=.gammeCouleurs()$noire) {
  if      (is.na(pval))    {cPval <- "#a0a0a0"}
  else if (pval < 0.00001) {cPval <- gammeBleue[4]}
  else if (pval < 0.001)   {cPval <- gammeBleue[3]}
  else if (pval < 0.01)    {cPval <- gammeBleue[2]}
  else if (pval < 0.05)    {cPval <- gammeBleue[1]}
  else                     {cPval <- valeurZero}
  return(cPval)
}
.transformRat <- function(rat) {
  if      (is.na(rat))   {cRat <- "#a0a0a0"}
  else if (rat < (-3))   {cRat <- "#00ff00"}
  else if (rat < (-1.5)) {cRat <- "#00aa00"}
  else if (rat < (-1))   {cRat <- "#006600"}
  else if (rat < (-0.5)) {cRat <- "#003300"}
  else if (rat < 0)      {cRat <- "#001100"}
  else if (rat == 0)     {cRat <- "#000000"}
  else if (rat < 0.5)    {cRat <- "#110000"}
  else if (rat < 1)      {cRat <- "#330000"}
  else if (rat < 1.5)    {cRat <- "#770000"}
  else if (rat < 3)      {cRat <- "#aa0000"}
  else                   {cRat <- "#ff0000"}
  return(cRat)
}
.htmlTag <- function(color) {
  htmlCode <- paste("<td align=\"center\" bgcolor=\"",
                    color,
                    "\">",
                    "</td>",
                    sep = "")
  return(htmlCode)
}
