###############################################
# AnaDiff Agilent
# Le 6 février 2019 - Sandra PELLETIER
###############################################

.transformRat <- function(rat, dataTest=conf$dataTest) {
  # renvoi le code couleur en fonction de la valeur du ratio
  # les ratios s'interpretent sur une gamme logarithmique, la correspondance n'est pas lineaire
  # Args:
  #   rat : valeur du ratio a transformer
  # Returns:
  #   le code couleur hexadécimal correspondant
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