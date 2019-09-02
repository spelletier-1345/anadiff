.gammeBH <- function(x) { # fonction pour BH
  if      (x < 0.00001) {color <- "00ffff"}
  else if (x < 0.01)    {color <- "0099ff"}
  else if (x < 0.05)    {color <- "0000ff"}
  else if (x < 0.1)     {color <- "222222"}
  else                  {color <- "000000"}
  return(color)
}