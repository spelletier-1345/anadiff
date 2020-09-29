.gammeInt <- function(x) { # fonction pour intensite
  if      (x > 9)   {color <- "ffff99"}
  else if (x > 8)   {color <- "ffff66"}
  else if (x > 7)   {color <- "ffff33"}
  else if (x > 6)   {color <- "eeee00"}
  else if (x > 5)   {color <- "dddd00"}
  else if (x > 4)   {color <- "bbbb00"}
  else if (x > 3)   {color <- "999900"}
  else if (x > 2)   {color <- "777700"}
  else if (x > 1)   {color <- "555500"}
  else if (x > 0.5) {color <- "333300"}
  else if (x > 0)   {color <- "222200"}
  else              {color <- "000000"}
  return(color)
}