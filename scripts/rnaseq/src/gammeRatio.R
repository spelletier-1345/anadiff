#!/usr/bin/env Rscript

# Copyright Sandra PELLETIER ([2022-04-20])
# 
# [sandra.pelletier@inrae.fr]
# 
# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software.  You can  use, 
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info". 
# 
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.

.gammeRatio <- function(x) { # fonction pour ratio
  if      (x < (-3))   {color <- "00ff00"}
  else if (x < (-1.5)) {color <- "00aa00"}
  else if (x < (-1))   {color <- "006600"}
  else if (x < (-0.5)) {color <- "003300"}
  else if (x < 0)      {color <- "001100"}
  else if (x==0 | x=="NaN" | x=="-Inf" | x=="Inf") {color <- "000000"}
  else if (x < 0.5)  {color <- "110000"}
  else if (x < 1)    {color <- "330000"}
  else if (x < 1.5)  {color <- "770000"}
  else if (x < 3)    {color <- "aa0000"}
  else               {color <- "ff0000"}
  return(color)
}
