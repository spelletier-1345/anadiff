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

.gammePval <- function(x) { # fonction pour pvalue
  if      (x < 0.00001) {color <- "00ffff"}
  else if (x < 0.001)   {color <- "0099ff"}
  else if (x < 0.01)    {color <- "0000ff"}
  else if (x < 0.05)    {color <- "222222"}
  else                  {color <- "000000"}
  return(color)
}
