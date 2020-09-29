#!/usr/bin/env Rscript
###############################################
# AnaDiff IRHS
# Le 20 février 2019 - Sandra PELLETIER
#
# Les données de configuration
#
###############################################

# Configuration
# Chemin fonction des ordinateurs
.etc <- list("/etc/anadiff.conf",
     "/usr/local/etc/anadiff.conf",
     "~/etc/anadiff.conf",
     "~/.local/etc/anadiff.conf")
.conf <- FALSE
for (.e in .etc) {
  if (file.exists(.e)) {
    source(.e)
    .conf <- TRUE
  }
}
