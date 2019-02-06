###############################################
# AnaDiff Agilent
# Gestion des cas particuliers de génomes
# Le 23 janvier 2019 - Sandra PELLETIER
###############################################

# Exceptions Aryane_v2
if (designPuce == "70158" | designPuce == "70465") { # Cas particulier pour AryANE_v2
  source("./src/exceptions/tabSAS_aryane_v2.R")
  .tabSASayane(export$annot, probeName=RG$genes$ProbeName, tab_sens, tab_antisens,
               color_sens, color_antisens, statBH, labelling)
}
