###############################################
# AnaDiff Agilent
# Création de la table double sens/antisens
# Le 6 février 2019 - Sandra PELLETIER
###############################################

export$tabDouble <- .tabSAS(annot, tab_sens, tab_antisens, color_sens, color_antisens, 
                     sens$sensLabel, sens$expName$tabDble, sens$expName$colDble)
assign(paste(sw, ex, sep="_"), export$tabDouble, envir=globalenv())
