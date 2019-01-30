###############################################
# AnaDiff Agilent
# Données d'un swap
# Le 30 janvier 2018 - Sandra PELLETIER
###############################################

# Graph, et soumission Geo
rg4graph <- .RG4Graph(RG=swap$RG, labelling=data$labelling, dirName=swap$dirName, dataTest=conf$dataTest)
expInt <- .nomFichier("geoSubmission.txt", paste(swap$dirName, "qualityControl_geoSubmission/", sep=""),
                      swap$swap, export="")
geo <- .normIntensite(rg4graph, "", expInt, "")
rm(rg4graph, expInt, geo)
