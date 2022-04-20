###############################################
# AnaDiff Agilent
# fin de l'analyse d'un swap
# Le 20 février 2019 - Sandra PELLETIER
###############################################

data$alertes <- rbind(data$alertes, swap$alerte)
req <- list(service="ServiceRscripts", method="addAnalyticsRscriptsValue", id=1,
            params=c('anaDiff_Agilent', toJSON(swap$json)))
if (conf$db) {
  res <- POST("http://147.99.112.52/cgi-bin/elvis/ServiceRscripts.py", body=req, encode = "json")}
.alertes(swap$alerte, swap$fileOut)
.writeLineOut(paste("\n===  Analyse du swap", sw, "terminée  ==="), swap$fileOut)
