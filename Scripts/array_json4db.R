###############################################
# AnaDiff Agilent
# Données d'un swap
# Le 30 janvier 2019 - Sandra PELLETIER
###############################################

swap$json[[ex]][[sens$sensLabel]] <- list(var = sens$normalize$var,
                                         remove = sens$probesRemoved,
                                         stat = sens$stat,
                                         bkg = sens$statBkg$stat)

swap$json[[ex]][[sens$sensLabel]]$"Int" <- list(ctrl=list(min=min(sens$normalize$tabResult$cont.bg),
                                                          mean=mean(sens$normalize$tabResult$cont.bg),
                                                          med=median(sens$normalize$tabResult$cont.bg)),
                                                ttmt=list(min=min(sens$normalize$tabResult$ttmt.bg),
                                                          mean=mean(sens$normalize$tabResult$ttmt.bg),
                                                          med=median(sens$normalize$tabResult$ttmt.bg),
                                                          max=max(sens$normalize$tabResult$ttmt.bg)))
