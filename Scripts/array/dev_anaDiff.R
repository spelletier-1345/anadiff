###############################################
# AnaDiff Agilent
# Sandra PELLETIER
# Version révisée le 26 juin 2019
###############################################

# Source du fichier make créer
# Suite à make

### Nettoyage de la mémoire
rm(list=ls(all.names = TRUE))

### Dossier de travail
setwd("/home/spelletier/Documents/Projets/anadiff/")

### Recherche du dernier make_anaDiff
make_anaDiff <- sort(list.files(path = "./Scripts/",
                                pattern = "make_anaDiff_",
                                full.names = TRUE),
                     decreasing = TRUE)[1]

### Source de la fonction
source(make_anaDiff)
rm(make_anaDiff)
### 