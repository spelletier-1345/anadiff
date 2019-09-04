###############################################
# AnaDiff Agilent
# Sandra PELLETIER
# Version révisée le 26 juin 2019
###############################################

### Nettoyage de la mémoire
rm(list=ls(all.names = TRUE))

### Dossier de travail
setwd("/home/spelletier/Documents/Projets/anadiff/")

### Recherche du dernier make_anaDiff
list.files(path = "./Scripts/", pattern = "make_anaDiff_*.R")
make_anaDiff <- sort(list.files(path = "./Scripts/", pattern = "make_anaDiff_", full.names = TRUE), decreasing = TRUE)[1]

### Source de la fonction
source(make_anaDiff)

### 