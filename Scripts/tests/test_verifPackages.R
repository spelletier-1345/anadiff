# Fonctionnement de verifPackages.R
# .verifPackages(cPackages)

# Arguments : vPackages
vPackages_1 <- c("base")    # verifPackages sans erreur
vPackages_2 <- c("baseXXX") # verifPackages avec erreur

###

source("./Scripts/src/verifPackages.R")

###

cat(">>> sans erreur : \n")
tt <- .verifPackages(vPackages_1)
cat("return : \n")
print(tt)

###

cat("\n>>> avec package 'baseXXX' manquant : \n")
tt <- .verifPackages(vPackages_2)
cat("return : \n")
print(tt)

###

rm(vPackages_1, vPackages_2, tt)