# Fonctionnement de verifPackages

# Données de départ : vPackages
# sans erreur :
vPackages_1 <- c("base")
# avec erreur
vPackages_2 <- c("baseXXX")

###

cat(">>> sans erreur : \n")
t <- .verifPackages(vPackages_1)
cat("return : \n")
print(t)

###

cat("\n>>> avec package 'baseXXX' manquant : \n")
t <- .verifPackages(vPackages_2)
cat("return : \n")
print(t)

###

rm(vPackages_1, vPackages_2, t)