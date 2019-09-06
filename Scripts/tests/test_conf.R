# Fonctionnement de conf.R
# .conf(conf)

# arguments : 
conf1 <- NULL
conf2 <-c(designPuce = "84550")

###

source("../src/conf.R")

###

cat(">>> conf1 : NULL\n")
print(summary(.conf(conf1)))

###

cat("\n>>> conf2 <-c(designPuce = \"84550\")\n")
print(summary(.conf(conf2)))

### 

rm(conf1, conf2)