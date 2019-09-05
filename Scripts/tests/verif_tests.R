tests <- list.files(path = "./Scripts/tests/", pattern = "test_")
ln <- c()
cpt <- 0
for (test in tests) {
  sink("./Scripts/tests/out_verif_tests.R")
  test <- paste("./Scripts/tests/", test, sep = "")
  res <- try(expr = source(test), silent = TRUE)
  sink(NULL)
  if(class(res)=="try-error") {
    cpt = cpt + 1
    ln <- c(ln, test)
  }
}
cat(paste(">>>", length(tests), "fonctions vérifiées\n"))
if (cpt!=0) {
  cat(paste(">>> ", cpt, "erreur(s) :\n"))
  print(ln)
} else {
  cat(paste("pas de test en erreur\n"))
}
if (file.exists("./Scripts/tests/out_verif_tests.R")) {
  file.remove("./Scripts/tests/out_verif_tests.R")
}
rm(ln, cpt, tests, test, res)