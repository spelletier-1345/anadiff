# ###############################################
# AnaDiff Help
# Le 3 avril 2019 - Sandra PELLETIER
###############################################

colorisation_help <- function() {
cat("Usage:
  $colorisation.R [-f <file>] [-option MOTIF]
Look for MOTIF in headers of <file> and apply the appropriate colorisation

Options:
  -r  ratio as regular expression
  -p  pvalue as regular expression
  -b  BH as regular expression

Return:
  An html file with colorisation for excel corresponding to <file> values
  Note: open it with excel and paste <file> values on

Example:
  $Rscript colorisation.R -f myfile.txt -b BH_* -p pval_* -r *_logFC
")
}
colorisation_help()
