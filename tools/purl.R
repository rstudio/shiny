library(knitr)

infile <- commandArgs(trailingOnly=TRUE)[1]
outfile <- commandArgs(trailingOnly=TRUE)[2]

src <- readLines(infile, warn=FALSE)
# Remove eval=FALSE from chunk headers
src <- gsub('(```\\{r .*?)\\beval\\s*=\\s*F(ALSE)?\\b\\s*,?', '\\1', src)

opts_knit$set(progress = FALSE, verbose = FALSE)
writeLines(knit(text=src, tangle=TRUE), outfile)
