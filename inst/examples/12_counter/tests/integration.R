
files <- list.files("./integration", full.names = FALSE)
origwd <- getwd()
setwd("./integration")
on.exit(setwd(origwd), add=TRUE)
lapply(files, source, local=environment())
