
files <- list.files("./server", full.names = FALSE)
origwd <- getwd()
setwd("./server")
on.exit(setwd(origwd), add=TRUE)
lapply(files, source, local=environment())
