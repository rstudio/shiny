library(shiny)

args <- commandArgs(trailingOnly=T)

if (length(args) == 0) {
  stop("Usage: shiny.sh <app_dir>")
}

app.path <- args[1]
setwd(app.path)

runApp(port=8100L)
