library(shiny)

op <- options(shiny.port = httpuv::randomPort())
onStop(function() { options(op) })

stop("boom")
