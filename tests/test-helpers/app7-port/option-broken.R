library(shiny)

op <- options(shiny.port = 7777)
onStop(function() { options(op) })

stop("boom")
