library(shiny)
library(rmdexamples)

shinyServer(function(input, output) {

  output$gage <- renderHTML({
    justgage("Foo", 5, 1, 10)
  })

})
