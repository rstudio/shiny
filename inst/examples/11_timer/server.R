shinyServer(function(input, output) {
  output$currentTime <- renderText({
    invalidateLater(1000)
    paste("The current time is", Sys.time())
  })
})