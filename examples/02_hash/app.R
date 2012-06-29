library(digest)

text <- reactive(function() {
  str <- input$input1
  if (input$addnewline)
    str <- paste(str, "\n", sep='')
  return(str)
})

output$md5_hash <- reactive(function() {
  digest(text(), algo='md5', serialize=F)
})

output$sha1_hash <- reactive(function() {
  digest(text(), algo='sha1', serialize=F)
})
