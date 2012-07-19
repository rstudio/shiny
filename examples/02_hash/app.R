library(shiny)
library(digest)

client <- clientPage(
  
  textInput("input1", caption="Input:", initialValue="Hello, world!"),
  checkboxInput("addnewline", caption = "Append newline", initialValue=TRUE),
    
  textOutput("md5_hash", caption = "MD5:"),
  textOutput("sha1_hash", caption = "SHA-1:")
  
)

server <- function(input, output) {
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
}

runApp(client, server, port=8500)