library(shiny)

clientPage(
  
  textInput("input1", caption="Input:", initialValue="Hello, world!"),
  checkboxInput("addnewline", caption = "Append newline", initialValue=TRUE),
  
  textOutput("md5_hash", caption = "MD5:"),
  textOutput("sha1_hash", caption = "SHA-1:")
  
)