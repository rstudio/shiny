library(shiny)

shinyUI(
  applicationPage(
    
    headerPanel(
      h1("Example 2: Computing Hashes")
    ),
    
    sidebarPanel(
      textInput("input1", "Input:", value="Hello, world!"),
      checkboxInput("addnewline", "Append newline", TRUE)
    ),
  
    mainPanel(
      textOutput("md5_hash", "MD5:"),
      textOutput("sha1_hash", "SHA-1:")
    )
  )
)