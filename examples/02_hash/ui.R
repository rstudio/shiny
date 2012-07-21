library(shiny)

shinyUI(
  applicationPage(
    
    headerPanel("Example 2: Computing Hashes"),
    
    sidebarPanel(
      textInput("input1", label="Input:", value="Hello, world!"),
      checkboxInput("addnewline", label = "Append newline", value=TRUE)
    ),
  
    mainPanel(
      textOutput("md5_hash", label = "MD5:"),
      textOutput("sha1_hash", label = "SHA-1:")
    )
  )
)