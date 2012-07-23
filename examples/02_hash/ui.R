library(shiny)

shinyUI(
  pageWithSidebar(
    
    headerPanel(
      h1("Example 2: Computing Hashes")
    ),
    
    sidebarPanel(
      textInput("input1", "Input:", value="Hello, world!"),
      checkboxInput("addnewline", "Append newline", TRUE)
    ),
  
    mainPanel(
      p("MD5: ", textOutput("md5_hash")),
      p("SHA-1: ", textOutput("sha1_hash"))
    )
  )
)