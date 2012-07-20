library(shiny)

clientPage(
  applicationUI(
    
    headerPanel("Example 2: Computing Hashes"),
    
    sidebarPanel(
      textInput("input1", caption="Input:", initialValue="Hello, world!"),
      checkboxInput("addnewline", caption = "Append newline", initialValue=TRUE)
    ),
  
    mainPanel(
      textOutput("md5_hash", caption = "MD5:"),
      textOutput("sha1_hash", caption = "SHA-1:")
    )
  )
)