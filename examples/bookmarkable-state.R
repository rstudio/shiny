library(shiny)

mymodUI <- function(id, initialValue) {
  ns <- NS(id)
  textInput(ns("text"), "Text", restoreInput(ns("text"), initialValue))
}


ui <- function(req) {
  fluidPage(
    verbatimTextOutput("url"),
    numericInput("n", "n", restoreInput("n", 10)),
    mymodUI("a", "Hello"),
    mymodUI("b", "World"),
    sliderInput("slider", "Slider", 0, 100, restoreInput("slider", c(10, 20))),
    tabsetPanel(id = "tabs", selected = restoreInput("tabs", NULL),
      tabPanel("Letters",
        selectInput("letter", "Letter", LETTERS, selected = restoreInput("letter", "A")),
        textOutput("letterOut", h1)
      ),
      tabPanel("Random",
        numericInput("runifCount", "How many?", restoreInput("runifCount", 10)),
        tableOutput("runif")
      )
    )
  )
}

server <- function(input, output, session) {
  output$url <- renderText({
    shiny:::saveBookmarkDataURL(input, NULL, NULL)
  })
  
  output$letterOut <- renderText(input$letter)
  
  output$runif <- renderTable({
    data.frame(
      values = runif(input$runifCount)
    )
  })
  
  observe({
    session$updateUrl(paste0("?", shiny:::saveBookmarkDataURL(input, NULL, NULL)))
  })
}

shinyApp(ui, server)

