library(shiny)
library(bslib)

# Define UI for displaying current time ----
ui <- page_fluid(
  h2(textOutput("currentTime"))
)

# Define server logic to show current time, update every second ----
server <- function(input, output, session) {
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
}

# Create Shiny app ----
shinyApp(ui, server)
