library(bslib)
library(shiny)

# Define UI for data download app ----
ui <- page_sidebar(

  # App title ----
  tableOutput("table"),

  # Sidebar layout with input and output definitions ----
  sidebar = sidebar(
    # Input: Choose dataset ----
    selectInput("dataset", "Choose a dataset:",
                choices = c("rock", "pressure", "cars")),

    # Button
    downloadButton("downloadData", "Download")
  ),
  title = "Downloading Data"
)

# Define server logic to display and download selected file ----
server <- function(input, output) {

  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })

  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )

}

# Create Shiny app ----
shinyApp(ui, server)
