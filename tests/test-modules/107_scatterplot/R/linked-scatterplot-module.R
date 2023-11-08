




linkedScatterUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
    column(6, plotOutput(ns("plot2"), brush = ns("brush")))
  )
}

linkedScatterServer <- function(id, data, left, right) {
  moduleServer(
    id,
    function(input, output, session) {
      # Yields the data frame with an additional column "selected_"
      # that indicates whether that observation is brushed
      dataWithSelection <- reactive({
        # dput(input$brush)
        brushedPoints(data(), input$brush, allRows = TRUE)
      })

      output$plot1 <- renderPlot({
        scatterPlot(dataWithSelection(), left())
      })

      output$plot2 <- renderPlot({
        scatterPlot(dataWithSelection(), right())
      })

      return(dataWithSelection)
    }
  )
}
