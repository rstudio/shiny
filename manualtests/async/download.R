options(shiny.trace=T)
library(shiny)
library(promises)
library(future)
plan(multiprocess)

ui <- fluidPage(
  h2("Async downloadHandler test"),
  tags$ol(
    tags$li("Verify that plot appears below"),
    tags$li("Verify that pressing Download results in 5 second delay, then rock.csv being downloaded"),
    tags$li("Check 'Throw on download?' checkbox and verify that pressing Download results in 5 second delay, then error, as well as stack traces in console")
  ),
  hr(),
  checkboxInput("throw", "Throw on download?"),
  downloadButton("download", "Download (wait 5 seconds)"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$download <- downloadHandler("rock.csv", function(file) {
    future({Sys.sleep(5)}) %...>%
    {
      if (input$throw) {
        stop("boom")
      } else {
        write.csv(rock, file)
      }
    }
  })

  output$plot <- renderPlot({
    plot(cars)
  })
}

shinyApp(ui, server)
