# # Create the object by calling reactiveVal
# r <- reactiveVal()
# values <- reactiveValues()
# values$a <- 3
# values[['a']] <- 4
# values[['b']] <- 6
#
#
# # Set the value by calling with an argument
# r(10)
#
# # Read the value by calling without arguments
# r()


## Only run examples in interactive R sesions
options(shiny.reactlog=TRUE)
shinyApp(
  fluidPage(
    actionButton("minus", "-1"),
    actionButton("plus", "+1"),
    br(),
    textOutput("value"),
    br(),
    imageOutput("normPlot"),
    br(),
    imageOutput("expPlot")
  ),
  function(input, output, session) {
    # input - #1
    # output - #2
    value <- reactiveVal(0)       # rv <- reactiveValues(value = 0)

    # observer:minus - #4
    observeEvent(input$minus, {
      newValue <- value() - 1     # newValue <- rv$value - 1
      value(newValue)             # rv$value <- newValue
    })

    # observer:plus - #5
    observeEvent(input$plus, {
      newValue <- value() + 1     # newValue <- rv$value + 1
      value(newValue)             # rv$value <- newValue
    })

    # renderText - #6
    output$value <- renderText({
      value()                     # rv$value
    })
    output$normPlot <- renderPlot({
      hist(rnorm(1000, sd = abs(value()) + 0.5))
    })
    output$expPlot <- renderPlot({
      hist(rexp(1000, abs(value()) + 0.5))
    })
  }
)
