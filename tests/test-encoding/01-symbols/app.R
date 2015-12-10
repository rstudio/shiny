library(shiny)
app <- shinyApp(
  ui = fluidPage(
    "\u2264 and \u2265",
    uiOutput("bin_prob_k"),
    uiOutput("bin_prob_inequality"),
    uiOutput("bin_prob_out"),
    plotOutput("pmf")
  ),
  server = function(input, output) {

    output$bin_prob_k = renderUI({
      numericInput("binom4_k", label = "Number of successes (k)", value = 3,
                   step = 1, min = 0, max = 10)
    })

    bin_choices = reactive({
      nms = paste("P(X",c("=","\u2264","<","\u2265",">"),input$binom4_k,")",sep="")
      return(nms)
    })

    output$bin_prob_inequality = renderUI({
      selectInput("bin_prob_type", label = "Inequality type", choices = bin_choices())
    })

    output$pmf <- renderPlot({
      n=10
      prob = 0.4
      probs = dbinom(0:n, size=n, prob=prob)
      mtext(input$bin_prob_type, side = 3, col = "red", cex = 1.2)
    })

    output$bin_prob_out = renderUI({
      withMathJax(paste0("\\(",input$bin_prob_type,"\\)",sep=""))
    })

  }
)
