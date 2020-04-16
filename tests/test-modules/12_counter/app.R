library(shiny)

ui <- fluidPage(
  # ======== Modules ========
  # mymoduleUI is defined in R/my-module.R
  mymoduleUI("mymodule1", "Click counter #1"),
  mymoduleUI("mymodule2", "Click counter #2"),
  # =========================
  wellPanel(
    sliderInput("size", "Data size", min = 5, max = 20, value = 10),
    div("Lexically sorted sequence:"),
    verbatimTextOutput("sequence")
  )
)

server <- function(input, output, session) {
  # ======== Modules ========
  # mymoduleServer is defined in R/my-module.R
  mymoduleServer("mymodule1")
  mymoduleServer("mymodule2")
  # =========================

  data <- reactive({
    # lexical_sort from R/utils.R
    lexical_sort(seq_len(input$size))
  })
  output$sequence <- renderText({
    paste(data(), collapse = " ")
  })
}

shinyApp(ui, server)
