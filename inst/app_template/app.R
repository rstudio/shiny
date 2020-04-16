ui <- fluidPage(
{{
# These blocks of code are processed with htmlTemplate()
if (isTRUE(module)) {
'  # ======== Modules ========
  # mymoduleUI is defined in R/my-module.R
  mymoduleUI("mymodule1", "Click counter #1"),
  mymoduleUI("mymodule2", "Click counter #2"),
  # =========================
'
}
}}
  wellPanel(
    sliderInput("size", "Data size", min = 5, max = 20, value = 10),
{{
if (isTRUE(rdir)) {
  '    div("Lexically sorted sequence:"),'
} else {
  '    div("Sorted sequence:"),'
}
}}
    verbatimTextOutput("sequence")
  )
)

server <- function(input, output, session) {
{{
if (isTRUE(module)) {
'  # ======== Modules ========
  # mymoduleServer is defined in R/my-module.R
  mymoduleServer("mymodule1")
  mymoduleServer("mymodule2")
  # =========================
'
}
}}
  data <- reactive({
    req(input$size)
{{
if (isTRUE(rdir)) {
'    # lexical_sort from R/sort.R
    lexical_sort(seq_len(input$size))'
} else {
'    sort(seq_len(input$size))'
}
}}
  })
  output$sequence <- renderText({
    paste(data(), collapse = " ")
  })
}

shinyApp(ui, server)
