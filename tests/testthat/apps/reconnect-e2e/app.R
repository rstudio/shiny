library(shiny)

# E2E_ALLOW: call session$allowReconnect(TRUE). E2E_BOOKMARK: URL bookmarking on.

# A function, so URL bookmarks can restore widgets on a first load.
ui <- function(req) fluidPage(
  actionButton("plus", "+1"),
  textInput("txt", "Text"),
  uiOutput("slider_ui"),
  textOutput("count"),
  textOutput("txtout"),
  textOutput("slider_val"),
  textOutput("process")
)

server <- function(input, output, session) {
  if (nzchar(Sys.getenv("E2E_ALLOW", ""))) session$allowReconnect(TRUE)
  count <- reactiveVal(0)
  observeEvent(input$plus, count(count() + 1))
  # restoreInput() reads the restore context: the URL's bookmark on a first
  # load, the client's inputs on a reconnect.
  output$slider_ui <- renderUI(sliderInput("n", "n", 1, 100, restoreInput("n", 10)))
  output$count <- renderText(paste("Count:", count()))
  output$txtout <- renderText(paste("Text:", input$txt))
  output$slider_val <- renderText(paste("n:", input$n))
  output$process <- renderText(paste("PID:", Sys.getpid()))
}

shinyApp(ui, server,
  enableBookmarking = if (nzchar(Sys.getenv("E2E_BOOKMARK", ""))) "url" else NULL)
