library(shiny)
enableResume(reload = "resume", debounce = 300)

log_line <- function(...) {
  path <- Sys.getenv("E2E_LOG", "")
  if (nzchar(path)) cat(paste0(..., "\n"), file = path, append = TRUE)
}

ui <- function(req) {
  log_line("UI GET ", req$QUERY_STRING)
  fluidPage(
    actionButton("plus", "+1"),
    actionButton("read", "Read file"),
    actionButton("setq", "Change URL"),
    actionButton("reset", "Reset"),
    textInput("note", "Note"),
    sliderInput("level", "Level", min = 0, max = 10, value = 0),
    fileInput("file", "File"),
    textOutput("count"),
    textOutput("noteout"),
    textOutput("levelout"),
    textOutput("fname"),
    textOutput("contents"),
    tags$script(HTML("
      $(document).on('shiny:message', function(e) {
        if ('resumed' in e.message) {
          document.body.setAttribute('data-pushed', 'inputs' in e.message ? Object.keys(e.message.inputs).sort().join(',') : '');
        }
      });
      $(document).on('shiny:resumed', function(e) { document.body.setAttribute('data-resumed', e.resumed); });
    "))
  )
}

server <- function(input, output, session) {
  log_line("SERVER START")
  count <- reactiveVal(0)
  observeEvent(input$plus, count(count() + 1))
  contents <- reactiveVal("")
  observeEvent(input$read, contents(paste(readLines(input$file$datapath), collapse = "")))
  observeEvent(input$setq, updateQueryString("?view=2", mode = "push"))
  observeEvent(input$reset, session$reload())
  output$count <- renderText(paste("Count:", count()))
  output$noteout <- renderText(paste("Note:", input$note))
  output$levelout <- renderText(paste("Level:", input$level))
  output$fname <- renderText(paste("File:", input$file$name))
  output$contents <- renderText(paste("Contents:", contents()))
}

shinyApp(ui, server, enableBookmarking = "url")
