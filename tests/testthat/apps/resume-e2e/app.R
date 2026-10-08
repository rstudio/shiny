library(shiny)

# E2E_APP_VERSION: appVersion. E2E_ALLOW: allowReconnect(TRUE).
# E2E_BLOCK: a source that cannot be saved, so every record is incomplete.
args <- list(debounce = 300)
if (nzchar(Sys.getenv("E2E_APP_VERSION", ""))) args$appVersion <- Sys.getenv("E2E_APP_VERSION")
if (!identical(Sys.getenv("SHINY_RESUME"), "FALSE")) do.call(enableResume, args)

log_line <- function(...) {
  path <- Sys.getenv("E2E_LOG", "")
  if (nzchar(path)) cat(paste0(..., "\n"), file = path, append = TRUE)
}

ui <- fluidPage(
  # Tab ids are random on every render: a resume must not depend on the
  # page's markup matching the app's.
  tabsetPanel(
    tabPanel("Main",
      actionButton("plus", "+1"),
      actionButton("send", "Send email"),
      actionButton("end", "End session"),
      sliderInput("n", "n", 1, 100, 10),
      textInput("risky", "Risky"),
      fileInput("file", "File"),
      actionButton("read", "Read file"),
      textOutput("count"),
      textOutput("emails"),
      textOutput("expensive"),
      textOutput("process"),
      textOutput("contents")
    ),
    tabPanel("Other", "Nothing here")
  ),
  tags$script(HTML("
    $(document).on('shiny:resumed', function(e) { document.body.setAttribute('data-resumed', e.resumed); });
  "))
)

server <- function(input, output, session) {
  if (nzchar(Sys.getenv("E2E_ALLOW", ""))) session$allowReconnect(TRUE)
  session$onSessionEnded(function() log_line("SESSION ENDED"))
  count <- reactiveVal(0)
  emails <- reactiveVal(0)
  contents <- reactiveVal("")
  if (nzchar(Sys.getenv("E2E_BLOCK", ""))) conn <- reactiveVal(function() "a connection")
  observeEvent(input$plus, count(count() + 1))
  observeEvent(input$send, { log_line("EMAIL SENT"); emails(emails() + 1) })
  observeEvent(input$end, session$close())
  observeEvent(input$risky, if (identical(input$risky, "boom")) stop("risky boom"))
  observeEvent(input$read, contents(paste(readLines(input$file$datapath), collapse = "")))
  expensive <- reactive({ log_line("COMPUTING expensive for n=", input$n); input$n * 1000 })
  output$count <- renderText(paste("Count:", count()))
  output$emails <- renderText(paste("Emails sent:", emails()))
  output$expensive <- renderText(paste("Expensive:", expensive()))
  output$process <- renderText(paste("PID:", Sys.getpid()))
  output$contents <- renderText(paste("Contents:", contents()))
}

shinyApp(ui, server)
