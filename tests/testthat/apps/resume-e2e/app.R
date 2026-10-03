library(shiny)

# E2E_RELOAD: enableResume(reload =). E2E_APP_VERSION: appVersion.
# E2E_EXTRA: an extra widget (the changed-UI test). E2E_ALLOW: allowReconnect(TRUE).
# E2E_ERROR_TEXT: the message the risky observer fails with.
args <- list(debounce = 300)
if (nzchar(Sys.getenv("E2E_RELOAD", ""))) args$reload <- Sys.getenv("E2E_RELOAD")
if (nzchar(Sys.getenv("E2E_APP_VERSION", ""))) args$appVersion <- Sys.getenv("E2E_APP_VERSION")
if (!identical(Sys.getenv("SHINY_RESUME"), "FALSE")) do.call(enableResume, args)

log_line <- function(...) {
  path <- Sys.getenv("E2E_LOG", "")
  if (nzchar(path)) cat(paste0(..., "\n"), file = path, append = TRUE)
}

ui <- fluidPage(
  if (nzchar(Sys.getenv("E2E_EXTRA", ""))) textInput("extra", "Added after restart"),
  actionButton("plus", "+1"),
  actionButton("send", "Send email"),
  actionButton("end", "End session"),
  sliderInput("n", "n", 1, 100, 10),
  textInput("note", "Note"),
  textInput("risky", "Risky"),
  dateRangeInput("dates", "Dates", start = "2024-01-01", end = "2024-01-31"),
  textOutput("count"),
  textOutput("emails"),
  textOutput("expensive"),
  textOutput("process"),
  textOutput("dates_txt"),
  tags$script(HTML("
    $(document).on('shiny:resumed', function(e) { document.body.setAttribute('data-resumed', e.resumed); });
  "))
)

server <- function(input, output, session) {
  if (nzchar(Sys.getenv("E2E_ALLOW", ""))) session$allowReconnect(TRUE)
  session$onSessionEnded(function() log_line("SESSION ENDED"))
  count <- reactiveVal(0)
  emails <- reactiveVal(0)
  observeEvent(input$plus, count(count() + 1))
  observeEvent(input$send, { log_line("EMAIL SENT"); emails(emails() + 1) })
  observeEvent(input$end, session$close())
  observeEvent(input$risky, {
    if (identical(input$risky, "boom")) stop(Sys.getenv("E2E_ERROR_TEXT", "risky boom"))
  })
  expensive <- reactive({ log_line("COMPUTING expensive for n=", input$n); input$n * 1000 })
  output$count <- renderText(paste("Count:", count()))
  output$emails <- renderText(paste("Emails sent:", emails()))
  output$expensive <- renderText(paste("Expensive:", expensive()))
  output$process <- renderText(paste("PID:", Sys.getpid()))
  output$dates_txt <- renderText(paste("Dates:", paste(input$dates, collapse = " ")))
}

shinyApp(ui, server)
