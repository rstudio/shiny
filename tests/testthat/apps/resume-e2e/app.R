library(shiny)

# Set by e2e tests: a declared app version (the stale-token test), and an
# allowReconnect(TRUE) call (main's retry gate with resume off).
if (nzchar(Sys.getenv("RESUME_E2E_APP_VERSION", ""))) {
  enableResume(appVersion = Sys.getenv("RESUME_E2E_APP_VERSION"))
}

log_line <- function(...) {
  path <- Sys.getenv("RESUME_E2E_LOG", "")
  if (nzchar(path)) cat(paste0(..., "\n"), file = path, append = TRUE)
}

ui <- fluidPage(
  # Set by the e2e test that restarts the app with a changed UI.
  if (nzchar(Sys.getenv("RESUME_E2E_EXTRA", ""))) textInput("extra", "Added after restart"),
  actionButton("plus", "+1"),
  actionButton("send", "Send email"),
  actionButton("crash", "Crash"),
  sliderInput("n", "n", 1, 100, 10),
  textOutput("count"),
  textOutput("emails"),
  textOutput("expensive"),
  textOutput("process"),
  textOutput("resumed"),
  tags$script(HTML("
    $(document).on('shiny:resumed', function(e) {
      document.body.setAttribute('data-resumed', e.resumed + ':' + e.from + ':' + e.dom);
    });
  "))
)

server <- function(input, output, session) {
  if (nzchar(Sys.getenv("RESUME_E2E_ALLOW", ""))) session$allowReconnect(TRUE)
  count <- reactiveVal(0)
  emails <- reactiveVal(0)
  observeEvent(input$plus, count(count() + 1))
  observeEvent(input$send, { log_line("EMAIL SENT"); emails(emails() + 1) })
  observeEvent(input$crash, stop("crash requested"))
  expensive <- reactive({ log_line("COMPUTING expensive for n=", input$n); input$n * 1000 })
  resumed <- reactiveVal("no")
  session$onResumed(function(info) resumed(paste(info$resumed, info$from, info$dom)))

  output$count <- renderText(paste("Count:", count()))
  output$emails <- renderText(paste("Emails sent:", emails()))
  output$expensive <- renderText(paste("Expensive:", expensive()))
  output$process <- renderText(paste("PID:", Sys.getpid()))
  output$resumed <- renderText(resumed())
}

shinyApp(ui, server)
