library(shiny)

# E2E_APP_VERSION: appVersion. E2E_ALLOW: allowReconnect(TRUE).
# E2E_BLOCK: a source that cannot be saved, so every record is incomplete.
# E2E_EXTRA_SOURCE: one more reactiveVal, so a saved record no longer fits.
args <- list(debounce = 300)
if (nzchar(Sys.getenv("E2E_APP_VERSION", ""))) args$appVersion <- Sys.getenv("E2E_APP_VERSION")
if (!identical(Sys.getenv("SHINY_RESUME"), "FALSE")) do.call(enableResume, args)

log_line <- function(...) {
  path <- Sys.getenv("E2E_LOG", "")
  if (nzchar(path)) cat(paste0(..., "\n"), file = path, append = TRUE)
}

panelUI <- function(id) {
  ns <- NS(id)
  div(class = "panel", actionButton(ns("plus"), paste("+1", id)), textOutput(ns("count")))
}

panelServer <- function(id) moduleServer(id, function(input, output, session) {
  count <- reactiveVal(0)
  observeEvent(input$plus, count(count() + 1))
  output$count <- renderText(paste("Panel count:", count()))
})

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
      textOutput("contents"),
      actionButton("add", "Add panel"),
      div(id = "panels"),
      textInput("word", "Word", "first"),
      actionButton("build", "Build"),
      textOutput("built")
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
  # Not textually `reactiveVal(0)`: identical non-toplevel calls get no label
  # (snapshot_body_label()), so the source would be ignored and the resume
  # would never reach the failure path.
  if (nzchar(Sys.getenv("E2E_EXTRA_SOURCE", ""))) extra <- reactiveVal(1)
  observeEvent(input$plus, count(count() + 1))
  observeEvent(input$send, { log_line("EMAIL SENT"); emails(emails() + 1) })
  observeEvent(input$end, session$close())
  observeEvent(input$risky, if (identical(input$risky, "boom")) stop("risky boom"))
  observeEvent(input$read, contents(paste(readLines(input$file$datapath), collapse = "")))
  observe(log_line("WORD ", input$word))
  expensive <- reactive({ log_line("COMPUTING expensive for n=", input$n); input$n * 1000 })
  output$count <- renderText(paste("Count:", count()))
  output$emails <- renderText(paste("Emails sent:", emails()))
  output$expensive <- renderText(paste("Expensive:", expensive()))
  output$process <- renderText(paste("PID:", Sys.getpid()))
  output$contents <- renderText(paste("Contents:", contents()))

  ids <- reactiveVal(character())
  observeEvent(input$add, {
    id <- paste0("p", input$add)
    ids(c(ids(), id))
    insertUI("#panels", ui = panelUI(id))
    panelServer(id)
  })
  # An output a handler defines; with resume off nothing is restored, so
  # the callback defines it again if the button was clicked.
  build <- function() output$built <- renderText(paste("Built:", input$word))
  observeEvent(input$build, build())
  session$onResume(function(info) {
    for (id in ids()) panelServer(id)
    if (isTRUE(input$build > 0)) build()
  })
}

shinyApp(ui, server)
