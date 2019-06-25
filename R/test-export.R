#' Register expressions for export in test mode
#'
#' This function registers expressions that will be evaluated when a test export
#' event occurs. These events are triggered by accessing a snapshot URL.
#'
#' This function only has an effect if the app is launched in test mode. This is
#' done by calling `runApp()` with `test.mode=TRUE`, or by setting the
#' global option `shiny.testmode` to `TRUE`.
#'
#' @param quoted_ Are the expression quoted? Default is `FALSE`.
#' @param env_ The environment in which the expression should be evaluated.
#' @param session_ A Shiny session object.
#' @param ... Named arguments that are quoted or unquoted expressions that will
#'   be captured and evaluated when snapshot URL is visited.
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'
#' options(shiny.testmode = TRUE)
#'
#' # This application shows the test snapshot URL; clicking on it will
#' # fetch the input, output, and exported values in JSON format.
#' shinyApp(
#'   ui = basicPage(
#'     h4("Snapshot URL: "),
#'     uiOutput("url"),
#'     h4("Current values:"),
#'     verbatimTextOutput("values"),
#'     actionButton("inc", "Increment x")
#'   ),
#'
#'   server = function(input, output, session) {
#'     vals <- reactiveValues(x = 1)
#'     y <- reactive({ vals$x + 1 })
#'
#'     observeEvent(input$inc, {
#'       vals$x <<- vals$x + 1
#'     })
#'
#'     exportTestValues(
#'       x = vals$x,
#'       y = y()
#'     )
#'
#'     output$url <- renderUI({
#'       url <- session$getTestSnapshotUrl(format="json")
#'       a(href = url, url)
#'     })
#'
#'     output$values <- renderText({
#'       paste0("vals$x: ", vals$x, "\ny: ", y())
#'     })
#'   }
#' )
#' }
#' @export
exportTestValues <- function(..., quoted_ = FALSE, env_ = parent.frame(),
  session_ = getDefaultReactiveDomain())
{
  session_$exportTestValues(..., quoted_ = quoted_, env_ = env_)
}
