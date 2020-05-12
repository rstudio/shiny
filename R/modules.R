# Creates an object whose $ and [[ pass through to the parent
# session, unless the name is matched in ..., in which case
# that value is returned instead. (See Decorator pattern.)
createSessionProxy <- function(parentSession, ...) {
  e <- new.env(parent = emptyenv())
  e$parent <- parentSession
  e$overrides <- list(...)

  structure(
    e,
    class = "session_proxy"
  )
}

#' @export
`$.session_proxy` <- function(x, name) {
  if (name %in% names(.subset2(x, "overrides")))
    .subset2(x, "overrides")[[name]]
  else
    .subset2(x, "parent")[[name]]
}

#' @export
`[[.session_proxy` <- `$.session_proxy`


#' @export
`$<-.session_proxy` <- function(x, name, value) {
  # this line allows users to write into session$userData
  # (e.g. it allows something like `session$userData$x <- TRUE`,
  # but not `session$userData <- TRUE`) from within a module
  # without any hacks (see PR #1732)
  if (identical(x[[name]], value)) return(x)
  stop("Attempted to assign value on session proxy.")
}

`[[<-.session_proxy` <- `$<-.session_proxy`

#' Shiny modules
#'
#' Shiny's module feature lets you break complicated UI and server logic into
#' smaller, self-contained pieces. Compared to large monolithic Shiny apps,
#' modules are easier to reuse and easier to reason about. See the article at
#' <http://shiny.rstudio.com/articles/modules.html> to learn more.
#'
#' Starting in Shiny 1.5.0, we recommend using `moduleServer` instead of
#' [`callModule()`], because the syntax is a little easier
#' to understand, and modules created with `moduleServer` can be tested with
#' [`testServer()`].
#'
#' @param module A Shiny module server function.
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function.
#' @param session Session from which to make a child scope (the default should
#'   almost always be used).
#'
#' @return The return value, if any, from executing the module server function
#' @seealso <http://shiny.rstudio.com/articles/modules.html>
#'
#' @examples
#' # Define the UI for a module
#' counterUI <- function(id, label = "Counter") {
#'   ns <- NS(id)
#'   tagList(
#'     actionButton(ns("button"), label = label),
#'     verbatimTextOutput(ns("out"))
#'   )
#' }
#'
#' # Define the server logic for a module
#' counterServer <- function(id) {
#'   moduleServer(
#'     id,
#'     function(input, output, session) {
#'       count <- reactiveVal(0)
#'       observeEvent(input$button, {
#'         count(count() + 1)
#'       })
#'       output$out <- renderText({
#'         count()
#'       })
#'       count
#'     }
#'   )
#' }
#'
#' # Use the module in an app
#' ui <- fluidPage(
#'   counterUI("counter1", "Counter #1"),
#'   counterUI("counter2", "Counter #2")
#' )
#' server <- function(input, output, session) {
#'   counterServer("counter1")
#'   counterServer("counter2")
#' }
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#'
#'
#' # If you want to pass extra parameters to the module's server logic, you can
#' # add them to your function. In this case `prefix` is text that will be
#' # printed before the count.
#' counterServer2 <- function(id, prefix = NULL) {
#'   moduleServer(
#'     id,
#'     function(input, output, session) {
#'       count <- reactiveVal(0)
#'       observeEvent(input$button, {
#'         count(count() + 1)
#'       })
#'       output$out <- renderText({
#'         paste0(prefix, count())
#'       })
#'       count
#'     }
#'   )
#' }
#'
#' ui <- fluidPage(
#'   counterUI("counter", "Counter"),
#' )
#' server <- function(input, output, session) {
#'   counterServer2("counter", "The current count is: ")
#' }
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @export
moduleServer <- function(id, module, session = getDefaultReactiveDomain()) {
  if (inherits(session, "MockShinySession")) {
    body(module) <- rlang::expr({
      session$setEnv(base::environment())
      !!body(module)
    })
    session$setReturned(callModule(module, id, session = session))
  } else {
    callModule(module, id, session = session)
  }
}


#' Invoke a Shiny module
#'
#' Note: As of Shiny 1.5.0, we recommend using [`moduleServer()`] instead of
#' [`callModule()`], because the syntax is a little easier
#' to understand, and modules created with `moduleServer` can be tested with
#' [`testServer()`].
#'
#' @param module A Shiny module server function
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param ... Additional parameters to pass to module server function
#' @param session Session from which to make a child scope (the default should
#'   almost always be used)
#'
#' @return The return value, if any, from executing the module server function
#' @export
callModule <- function(module, id, ..., session = getDefaultReactiveDomain()) {
  if (!inherits(session, c("ShinySession", "session_proxy", "MockShinySession"))) {
    stop("session must be a ShinySession or session_proxy object.")
  }
  childScope <- session$makeScope(id)

  withReactiveDomain(childScope, {
    if (!is.function(module)) {
      stop("module argument must be a function")
    }

    module(childScope$input, childScope$output, childScope, ...)
  })
}
