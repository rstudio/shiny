
#' @include reactive-domains.R
NULL

#' @include reactives.R
NULL

#' Get the query string / hash component from the URL
#'
#' Two user friendly wrappers for getting the query string and the hash
#' component from the app's URL.
#'
#' These can be particularly useful if you want to display different content
#' depending on the values in the query string / hash (e.g. instead of basing
#' the conditional on an input or a calculated reactive, you can base it on the
#' query string). However, note that, if you're changing the query string / hash
#' programmatically from within the server code, you must use
#' `updateQueryString(_yourNewQueryString_, mode = "push")`. The default
#' `mode` for `updateQueryString` is `"replace"`, which doesn't
#' raise any events, so any observers or reactives that depend on it will
#' *not* get triggered. However, if you're changing the query string / hash
#' directly by typing directly in the browser and hitting enter, you don't have
#' to worry about this.
#'
#' @param session	A Shiny session object.
#'
#' @return For `getQueryString`, a named list. For example, the query
#'   string `?param1=value1&param2=value2` becomes `list(param1 =
#'   value1, param2 = value2)`. For `getUrlHash`, a character vector with
#'   the hash (including the leading `#` symbol).
#'
#' @seealso [updateQueryString()]
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'
#'   ## App 1: getQueryString
#'   ## Printing the value of the query string
#'   ## (Use the back and forward buttons to see how the browser
#'   ## keeps a record of each state)
#'   shinyApp(
#'     ui = fluidPage(
#'       textInput("txt", "Enter new query string"),
#'       helpText("Format: ?param1=val1&param2=val2"),
#'       actionButton("go", "Update"),
#'       hr(),
#'       verbatimTextOutput("query")
#'     ),
#'     server = function(input, output, session) {
#'       observeEvent(input$go, {
#'         updateQueryString(input$txt, mode = "push")
#'       })
#'       output$query <- renderText({
#'         query <- getQueryString()
#'         queryText <- paste(names(query), query,
#'                        sep = "=", collapse=", ")
#'         paste("Your query string is:\n", queryText)
#'       })
#'     }
#'   )
#'
#'   ## App 2: getUrlHash
#'   ## Printing the value of the URL hash
#'   ## (Use the back and forward buttons to see how the browser
#'   ## keeps a record of each state)
#'   shinyApp(
#'     ui = fluidPage(
#'       textInput("txt", "Enter new hash"),
#'       helpText("Format: #hash"),
#'       actionButton("go", "Update"),
#'       hr(),
#'       verbatimTextOutput("hash")
#'     ),
#'     server = function(input, output, session) {
#'       observeEvent(input$go, {
#'         updateQueryString(input$txt, mode = "push")
#'       })
#'       output$hash <- renderText({
#'         hash <- getUrlHash()
#'         paste("Your hash is:\n", hash)
#'       })
#'     }
#'   )
#' }
#' @export
getQueryString <- function(session = getDefaultReactiveDomain()) {
  parseQueryString(session$clientData$url_search)
}

#' @rdname getQueryString
#' @export
getUrlHash <- function(session = getDefaultReactiveDomain()) {
  session$clientData$url_hash
}
