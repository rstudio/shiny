#' Insert and remove UI objects
#'
#' These functions allow you to dynamically add and remove arbitrary UI
#' into your app, whenever you want, as many times as you want.
#' Unlike [renderUI()], the UI generated with `insertUI()` is persistent:
#' once it's created, it stays there until removed by `removeUI()`. Each
#' new call to `insertUI()` creates more UI objects, in addition to
#' the ones already there (all independent from one another). To
#' update a part of the UI (ex: an input object), you must use the
#' appropriate `render` function or a customized `reactive`
#' function.
#'
#' It's particularly useful to pair `removeUI` with `insertUI()`, but there is
#' no restriction on what you can use it on. Any element that can be selected
#' through a jQuery selector can be removed through this function.
#'
#' @param selector A string that is accepted by jQuery's selector
#'   (i.e. the string `s` to be placed in a `$(s)` jQuery call).
#'
#'   For `insertUI()` this determines the element(s) relative to which you
#'   want to insert your UI object. For `removeUI()` this determine the
#'   element(s) to be removed. If you want to remove a Shiny input or output,
#'   note that many of these are wrapped in `<div>`s, so you may need to use a
#'   somewhat complex selector --- see the Examples below. (Alternatively, you
#'   could also wrap the inputs/outputs that you want to be able to remove
#'   easily in a `<div>` with an id.)
#' @param where Where your UI object should go relative to the selector:
#'   \describe{
#'     \item{`beforeBegin`}{Before the selector element itself}
#'     \item{`afterBegin`}{Just inside the selector element, before its
#'     first child}
#'     \item{`beforeEnd`}{Just inside the selector element, after its
#'     last child (default)}
#'     \item{`afterEnd`}{After the selector element itself}
#'   }
#'   Adapted from <https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentHTML>.
#' @param ui The UI object you want to insert. This can be anything that
#'   you usually put inside your apps's `ui` function. If you're inserting
#'   multiple elements in one call, make sure to wrap them in either a
#'   `tagList()` or a `tags$div()` (the latter option has the
#'   advantage that you can give it an `id` to make it easier to
#'   reference or remove it later on). If you want to insert raw html, use
#'   `ui = HTML()`.
#' @param multiple In case your selector matches more than one element,
#'   `multiple` determines whether Shiny should insert the UI object
#'   relative to all matched elements or just relative to the first
#'   matched element (default).
#' @param immediate Whether the UI object should be immediately inserted
#'   or removed, or whether Shiny should wait until all outputs have been
#'   updated and all observers have been run (default).
#' @param session The shiny session. Advanced use only.
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' # Define UI
#' ui <- fluidPage(
#'   actionButton("add", "Add UI")
#' )
#'
#' # Server logic
#' server <- function(input, output, session) {
#'   observeEvent(input$add, {
#'     insertUI(
#'       selector = "#add",
#'       where = "afterEnd",
#'       ui = textInput(paste0("txt", input$add),
#'                      "Insert some text")
#'     )
#'   })
#' }
#'
#' # Complete app with UI and server components
#' shinyApp(ui, server)
#' }
#'
#' if (interactive()) {
#' # Define UI
#' ui <- fluidPage(
#'   actionButton("rmv", "Remove UI"),
#'   textInput("txt", "This is no longer useful")
#' )
#'
#' # Server logic
#' server <- function(input, output, session) {
#'   observeEvent(input$rmv, {
#'     removeUI(
#'       selector = "div:has(> #txt)"
#'     )
#'   })
#' }
#'
#' # Complete app with UI and server components
#' shinyApp(ui, server)
#' }
#' @export
insertUI <- function(selector,
  where = c("beforeBegin", "afterBegin", "beforeEnd", "afterEnd"),
  ui,
  multiple = FALSE,
  immediate = FALSE,
  session = getDefaultReactiveDomain()) {

  force(selector)
  force(ui)
  force(session)
  force(multiple)
  if (missing(where)) where <- "beforeEnd"
  where <- match.arg(where)

  callback <- function() {
    session$sendInsertUI(selector = selector,
                         multiple = multiple,
                         where = where,
                         content = processDeps(ui, session))
  }

  if (!immediate) session$onFlushed(callback, once = TRUE)
  else callback()
}


#' @rdname insertUI
#' @export
removeUI <- function(selector,
  multiple = FALSE,
  immediate = FALSE,
  session = getDefaultReactiveDomain()) {

  force(selector)
  force(multiple)
  force(session)

  callback <- function() {
    session$sendRemoveUI(selector = selector,
                         multiple = multiple)
  }

  if (!immediate) session$onFlushed(callback, once = TRUE)
  else callback()
}
