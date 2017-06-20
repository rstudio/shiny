#' Insert UI objects
#'
#' Insert a UI object into the app.
#'
#' This function allows you to dynamically add an arbitrarily large UI
#' object into your app, whenever you want, as many times as you want.
#' Unlike \code{\link{renderUI}}, the UI generated with \code{insertUI}
#' is not updatable as a whole: once it's created, it stays there. Each
#' new call to \code{insertUI} creates more UI objects, in addition to
#' the ones already there (all independent from one another). To
#' update a part of the UI (ex: an input object), you must use the
#' appropriate \code{render} function or a customized \code{reactive}
#' function. To remove any part of your UI, use \code{\link{removeUI}}.
#'
#' @param selector A string that is accepted by jQuery's selector (i.e. the
#' string \code{s} to be placed in a \code{$(s)} jQuery call). This selector
#' will determine the element(s) relative to which you want to insert your
#' UI object.
#'
#' @param where Where your UI object should go relative to the selector:
#' \describe{
#'   \item{\code{beforeBegin}}{Before the selector element itself}
#'   \item{\code{afterBegin}}{Just inside the selector element, before its
#'   first child}
#'   \item{\code{beforeEnd}}{Just inside the selector element, after its
#'   last child (default)}
#'   \item{\code{afterEnd}}{After the selector element itself}
#' }
#' Adapted from
#' \href{https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentHTML}{here}.
#'
#' @param ui The UI object you want to insert. This can be anything that
#' you usually put inside your apps's \code{ui} function. If you're inserting
#' multiple elements in one call, make sure to wrap them in either a
#' \code{tagList()} or a \code{tags$div()} (the latter option has the
#' advantage that you can give it an \code{id} to make it easier to
#' reference or remove it later on). If you want to insert raw html, use
#' \code{ui = HTML()}.
#'
#' @param multiple In case your selector matches more than one element,
#' \code{multiple} determines whether Shiny should insert the UI object
#' relative to all matched elements or just relative to the first
#' matched element (default).
#'
#' @param immediate Whether the UI object should be immediately inserted into
#' the app when you call \code{insertUI}, or whether Shiny should wait until
#' all outputs have been updated and all observers have been run (default).
#'
#' @param session The shiny session within which to call \code{insertUI}.
#'
#' @seealso \code{\link{removeUI}}
#'
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


#' Remove UI objects
#'
#' Remove a UI object from the app.
#'
#' This function allows you to remove any part of your UI. Once \code{removeUI}
#' is executed on some element, it is gone forever.
#'
#' While it may be a particularly useful pattern to pair this with
#' \code{\link{insertUI}} (to remove some UI you had previously inserted),
#' there is no restriction on what you can use \code{removeUI} on. Any
#' element that can be selected through a jQuery selector can be removed
#' through this function.
#'
#' @param selector A string that is accepted by jQuery's selector (i.e. the
#' string \code{s} to be placed in a \code{$(s)} jQuery call). This selector
#' will determine the element(s) to be removed. If you want to remove a
#' Shiny input or output, note that many of these are wrapped in \code{div}s,
#' so you may need to use a somewhat complex selector -- see the Examples below.
#' (Alternatively, you could also wrap the inputs/outputs that you want to be
#' able to remove easily in a \code{div} with an id.)
#'
#' @param multiple In case your selector matches more than one element,
#' \code{multiple} determines whether Shiny should remove all the matched
#' elements or just the first matched element (default).
#'
#' @param immediate Whether the element(s) should be immediately removed from
#' the app when you call \code{removeUI}, or whether Shiny should wait until
#' all outputs have been updated and all observers have been run (default).
#'
#' @param session The shiny session within which to call \code{removeUI}.
#'
#' @seealso \code{\link{insertUI}}
#'
#' @examples
#' ## Only run this example in interactive R sessions
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


#' Dynamically add a tabPanel
#'
#' Dynamically add a \code{tabPanel()} into an existing \code{tabsetPanel}.
#'
#' @param tabsetPanelId The \code{id} of the \code{tabsetPanel()} into which
#'   \code{tab} will be inserted.
#'
#' @param tab The tab element to be added (must be created with
#'   \code{tabPanel}).
#'
#' @param target The \code{value} of an existing \code{tabPanel()}, next to
#'   which \code{tab} will be added. If \code{NULL},the \code{tab} will be
#'   placed either as the first tab or the last tab, depending on the
#'   \code{position} argument.
#'
#' @param position Should \code{tab} be added to the right ot to the left
#'   of \code{target}?
#'
#' @param immediate Whether \code{tab} should be immediately inserted into
#'   the app when you call \code{insertTab}, or whether Shiny should wait until
#'   all outputs have been updated and all observers have been run (default).
#'
#' @param session The shiny session within which to call \code{insertTab}.
#'
#' @seealso \code{\link{removeTab}}, \code{\link{showTab}},
#'   \code{\link{hideTab}}
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(actionButton("add", "Add tab")),
#'     mainPanel(
#'       tabsetPanel(id = "tabs",
#'         tabPanel("Hello", "This is the hello tab"),
#'         tabPanel("Foo", "This is the foo tab"),
#'         tabPanel("Bar", "This is the bar tab")
#'       )
#'     )
#'   )
#' )
#' server <- function(input, output, session) {
#'   observeEvent(input$add, {
#'     insertTab(tabsetPanelId = "tabs",
#'       tabPanel("Dynamic", "This a dynamically-added tab"),
#'       target = "Bar"
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
insertTab <- function(tabsetPanelId, tab, target = NULL,
  position = c("right", "left"), immediate = FALSE,
  session = getDefaultReactiveDomain()) {

  force(tabsetPanelId)
  force(tab)
  force(target)
  position <- match.arg(position)
  force(session)

  # tabContent <- tags$div(class = "tab-content")
  # buildItem(tabsetId, divTag)

  iconClass <- tab$attribs$`data-icon-class`
  icon <- if (!is.null(iconClass)) {
    # for font-awesome we specify fixed-width
    if (grepl("fa-", iconClass, fixed = TRUE))
      iconClass <- paste(iconClass, "fa-fw")
    icon(name = NULL, class = iconClass)
  } else NULL

  callback <- function() {
    session$sendInsertTab(
      tabsetPanelId = tabsetPanelId,
      tab = processDeps(tab, session),
      icon = processDeps(icon, session),
      target = target,
      position = position)
  }

  if (!immediate) session$onFlushed(callback, once = TRUE)
  else callback()
}


#' Dynamically remove a tabPanel
#'
#' Dynamically remove a \code{tabPanel()} from an existing \code{tabsetPanel}.
#'
#' @param tabsetPanelId The \code{id} of the \code{tabsetPanel()} from which
#'   \code{target} will be removed.
#'
#' @param target The \code{value} of the \code{tabPanel()} to be removed.
#'
#' @param immediate Whether \code{tab} should be immediately removed from
#'   the app when you call \code{removeTab}, or whether Shiny should wait until
#'   all outputs have been updated and all observers have been run (default).
#'
#' @param session The shiny session within which to call \code{removeTab}.
#'
#' @seealso \code{\link{insertTab}}, \code{\link{showTab}},
#'   \code{\link{hideTab}}
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(actionButton("remove", "Remove tab")),
#'     mainPanel(
#'       tabsetPanel(id = "tabs",
#'         tabPanel("Hello", "This is the hello tab"),
#'         tabPanel("Foo", "This is the foo tab"),
#'         tabPanel("Bar", "This is the bar tab")
#'       )
#'     )
#'   )
#' )
#' server <- function(input, output, session) {
#'   observeEvent(input$remove, {
#'     removeTab(tabsetPanelId = "tabs",
#'       target = "Bar"
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
removeTab <- function(tabsetPanelId, target, immediate = FALSE,
  session = getDefaultReactiveDomain()) {

  force(tabsetPanelId)
  force(target)
  force(session)

  callback <- function() {
    session$sendRemoveTab(
      tabsetPanelId = tabsetPanelId,
      target = target)
  }

  if (!immediate) session$onFlushed(callback, once = TRUE)
  else callback()
}


#' Dynamically show a tabPanel
#'
#' Dynamically show (expose) a hidden \code{tabPanel()} from an existing
#' \code{tabsetPanel}.
#'
#' @param tabsetPanelId The \code{id} of the \code{tabsetPanel()} that
#'   \code{target} belongs to.
#'
#' @param target The \code{value} of the \code{tabPanel()} to be shown.
#'
#' @param immediate Whether \code{tab} should be immediately shown from when
#'   you call \code{removeTab}, or whether Shiny should wait until all
#'   outputs have been updated and all observers have been run (default).
#'
#' @param session The shiny session within which to call \code{showTab}.
#'
#' @seealso \code{\link{insertTab}}, \code{\link{removeTab}},
#'   \code{\link{hideTab}}
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(actionButton("remove", "Remove tab")),
#'     mainPanel(
#'       tabsetPanel(id = "tabs",
#'         tabPanel("Hello", "This is the hello tab"),
#'         tabPanel("Foo", "This is the foo tab"),
#'         tabPanel("Bar", "This is the bar tab")
#'       )
#'     )
#'   )
#' )
#' server <- function(input, output, session) {
#'   observeEvent(input$remove, {
#'     showTab(tabsetPanelId = "tabs",
#'       target = "Bar"
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
showTab <- function(tabsetPanelId, target, immediate = FALSE,
  session = getDefaultReactiveDomain()) {

  force(tabsetPanelId)
  force(target)
  force(session)

  callback <- function() {
    session$sendShowTab(
      tabsetPanelId = tabsetPanelId,
      target = target)
  }

  if (!immediate) session$onFlushed(callback, once = TRUE)
  else callback()
}


#' Dynamically hide a tabPanel
#'
#' Dynamically hide \code{tabPanel()} from an existing
#' \code{tabsetPanel}.
#'
#' @param tabsetPanelId The \code{id} of the \code{tabsetPanel()} that
#'   \code{target} belongs to.
#'
#' @param target The \code{value} of the \code{tabPanel()} to be hidden.
#'
#' @param immediate Whether \code{tab} should be immediately shown from when
#'   you call \code{removeTab}, or whether Shiny should wait until all
#'   outputs have been updated and all observers have been run (default).
#'
#' @param session The shiny session within which to call \code{hideTab}.
#'
#' @seealso \code{\link{insertTab}}, \code{\link{removeTab}},
#'   \code{\link{showTab}}
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(actionButton("remove", "Remove tab")),
#'     mainPanel(
#'       tabsetPanel(id = "tabs",
#'         tabPanel("Hello", "This is the hello tab"),
#'         tabPanel("Foo", "This is the foo tab"),
#'         tabPanel("Bar", "This is the bar tab")
#'       )
#'     )
#'   )
#' )
#' server <- function(input, output, session) {
#'   observeEvent(input$remove, {
#'     hideTab(tabsetPanelId = "tabs",
#'       target = "Bar"
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
hideTab <- function(tabsetPanelId, target, immediate = FALSE,
  session = getDefaultReactiveDomain()) {

  force(tabsetPanelId)
  force(target)
  force(session)

  callback <- function() {
    session$sendHideTab(
      tabsetPanelId = tabsetPanelId,
      target = target)
  }

  if (!immediate) session$onFlushed(callback, once = TRUE)
  else callback()
}

