
getIcon <- function(tab) {
  iconClass <- tab$attribs$`data-icon-class`
  if (!is.null(iconClass)) {
    # for font-awesome we specify fixed-width
    if (grepl("fa-", iconClass, fixed = TRUE))
      iconClass <- paste(iconClass, "fa-fw")
    icon(name = NULL, class = iconClass)
  } else NULL
}

#' Dynamically insert/remove a tabPanel
#'
#' Dynamically insert or remove a \code{\link{tabPanel}} from an existing
#' \code{\link{tabsetPanel}}, \code{\link{navlistPanel}} or 
#' \code{\link{navbarPage}}.
#' 
#' When you want to insert a new tab before of after an existing tab, you 
#' should use \code{insertTab}. When you want to prepend a tab (i.e. add a
#' tab to the beginning of the \code{tabsetPanel}), use \code{prependTab}.
#' When you want to append a tab (i.e. add a tab to the end of the 
#' \code{tabsetPanel}), use \code{appendTab}.
#' 
#' For \code{navbarPage}, you can insert/remove conventional 
#' \code{tabPanel}s (whether at the top level or nested inside a
#' \code{navbarMenu}), as well as an entire \code{\link{navbarMenu}}. 
#' For the latter case, \code{target} should be the \code{menuName} that 
#' you gave your \code{navbarMenu} when you first created it (by default, 
#' this is equal to the value of the \code{title} argument).
#'
#' @param inputId The \code{id} of the \code{tabsetPanel} (or 
#'   \code{navlistPanel} or \code{navbarPage})into which \code{tab} will 
#'   be inserted/removed.
#'
#' @param tab The tab element to be added (must be created with
#'   \code{tabPanel}).
#'
#' @param target The \code{value} of an existing \code{tabPanel}, next to
#'   which \code{tab} will be added. If \code{NULL},the \code{tab} will be
#'   placed either as the first tab or the last tab, depending on the
#'   \code{position} argument.
#'
#' @param position Should \code{tab} be added before or after the 
#' \code{target} tab?
#'
#' @param session The shiny session within which to call \code{insertTab}.
#'
#' @seealso \code{\link{showTab}}
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       actionButton("add", "Add 'Dynamic' tab"),
#'       actionButton("remove", "Remove 'Foo' tab")
#'     ),
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
#'     insertTab(inputId = "tabs",
#'       tabPanel("Dynamic", "This a dynamically-added tab"),
#'       target = "Bar"
#'     )
#'   })
#'   observeEvent(input$remove, {
#'     removeTab(inputId = "tabs", target = "Foo")
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
insertTab <- function(inputId, tab, target,
  position = c("before", "after"),
  session = getDefaultReactiveDomain()) {
  
  force(inputId)
  force(tab)
  force(target)
  position <- match.arg(position)
  force(session)

  callback <- function() {
    session$sendInsertTab(
      inputId = inputId,
      tab = processDeps(tab, session),
      icon = processDeps(getIcon(tag), session),
      target = target,
      prepend = FALSE,
      append = FALSE,
      position = position)
  }

  session$onFlushed(callback, once = TRUE)
}

#' @rdname insertTab
#' @export
prependTab <- function(inputId, tab, menuName = NULL,
  session = getDefaultReactiveDomain()) {

  force(inputId)
  force(tab)
  force(menuName)
  force(session)
  
  callback <- function() {
      session$sendInsertTab(
        inputId = inputId,
        tab = processDeps(tab, session),
        icon = processDeps(getIcon(tag), session),
        target = NULL,
        prepend = TRUE,
        append = FALSE,
        position = NULL)
    }

  session$onFlushed(callback, once = TRUE)
}

#' @rdname insertTab
#' @export
appendTab <- function(inputId, tab, menuName = NULL,
  session = getDefaultReactiveDomain()) {

  force(inputId)
  force(tab)
  force(menuName)
  force(session)
  
  callback <- function() {
      session$sendInsertTab(
        inputId = inputId,
        tab = processDeps(tab, session),
        icon = processDeps(getIcon(tag), session),
        target = NULL,
        prepend = FALSE,
        append = TRUE,
        position = NULL)
    }

  session$onFlushed(callback, once = TRUE)
}

#' @rdname insertTab
#' @export
removeTab <- function(inputId, target, immediate = FALSE,
  session = getDefaultReactiveDomain()) {

  force(inputId)
  force(target)
  force(session)

  callback <- function() {
    session$sendRemoveTab(
      inputId = inputId,
      target = target)
  }

  if (!immediate) session$onFlushed(callback, once = TRUE)
  else callback()
}


#' Dynamically hide/show a tabPanel
#'
#' Dynamically hide or show a \code{\link{tabPanel}} from an existing
#' \code{\link{tabsetPanel}}, \code{\link{navlistPanel}} or 
#' \code{\link{navbarPage}}.
#' 
#' For \code{navbarPage}, you can hide/show conventional 
#' \code{tabPanel}s (whether at the top level or nested inside a
#' \code{navbarMenu}), as well as an entire \code{\link{navbarMenu}}. 
#' For the latter case, \code{target} should be the \code{menuName} that 
#' you gave your \code{navbarMenu} when you first created it (by default, 
#' this is equal to the value of the \code{title} argument).
#'
#' @param inputId The \code{id} of the \code{tabsetPanel} (or 
#'   \code{navlistPanel} or \code{navbarPage})into which \code{tab} will 
#'   be inserted/removed.
#'
#' @param target The \code{value} of the \code{tabPanel} to be
#'   hidden/shown. See Details if you want to hide/show an entire
#'   \code{navbarMenu} instead.
#'   
#' @param session The shiny session within which to call this 
#'   function.
#'
#' @seealso \code{\link{insertTab}}
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(actionButton("show", "Show tab")),
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
#'   # Hide tab as soon as app starts up
#'   hideTab(inputId = "tabs", target = "Foo")
#'
#'   observeEvent(input$show, {
#'     showTab(inputId = "tabs", target = "Foo")
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' 
#' # TODO: add example usage for `navbarMenu`
#' 
#' @export
showTab <- function(inputId, target,
  session = getDefaultReactiveDomain()) {

  force(inputId)
  force(target)
  force(session)

  callback <- function() {
    session$sendChangeTabVisibility(
      inputId = inputId,
      target = target,
      type = "show"
    )
  }

  session$onFlushed(callback, once = TRUE)
}

#' @rdname showTab
#' @export
hideTab <- function(inputId, target,
  session = getDefaultReactiveDomain()) {

  force(inputId)
  force(target)
  force(session)

  callback <- function() {
    session$sendChangeTabVisibility(
      inputId = inputId,
      target = target,
      type = "hide"
    )
  }

  session$onFlushed(callback, once = TRUE)
}
