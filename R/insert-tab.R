#' Dynamically insert/remove a tabPanel
#'
#' Dynamically insert or remove a [tabPanel()] (or a
#' [navbarMenu()]) from an existing [tabsetPanel()],
#' [navlistPanel()] or [navbarPage()].
#'
#' When you want to insert a new tab before or after an existing tab, you
#' should use `insertTab`. When you want to prepend a tab (i.e. add a
#' tab to the beginning of the `tabsetPanel`), use `prependTab`.
#' When you want to append a tab (i.e. add a tab to the end of the
#' `tabsetPanel`), use `appendTab`.
#'
#' For `navbarPage`, you can insert/remove conventional
#' `tabPanel`s (whether at the top level or nested inside a
#' `navbarMenu`), as well as an entire [navbarMenu()].
#' For the latter case, `target` should be the `menuName` that
#' you gave your `navbarMenu` when you first created it (by default,
#' this is equal to the value of the `title` argument).
#'
#' @param inputId The `id` of the `tabsetPanel` (or
#'   `navlistPanel` or `navbarPage`) into which `tab` will
#'   be inserted/removed.
#'
#' @param tab The item to be added (must be created with `tabPanel`,
#'   or with `navbarMenu`).
#'
#' @param target If inserting: the `value` of an existing
#'   `tabPanel`, next to which `tab` will be added.
#'   If removing: the `value` of the `tabPanel` that
#'   you want to remove. See Details if you want to insert next to/remove
#'   an entire `navbarMenu` instead.
#'
#' @param position Should `tab` be added before or after the
#'   `target` tab?
#'
#' @param select Should `tab` be selected upon being inserted?
#'
#' @param session The shiny session within which to call this function.
#'
#' @seealso [showTab()]
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'
#' # example app for inserting/removing a tab
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
#'
#'
#' # example app for prepending/appending a navbarMenu
#' ui <- navbarPage("Navbar page", id = "tabs",
#'   tabPanel("Home",
#'     actionButton("prepend", "Prepend a navbarMenu"),
#'     actionButton("append", "Append a navbarMenu")
#'   )
#' )
#' server <- function(input, output, session) {
#'   observeEvent(input$prepend, {
#'     id <- paste0("Dropdown", input$prepend, "p")
#'     prependTab(inputId = "tabs",
#'       navbarMenu(id,
#'         tabPanel("Drop1", paste("Drop1 page from", id)),
#'         tabPanel("Drop2", paste("Drop2 page from", id)),
#'         "------",
#'         "Header",
#'         tabPanel("Drop3", paste("Drop3 page from", id))
#'       )
#'     )
#'   })
#'   observeEvent(input$append, {
#'     id <- paste0("Dropdown", input$append, "a")
#'     appendTab(inputId = "tabs",
#'       navbarMenu(id,
#'         tabPanel("Drop1", paste("Drop1 page from", id)),
#'         tabPanel("Drop2", paste("Drop2 page from", id)),
#'         "------",
#'         "Header",
#'         tabPanel("Drop3", paste("Drop3 page from", id))
#'       )
#'     )
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' }
#' @export
insertTab <- function(inputId, tab, target = NULL,
                      position = c("after", "before"), select = FALSE,
                      session = getDefaultReactiveDomain()) {
  bslib::nav_insert(
    inputId, tab, target,
    match.arg(position), select, session
  )
}

#' @param menuName This argument should only be used when you want to
#'   prepend (or append) `tab` to the beginning (or end) of an
#'   existing [navbarMenu()] (which must itself be part of
#'   an existing [navbarPage()]). In this case, this argument
#'   should be the `menuName` that you gave your `navbarMenu`
#'   when you first created it (by default, this is equal to the value
#'   of the `title` argument). Note that you still need to set the
#'   `inputId` argument to whatever the `id` of the parent
#'   `navbarPage` is. If `menuName` is left as `NULL`,
#'   `tab` will be prepended (or appended) to whatever
#'   `inputId` is.
#'
#' @rdname insertTab
#' @export
prependTab <- function(inputId, tab, select = FALSE, menuName = NULL,
                       session = getDefaultReactiveDomain()) {
  bslib::nav_prepend(inputId, tab, menu_title = menuName, select = select, session = session)
}

#' @rdname insertTab
#' @export
appendTab <- function(inputId, tab, select = FALSE, menuName = NULL,
                      session = getDefaultReactiveDomain()) {
  bslib::nav_append(inputId, tab, menu_title = menuName, select = select, session = session)
}

#' @rdname insertTab
#' @export
removeTab <- function(inputId, target,
                      session = getDefaultReactiveDomain()) {
  bslib::nav_remove(inputId, target, session)
}


#' Dynamically hide/show a tabPanel
#'
#' Dynamically hide or show a [tabPanel()] (or a
#' [navbarMenu()])from an existing [tabsetPanel()],
#' [navlistPanel()] or [navbarPage()].
#'
#' For `navbarPage`, you can hide/show conventional
#' `tabPanel`s (whether at the top level or nested inside a
#' `navbarMenu`), as well as an entire [navbarMenu()].
#' For the latter case, `target` should be the `menuName` that
#' you gave your `navbarMenu` when you first created it (by default,
#' this is equal to the value of the `title` argument).
#'
#' @param inputId The `id` of the `tabsetPanel` (or
#'   `navlistPanel` or `navbarPage`) in which to find
#'   `target`.
#'
#' @param target The `value` of the `tabPanel` to be
#'   hidden/shown. See Details if you want to hide/show an entire
#'   `navbarMenu` instead.
#'
#' @param select Should `target` be selected upon being shown?
#'
#' @param session The shiny session within which to call this function.
#'
#' @seealso [insertTab()]
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'
#' ui <- navbarPage("Navbar page", id = "tabs",
#'   tabPanel("Home",
#'     actionButton("hideTab", "Hide 'Foo' tab"),
#'     actionButton("showTab", "Show 'Foo' tab"),
#'     actionButton("hideMenu", "Hide 'More' navbarMenu"),
#'     actionButton("showMenu", "Show 'More' navbarMenu")
#'   ),
#'   tabPanel("Foo", "This is the foo tab"),
#'   tabPanel("Bar", "This is the bar tab"),
#'   navbarMenu("More",
#'     tabPanel("Table", "Table page"),
#'     tabPanel("About", "About page"),
#'     "------",
#'     "Even more!",
#'     tabPanel("Email", "Email page")
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$hideTab, {
#'     hideTab(inputId = "tabs", target = "Foo")
#'   })
#'
#'   observeEvent(input$showTab, {
#'     showTab(inputId = "tabs", target = "Foo")
#'   })
#'
#'   observeEvent(input$hideMenu, {
#'     hideTab(inputId = "tabs", target = "More")
#'   })
#'
#'   observeEvent(input$showMenu, {
#'     showTab(inputId = "tabs", target = "More")
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @export
showTab <- function(inputId, target, select = FALSE,
                    session = getDefaultReactiveDomain()) {
  force(target)

  if (select) updateTabsetPanel(session, inputId, selected = target)
  inputId <- session$ns(inputId)

  callback <- function() {
    session$sendChangeTabVisibility(
      inputId = inputId,
      target = target,
      type = "show"
    )
  }
  session$onFlush(callback, once = TRUE)
}

#' @rdname showTab
#' @export
hideTab <- function(inputId, target,
                    session = getDefaultReactiveDomain()) {
  force(target)
  inputId <- session$ns(inputId)

  callback <- function() {
    session$sendChangeTabVisibility(
      inputId = inputId,
      target = target,
      type = "hide"
    )
  }
  session$onFlush(callback, once = TRUE)
}
