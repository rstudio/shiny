#' @include utils.R
NULL

#' Create a Bootstrap page
#'
#' Create a Shiny UI page that loads the CSS and JavaScript for
#' [Bootstrap](http://getbootstrap.com/), and has no content in the page
#' body (other than what you provide).
#'
#' This function is primarily intended for users who are proficient in HTML/CSS,
#' and know how to lay out pages in Bootstrap. Most applications should use
#' [fluidPage()] along with layout functions like
#' [fluidRow()] and [sidebarLayout()].
#'
#' @param ... The contents of the document body.
#' @param title The browser window title (defaults to the host URL of the page)
#' @param responsive This option is deprecated; it is no longer optional with
#'   Bootstrap 3.
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory, e.g. `www/bootstrap.css`)
#'
#' @return A UI defintion that can be passed to the [shinyUI] function.
#'
#' @note The `basicPage` function is deprecated, you should use the
#'   [fluidPage()] function instead.
#'
#' @seealso [fluidPage()], [fixedPage()]
#' @export
bootstrapPage <- function(..., title = NULL, responsive = NULL, theme = NULL) {

  if (!is.null(responsive)) {
    shinyDeprecated("The 'responsive' argument is no longer used with Bootstrap 3.")
  }

  attachDependencies(
    tagList(
      if (!is.null(title)) tags$head(tags$title(title)),
      if (!is.null(theme)) {
        tags$head(tags$link(rel="stylesheet", type="text/css", href = theme))
      },

      # remainder of tags passed to the function
      list(...)
    ),
    bootstrapLib()
  )
}

#' Bootstrap libraries
#'
#' This function returns a set of web dependencies necessary for using Bootstrap
#' components in a web page.
#'
#' It isn't necessary to call this function if you use
#' [bootstrapPage()] or others which use `bootstrapPage`, such
#' [basicPage()], [fluidPage()], [fillPage()],
#' [pageWithSidebar()], and [navbarPage()], because they
#' already include the Bootstrap web dependencies.
#'
#' @inheritParams bootstrapPage
#' @export
bootstrapLib <- function(theme = NULL) {
  htmlDependency("bootstrap", "3.4.1",
    c(
      href = "shared/bootstrap",
      file = system.file("www/shared/bootstrap", package = "shiny")
    ),
    script = c(
      "js/bootstrap.min.js",
      # These shims are necessary for IE 8 compatibility
      "shim/html5shiv.min.js",
      "shim/respond.min.js"
    ),
    stylesheet = if (is.null(theme)) "css/bootstrap.min.css",
    meta = list(viewport = "width=device-width, initial-scale=1")
  )
}

#' @rdname bootstrapPage
#' @export
basicPage <- function(...) {
  bootstrapPage(div(class="container-fluid", list(...)))
}


#' Create a page that fills the window
#'
#' `fillPage` creates a page whose height and width always fill the
#' available area of the browser window.
#'
#' The [fluidPage()] and [fixedPage()] functions are used
#' for creating web pages that are laid out from the top down, leaving
#' whitespace at the bottom if the page content's height is smaller than the
#' browser window, and scrolling if the content is larger than the window.
#'
#' `fillPage` is designed to latch the document body's size to the size of
#' the window. This makes it possible to fill it with content that also scales
#' to the size of the window.
#'
#' For example, `fluidPage(plotOutput("plot", height = "100%"))` will not
#' work as expected; the plot element's effective height will be `0`,
#' because the plot's containing elements (`<div>` and `<body>`) have
#' *automatic* height; that is, they determine their own height based on
#' the height of their contained elements. However,
#' `fillPage(plotOutput("plot", height = "100%"))` will work because
#' `fillPage` fixes the `<body>` height at 100% of the window height.
#'
#' Note that `fillPage(plotOutput("plot"))` will not cause the plot to fill
#' the page. Like most Shiny output widgets, `plotOutput`'s default height
#' is a fixed number of pixels. You must explicitly set `height = "100%"`
#' if you want a plot (or htmlwidget, say) to fill its container.
#'
#' One must be careful what layouts/panels/elements come between the
#' `fillPage` and the plots/widgets. Any container that has an automatic
#' height will cause children with `height = "100%"` to misbehave. Stick
#' to functions that are designed for fill layouts, such as the ones in this
#' package.
#'
#' @param ... Elements to include within the page.
#' @param padding Padding to use for the body. This can be a numeric vector
#'   (which will be interpreted as pixels) or a character vector with valid CSS
#'   lengths. The length can be between one and four. If one, then that value
#'   will be used for all four sides. If two, then the first value will be used
#'   for the top and bottom, while the second value will be used for left and
#'   right. If three, then the first will be used for top, the second will be
#'   left and right, and the third will be bottom. If four, then the values will
#'   be interpreted as top, right, bottom, and left respectively.
#' @param title The title to use for the browser window/tab (it will not be
#'   shown in the document).
#' @param bootstrap If `TRUE`, load the Bootstrap CSS library.
#' @param theme URL to alternative Bootstrap stylesheet.
#'
#' @family layout functions
#'
#' @examples
#' fillPage(
#'   tags$style(type = "text/css",
#'     ".half-fill { width: 50%; height: 100%; }",
#'     "#one { float: left; background-color: #ddddff; }",
#'     "#two { float: right; background-color: #ccffcc; }"
#'   ),
#'   div(id = "one", class = "half-fill",
#'     "Left half"
#'   ),
#'   div(id = "two", class = "half-fill",
#'     "Right half"
#'   ),
#'   padding = 10
#' )
#'
#' fillPage(
#'   fillRow(
#'     div(style = "background-color: red; width: 100%; height: 100%;"),
#'     div(style = "background-color: blue; width: 100%; height: 100%;")
#'   )
#' )
#' @export
fillPage <- function(..., padding = 0, title = NULL, bootstrap = TRUE,
  theme = NULL) {

  fillCSS <- tags$head(tags$style(type = "text/css",
    "html, body { width: 100%; height: 100%; overflow: hidden; }",
    sprintf("body { padding: %s; margin: 0; }", collapseSizes(padding))
  ))

  if (isTRUE(bootstrap)) {
    bootstrapPage(title = title, theme = theme, fillCSS, ...)
  } else {
    tagList(
      fillCSS,
      if (!is.null(title)) tags$head(tags$title(title)),
      ...
    )
  }
}

collapseSizes <- function(padding) {
  paste(
    sapply(padding, shiny::validateCssUnit, USE.NAMES = FALSE),
    collapse = " ")
}

#' Create a page with a top level navigation bar
#'
#' Create a page that contains a top level navigation bar that can be used to
#' toggle a set of [tabPanel()] elements.
#'
#' @param title The title to display in the navbar
#' @param ... [tabPanel()] elements to include in the page. The
#'   `navbarMenu` function also accepts strings, which will be used as menu
#'   section headers. If the string is a set of dashes like `"----"` a
#'   horizontal separator will be displayed in the menu.
#' @param id If provided, you can use `input$`*`id`* in your
#'   server logic to determine which of the current tabs is active. The value
#'   will correspond to the `value` argument that is passed to
#'   [tabPanel()].
#' @param selected The `value` (or, if none was supplied, the `title`)
#'   of the tab that should be selected by default. If `NULL`, the first
#'   tab will be selected.
#' @param position Determines whether the navbar should be displayed at the top
#'   of the page with normal scrolling behavior (`"static-top"`), pinned at
#'   the top (`"fixed-top"`), or pinned at the bottom
#'   (`"fixed-bottom"`). Note that using `"fixed-top"` or
#'   `"fixed-bottom"` will cause the navbar to overlay your body content,
#'   unless you add padding, e.g.: \code{tags$style(type="text/css", "body
#'   {padding-top: 70px;}")}
#' @param header Tag or list of tags to display as a common header above all
#'   tabPanels.
#' @param footer Tag or list of tags to display as a common footer below all
#'   tabPanels
#' @param inverse `TRUE` to use a dark background and light text for the
#'   navigation bar
#' @param collapsible `TRUE` to automatically collapse the navigation
#'   elements into a menu when the width of the browser is less than 940 pixels
#'   (useful for viewing on smaller touchscreen device)
#' @param collapsable Deprecated; use `collapsible` instead.
#' @param fluid `TRUE` to use a fluid layout. `FALSE` to use a fixed
#'   layout.
#' @param responsive This option is deprecated; it is no longer optional with
#'   Bootstrap 3.
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory). For example, to use the theme located at
#'   `www/bootstrap.css` you would use `theme = "bootstrap.css"`.
#' @param windowTitle The title that should be displayed by the browser window.
#'   Useful if `title` is not a string.
#' @param icon Optional icon to appear on a `navbarMenu` tab.
#'
#' @return A UI defintion that can be passed to the [shinyUI] function.
#'
#' @details The `navbarMenu` function can be used to create an embedded
#'   menu within the navbar that in turns includes additional tabPanels (see
#'   example below).
#'
#' @seealso [tabPanel()], [tabsetPanel()],
#'   [updateNavbarPage()], [insertTab()],
#'   [showTab()]
#'
#' @family layout functions
#'
#' @examples
#' navbarPage("App Title",
#'   tabPanel("Plot"),
#'   tabPanel("Summary"),
#'   tabPanel("Table")
#' )
#'
#' navbarPage("App Title",
#'   tabPanel("Plot"),
#'   navbarMenu("More",
#'     tabPanel("Summary"),
#'     "----",
#'     "Section header",
#'     tabPanel("Table")
#'   )
#' )
#' @export
navbarPage <- function(title,
                       ...,
                       id = NULL,
                       selected = NULL,
                       position = c("static-top", "fixed-top", "fixed-bottom"),
                       header = NULL,
                       footer = NULL,
                       inverse = FALSE,
                       collapsible = FALSE,
                       collapsable,
                       fluid = TRUE,
                       responsive = NULL,
                       theme = NULL,
                       windowTitle = title) {

  if (!missing(collapsable)) {
    shinyDeprecated("`collapsable` is deprecated; use `collapsible` instead.")
    collapsible <- collapsable
  }

  # alias title so we can avoid conflicts w/ title in withTags
  pageTitle <- title

  # navbar class based on options
  navbarClass <- "navbar navbar-default"
  position <- match.arg(position)
  if (!is.null(position))
    navbarClass <- paste(navbarClass, " navbar-", position, sep = "")
  if (inverse)
    navbarClass <- paste(navbarClass, "navbar-inverse")

  if (!is.null(id))
    selected <- restoreInput(id = id, default = selected)

  # build the tabset
  tabs <- list(...)
  tabset <- buildTabset(tabs, "nav navbar-nav", NULL, id, selected)

  # function to return plain or fluid class name
  className <- function(name) {
    if (fluid)
      paste(name, "-fluid", sep="")
    else
      name
  }

  # built the container div dynamically to support optional collapsibility
  if (collapsible) {
    navId <- paste("navbar-collapse-", p_randomInt(1000, 10000), sep="")
    containerDiv <- div(class=className("container"),
      div(class="navbar-header",
        tags$button(type="button", class="navbar-toggle collapsed",
          `data-toggle`="collapse", `data-target`=paste0("#", navId),
          span(class="sr-only", "Toggle navigation"),
          span(class="icon-bar"),
          span(class="icon-bar"),
          span(class="icon-bar")
        ),
        span(class="navbar-brand", pageTitle)
      ),
      div(class="navbar-collapse collapse", id=navId, tabset$navList)
    )
  } else {
    containerDiv <- div(class=className("container"),
      div(class="navbar-header",
        span(class="navbar-brand", pageTitle)
      ),
      tabset$navList
    )
  }

  # build the main tab content div
  contentDiv <- div(class=className("container"))
  if (!is.null(header))
    contentDiv <- tagAppendChild(contentDiv, div(class="row", header))
  contentDiv <- tagAppendChild(contentDiv, tabset$content)
  if (!is.null(footer))
    contentDiv <- tagAppendChild(contentDiv, div(class="row", footer))

  # build the page
  bootstrapPage(
    title = windowTitle,
    responsive = responsive,
    theme = theme,
    tags$nav(class=navbarClass, role="navigation", containerDiv),
    contentDiv
  )
}

#' @param menuName A name that identifies this `navbarMenu`. This
#'   is needed if you want to insert/remove or show/hide an entire
#'   `navbarMenu`.
#'
#' @rdname navbarPage
#' @export
navbarMenu <- function(title, ..., menuName = title, icon = NULL) {
  structure(list(title = title,
                 menuName = menuName,
                 tabs = list(...),
                 iconClass = iconClass(icon)),
            class = "shiny.navbarmenu")
}

#' Create a well panel
#'
#' Creates a panel with a slightly inset border and grey background. Equivalent
#' to Bootstrap's `well` CSS class.
#'
#' @param ... UI elements to include inside the panel.
#' @return The newly created panel.
#' @export
wellPanel <- function(...) {
  div(class="well", ...)
}

#' Conditional Panel
#'
#' Creates a panel that is visible or not, depending on the value of a
#' JavaScript expression. The JS expression is evaluated once at startup and
#' whenever Shiny detects a relevant change in input/output.
#'
#' In the JS expression, you can refer to `input` and `output`
#' JavaScript objects that contain the current values of input and output. For
#' example, if you have an input with an id of `foo`, then you can use
#' `input.foo` to read its value. (Be sure not to modify the input/output
#' objects, as this may cause unpredictable behavior.)
#'
#' @param condition A JavaScript expression that will be evaluated repeatedly to
#'   determine whether the panel should be displayed.
#' @param ns The [`namespace()`][NS] object of the current module, if
#'   any.
#' @param ... Elements to include in the panel.
#'
#' @note You are not recommended to use special JavaScript characters such as a
#'   period `.` in the input id's, but if you do use them anyway, for
#'   example, `inputId = "foo.bar"`, you will have to use
#'   `input["foo.bar"]` instead of `input.foo.bar` to read the input
#'   value.
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   ui <- fluidPage(
#'     sidebarPanel(
#'       selectInput("plotType", "Plot Type",
#'         c(Scatter = "scatter", Histogram = "hist")
#'       ),
#'       # Only show this panel if the plot type is a histogram
#'       conditionalPanel(
#'         condition = "input.plotType == 'hist'",
#'         selectInput(
#'           "breaks", "Breaks",
#'           c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
#'         ),
#'         # Only show this panel if Custom is selected
#'         conditionalPanel(
#'           condition = "input.breaks == 'custom'",
#'           sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
#'         )
#'       )
#'     ),
#'     mainPanel(
#'       plotOutput("plot")
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     x <- rnorm(100)
#'     y <- rnorm(100)
#'
#'     output$plot <- renderPlot({
#'       if (input$plotType == "scatter") {
#'         plot(x, y)
#'       } else {
#'         breaks <- input$breaks
#'         if (breaks == "custom") {
#'           breaks <- input$breakCount
#'         }
#'
#'         hist(x, breaks = breaks)
#'       }
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' @export
conditionalPanel <- function(condition, ..., ns = NS(NULL)) {
  div(`data-display-if`=condition, `data-ns-prefix`=ns(""), ...)
}

#' Create a help text element
#'
#' Create help text which can be added to an input form to provide additional
#' explanation or context.
#'
#' @param ... One or more help text strings (or other inline HTML elements)
#' @return A help text element that can be added to a UI definition.
#'
#' @examples
#' helpText("Note: while the data view will show only",
#'          "the specified number of observations, the",
#'          "summary will be based on the full dataset.")
#' @export
helpText <- function(...) {
  span(class="help-block", ...)
}


#' Create a tab panel
#'
#'
#' @param title Display title for tab
#' @param ... UI elements to include within the tab
#' @param value The value that should be sent when `tabsetPanel` reports
#'   that this tab is selected. If omitted and `tabsetPanel` has an
#'   `id`, then the title will be used.
#' @param icon Optional icon to appear on the tab. This attribute is only
#' valid when using a `tabPanel` within a [navbarPage()].
#' @return A tab that can be passed to [tabsetPanel()]
#'
#' @seealso [tabsetPanel()]
#'
#' @examples
#' # Show a tabset that includes a plot, summary, and
#' # table view of the generated distribution
#' mainPanel(
#'   tabsetPanel(
#'     tabPanel("Plot", plotOutput("plot")),
#'     tabPanel("Summary", verbatimTextOutput("summary")),
#'     tabPanel("Table", tableOutput("table"))
#'   )
#' )
#' @export
#' @describeIn tabPanel Create a tab panel that can be included within a [tabsetPanel()] or a [navbarPage()].
tabPanel <- function(title, ..., value = title, icon = NULL) {
  div(
    class = "tab-pane",
    title = title,
    `data-value` = value,
    `data-icon-class` = iconClass(icon),
    ...
  )
}
#' @export
#' @describeIn tabPanel Create a tab panel that drops the title argument.
#'   This function should be used within `tabsetPanel(type = "hidden")`. See [tabsetPanel()] for example usage.
tabPanelBody <- function(value, ..., icon = NULL) {
  if (
    !is.character(value) ||
    length(value) != 1 ||
    any(is.na(value)) ||
    nchar(value) == 0
  ) {
    stop("`value` must be a single, non-empty string value")
  }
  tabPanel(title = NULL, ..., value = value, icon = icon)
}

#' Create a tabset panel
#'
#' Create a tabset that contains [tabPanel()] elements. Tabsets are
#' useful for dividing output into multiple independently viewable sections.
#'
#' @param ... [tabPanel()] elements to include in the tabset
#' @param id If provided, you can use `input$`*`id`* in your
#'   server logic to determine which of the current tabs is active. The value
#'   will correspond to the `value` argument that is passed to
#'   [tabPanel()].
#' @param selected The `value` (or, if none was supplied, the `title`)
#'   of the tab that should be selected by default. If `NULL`, the first
#'   tab will be selected.
#' @param type  \describe{
#'   \item{`"tabs"`}{Standard tab look}
#'   \item{`"pills"`}{Selected tabs use the background fill color}
#'   \item{`"hidden"`}{Hides the selectable tabs. Use `type = "hidden"` in
#'   conjunction with [tabPanelBody()] and [updateTabsetPanel()] to control the
#'   active tab via other input controls. (See example below)}
#' }
#' @param position This argument is deprecated; it has been discontinued in
#'   Bootstrap 3.
#' @return A tabset that can be passed to [mainPanel()]
#'
#' @seealso [tabPanel()], [updateTabsetPanel()],
#'   [insertTab()], [showTab()]
#'
#' @examples
#' # Show a tabset that includes a plot, summary, and
#' # table view of the generated distribution
#' mainPanel(
#'   tabsetPanel(
#'     tabPanel("Plot", plotOutput("plot")),
#'     tabPanel("Summary", verbatimTextOutput("summary")),
#'     tabPanel("Table", tableOutput("table"))
#'   )
#' )
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       radioButtons("controller", "Controller", 1:3, 1)
#'     ),
#'     mainPanel(
#'       tabsetPanel(
#'         id = "hidden_tabs",
#'         # Hide the tab values.
#'         # Can only switch tabs by using `updateTabsetPanel()`
#'         type = "hidden",
#'         tabPanelBody("panel1", "Panel 1 content"),
#'         tabPanelBody("panel2", "Panel 2 content"),
#'         tabPanelBody("panel3", "Panel 3 content")
#'       )
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$controller, {
#'     updateTabsetPanel(session, "hidden_tabs", selected = paste0("panel", input$controller))
#'   })
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' @export
tabsetPanel <- function(...,
                        id = NULL,
                        selected = NULL,
                        type = c("tabs", "pills", "hidden"),
                        position = NULL) {
  if (!is.null(position)) {
    shinyDeprecated(msg = paste("tabsetPanel: argument 'position' is deprecated;",
                                "it has been discontinued in Bootstrap 3."),
                    version = "0.10.2.2")
  }

  if (!is.null(id))
    selected <- restoreInput(id = id, default = selected)

  # build the tabset
  tabs <- list(...)
  type <- match.arg(type)

  tabset <- buildTabset(tabs, paste0("nav nav-", type), NULL, id, selected)

  # create the content
  first <- tabset$navList
  second <- tabset$content

  # create the tab div
  tags$div(class = "tabbable", first, second)
}

#' Create a navigation list panel
#'
#' Create a navigation list panel that provides a list of links on the left
#' which navigate to a set of tabPanels displayed to the right.
#'
#' @param ... [tabPanel()] elements to include in the navlist
#' @param id If provided, you can use `input$`*`id`* in your
#'   server logic to determine which of the current navlist items is active. The
#'   value will correspond to the `value` argument that is passed to
#'   [tabPanel()].
#' @param selected The `value` (or, if none was supplied, the `title`)
#'   of the navigation item that should be selected by default. If `NULL`,
#'   the first navigation will be selected.
#' @param well `TRUE` to place a well (gray rounded rectangle) around the
#'   navigation list.
#' @param fluid `TRUE` to use fluid layout; `FALSE` to use fixed
#'   layout.
#' @param widths Column withs of the navigation list and tabset content areas
#'   respectively.
#'
#' @details You can include headers within the `navlistPanel` by including
#'   plain text elements in the list. Versions of Shiny before 0.11 supported
#'   separators with "------", but as of 0.11, separators were no longer
#'   supported. This is because version 0.11 switched to Bootstrap 3, which
#'   doesn't support separators.
#'
#' @seealso [tabPanel()], [updateNavlistPanel()],
#'    [insertTab()], [showTab()]
#'
#' @examples
#' fluidPage(
#'
#'   titlePanel("Application Title"),
#'
#'   navlistPanel(
#'     "Header",
#'     tabPanel("First"),
#'     tabPanel("Second"),
#'     tabPanel("Third")
#'   )
#' )
#' @export
navlistPanel <- function(...,
                         id = NULL,
                         selected = NULL,
                         well = TRUE,
                         fluid = TRUE,
                         widths = c(4, 8)) {

  # text filter for headers
  textFilter <- function(text) {
      tags$li(class="navbar-brand", text)
  }

  if (!is.null(id))
    selected <- restoreInput(id = id, default = selected)

  # build the tabset
  tabs <- list(...)
  tabset <- buildTabset(tabs,
                        "nav nav-pills nav-stacked",
                        textFilter,
                        id,
                        selected)

  # create the columns
  columns <- list(
    column(widths[[1]], class=ifelse(well, "well", ""), tabset$navList),
    column(widths[[2]], tabset$content)
  )

  # return the row
  if (fluid)
    fluidRow(columns)
  else
    fixedRow(columns)
}

# Helpers to build tabsetPanels (& Co.) and their elements
markTabAsSelected <- function(x) {
  attr(x, "selected") <- TRUE
  x
}

isTabSelected <- function(x) {
  isTRUE(attr(x, "selected", exact = TRUE))
}

containsSelectedTab <- function(tabs) {
  any(vapply(tabs, isTabSelected, logical(1)))
}

findAndMarkSelectedTab <- function(tabs, selected, foundSelected) {
  tabs <- lapply(tabs, function(div) {
    if (foundSelected || is.character(div)) {
      # Strings are not selectable items

    } else if (inherits(div, "shiny.navbarmenu")) {
      # Recur for navbarMenus
      res <- findAndMarkSelectedTab(div$tabs, selected, foundSelected)
      div$tabs <- res$tabs
      foundSelected <<- res$foundSelected

    } else {
      # Base case: regular tab item. If the `selected` argument is
      # provided, check for a match in the existing tabs; else,
      # mark first available item as selected
      if (is.null(selected)) {
        foundSelected <<- TRUE
        div <- markTabAsSelected(div)
      } else {
        tabValue <- div$attribs$`data-value` %OR% div$attribs$title
        if (identical(selected, tabValue)) {
          foundSelected <<- TRUE
          div <- markTabAsSelected(div)
        }
      }
    }
    return(div)
  })
  return(list(tabs = tabs, foundSelected = foundSelected))
}

# Returns the icon object (or NULL if none), provided either a
# tabPanel, OR the icon class
getIcon <- function(tab = NULL, iconClass = NULL) {
  if (!is.null(tab)) iconClass <- tab$attribs$`data-icon-class`
  if (!is.null(iconClass)) {
    if (grepl("fa-", iconClass, fixed = TRUE)) {
      # for font-awesome we specify fixed-width
      iconClass <- paste(iconClass, "fa-fw")
    }
    icon(name = NULL, class = iconClass)
  } else NULL
}

# Text filter for navbarMenu's (plain text) separators
navbarMenuTextFilter <- function(text) {
  if (grepl("^\\-+$", text)) tags$li(class = "divider")
  else tags$li(class = "dropdown-header", text)
}

# This function is called internally by navbarPage, tabsetPanel
# and navlistPanel
buildTabset <- function(tabs, ulClass, textFilter = NULL, id = NULL,
                        selected = NULL, foundSelected = FALSE) {

  res <- findAndMarkSelectedTab(tabs, selected, foundSelected)
  tabs <- res$tabs
  foundSelected <- res$foundSelected

  # add input class if we have an id
  if (!is.null(id)) ulClass <- paste(ulClass, "shiny-tab-input")

  if (anyNamed(tabs)) {
    nms <- names(tabs)
    nms <- nms[nzchar(nms)]
    stop("Tabs should all be unnamed arguments, but some are named: ",
      paste(nms, collapse = ", "))
  }

  tabsetId <- p_randomInt(1000, 10000)
  tabs <- lapply(seq_len(length(tabs)), buildTabItem,
            tabsetId = tabsetId, foundSelected = foundSelected,
            tabs = tabs, textFilter = textFilter)

  tabNavList <- tags$ul(class = ulClass, id = id,
                  `data-tabsetid` = tabsetId, lapply(tabs, "[[", 1))

  tabContent <- tags$div(class = "tab-content",
                  `data-tabsetid` = tabsetId, lapply(tabs, "[[", 2))

  list(navList = tabNavList, content = tabContent)
}

# Builds tabPanel/navbarMenu items (this function used to be
# declared inside the buildTabset() function and it's been
# refactored for clarity and reusability). Called internally
# by buildTabset.
buildTabItem <- function(index, tabsetId, foundSelected, tabs = NULL,
                         divTag = NULL, textFilter = NULL) {

  divTag <- if (!is.null(divTag)) divTag else tabs[[index]]

  if (is.character(divTag) && !is.null(textFilter)) {
    # text item: pass it to the textFilter if it exists
    liTag <- textFilter(divTag)
    divTag <- NULL

  } else if (inherits(divTag, "shiny.navbarmenu")) {
    # navbarMenu item: build the child tabset
    tabset <- buildTabset(divTag$tabs, "dropdown-menu",
      navbarMenuTextFilter, foundSelected = foundSelected)

    # if this navbarMenu contains a selected item, mark it active
    containsSelected <- containsSelectedTab(divTag$tabs)
    liTag <- tags$li(
      class = paste0("dropdown", if (containsSelected) " active"),
      tags$a(href = "#",
        class = "dropdown-toggle", `data-toggle` = "dropdown",
        `data-value` = divTag$menuName,
        getIcon(iconClass = divTag$iconClass),
        divTag$title, tags$b(class = "caret")
      ),
      tabset$navList   # inner tabPanels items
    )
    # list of tab content divs from the child tabset
    divTag <- tabset$content$children

  } else {
    # tabPanel item: create the tab's liTag and divTag
    tabId <- paste("tab", tabsetId, index, sep = "-")
    liTag <- tags$li(
               tags$a(
                 href = paste("#", tabId, sep = ""),
                 `data-toggle` = "tab",
                 `data-value` = divTag$attribs$`data-value`,
                 getIcon(iconClass = divTag$attribs$`data-icon-class`),
                 divTag$attribs$title
               )
    )
    # if this tabPanel is selected item, mark it active
    if (isTabSelected(divTag)) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "tab-pane active"
    }
    divTag$attribs$id <- tabId
    divTag$attribs$title <- NULL
  }
  return(list(liTag = liTag, divTag = divTag))
}


#' Create a text output element
#'
#' Render a reactive output variable as text within an application page.
#' `textOutput()` is usually paired with [renderText()] and puts regular text
#' in `<div>` or `<span>`; `verbatimTextOutput()` is usually paired with
#' [renderPrint()] and provudes fixed-width text in a `<pre>`.
#'
#' In both funtions, text is HTML-escaped prior to rendering.
#'
#' @param outputId output variable to read the value from
#' @param container a function to generate an HTML element to contain the text
#' @param inline use an inline (`span()`) or block container (`div()`)
#'   for the output
#' @return A output element for use in UI.
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   shinyApp(
#'     ui = basicPage(
#'       textInput("txt", "Enter the text to display below:"),
#'       textOutput("text"),
#'       verbatimTextOutput("verb")
#'     ),
#'     server = function(input, output) {
#'       output$text <- renderText({ input$txt })
#'       output$verb <- renderText({ input$txt })
#'     }
#'   )
#' }
#' @export
textOutput <- function(outputId, container = if (inline) span else div, inline = FALSE) {
  container(id = outputId, class = "shiny-text-output")
}

#' @param placeholder if the output is empty or `NULL`, should an empty
#'   rectangle be displayed to serve as a placeholder? (does not affect
#'   behavior when the the output in nonempty)
#' @export
#' @rdname textOutput
verbatimTextOutput <- function(outputId, placeholder = FALSE) {
  pre(id = outputId,
      class = paste(c("shiny-text-output", if (!placeholder) "noplaceholder"),
                    collapse = " ")
      )
}


#' @name plotOutput
#' @rdname plotOutput
#' @export
imageOutput <- function(outputId, width = "100%", height="400px",
                        click = NULL, dblclick = NULL, hover = NULL, brush = NULL,
                        inline = FALSE) {

  style <- if (!inline) {
    paste("width:", validateCssUnit(width), ";", "height:", validateCssUnit(height))
  }


  # Build up arguments for call to div() or span()
  args <- list(
    id = outputId,
    class = "shiny-image-output",
    style = style
  )

  # Given a named list with options, replace names like "delayType" with
  # "data-hover-delay-type" (given a prefix "hover")
  formatOptNames <- function(opts, prefix) {
    newNames <- paste("data", prefix, names(opts), sep = "-")
    # Replace capital letters with "-" and lowercase letter
    newNames <- gsub("([A-Z])", "-\\L\\1", newNames, perl = TRUE)
    names(opts) <- newNames
    opts
  }

  if (!is.null(click)) {
    # If click is a string, turn it into clickOpts object
    if (is.character(click)) {
      click <- clickOpts(id = click)
    }
    args <- c(args, formatOptNames(click, "click"))
  }

  if (!is.null(dblclick)) {
    if (is.character(dblclick)) {
      dblclick <- clickOpts(id = dblclick)
    }
    args <- c(args, formatOptNames(dblclick, "dblclick"))
  }

  if (!is.null(hover)) {
    if (is.character(hover)) {
      hover <- hoverOpts(id = hover)
    }
    args <- c(args, formatOptNames(hover, "hover"))
  }

  if (!is.null(brush)) {
    if (is.character(brush)) {
      brush <- brushOpts(id = brush)
    }
    args <- c(args, formatOptNames(brush, "brush"))
  }

  container <- if (inline) span else div
  do.call(container, args)
}

#' Create an plot or image output element
#'
#' Render a [renderPlot()] or [renderImage()] within an
#' application page.
#'
#' @section Interactive plots:
#'
#'   Plots and images in Shiny support mouse-based interaction, via clicking,
#'   double-clicking, hovering, and brushing. When these interaction events
#'   occur, the mouse coordinates will be sent to the server as `input$`
#'   variables, as specified by `click`, `dblclick`, `hover`, or
#'   `brush`.
#'
#'   For `plotOutput`, the coordinates will be sent scaled to the data
#'   space, if possible. (At the moment, plots generated by base graphics and
#'   ggplot2 support this scaling, although plots generated by lattice and
#'   others do not.) If scaling is not possible, the raw pixel coordinates will
#'   be sent. For `imageOutput`, the coordinates will be sent in raw pixel
#'   coordinates.
#'
#'   With ggplot2 graphics, the code in `renderPlot` should return a ggplot
#'   object; if instead the code prints the ggplot2 object with something like
#'   `print(p)`, then the coordinates for interactive graphics will not be
#'   properly scaled to the data space.
#'
#' @param outputId output variable to read the plot/image from.
#' @param width,height Image width/height. Must be a valid CSS unit (like
#'   `"100%"`, `"400px"`, `"auto"`) or a number, which will be
#'   coerced to a string and have `"px"` appended. These two arguments are
#'   ignored when `inline = TRUE`, in which case the width/height of a plot
#'   must be specified in `renderPlot()`. Note that, for height, using
#'   `"auto"` or `"100%"` generally will not work as expected,
#'   because of how height is computed with HTML/CSS.
#' @param click This can be `NULL` (the default), a string, or an object
#'   created by the [clickOpts()] function. If you use a value like
#'   `"plot_click"` (or equivalently, `clickOpts(id="plot_click")`),
#'   the plot will send coordinates to the server whenever it is clicked, and
#'   the value will be accessible via `input$plot_click`. The value will be
#'   a named list  with `x` and `y` elements indicating the mouse
#'   position.
#' @param dblclick This is just like the `click` argument, but for
#'   double-click events.
#' @param hover Similar to the `click` argument, this can be `NULL`
#'   (the default), a string, or an object created by the
#'   [hoverOpts()] function. If you use a value like
#'   `"plot_hover"` (or equivalently, `hoverOpts(id="plot_hover")`),
#'   the plot will send coordinates to the server pauses on the plot, and the
#'   value will be accessible via `input$plot_hover`. The value will be a
#'   named list with `x` and `y` elements indicating the mouse
#'   position. To control the hover time or hover delay type, you must use
#'   [hoverOpts()].
#' @param brush Similar to the `click` argument, this can be `NULL`
#'   (the default), a string, or an object created by the
#'   [brushOpts()] function. If you use a value like
#'   `"plot_brush"` (or equivalently, `brushOpts(id="plot_brush")`),
#'   the plot will allow the user to "brush" in the plotting area, and will send
#'   information about the brushed area to the server, and the value will be
#'   accessible via `input$plot_brush`. Brushing means that the user will
#'   be able to draw a rectangle in the plotting area and drag it around. The
#'   value will be a named list with `xmin`, `xmax`, `ymin`, and
#'   `ymax` elements indicating the brush area. To control the brush
#'   behavior, use [brushOpts()]. Multiple
#'   `imageOutput`/`plotOutput` calls may share the same `id`
#'   value; brushing one image or plot will cause any other brushes with the
#'   same `id` to disappear.
#' @inheritParams textOutput
#' @note The arguments `clickId` and `hoverId` only work for R base graphics
#'   (see the \pkg{\link[graphics:graphics-package]{graphics}} package). They do
#'   not work for \pkg{\link[grid:grid-package]{grid}}-based graphics, such as
#'   \pkg{ggplot2}, \pkg{lattice}, and so on.
#' @return A plot or image output element that can be included in a panel.
#' @seealso For the corresponding server-side functions, see [renderPlot()] and
#'   [renderImage()].
#'
#' @examples
#' # Only run these examples in interactive R sessions
#' if (interactive()) {
#'
#' # A basic shiny app with a plotOutput
#' shinyApp(
#'   ui = fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(
#'         actionButton("newplot", "New plot")
#'       ),
#'       mainPanel(
#'         plotOutput("plot")
#'       )
#'     )
#'   ),
#'   server = function(input, output) {
#'     output$plot <- renderPlot({
#'       input$newplot
#'       # Add a little noise to the cars data
#'       cars2 <- cars + rnorm(nrow(cars))
#'       plot(cars2)
#'     })
#'   }
#' )
#'
#'
#' # A demonstration of clicking, hovering, and brushing
#' shinyApp(
#'   ui = basicPage(
#'     fluidRow(
#'       column(width = 4,
#'         plotOutput("plot", height=300,
#'           click = "plot_click",  # Equiv, to click=clickOpts(id="plot_click")
#'           hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
#'           brush = brushOpts(id = "plot_brush")
#'         ),
#'         h4("Clicked points"),
#'         tableOutput("plot_clickedpoints"),
#'         h4("Brushed points"),
#'         tableOutput("plot_brushedpoints")
#'       ),
#'       column(width = 4,
#'         verbatimTextOutput("plot_clickinfo"),
#'         verbatimTextOutput("plot_hoverinfo")
#'       ),
#'       column(width = 4,
#'         wellPanel(actionButton("newplot", "New plot")),
#'         verbatimTextOutput("plot_brushinfo")
#'       )
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     data <- reactive({
#'       input$newplot
#'       # Add a little noise to the cars data so the points move
#'       cars + rnorm(nrow(cars))
#'     })
#'     output$plot <- renderPlot({
#'       d <- data()
#'       plot(d$speed, d$dist)
#'     })
#'     output$plot_clickinfo <- renderPrint({
#'       cat("Click:\n")
#'       str(input$plot_click)
#'     })
#'     output$plot_hoverinfo <- renderPrint({
#'       cat("Hover (throttled):\n")
#'       str(input$plot_hover)
#'     })
#'     output$plot_brushinfo <- renderPrint({
#'       cat("Brush (debounced):\n")
#'       str(input$plot_brush)
#'     })
#'     output$plot_clickedpoints <- renderTable({
#'       # For base graphics, we need to specify columns, though for ggplot2,
#'       # it's usually not necessary.
#'       res <- nearPoints(data(), input$plot_click, "speed", "dist")
#'       if (nrow(res) == 0)
#'         return()
#'       res
#'     })
#'     output$plot_brushedpoints <- renderTable({
#'       res <- brushedPoints(data(), input$plot_brush, "speed", "dist")
#'       if (nrow(res) == 0)
#'         return()
#'       res
#'     })
#'   }
#' )
#'
#'
#' # Demo of clicking, hovering, brushing with imageOutput
#' # Note that coordinates are in pixels
#' shinyApp(
#'   ui = basicPage(
#'     fluidRow(
#'       column(width = 4,
#'         imageOutput("image", height=300,
#'           click = "image_click",
#'           hover = hoverOpts(
#'             id = "image_hover",
#'             delay = 500,
#'             delayType = "throttle"
#'           ),
#'           brush = brushOpts(id = "image_brush")
#'         )
#'       ),
#'       column(width = 4,
#'         verbatimTextOutput("image_clickinfo"),
#'         verbatimTextOutput("image_hoverinfo")
#'       ),
#'       column(width = 4,
#'         wellPanel(actionButton("newimage", "New image")),
#'         verbatimTextOutput("image_brushinfo")
#'       )
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     output$image <- renderImage({
#'       input$newimage
#'
#'       # Get width and height of image output
#'       width  <- session$clientData$output_image_width
#'       height <- session$clientData$output_image_height
#'
#'       # Write to a temporary PNG file
#'       outfile <- tempfile(fileext = ".png")
#'
#'       png(outfile, width=width, height=height)
#'       plot(rnorm(200), rnorm(200))
#'       dev.off()
#'
#'       # Return a list containing information about the image
#'       list(
#'         src = outfile,
#'         contentType = "image/png",
#'         width = width,
#'         height = height,
#'         alt = "This is alternate text"
#'       )
#'     })
#'     output$image_clickinfo <- renderPrint({
#'       cat("Click:\n")
#'       str(input$image_click)
#'     })
#'     output$image_hoverinfo <- renderPrint({
#'       cat("Hover (throttled):\n")
#'       str(input$image_hover)
#'     })
#'     output$image_brushinfo <- renderPrint({
#'       cat("Brush (debounced):\n")
#'       str(input$image_brush)
#'     })
#'   }
#' )
#'
#' }
#' @export
plotOutput <- function(outputId, width = "100%", height="400px",
                       click = NULL, dblclick = NULL, hover = NULL, brush = NULL,
                       inline = FALSE) {

  # Result is the same as imageOutput, except for HTML class
  res <- imageOutput(outputId, width, height, click, dblclick,
                     hover, brush, inline)

  res$attribs$class <- "shiny-plot-output"
  res
}

#' Create a table output element
#'
#' Render a [renderTable()] or [renderDataTable()] within an
#' application page. `renderTable` uses a standard HTML table, while
#' `renderDataTable` uses the DataTables Javascript library to create an
#' interactive table with more features.
#'
#' @param outputId output variable to read the table from
#' @return A table output element that can be included in a panel
#'
#' @seealso [renderTable()], [renderDataTable()].
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   # table example
#'   shinyApp(
#'     ui = fluidPage(
#'       fluidRow(
#'         column(12,
#'           tableOutput('table')
#'         )
#'       )
#'     ),
#'     server = function(input, output) {
#'       output$table <- renderTable(iris)
#'     }
#'   )
#'
#'
#'   # DataTables example
#'   shinyApp(
#'     ui = fluidPage(
#'       fluidRow(
#'         column(12,
#'           dataTableOutput('table')
#'         )
#'       )
#'     ),
#'     server = function(input, output) {
#'       output$table <- renderDataTable(iris)
#'     }
#'   )
#' }
#' @export
tableOutput <- function(outputId) {
  div(id = outputId, class="shiny-html-output")
}

dataTableDependency <- list(
  htmlDependency(
    "datatables", "1.10.5", c(href = "shared/datatables"),
    script = "js/jquery.dataTables.min.js"
  ),
  htmlDependency(
    "datatables-bootstrap", "1.10.5", c(href = "shared/datatables"),
    stylesheet = c("css/dataTables.bootstrap.css", "css/dataTables.extra.css"),
    script = "js/dataTables.bootstrap.js"
  )
)

#' @rdname tableOutput
#' @export
dataTableOutput <- function(outputId) {
  attachDependencies(
    div(id = outputId, class="shiny-datatable-output"),
    dataTableDependency
  )
}

#' Create an HTML output element
#'
#' Render a reactive output variable as HTML within an application page. The
#' text will be included within an HTML `div` tag, and is presumed to
#' contain HTML content which should not be escaped.
#'
#' `uiOutput` is intended to be used with `renderUI` on the server
#' side. It is currently just an alias for `htmlOutput`.
#'
#' @param outputId output variable to read the value from
#' @param ... Other arguments to pass to the container tag function. This is
#'   useful for providing additional classes for the tag.
#' @inheritParams textOutput
#' @return An HTML output element that can be included in a panel
#' @examples
#' htmlOutput("summary")
#'
#' # Using a custom container and class
#' tags$ul(
#'   htmlOutput("summary", container = tags$li, class = "custom-li-output")
#' )
#' @export
htmlOutput <- function(outputId, inline = FALSE,
  container = if (inline) span else div, ...)
{
  if (anyUnnamed(list(...))) {
    warning("Unnamed elements in ... will be replaced with dynamic UI.")
  }
  container(id = outputId, class="shiny-html-output", ...)
}

#' @rdname htmlOutput
#' @export
uiOutput <- htmlOutput

#' Create a download button or link
#'
#' Use these functions to create a download button or link; when clicked, it
#' will initiate a browser download. The filename and contents are specified by
#' the corresponding [downloadHandler()] defined in the server
#' function.
#'
#' @param outputId The name of the output slot that the `downloadHandler`
#'   is assigned to.
#' @param label The label that should appear on the button.
#' @param class Additional CSS classes to apply to the tag, if any.
#' @param ... Other arguments to pass to the container tag function.
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   downloadButton("downloadData", "Download")
#' )
#'
#' server <- function(input, output) {
#'   # Our dataset
#'   data <- mtcars
#'
#'   output$downloadData <- downloadHandler(
#'     filename = function() {
#'       paste("data-", Sys.Date(), ".csv", sep="")
#'     },
#'     content = function(file) {
#'       write.csv(data, file)
#'     }
#'   )
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @aliases downloadLink
#' @seealso [downloadHandler()]
#' @export
downloadButton <- function(outputId,
                           label="Download",
                           class=NULL, ...) {
  aTag <- tags$a(id=outputId,
                 class=paste('btn btn-default shiny-download-link', class),
                 href='',
                 target='_blank',
                 download=NA,
                 icon("download"),
                 label, ...)
}

#' @rdname downloadButton
#' @export
downloadLink <- function(outputId, label="Download", class=NULL, ...) {
  tags$a(id=outputId,
         class=paste(c('shiny-download-link', class), collapse=" "),
         href='',
         target='_blank',
         download=NA,
         label, ...)
}


#' Create an icon
#'
#' Create an icon for use within a page. Icons can appear on their own, inside
#' of a button, or as an icon for a [tabPanel()] within a
#' [navbarPage()].
#'
#' @param name Name of icon. Icons are drawn from the
#'   [Font Awesome Free](https://fontawesome.com/) (currently icons from
#'   the v5.13.0 set are supported with the v4 naming convention) and
#'   [Glyphicons](http://getbootstrap.com/components/#glyphicons)
#'   libraries. Note that the "fa-" and "glyphicon-" prefixes should not be used
#'   in icon names (i.e. the "fa-calendar" icon should be referred to as
#'   "calendar")
#' @param class Additional classes to customize the style of the icon (see the
#'   [usage examples](http://fontawesome.io/examples/) for details on
#'   supported styles).
#' @param lib Icon library to use ("font-awesome" or "glyphicon")
#'
#' @return An icon element
#'
#' @seealso For lists of available icons, see
#'   [http://fontawesome.io/icons/](http://fontawesome.io/icons/) and
#'   [http://getbootstrap.com/components/#glyphicons](http://getbootstrap.com/components/#glyphicons).
#'
#'
#' @examples
#' # add an icon to a submit button
#' submitButton("Update View", icon = icon("refresh"))
#'
#' navbarPage("App Title",
#'   tabPanel("Plot", icon = icon("bar-chart-o")),
#'   tabPanel("Summary", icon = icon("list-alt")),
#'   tabPanel("Table", icon = icon("table"))
#' )
#' @export
icon <- function(name, class = NULL, lib = "font-awesome") {
  prefixes <- list(
    "font-awesome" = "fa",
    "glyphicon" = "glyphicon"
  )
  prefix <- prefixes[[lib]]

  # determine stylesheet
  if (is.null(prefix)) {
    stop("Unknown font library '", lib, "' specified. Must be one of ",
         paste0('"', names(prefixes), '"', collapse = ", "))
  }

  # build the icon class (allow name to be null so that other functions
  # e.g. buildTabset can pass an explicit class value)
  iconClass <- ""
  if (!is.null(name)) {
    prefix_class <- prefix
    if (prefix_class == "fa" && name %in% font_awesome_brands) {
      prefix_class <- "fab"
    }
    iconClass <- paste0(prefix_class, " ", prefix, "-", name)
  }
  if (!is.null(class))
    iconClass <- paste(iconClass, class)

  iconTag <- tags$i(class = iconClass)

  # font-awesome needs an additional dependency (glyphicon is in bootstrap)
  if (lib == "font-awesome") {
    htmlDependencies(iconTag) <- htmlDependency(
      "font-awesome", "5.13.0", "www/shared/fontawesome", package = "shiny",
      stylesheet = c(
        "css/all.min.css",
        "css/v4-shims.min.css"
      )
    )
  }

  htmltools::browsable(iconTag)
}

# Helper funtion to extract the class from an icon
iconClass <- function(icon) {
  if (!is.null(icon)) icon$attribs$class
}
