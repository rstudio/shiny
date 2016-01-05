#' @include utils.R
NULL

#' Create a Bootstrap page
#'
#' Create a Shiny UI page that loads the CSS and JavaScript for
#' \href{http://getbootstrap.com/}{Bootstrap}, and has no content in the page
#' body (other than what you provide).
#'
#' This function is primarily intended for users who are proficient in HTML/CSS,
#' and know how to lay out pages in Bootstrap. Most applications should use
#' \code{\link{fluidPage}} along with layout functions like
#' \code{\link{fluidRow}} and \code{\link{sidebarLayout}}.
#'
#' @param ... The contents of the document body.
#' @param title The browser window title (defaults to the host URL of the page)
#' @param responsive This option is deprecated; it is no longer optional with
#'   Bootstrap 3.
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory, e.g. \code{www/bootstrap.css})
#'
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#'
#' @note The \code{basicPage} function is deprecated, you should use the
#'   \code{\link{fluidPage}} function instead.
#'
#' @seealso \code{\link{fluidPage}}, \code{\link{fixedPage}}
#'
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
    bootstrapDependency()
  )
}

#' Bootstrap libraries
#'
#' This function returns a set of web dependencies necessary for using Bootstrap
#' components in a web page.
#'
#' It isn't necessary to call this function if you use
#' \code{\link{bootstrapPage}} or others which use \code{bootstrapPage}, such
#' \code{\link{basicPage}}, \code{\link{fluidPage}}, \code{\link{fillPage}},
#' \code{\link{pageWithSidebar}}, and \code{\link{navbarPage}}, because they
#' already include the Bootstrap web dependencies.
#'
#' @inheritParams bootstrapPage
#' @export
bootstrapLib <- function(theme = NULL) {
  attachDependencies(tagList(), bootstrapDependency(theme))
}

bootstrapDependency <- function(theme = NULL) {
  htmlDependency("bootstrap", "3.3.5",
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
#' \code{fillPage} creates a page whose height and width always fill the
#' available area of the browser window.
#'
#' The \code{\link{fluidPage}} and \code{\link{fixedPage}} functions are used
#' for creating web pages that are laid out from the top down, leaving
#' whitespace at the bottom if the page content's height is smaller than the
#' browser window, and scrolling if the content is larger than the window.
#'
#' \code{fillPage} is designed to latch the document body's size to the size of
#' the window. This makes it possible to fill it with content that also scales
#' to the size of the window.
#'
#' For example, \code{fluidPage(plotOutput("plot", height = "100\%"))} will not
#' work as expected; the plot element's effective height will be \code{0},
#' because the plot's containing elements (\code{<div>} and \code{<body>}) have
#' \emph{automatic} height; that is, they determine their own height based on
#' the height of their contained elements. However,
#' \code{fillPage(plotOutput("plot", height = "100\%"))} will work because
#' \code{fillPage} fixes the \code{<body>} height at 100\% of the window height.
#'
#' Note that \code{fillPage(plotOutput("plot"))} will not cause the plot to fill
#' the page. Like most Shiny output widgets, \code{plotOutput}'s default height
#' is a fixed number of pixels. You must explicitly set \code{height = "100\%"}
#' if you want a plot (or htmlwidget, say) to fill its container.
#'
#' One must be careful what layouts/panels/elements come between the
#' \code{fillPage} and the plots/widgets. Any container that has an automatic
#' height will cause children with \code{height = "100\%"} to misbehave. Stick
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
#' @param bootstrap If \code{TRUE}, load the Bootstrap CSS library.
#' @param theme URL to alternative Bootstrap stylesheet.
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
#'
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

#' Create a page with a sidebar
#'
#' Create a Shiny UI that contains a header with the application title, a
#' sidebar for input controls, and a main area for output.
#'
#' @param headerPanel The \link{headerPanel} with the application title
#' @param sidebarPanel The \link{sidebarPanel} containing input controls
#' @param mainPanel The \link{mainPanel} containing outputs

#' @return A UI defintion that can be passed to the \link{shinyUI} function
#'
#' @note This function is deprecated. You should use \code{\link{fluidPage}}
#' along with \code{\link{sidebarLayout}} to implement a page with a sidebar.
#'
#' @examples
#' # Define UI
#' shinyUI(pageWithSidebar(
#'
#'   # Application title
#'   headerPanel("Hello Shiny!"),
#'
#'   # Sidebar with a slider input
#'   sidebarPanel(
#'     sliderInput("obs",
#'                 "Number of observations:",
#'                 min = 0,
#'                 max = 1000,
#'                 value = 500)
#'   ),
#'
#'   # Show a plot of the generated distribution
#'   mainPanel(
#'     plotOutput("distPlot")
#'   )
#' ))
#'
#' @export
pageWithSidebar <- function(headerPanel,
                            sidebarPanel,
                            mainPanel) {

  bootstrapPage(
    # basic application container divs
    div(
      class="container-fluid",
      div(class="row",
          headerPanel
      ),
      div(class="row",
          sidebarPanel,
          mainPanel
      )
    )
  )
}

#' Create a page with a top level navigation bar
#'
#' Create a page that contains a top level navigation bar that can be used to
#' toggle a set of \code{\link{tabPanel}} elements.
#'
#' @param title The title to display in the navbar
#' @param ... \code{\link{tabPanel}} elements to include in the page
#' @param id If provided, you can use \code{input$}\emph{\code{id}} in your
#'   server logic to determine which of the current tabs is active. The value
#'   will correspond to the \code{value} argument that is passed to
#'   \code{\link{tabPanel}}.
#' @param position Determines whether the navbar should be displayed at the top
#'   of the page with normal scrolling behavior (\code{"static-top"}), pinned
#'   at the top (\code{"fixed-top"}), or pinned at the bottom
#'   (\code{"fixed-bottom"}). Note that using \code{"fixed-top"} or
#'   \code{"fixed-bottom"} will cause the navbar to overlay your body content,
#'   unless you add padding, e.g.:
#'   \code{tags$style(type="text/css", "body {padding-top: 70px;}")}
#' @param header Tag or list of tags to display as a common header above all
#'   tabPanels.
#' @param footer Tag or list of tags to display as a common footer below all
#'   tabPanels
#' @param inverse \code{TRUE} to use a dark background and light text for the
#'   navigation bar
#' @param collapsible \code{TRUE} to automatically collapse the navigation
#'   elements into a menu when the width of the browser is less than 940 pixels
#'   (useful for viewing on smaller touchscreen device)
#' @param collapsable Deprecated; use \code{collapsible} instead.
#' @param fluid \code{TRUE} to use a fluid layout. \code{FALSE} to use a fixed
#'   layout.
#' @param responsive This option is deprecated; it is no longer optional with
#'   Bootstrap 3.
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory). For example, to use the theme located at
#'   \code{www/bootstrap.css} you would use \code{theme = "bootstrap.css"}.
#' @param windowTitle The title that should be displayed by the browser window.
#'   Useful if \code{title} is not a string.
#' @param icon Optional icon to appear on a \code{navbarMenu} tab.
#'
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#'
#' @details The \code{navbarMenu} function can be used to create an embedded
#'   menu within the navbar that in turns includes additional tabPanels (see
#'   example below).
#'
#' @seealso \code{\link{tabPanel}}, \code{\link{tabsetPanel}},
#'   \code{\link{updateNavbarPage}}
#'
#' @examples
#' shinyUI(navbarPage("App Title",
#'   tabPanel("Plot"),
#'   tabPanel("Summary"),
#'   tabPanel("Table")
#' ))
#'
#' shinyUI(navbarPage("App Title",
#'   tabPanel("Plot"),
#'   navbarMenu("More",
#'     tabPanel("Summary"),
#'     tabPanel("Table")
#'   )
#' ))
#' @export
navbarPage <- function(title,
                       ...,
                       id = NULL,
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

  # build the tabset
  tabs <- list(...)
  tabset <- buildTabset(tabs, "nav navbar-nav", NULL, id)

  # built the container div dynamically to support optional collapsibility
  if (collapsible) {
    navId <- paste("navbar-collapse-", p_randomInt(1000, 10000), sep="")
    containerDiv <- div(class="container",
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
    containerDiv <- div(class="container",
      div(class="navbar-header",
        span(class="navbar-brand", pageTitle)
      ),
      tabset$navList
    )
  }

  # function to return plain or fluid class name
  className <- function(name) {
    if (fluid)
      paste(name, "-fluid", sep="")
    else
      name
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

#' @rdname navbarPage
#' @export
navbarMenu <- function(title, ..., icon = NULL) {
  structure(list(title = title,
                 tabs = list(...),
                 iconClass = iconClass(icon)),
            class = "shiny.navbarmenu")
}

#' Create a header panel
#'
#' Create a header panel containing an application title.
#'
#' @param title An application title to display
#' @param windowTitle The title that should be displayed by the browser window.
#'   Useful if \code{title} is not a string.
#' @return A headerPanel that can be passed to \link{pageWithSidebar}
#'
#' @examples
#' headerPanel("Hello Shiny!")
#' @export
headerPanel <- function(title, windowTitle=title) {
  tagList(
    tags$head(tags$title(windowTitle)),
    div(class="col-sm-12",
      h1(title)
    )
  )
}

#' Create a well panel
#'
#' Creates a panel with a slightly inset border and grey background. Equivalent
#' to Bootstrap's \code{well} CSS class.
#'
#' @param ... UI elements to include inside the panel.
#' @return The newly created panel.
#'
#' @export
wellPanel <- function(...) {
  div(class="well", ...)
}

#' Create a sidebar panel
#'
#' Create a sidebar panel containing input controls that can in turn be passed
#' to \code{\link{sidebarLayout}}.
#'
#' @param ... UI elements to include on the sidebar
#' @param width The width of the sidebar. For fluid layouts this is out of 12
#'   total units; for fixed layouts it is out of whatever the width of the
#'   sidebar's parent column is.
#' @return A sidebar that can be passed to \code{\link{sidebarLayout}}
#'
#' @examples
#' # Sidebar with controls to select a dataset and specify
#' # the number of observations to view
#' sidebarPanel(
#'   selectInput("dataset", "Choose a dataset:",
#'               choices = c("rock", "pressure", "cars")),
#'
#'   numericInput("obs", "Observations:", 10)
#' )
#' @export
sidebarPanel <- function(..., width = 4) {
  div(class=paste0("col-sm-", width),
    tags$form(class="well",
      ...
    )
  )
}

#' Create a main panel
#'
#' Create a main panel containing output elements that can in turn be passed to
#' \code{\link{sidebarLayout}}.
#'
#' @param ... Output elements to include in the main panel
#' @param width The width of the main panel. For fluid layouts this is out of 12
#'   total units; for fixed layouts it is out of whatever the width of the main
#'   panel's parent column is.
#' @return A main panel that can be passed to \code{\link{sidebarLayout}}.
#'
#' @examples
#' # Show the caption and plot of the requested variable against mpg
#' mainPanel(
#'    h3(textOutput("caption")),
#'    plotOutput("mpgPlot")
#' )
#' @export
mainPanel <- function(..., width = 8) {
  div(class=paste0("col-sm-", width),
    ...
  )
}

#' Conditional Panel
#'
#' Creates a panel that is visible or not, depending on the value of a
#' JavaScript expression. The JS expression is evaluated once at startup and
#' whenever Shiny detects a relevant change in input/output.
#'
#' In the JS expression, you can refer to \code{input} and \code{output}
#' JavaScript objects that contain the current values of input and output. For
#' example, if you have an input with an id of \code{foo}, then you can use
#' \code{input.foo} to read its value. (Be sure not to modify the input/output
#' objects, as this may cause unpredictable behavior.)
#'
#' @param condition A JavaScript expression that will be evaluated repeatedly to
#'   determine whether the panel should be displayed.
#' @param ... Elements to include in the panel.
#'
#' @note You are not recommended to use special JavaScript characters such as a
#'   period \code{.} in the input id's, but if you do use them anyway, for
#'   example, \code{inputId = "foo.bar"}, you will have to use
#'   \code{input["foo.bar"]} instead of \code{input.foo.bar} to read the input
#'   value.
#' @examples
#' sidebarPanel(
#'    selectInput(
#'       "plotType", "Plot Type",
#'       c(Scatter = "scatter",
#'         Histogram = "hist")),
#'
#'    # Only show this panel if the plot type is a histogram
#'    conditionalPanel(
#'       condition = "input.plotType == 'hist'",
#'       selectInput(
#'          "breaks", "Breaks",
#'          c("Sturges",
#'            "Scott",
#'            "Freedman-Diaconis",
#'            "[Custom]" = "custom")),
#'
#'       # Only show this panel if Custom is selected
#'       conditionalPanel(
#'          condition = "input.breaks == 'custom'",
#'          sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
#'       )
#'    )
#' )
#'
#' @export
conditionalPanel <- function(condition, ...) {
  div('data-display-if'=condition, ...)
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
#' Create a tab panel that can be included within a \code{\link{tabsetPanel}}.
#'
#' @param title Display title for tab
#' @param ... UI elements to include within the tab
#' @param value The value that should be sent when \code{tabsetPanel} reports
#'   that this tab is selected. If omitted and \code{tabsetPanel} has an
#'   \code{id}, then the title will be used..
#' @param icon Optional icon to appear on the tab. This attribute is only
#' valid when using a \code{tabPanel} within a \code{\link{navbarPage}}.
#' @return A tab that can be passed to \code{\link{tabsetPanel}}
#'
#' @seealso \code{\link{tabsetPanel}}
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
tabPanel <- function(title, ..., value = title, icon = NULL) {
  divTag <- div(class="tab-pane",
                title=title,
                `data-value`=value,
                `data-icon-class` = iconClass(icon),
                ...)
}

#' Create a tabset panel
#'
#' Create a tabset that contains \code{\link{tabPanel}} elements. Tabsets are
#' useful for dividing output into multiple independently viewable sections.
#'
#' @param ... \code{\link{tabPanel}} elements to include in the tabset
#' @param id If provided, you can use \code{input$}\emph{\code{id}} in your
#'   server logic to determine which of the current tabs is active. The value
#'   will correspond to the \code{value} argument that is passed to
#'   \code{\link{tabPanel}}.
#' @param selected The \code{value} (or, if none was supplied, the \code{title})
#'   of the tab that should be selected by default. If \code{NULL}, the first
#'   tab will be selected.
#' @param type Use "tabs" for the standard look; Use "pills" for a more plain
#'   look where tabs are selected using a background fill color.
#' @param position The position of the tabs relative to the content. Valid
#'   values are "above", "below", "left", and "right" (defaults to "above").
#'   Note that the \code{position} argument is not valid when \code{type} is
#'   "pill".
#' @return A tabset that can be passed to \code{\link{mainPanel}}
#'
#' @seealso \code{\link{tabPanel}}, \code{\link{updateTabsetPanel}}
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
tabsetPanel <- function(...,
                        id = NULL,
                        selected = NULL,
                        type = c("tabs", "pills"),
                        position = c("above", "below", "left", "right")) {

  # build the tabset
  tabs <- list(...)
  type <- match.arg(type)
  tabset <- buildTabset(tabs, paste0("nav nav-", type), NULL, id, selected)

  # position the nav list and content appropriately
  position <- match.arg(position)
  if (position %in% c("above", "left", "right")) {
    first <- tabset$navList
    second <- tabset$content
  } else if (position %in% c("below")) {
    first <- tabset$content
    second <- tabset$navList
  }

  # create the tab div
  tags$div(class = paste("tabbable tabs-", position, sep=""), first, second)
}

#' Create a navigation list panel
#'
#' Create a navigation list panel that provides a list of links on the left
#' which navigate to a set of tabPanels displayed to the right.
#'
#' @param ... \code{\link{tabPanel}} elements to include in the navlist
#' @param id If provided, you can use \code{input$}\emph{\code{id}} in your
#'   server logic to determine which of the current navlist items is active. The
#'   value will correspond to the \code{value} argument that is passed to
#'   \code{\link{tabPanel}}.
#' @param selected The \code{value} (or, if none was supplied, the \code{title})
#'   of the navigation item that should be selected by default. If \code{NULL},
#'   the first navigation will be selected.
#' @param well \code{TRUE} to place a well (gray rounded rectangle) around the
#'   navigation list.
#' @param fluid \code{TRUE} to use fluid layout; \code{FALSE} to use fixed
#'   layout.
#' @param widths Column withs of the navigation list and tabset content areas
#'   respectively.
#'
#' @details You can include headers within the \code{navlistPanel} by including
#'   plain text elements in the list. Versions of Shiny before 0.11 supported
#'   separators with "------", but as of 0.11, separators were no longer
#'   supported. This is because version 0.11 switched to Bootstrap 3, which
#'   doesn't support separators.
#'
#' @seealso \code{\link{tabPanel}}, \code{\link{updateNavlistPanel}}
#' @examples
#' shinyUI(fluidPage(
#'
#'   titlePanel("Application Title"),
#'
#'   navlistPanel(
#'     "Header",
#'     tabPanel("First"),
#'     tabPanel("Second"),
#'     tabPanel("Third")
#'   )
#' ))
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


buildTabset <- function(tabs,
                        ulClass,
                        textFilter = NULL,
                        id = NULL,
                        selected = NULL) {

  # build tab nav list and tab content div

  # add tab input sentinel class if we have an id
  if (!is.null(id))
    ulClass <- paste(ulClass, "shiny-tab-input")

  tabNavList <- tags$ul(class = ulClass, id = id)
  tabContent <- tags$div(class = "tab-content")
  firstTab <- TRUE
  tabsetId <- p_randomInt(1000, 10000)
  tabId <- 1
  for (divTag in tabs) {

    # check for text; pass it to the textFilter or skip it if there is none
    if (is.character(divTag)) {
      if (!is.null(textFilter))
        tabNavList <- tagAppendChild(tabNavList, textFilter(divTag))
      next
    }

    # compute id and assign it to the div
    thisId <- paste("tab", tabsetId, tabId, sep="-")
    divTag$attribs$id <- thisId
    tabId <- tabId + 1

    tabValue <- divTag$attribs$`data-value`

    # function to append an optional icon to an aTag
    appendIcon <- function(aTag, iconClass) {
      if (!is.null(iconClass)) {
        # for font-awesome we specify fixed-width
        if (grepl("fa-", iconClass, fixed = TRUE))
          iconClass <- paste(iconClass, "fa-fw")
        aTag <- tagAppendChild(aTag, icon(name = NULL, class = iconClass))
      }
      aTag
    }

    # check for a navbarMenu and handle appropriately
    if (inherits(divTag, "shiny.navbarmenu")) {

      # create the a tag
      aTag <- tags$a(href="#",
                     class="dropdown-toggle",
                     `data-toggle`="dropdown")

      # add optional icon
      aTag <- appendIcon(aTag, divTag$iconClass)

      # add the title and caret
      aTag <- tagAppendChild(aTag, divTag$title)
      aTag <- tagAppendChild(aTag, tags$b(class="caret"))

      # build the dropdown list element
      liTag <- tags$li(class = "dropdown", aTag)

      # build the child tabset
      tabset <- buildTabset(divTag$tabs, "dropdown-menu")
      liTag <- tagAppendChild(liTag, tabset$navList)

      # don't add a standard tab content div, rather add the list of tab
      # content divs that are contained within the tabset
      divTag <- NULL
      tabContent <- tagAppendChildren(tabContent,
                                      list = tabset$content$children)
    }
    # else it's a standard navbar item
    else {
      # create the a tag
      aTag <- tags$a(href=paste("#", thisId, sep=""),
                     `data-toggle` = "tab",
                     `data-value` = tabValue)

      # append optional icon
      aTag <- appendIcon(aTag, divTag$attribs$`data-icon-class`)

      # add the title
      aTag <- tagAppendChild(aTag, divTag$attribs$title)

      # create the li tag
      liTag <- tags$li(aTag)
    }

    if (is.null(tabValue)) {
      tabValue <- divTag$attribs$title
    }

    # If appropriate, make this the selected tab (don't ever do initial
    # selection of tabs that are within a navbarMenu)
    if ((ulClass != "dropdown-menu") &&
       ((firstTab && is.null(selected)) ||
        (!is.null(selected) && identical(selected, tabValue)))) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "tab-pane active"
      firstTab = FALSE
    }

    divTag$attribs$title <- NULL

    # append the elements to our lists
    tabNavList <- tagAppendChild(tabNavList, liTag)
    tabContent <- tagAppendChild(tabContent, divTag)
  }

  list(navList = tabNavList, content = tabContent)
}


#' Create a text output element
#'
#' Render a reactive output variable as text within an application page. The
#' text will be included within an HTML \code{div} tag by default.
#' @param outputId output variable to read the value from
#' @param container a function to generate an HTML element to contain the text
#' @param inline use an inline (\code{span()}) or block container (\code{div()})
#'   for the output
#' @return A text output element that can be included in a panel
#' @details Text is HTML-escaped prior to rendering. This element is often used
#'   to display \link{renderText} output variables.
#' @examples
#' h3(textOutput("caption"))
#' @export
textOutput <- function(outputId, container = if (inline) span else div, inline = FALSE) {
  container(id = outputId, class = "shiny-text-output")
}

#' Create a verbatim text output element
#'
#' Render a reactive output variable as verbatim text within an
#' application page. The text will be included within an HTML \code{pre} tag.
#' @param outputId output variable to read the value from
#' @return A verbatim text output element that can be included in a panel
#' @details Text is HTML-escaped prior to rendering. This element is often used
#' with the \link{renderPrint} function to preserve fixed-width formatting
#' of printed objects.
#' @examples
#' mainPanel(
#'   h4("Summary"),
#'   verbatimTextOutput("summary"),
#'
#'   h4("Observations"),
#'   tableOutput("view")
#' )
#' @export
verbatimTextOutput <- function(outputId) {
  textOutput(outputId, container = pre)
}


#' @name plotOutput
#' @rdname plotOutput
#' @export
imageOutput <- function(outputId, width = "100%", height="400px",
                        click = NULL, dblclick = NULL,
                        hover = NULL, hoverDelay = NULL, hoverDelayType = NULL,
                        brush = NULL,
                        clickId = NULL, hoverId = NULL,
                        inline = FALSE) {

  if (!is.null(clickId)) {
    shinyDeprecated(
      msg = paste("The 'clickId' argument is deprecated. ",
                  "Please use 'click' instead. ",
                  "See ?imageOutput or ?plotOutput for more information."),
      version = "0.11.1"
    )
    click <- clickId
  }

  if (!is.null(hoverId)) {
    shinyDeprecated(
      msg = paste("The 'hoverId' argument is deprecated. ",
                  "Please use 'hover' instead. ",
                  "See ?imageOutput or ?plotOutput for more information."),
      version = "0.11.1"
    )
    hover <- hoverId
  }

  if (!is.null(hoverDelay) || !is.null(hoverDelayType)) {
    shinyDeprecated(
      msg = paste("The 'hoverDelay'and 'hoverDelayType' arguments are deprecated. ",
                  "Please use 'hoverOpts' instead. ",
                  "See ?imageOutput or ?plotOutput for more information."),
      version = "0.11.1"
    )
    hover <- hoverOpts(id = hover, delay = hoverDelay, delayType = hoverDelayType)
  }

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
#' Render a \code{\link{renderPlot}} or \code{\link{renderImage}} within an
#' application page.
#'
#' @section Interactive plots:
#'
#'   Plots and images in Shiny support mouse-based interaction, via clicking,
#'   double-clicking, hovering, and brushing. When these interaction events
#'   occur, the mouse coordinates will be sent to the server as \code{input$}
#'   variables, as specified by \code{click}, \code{dblclick}, \code{hover}, or
#'   \code{brush}.
#'
#'   For \code{plotOutput}, the coordinates will be sent scaled to the data
#'   space, if possible. (At the moment, plots generated by base graphics and
#'   ggplot2 support this scaling, although plots generated by lattice and
#'   others do not.) If scaling is not possible, the raw pixel coordinates will
#'   be sent. For \code{imageOutput}, the coordinates will be sent in raw pixel
#'   coordinates.
#'
#'   With ggplot2 graphics, the code in \code{renderPlot} should return a ggplot
#'   object; if instead the code prints the ggplot2 object with something like
#'   \code{print(p)}, then the coordinates for interactive graphics will not be
#'   properly scaled to the data space.
#'
#' @param outputId output variable to read the plot/image from.
#' @param width,height Image width/height. Must be a valid CSS unit (like
#'   \code{"100\%"}, \code{"400px"}, \code{"auto"}) or a number, which will be
#'   coerced to a string and have \code{"px"} appended. These two arguments are
#'   ignored when \code{inline = TRUE}, in which case the width/height of a plot
#'   must be specified in \code{renderPlot()}. Note that, for height, using
#'   \code{"auto"} or \code{"100\%"} generally will not work as expected,
#'   because of how height is computed with HTML/CSS.
#' @param click This can be \code{NULL} (the default), a string, or an object
#'   created by the \code{\link{clickOpts}} function. If you use a value like
#'   \code{"plot_click"} (or equivalently, \code{clickOpts(id="plot_click")}),
#'   the plot will send coordinates to the server whenever it is clicked, and
#'   the value will be accessible via \code{input$plot_click}. The value will be
#'   a named list  with \code{x} and \code{y} elements indicating the mouse
#'   position.
#' @param dblclick This is just like the \code{click} argument, but for
#'   double-click events.
#' @param hover Similar to the \code{click} argument, this can be \code{NULL}
#'   (the default), a string, or an object created by the
#'   \code{\link{hoverOpts}} function. If you use a value like
#'   \code{"plot_hover"} (or equivalently, \code{hoverOpts(id="plot_hover")}),
#'   the plot will send coordinates to the server pauses on the plot, and the
#'   value will be accessible via \code{input$plot_hover}. The value will be a
#'   named list with \code{x} and \code{y} elements indicating the mouse
#'   position. To control the hover time or hover delay type, you must use
#'   \code{\link{hoverOpts}}.
#' @param clickId Deprecated; use \code{click} instead. Also see the
#'   \code{\link{clickOpts}} function.
#' @param hoverId Deprecated; use \code{hover} instead. Also see the
#'   \code{\link{hoverOpts}} function.
#' @param hoverDelay Deprecated; use \code{hover} instead. Also see the
#'   \code{\link{hoverOpts}} function.
#' @param hoverDelayType Deprecated; use \code{hover} instead. Also see the
#'   \code{\link{hoverOpts}} function.
#' @param brush Similar to the \code{click} argument, this can be \code{NULL}
#'   (the default), a string, or an object created by the
#'   \code{\link{brushOpts}} function. If you use a value like
#'   \code{"plot_brush"} (or equivalently, \code{brushOpts(id="plot_brush")}),
#'   the plot will allow the user to "brush" in the plotting area, and will send
#'   information about the brushed area to the server, and the value will be
#'   accessible via \code{input$plot_brush}. Brushing means that the user will
#'   be able to draw a rectangle in the plotting area and drag it around. The
#'   value will be a named list with \code{xmin}, \code{xmax}, \code{ymin}, and
#'   \code{ymax} elements indicating the brush area. To control the brush
#'   behavior, use \code{\link{brushOpts}}. Multiple
#'   \code{imageOutput}/\code{plotOutput} calls may share the same \code{id}
#'   value; brushing one image or plot will cause any other brushes with the
#'   same \code{id} to disappear.
#' @inheritParams textOutput
#' @note The arguments \code{clickId} and \code{hoverId} only work for R base
#'   graphics (see the \pkg{\link{graphics}} package). They do not work for
#'   \pkg{\link[grid:grid-package]{grid}}-based graphics, such as \pkg{ggplot2},
#'   \pkg{lattice}, and so on.
#'
#' @return A plot or image output element that can be included in a panel.
#' @seealso For the corresponding server-side functions, see
#'   \code{\link{renderPlot}} and  \code{\link{renderImage}}.
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
                       click = NULL, dblclick = NULL,
                       hover = NULL, hoverDelay = NULL, hoverDelayType = NULL,
                       brush = NULL,
                       clickId = NULL, hoverId = NULL,
                       inline = FALSE) {

  # Result is the same as imageOutput, except for HTML class
  res <- imageOutput(outputId, width, height, click, dblclick,
                     hover, hoverDelay, hoverDelayType, brush,
                     clickId, hoverId, inline)

  res$attribs$class <- "shiny-plot-output"
  res
}

#' Create a table output element
#'
#' Render a \code{\link{renderTable}} or \code{\link{renderDataTable}} within an
#' application page. \code{renderTable} uses a standard HTML table, while
#' \code{renderDataTable} uses the DataTables Javascript library to create an
#' interactive table with more features.
#'
#' @param outputId output variable to read the table from
#' @return A table output element that can be included in a panel
#'
#' @seealso \code{\link{renderTable}}, \code{\link{renderDataTable}}.
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
#' text will be included within an HTML \code{div} tag, and is presumed to
#' contain HTML content which should not be escaped.
#'
#' \code{uiOutput} is intended to be used with \code{renderUI} on the server
#' side. It is currently just an alias for \code{htmlOutput}.
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
#' the corresponding \code{\link{downloadHandler}} defined in the server
#' function.
#'
#' @param outputId The name of the output slot that the \code{downloadHandler}
#'   is assigned to.
#' @param label The label that should appear on the button.
#' @param class Additional CSS classes to apply to the tag, if any.
#'
#' @examples
#' \dontrun{
#' # In server.R:
#' output$downloadData <- downloadHandler(
#'   filename = function() {
#'     paste('data-', Sys.Date(), '.csv', sep='')
#'   },
#'   content = function(con) {
#'     write.csv(data, con)
#'   }
#' )
#'
#' # In ui.R:
#' downloadLink('downloadData', 'Download')
#' }
#'
#' @aliases downloadLink
#' @seealso downloadHandler
#' @export
downloadButton <- function(outputId,
                           label="Download",
                           class=NULL) {
  aTag <- tags$a(id=outputId,
                 class=paste('btn btn-default shiny-download-link', class),
                 href='',
                 target='_blank',
                 icon("download"),
                 label)
}

#' @rdname downloadButton
#' @export
downloadLink <- function(outputId, label="Download", class=NULL) {
  tags$a(id=outputId,
         class=paste(c('shiny-download-link', class), collapse=" "),
         href='',
         target='_blank',
         label)
}


#' Create an icon
#'
#' Create an icon for use within a page. Icons can appear on their own, inside
#' of a button, or as an icon for a \code{\link{tabPanel}} within a
#' \code{\link{navbarPage}}.
#'
#' @param name Name of icon. Icons are drawn from the
#'   \href{http://fontawesome.io/icons/}{Font Awesome} and
#'   \href{http://getbootstrap.com/components/#glyphicons}{Glyphicons"}
#'   libraries. Note that the "fa-" and "glyphicon-" prefixes should not be used
#'   in icon names (i.e. the "fa-calendar" icon should be referred to as
#'   "calendar")
#' @param class Additional classes to customize the style of the icon (see the
#'   \href{http://fontawesome.io/examples/}{usage examples} for details on
#'   supported styles).
#' @param lib Icon library to use ("font-awesome" or "glyphicon")
#'
#' @return An icon element
#'
#' @seealso For lists of available icons, see
#'   \href{http://fontawesome.io/icons/}{http://fontawesome.io/icons/} and
#'   \href{http://getbootstrap.com/components/#glyphicons}{http://getbootstrap.com/components/#glyphicons}.
#'
#'
#' @examples
#' icon("calendar")               # standard icon
#' icon("calendar", "fa-3x")      # 3x normal size
#' icon("cog", lib = "glyphicon") # From glyphicon library
#'
#' # add an icon to a submit button
#' submitButton("Update View", icon = icon("refresh"))
#'
#' shinyUI(navbarPage("App Title",
#'   tabPanel("Plot", icon = icon("bar-chart-o")),
#'   tabPanel("Summary", icon = icon("list-alt")),
#'   tabPanel("Table", icon = icon("table"))
#' ))
#'
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
  if (!is.null(name))
    iconClass <- paste0(prefix, " ", prefix, "-", name)
  if (!is.null(class))
    iconClass <- paste(iconClass, class)

  iconTag <- tags$i(class = iconClass)

  # font-awesome needs an additional dependency (glyphicon is in bootstrap)
  if (lib == "font-awesome") {
    htmlDependencies(iconTag) <- htmlDependency(
      "font-awesome", "4.5.0", c(href="shared/font-awesome"),
      stylesheet = "css/font-awesome.min.css"
    )
  }

  iconTag
}

# Helper funtion to extract the class from an icon
iconClass <- function(icon) {
  if (!is.null(icon)) icon$attribs$class
}
