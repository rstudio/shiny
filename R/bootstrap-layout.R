
#' Create a page with fluid layout
#'
#' Functions for creating fluid page layouts. A fluid page layout consists of
#' rows which in turn include columns. Rows exist for the purpose of making sure
#' their elements appear on the same line (if the browser has adequate width).
#' Columns exist for the purpose of defining how much horizontal space within a
#' 12-unit wide grid it's elements should occupy. Fluid pages scale their
#' components in realtime to fill all available browser width.
#'
#' @param ... Elements to include within the page
#' @param title The browser window title (defaults to the host URL of the page).
#'   Can also be set as a side effect of the \code{\link{titlePanel}} function.
#' @param responsive \code{TRUE} to use responsive layout (automatically adapt
#'   and resize page elements based on the size of the viewing device)
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory). For example, to use the theme located at
#'   \code{www/bootstrap.css} you would use \code{theme = "bootstrap.css"}.
#'
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#'
#' @details To create a fluid page use the \code{fluidPage} function and include
#'   instances of \code{fluidRow} and \code{\link{column}} within it. As an
#'   alternative to low-level row and column functions you can also use
#'   higher-level layout functions like \code{\link{sidebarLayout}}.
#'
#' @note See the
#'   \href{https://github.com/rstudio/shiny/wiki/Shiny-Application-Layout-Guide}{
#'    Shiny-Application-Layout-Guide} for additional details on laying out fluid
#'   pages.
#'
#' @seealso \code{\link{column}}, \code{\link{sidebarLayout}}
#'
#' @examples
#' shinyUI(fluidPage(
#'
#'   # Application title
#'   titlePanel("Hello Shiny!"),
#'
#'   sidebarLayout(
#'
#'     # Sidebar with a slider input
#'     sidebarPanel(
#'       sliderInput("obs",
#'                   "Number of observations:",
#'                   min = 0,
#'                   max = 1000,
#'                   value = 500)
#'     ),
#'
#'     # Show a plot of the generated distribution
#'     mainPanel(
#'       plotOutput("distPlot")
#'     )
#'   )
#' ))
#'
#' shinyUI(fluidPage(
#'   title = "Hello Shiny!",
#'   fluidRow(
#'     column(width = 4,
#'       "4"
#'     ),
#'     column(width = 3, offset = 2,
#'       "3 offset 2"
#'     )
#'   )
#' ))
#'
#' @rdname fluidPage
#' @export
fluidPage <- function(..., title = NULL, responsive = TRUE, theme = NULL) {
  bootstrapPage(div(class = "container-fluid", ...),
                title = title,
                responsive = responsive,
                theme = theme)
}


#' @rdname fluidPage
#' @export
fluidRow <- function(...) {
  div(class = "row-fluid", ...)
}

#' Create a page with a fixed layout
#'
#' Functions for creating fixed page layouts. A fixed page layout consists of
#' rows which in turn include columns. Rows exist for the purpose of making sure
#' their elements appear on the same line (if the browser has adequate width).
#' Columns exist for the purpose of defining how much horizontal space within a
#' 12-unit wide grid it's elements should occupy. Fixed pages limit their width
#' to 940 pixels on a typical display, and 724px or 1170px on smaller and larger
#' displays respectively.
#'
#' @param ... Elements to include within the container
#' @param title The browser window title (defaults to the host URL of the page)
#' @param responsive \code{TRUE} to use responsive layout (automatically adapt
#'   and resize page elements based on the size of the viewing device)
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory). For example, to use the theme located at
#'   \code{www/bootstrap.css} you would use \code{theme = "bootstrap.css"}.
#'
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#'
#' @details To create a fixed page use the \code{fixedPage} function and include
#'   instances of \code{fixedRow} and \code{\link{column}} within it. Note that
#'   unlike \code{\link{fluidPage}}, fixed pages cannot make use of higher-level
#'   layout functions like \code{sidebarLayout}, rather, all layout must be done
#'   with \code{fixedRow} and \code{column}.
#'
#' @note See the
#'   \href{https://github.com/rstudio/shiny/wiki/Shiny-Application-Layout-Guide}{
#'   Shiny Application Layout Guide} for additional details on laying out fixed
#'   pages.
#'
#' @seealso \code{\link{column}}
#'
#' @examples
#' shinyUI(fixedPage(
#'   title = "Hello, Shiny!",
#'   fixedRow(
#'     column(width = 4,
#'       "4"
#'     ),
#'     column(width = 3, offset = 2,
#'       "3 offset 2"
#'     )
#'   )
#' ))
#'
#' @rdname fixedPage
#' @export
fixedPage <- function(..., title = NULL, responsive = TRUE, theme = NULL) {
  bootstrapPage(div(class = "container", ...),
                title = title,
                responsive = responsive,
                theme = theme)
}

#' @rdname fixedPage
#' @export
fixedRow <- function(...) {
  div(class = "row", ...)
}


#' Create a column within a UI definition
#'
#' Create a column for use within a  \code{\link{fluidRow}} or
#' \code{\link{fixedRow}}
#'
#' @param width The grid width of the column (must be between 1 and 12)
#' @param ... Elements to include within the column
#' @param offset The number of columns to offset this column from the end of the
#'   previous column.
#'
#' @return A column that can be included within a
#'   \code{\link{fluidRow}} or \code{\link{fixedRow}}.
#'
#'
#' @seealso \code{\link{fluidRow}}, \code{\link{fixedRow}}.
#'
#' @examples
#' fluidRow(
#'   column(4,
#'     sliderInput("obs", "Number of observations:",
#'                 min = 1, max = 1000, value = 500)
#'   ),
#'   column(8,
#'     plotOutput("distPlot")
#'   )
#' )
#'
#' fluidRow(
#'   column(width = 4,
#'     "4"
#'   ),
#'   column(width = 3, offset = 2,
#'     "3 offset 2"
#'   )
#' )
#' @export
column <- function(width, ..., offset = 0) {

  if (!is.numeric(width) || (width < 1) || (width > 12))
    stop("column width must be between 1 and 12")

  colClass <- paste0("span", width)
  if (offset > 0)
    colClass <- paste0(colClass, " offset", offset)
  div(class = colClass, ...)
}


#' Create a panel containing an application title.
#'
#' @param title An application title to display
#' @param windowTitle The title that should be displayed by the browser window.
#'
#' @details Calling this function has the side effect of including a
#'   \code{title} tag within the head. You can also specify a page title
#'   explicitly using the `title` parameter of the top-level page function.
#'
#'
#' @examples
#' titlePanel("Hello Shiny!")
#'
#' @export
titlePanel <- function(title, windowTitle=title) {
  tagList(
    tags$head(tags$title(windowTitle)),
    h2(style = "padding: 10px 0px;", title)
  )
}

#' Layout a sidebar and main area
#'
#' Create a layout with a sidebar and main area. The sidebar is displayed with a
#' distinct background color and typically contains input controls. The main
#' area occupies 2/3 of the horizontal width and typically contains outputs.
#'
#' @param sidebarPanel The \link{sidebarPanel} containing input controls
#' @param mainPanel The \link{mainPanel} containing outputs
#' @param position The position of the sidebar relative to the main area ("left"
#'   or "right")
#' @param fluid \code{TRUE} to use fluid layout; \code{FALSE} to use fixed
#'   layout.
#'
#' @examples
#' # Define UI
#' shinyUI(fluidPage(
#'
#'   # Application title
#'   titlePanel("Hello Shiny!"),
#'
#'   sidebarLayout(
#'
#'     # Sidebar with a slider input
#'     sidebarPanel(
#'       sliderInput("obs",
#'                   "Number of observations:",
#'                   min = 0,
#'                   max = 1000,
#'                   value = 500)
#'     ),
#'
#'     # Show a plot of the generated distribution
#'     mainPanel(
#'       plotOutput("distPlot")
#'     )
#'   )
#' ))
#'
#' @export
sidebarLayout <- function(sidebarPanel,
                          mainPanel,
                          position = c("left", "right"),
                          fluid = TRUE) {

  # determine the order
  position <- match.arg(position)
  if (position == "left") {
    firstPanel <- sidebarPanel
    secondPanel <- mainPanel
  }
  else if (position == "right") {
    firstPanel <- mainPanel
    secondPanel <- sidebarPanel
  }

  # return as as row
  if (fluid)
    fluidRow(firstPanel, secondPanel)
  else
    fixedRow(firstPanel, secondPanel)
}

#' Lay out UI elements vertically
#'
#' Create a container that includes one or more rows of content (each element
#' passed to the container will appear on it's own line in the UI)
#'
#' @param ... Elements to include within the container
#' @param fluid \code{TRUE} to use fluid layout; \code{FALSE} to use fixed
#'   layout.
#'
#' @seealso \code{\link{fluidPage}}, \code{\link{horizontalLayout}}
#'
#' @examples
#' shinyUI(fluidPage(
#'   verticalLayout(
#'     a(href="http://example.com/link1", "Link One"),
#'     a(href="http://example.com/link2", "Link Two"),
#'     a(href="http://example.com/link3", "Link Three")
#'   )
#' ))
#' @export
verticalLayout <- function(..., fluid = TRUE) {
  lapply(list(...), function(row) {
    col <- column(12, row)
    if (fluid)
      fluidRow(col)
    else
      fixedRow(col)
  })
}

#' Lay out UI elements horizontally
#'
#' Create a container that lays out elements next to each other.
#'
#' This layout is implemented using tables. These days, using HTML tables for
#' layout purposes is generally frowned upon by experienced web designers as not
#' being "semantically correct", but it's the only reasonable way to provide
#' decent vertical alignment options.
#'
#' The layout will create a table with a single row (\code{tr}), and a cell
#' (\code{td}) within that row for each of the child elements. By default, the
#' width of the cells will be set to (100 / number of children)\%. You can
#' override this by including a specific \code{width} value in \code{cellArgs},
#' but you can't customize widths on a per-column basis.
#'
#' @param ... Unnamed arguments will become child elements of the layout. Named
#'   arguments will become HTML attributes on the outermost tag.
#' @param valign The vertical alignment of the children.
#' @param cellArgs Any additional attributes that should be used for each cell
#'   of the layout.
#'
#' @seealso \code{\link{verticalLayout}}
#'
#' #' @examples
#' horizontalLayout(
#'   numericInput("rows", "How many rows?", 5),
#'   selectInput("letter", "Which letter?", LETTERS),
#'   sliderInput("value", "What value?", 0, 100, 50)
#' )
#' @export
horizontalLayout <- function(..., valign = c("top", "middle", "bottom"),
  cellArgs = list()) {

  valign <- match.arg(valign)

  children <- list(...)
  childIdx <- !nzchar(names(children) %OR% character(length(children)))
  attribs <- children[!childIdx]
  children <- children[childIdx]

  count <- length(children)
  width <- 100 / count
  if (is.null(cellArgs$width))
    cellArgs$width <- sprintf("%.3f%%", width)
  if (is.null(cellArgs$valign))
    cellArgs$valign <- valign

  do.call(tags$table, c(list(class="shiny-horizontal-layout"),
    attribs,
    list(tags$tbody(
      tags$tr(
        lapply(children, function(x) {
          do.call(tags$td, c(cellArgs, list(x)))
        })
      )
    ))
  ))
}


