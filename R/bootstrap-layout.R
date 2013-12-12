
#' Create a page with fluid layout
#' 
#' Functions for creating fluid page layouts. A fluid layout consists of rows
#' which in turn include columns. Rows exist for the purpose of making sure
#' their elements appear on the same line (if the browser has adequate width).
#' Columns exist for the purpose of defining how much horizontal space within
#' a 12-unit wide grid it's elements should occupy. Fluid pages scale their 
#' components in realtime to fill all available browser width. 
#' 
#' @param ... Elements to include within the page
#' @param head Tag or list of tags to be inserted into the head of the document
#' (for example, addition of required Javascript or CSS resources via
#' \code{tags$script} or \code{tags$style})
#' 
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#' 
#' @details To create a fluid page use the \code{fluidPage} function and 
#' include instances of \code{fluidRow} and \code{\link{column}} within it. 
#' As an alternative to low-level row and column functions you can also use 
#' higher-level layout functions like \code{\link{sidebarLayout}}, 
#' \code{\link{horizontalLayout}}, or \code{\link{columnLayout}}.
#' 
#' @note See the documentation on the bootstrap
#' \href{http://getbootstrap.com/2.3.2/scaffolding.html#fluidGridSystem}{
#' fluid grid system} for additional details.
#' 
#' @seealso \code{\link{column}}
#' 
#' @examples
#' shinyUI(fluidPage(
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
fluidPage <- function(..., head = list()) {
  bootstrapPage(div(class = "container-fluid", ...),
                head = head)
                
}

#' @param class Optional css class to attach to the row
#' 
#' @rdname fluidPage
#' @export
fluidRow <- function(..., class = NULL) {
  div(class = paste("row-fluid", class), ...)
}

#' Create a page with a fixed layout
#' 
#' Functions for creating fixed page layouts. A fixed layout consists of rows
#' which in turn include columns. Rows exist for the purpose of making sure
#' their elements appear on the same line (if the browser has adequate width).
#' Columns exist for the purpose of defining how much horizontal space within
#' a 12-unit wide grid it's elements should occupy. Fixed pages limit their
#' width to 940 pixels.
#' 
#' @param ... Elements to include within the page
#' @param head Tag or list of tags to be inserted into the head of the document
#' (for example, addition of required Javascript or CSS resources via
#' \code{tags$script} or \code{tags$style})
#' 
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#' 
#' @details To create a fixed page use the \code{fixedPage} function and 
#' include instances of \code{fixedRow} and \code{\link{column}} within it. 
#' Note that unlike \code{\link{fluidPage}}, fixed pages cannot make use
#' of higher-level layout functions like \code{sidebarLayout}, rather, all
#' layout must be done with \code{fixedRow} and \code{column}.
#' 
#' @note See the documentation on the bootstrap 
#' \href{http://getbootstrap.com/2.3.2/scaffolding.html#gridSystem}{
#' fixed grid system} for additional details.
#' 
#' @seealso \code{\link{column}}
#' 
#' @examples
#' shinyUI(fixedPage(
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
fixedPage <- function(..., head = list()) {
  bootstrapPage(div(class = "container", ...),
                head = head)
}

#' @param class Optional css class to attach to the row
#' 
#' @rdname fixedPage
#' @export
fixedRow <- function(..., class = NULL) {
  div(class = paste("row", class), ...)
}


#' Create a column within a UI definition
#' 
#' Create a column for use within a \code{\link{columnLayout}},
#' \code{\link{fluidRow}}, or \code{\link{fixedRow}}
#' 
#' @param width The grid width of the column (must be between 1 and 12)
#' @param ... Elements to include within the column
#' @param offset The number of columns to offset this column from the 
#' end of the previous column.
#' 
#' @return A column that can be included within a \code{columnLayout}, \code{\link{fluidRow}}, or \code{\link{fixedRow}}.
#' 
#' 
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
#' \code{title} tag within the head. 
#' 
#' @note The \code{titlePanel} function can only be used within a 
#' \code{\link{fluidPage}}.
#'     
#' @examples
#' titlePanel("Hello Shiny!")
#' 
#' @export
titlePanel <- function(title, windowTitle=title) {    
  tagList(
    tags$head(tags$title(windowTitle)),
    fluidRow(style = "padding: 10px 0px;",
      column(12, 
        h2(title)))
  )
}

#' Layout a sidebar and main area
#' 
#' Create a layout with a sidebar and main area. The sidebar is displayed with
#' a distinct background color and typically contains input controls. The
#' main area occupies 2/3 of the horizontal width and typically contains 
#' outputs. 
#' 
#' @param sidebarPanel The \link{sidebarPanel} containing input controls
#' @param mainPanel The \link{mainPanel} containing outputs
#' @param position The position of the sidebar relative to the main area
#' ("left" or "right")
#' 
#' @note The \code{sidebarLayout} function can only be used within a 
#' \code{\link{fluidPage}}.
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
                          position = c("left", "right")) {
  
  # validate that inputs were created by their respective functions
  validateSpan(sidebarPanel, "sidebarPanel", 4)
  validateSpan(mainPanel, "mainPanel", 8)
  
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
  
  # return as a column layout
  columnLayout(firstPanel, secondPanel)
}

#' Layout a set of columns
#' 
#' Layout a set of columns created using the \code{\link{column}} function. The
#' widths of the columns should total no more than 12 units.
#' 
#' @param ... Columns to include within the layout
#' 
#' @note The \code{columnLayout} function can only be used within a 
#' \code{\link{fluidPage}}.
#' 
#' @examples
#' shinyUI(fluidPage(
#'
#'   titlePanel("New Application"),
#' 
#'   columnLayout(
#'   
#'     # Sidebar with a slider input for number of observations
#'     column(width = 4,
#'       wellPanel(
#'         sliderInput("obs", 
#'                     "Number of observations:", 
#'                     min = 1, 
#'                     max = 1000, 
#'                     value = 500)
#'       )
#'     ),
#'   
#'     column(width = 8,
#'       # Show a plot of the generated distribution
#'       plotOutput("distPlot")
#'     )
#' )
# ))
#' 
#' @export
columnLayout <- function(...) {
  fluidRow(...)
}

#' Layout UI elements vertically
#' 
#' Create a container that includes one or more rows of content (each element
#' passed to the container will appear on it's own line in the UI)
#' 
#' @param ... Elements to include within the container
#' 
#' @note The \code{verticalLayout} function can only be used within a 
#' \code{\link{fluidPage}}.
#' 
#' @export
verticalLayout <- function(...) {
  lapply(list(...), function(row) fluidRow(column(12, row)))
}

#' Layout UI elements horizontally
#' 
#' Create a container that includes several elements laid out side-by-side.
#' 
#' @param ... Elements to include within the container
#' 
#' @details To force elements to the left or right of the container you can 
#' use the \code{\link{pullLeft}} and \code{\link{pullRight}} functions.
#' 
#' @export
horizontalLayout <- function(...) {
  fluidRow(column(12, ...))
}

#' Pull elements left or right
#' 
#' Pull an element to the left or right side of a \code{\link{horizontalLayout}}.
#' 
#' @param ... Element or list of elements to pull left or right
#' 
#' @rdname horizontalLayout
#' @export
pullLeft <- function(...) {
  lapply(flattenTags(list(...)), function(element) {
    if (!isTag(element))
      stop("pullLeft - passed argument not a shiny UI element", call. = FALSE)
    element$attribs$class <- paste(element$attribs$class, "pull-left")
    element
  })
}

#' @rdname horizontalLayout
#' @export
pullRight <- function(...) {
  lapply(flattenTags(list(...)), function(element) {
    if (!isTag(element))
      stop("pullRight - passed argument not a shiny UI element", call. = FALSE)
    element$attribs$class <- paste(element$attribs$class, "pull-right")
    element
  })
}

# Helper function to test whether an element has a span class
validateSpan <- function(element, name, width = NA) {
  
  if (!is.list(element) || 
      is.null(element$attribs) || 
      is.null(element$attribs$class)) {
    stop(name, " does not have a valid column span", call. = FALSE)
  } 
  else {
    test <- paste0("span", ifelse(is.na(width), "", width))
    if (!grepl(test, element$attribs$class)) {
      msg <- paste(name, "does not have a valid column span")
      if (!is.na(width)) {
        msg <- paste0(msg, " (it must be span", width, ")")
        stop(msg, call. = FALSE)
      }
    }
  }
}



