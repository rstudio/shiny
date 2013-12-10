
#' Functions for creating bootstrap grid layouts
#' 
#' Functions for creating a bootstrap fluid or fixed grid layout. Fluid layouts 
#' scale their components in realtime to fill all available browser width. 
#' Fixed layouts scale components at predefined size thresholds and limit the
#' total width to 940 pixels.
#' 
#' @param ... Elements to include within the grid
#' 
#' @return A bootstrap grid layout that can be added to a UI definition.
#' 
#' @details A bootstrap grid layout consists of containers that include rows 
#' which in turn include columns. Rows exist for the purpose of making sure
#' their elements appear on the same line (if the browser has adequate width).
#' Columns exist for the purpose of defining how much horizontal space within
#' a 12-unit wide grid it's elements should occupy.
#' 
#' To create a fluid grid layout use the \code{fluidContainer} function and 
#' include instances of \code{fluidRow} within it. To create a fixed grid
#' layout use the \code{fixedContainer} function includes instances of 
#' \code{fixedRow} within it.
#' 
#' @note See the documentation on the bootstrap \href{http://getbootstrap.com/2.3.2/scaffolding.html#fluidGridSystem}{fluid grid system} and 
#' \href{http://getbootstrap.com/2.3.2/scaffolding.html#gridSystem}{fixed grid system}
#' for more details on the differences between fluid and fixed layouts.
#' 
#' @seealso \code{\link{bootstrapPage}}, \code{\link{column}}
#' 
#' @examples
#' shinyUI(bootstrapPage(
#'   fluidContainer(
#'     fluidRow(
#'       column(width = 4,
#'         "4"
#'       ),
#'       column(width = 3, offset = 2,
#'         "3 offset 2"
#'       )
#'     )
#'  )
#' ))
#' 
#' @rdname gridLayout
#' @export
fluidContainer <- function(...) {
  div(class = "container-fluid", ...)
}

#' @rdname gridLayout
#' @export
fluidRow <- function(...) {
  div(class = "row-fluid", ...)
}

#' @rdname gridLayout
#' @export
fixedContainer <- function(...) {
  div(class = "container", ...)
}

#' @rdname gridLayout
#' @export
fixedRow <- function(...) {
  div(class = "row", ...)
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
  
  # TODO: validate that the columns width is from 1 to 12
  
  colClass <- paste("span", width, sep="")
  if (offset > 0)
    colClass <- paste(colClass, " offset", offset, sep="")
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
#' @examples
#' titlePanel("Hello Shiny!")
#' 
#' @export
titlePanel <- function(title, windowTitle=title) {    
  tagList(
    tags$head(tags$title(windowTitle)),
    fluidContainer(fluidRow(h2(title)))
  )
}

#' Create a container panel
#' 
#' Create a container that includes one or more rows of content (each element
#' passed to the container will appear on it's own line in the UI)
#' 
#' @param ... Elements to include within the container
#' 
#' @export
containerPanel <- function(...) {
  rows <- list(...)
  container <- fluidContainer()
  for (row in rows)
    container <- tagAppendChild(container, fluidRow(row))
  container
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
#' @examples
#' # Define UI
#' shinyUI(bootstrapPage(
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
   
  # TODO: validate that sidebarPanel and mainPanel were created by
  # their respective functions (i.e. have the correct span)
  
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
#' @export
columnLayout <- function(...) {
  
  # TODO: validate that all of the columns have a spanX in their class
  
  fluidContainer(
    fluidRow(...)
  )
}


