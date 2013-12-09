
#' Functions for creating top-level page containers
#' 
#' Functions for creating a top-level application page that uses either 
#' fluid or fixed layout. Fluid pages scale their components in realtime to 
#' fill all available browser width. Fixed pages scale components at predefined
#' size thresholds and limit the total width to 940 pixels.
#' 
#' @param head Tag or list of tags to be inserted into the head of the document
#' (for example, addition of required Javascript or CSS resources via
#' \code{tags$script} or \code{tags$style})
#'   
#' @return A UI defintion that can be passed to the \link{shinyUI} function.
#' 
#' @details The \code{isPageFluid} function can be used within other UI 
#' definition functions to determine whether the current top-level page 
#' is using fluid or fixed layout.
#' 
#' See the documentation on the bootstrap \href{http://getbootstrap.com/2.3.2/scaffolding.html#fluidGridSystem}{fluid grid system} and 
#' \href{http://getbootstrap.com/2.3.2/scaffolding.html#gridSystem}{fixed grid system} for more details on the differences between 
#' fluid and fixed pages.
#' 
#' @seealso \code{\link{gridContainer}}, \code{\link{gridCol}}, \code{\link{gridRow}}
#' 
#' @rdname pageContainers
#' 
#' @export
fluidPage <- function(..., head = list()) {
  containerPage(fluid = TRUE, ..., head)
}

#' @rdname pageContainers
#' @export
fixedPage <- function(..., head = list()) {
  containerPage(fluid = FALSE, ..., head)
}

#' @rdname pageContainers
#' @export
isPageFluid <- function() {
  .globals$pageIsFluid
}

# Store global state on whether the page currently being built is fluid
.globals$pageIsFluid <- FALSE

# Helper function to build a container page (fluidPage or fixedPage)
containerPage <- function(fluid, ..., head = list()) {
  
  # set the fluid status for the page then unset it when done
  .globals$pageIsFluid <- fluid  
  on.exit(.globals$pageIsFluid <- FALSE)
  
  # build page
  containerDiv <- gridContainer()
  for (containerRow in list(...)) 
    containerDiv <- tagAppendChild(containerDiv, gridRow(containerRow))
  bootstrapPage(containerDiv, head = head)
}


#' Functions for creating bootstrap grid layouts
#' 
#' @param fluid Whether to use fluid layout for the grid. Automatically 
#' defaults to the appropriate value for the current page being built.
#' 
#' @return A grid container or element that can be added to a UI definition.
#' 
#' @details See the documentation on the bootstrap \href{http://getbootstrap.com/2.3.2/scaffolding.html#fluidGridSystem}{fluid grid system} and 
#' \href{http://getbootstrap.com/2.3.2/scaffolding.html#gridSystem}{fixed grid system} for more details on using containers, rows, and columns to layout pages.
#' 
#' @seealso \code{\link{fluidPage}}, \code{\link{fixedPage}}
#' 
#' @rdname gridLayout
#' @export
gridContainer <- function(..., fluid = isPageFluid()) {
  div(class = gridClass("container", fluid), ...)
}

#' @rdname gridLayout
#' @export
gridRow <- function(..., fluid = isPageFluid()) {
  div(class = gridClass("row", fluid), ...)
}

#' @rdname gridLayout
#' 
#' @param width The grid width of the column (must be between 1 and 12)
#' @param offset The number of columns to offset this column from the 
#' end of the previous column.
#' 
#' @export
gridCol <- function(width, ..., offset = NA) {
  colClass <- paste("span", width, sep="")
  if (!is.na(offset))
    colClass <- paste(colClass, " offset", offset, sep="")
  div(class = colClass, ...)
}

# Helper function to append -fluid to containers and rows if appropriate
gridClass <- function(class, fluid = isPageFluid()) {
  if (fluid)
    paste(class, "-fluid", sep="")
  else
    class
}
