
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
#' @param responsive This option is deprecated; it is no longer optional with
#'   Bootstrap 3.
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
#' @note See the \href{http://shiny.rstudio.com/articles/layout-guide.html}{
#'   Shiny-Application-Layout-Guide} for additional details on laying out fluid
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
fluidPage <- function(..., title = NULL, responsive = NULL, theme = NULL) {
  bootstrapPage(div(class = "container-fluid", ...),
                title = title,
                responsive = responsive,
                theme = theme)
}


#' @rdname fluidPage
#' @export
fluidRow <- function(...) {
  div(class = "row", ...)
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
#' @param responsive This option is deprecated; it is no longer optional with
#'   Bootstrap 3.
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
#' @note See the \href{http://shiny.rstudio.com/articles/layout-guide.html}{
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
fixedPage <- function(..., title = NULL, responsive = NULL, theme = NULL) {
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

  colClass <- paste0("col-sm-", width)
  if (offset > 0)
    colClass <- paste0(colClass, " col-sm-offset-", offset)
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
    h2(title)
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
#' @seealso \code{\link{fluidPage}}, \code{\link{flowLayout}}
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

#' Flow layout
#'
#' Lays out elements in a left-to-right, top-to-bottom arrangement. The elements
#' on a given row will be top-aligned with each other. This layout will not work
#' well with elements that have a percentage-based width (e.g.
#' \code{\link{plotOutput}} at its default setting of \code{width = "100\%"}).
#'
#' @param ... Unnamed arguments will become child elements of the layout. Named
#'   arguments will become HTML attributes on the outermost tag.
#' @param cellArgs Any additional attributes that should be used for each cell
#'   of the layout.
#'
#' @seealso \code{\link{verticalLayout}}
#'
#' @examples
#' flowLayout(
#'   numericInput("rows", "How many rows?", 5),
#'   selectInput("letter", "Which letter?", LETTERS),
#'   sliderInput("value", "What value?", 0, 100, 50)
#' )
#' @export
flowLayout <- function(..., cellArgs = list()) {

  children <- list(...)
  childIdx <- !nzchar(names(children) %OR% character(length(children)))
  attribs <- children[!childIdx]
  children <- children[childIdx]

  do.call(tags$div, c(list(class = "shiny-flow-layout"),
    attribs,
    lapply(children, function(x) {
      do.call(tags$div, c(cellArgs, list(x)))
    })
  ))
}

#' Input panel
#'
#' A \code{\link{flowLayout}} with a grey border and light grey background,
#' suitable for wrapping inputs.
#'
#' @param ... Input controls or other HTML elements.
#'
#' @export
inputPanel <- function(...) {
  div(class = "shiny-input-panel",
    flowLayout(...)
  )
}

#' Split layout
#'
#' Lays out elements horizontally, dividing the available horizontal space into
#' equal parts (by default).
#'
#' @param ... Unnamed arguments will become child elements of the layout. Named
#'   arguments will become HTML attributes on the outermost tag.
#' @param cellWidths Character or numeric vector indicating the widths of the
#'   individual cells. Recycling will be used if needed. Character values will
#'   be interpreted as CSS lengths (see \code{\link{validateCssUnit}}), numeric
#'   values as pixels.
#' @param cellArgs Any additional attributes that should be used for each cell
#'   of the layout.
#'
#' @examples
#' # Equal sizing
#' splitLayout(
#'   plotOutput("plot1"),
#'   plotOutput("plot2")
#' )
#'
#' # Custom widths
#' splitLayout(cellWidths = c("25%", "75%"),
#'   plotOutput("plot1"),
#'   plotOutput("plot2")
#' )
#'
#' # All cells at 300 pixels wide, with cell padding
#' # and a border around everything
#' splitLayout(
#'   style = "border: 1px solid silver;",
#'   cellWidths = 300,
#'   cellArgs = list(style = "padding: 6px"),
#'   plotOutput("plot1"),
#'   plotOutput("plot2"),
#'   plotOutput("plot3")
#' )
#' @export
splitLayout <- function(..., cellWidths = NULL, cellArgs = list()) {

  children <- list(...)
  childIdx <- !nzchar(names(children) %OR% character(length(children)))
  attribs <- children[!childIdx]
  children <- children[childIdx]
  count <- length(children)

  if (length(cellWidths) == 0 || is.na(cellWidths)) {
    cellWidths <- sprintf("%.3f%%", 100 / count)
  }
  cellWidths <- rep(cellWidths, length.out = count)
  cellWidths <- sapply(cellWidths, validateCssUnit)

  do.call(tags$div, c(list(class = "shiny-split-layout"),
    attribs,
    mapply(children, cellWidths, FUN = function(x, w) {
      do.call(tags$div, c(
        list(style = sprintf("width: %s;", w)),
        cellArgs,
        list(x)
      ))
    }, SIMPLIFY = FALSE)
  ))
}

#' Flex Box-based row/column layouts
#'
#' Creates row and column layouts with proportionally-sized cells, using the
#' Flex Box layout model of CSS3. These can be nested to create arbitrary
#' proportional-grid layouts. \strong{Warning:} Flex Box is not well supported
#' by Internet Explorer, so these functions should only be used where modern
#' browsers can be assumed.
#'
#' @details If you try to use \code{fillRow} and \code{fillCol} inside of other
#'   Shiny containers, such as \code{\link{sidebarLayout}},
#'   \code{\link{navbarPage}}, or even \code{tags$div}, you will probably find
#'   that they will not appear. This is due to \code{fillRow} and \code{fillCol}
#'   defaulting to \code{height="100\%"}, which will only work inside of
#'   containers that have determined their own size (rather than shrinking to
#'   the size of their contents, as is usually the case in HTML).
#'
#'   To avoid this problem, you have two options:
#'   \itemize{
#'     \item only use \code{fillRow}/\code{fillCol} inside of \code{fillPage},
#'       \code{fillRow}, or \code{fillCol}
#'     \item provide an explicit \code{height} argument to
#'       \code{fillRow}/\code{fillCol}
#'   }
#'
#' @param ... UI objects to put in each row/column cell; each argument will
#'   occupy a single cell. (To put multiple items in a single cell, you can use
#'   \code{\link{tagList}} or \code{\link{div}} to combine them.) Named
#'   arguments will be used as attributes on the \code{div} element that
#'   encapsulates the row/column.
#' @param flex Determines how space should be distributed to the cells. Can be a
#'   single value like \code{1} or \code{2} to evenly distribute the available
#'   space; or use a vector of numbers to specify the proportions. For example,
#'   \code{flex = c(2, 3)} would cause the space to be split 40\%/60\% between
#'   two cells. NA values will cause the corresponding cell to be sized
#'   according to its contents (without growing or shrinking).
#' @param width,height The total amount of width and height to use for the
#'   entire row/column. For the default height of \code{"100\%"} to be
#'   effective, the parent must be \code{fillPage}, another
#'   \code{fillRow}/\code{fillCol}, or some other HTML element whose height is
#'   not determined by the height of its contents.
#'
#' @examples
#' \donttest{
#' # Only run this example in interactive R sessions.
#' # NOTE: This example should be run with example(fillRow, ask = FALSE) to
#' # avoid being prompted to hit Enter during plot rendering.
#' if (interactive()) {
#'
#' ui <- fillPage(fillRow(
#'   plotOutput("plotLeft", height = "100%"),
#'   fillCol(
#'     plotOutput("plotTopRight", height = "100%"),
#'     plotOutput("plotBottomRight", height = "100%")
#'   )
#' ))
#'
#' server <- function(input, output, session) {
#'   output$plotLeft <- renderPlot(plot(cars))
#'   output$plotTopRight <- renderPlot(plot(pressure))
#'   output$plotBottomRight <- renderPlot(plot(AirPassengers))
#' }
#'
#' shinyApp(ui, server)
#'
#' }
#' }
#' @export
fillRow <- function(..., flex = 1, width = "100%", height = "100%") {
  flexfill(..., direction = "row", flex = flex, width = width, height = height)
}

#' @rdname fillRow
#' @export
fillCol <- function(..., flex = 1, width = "100%", height = "100%") {
  flexfill(..., direction = "column", flex = flex, width = width, height = height)
}

flexfill <- function(..., direction, flex, width = width, height = height) {
  children <- list(...)
  attrs <- list()

  if (!is.null(names(children))) {
    attrs <- children[names(children) != ""]
    children <- children[names(children) == ""]
  }

  if (length(flex) > length(children)) {
    flex <- flex[1:length(children)]
  }

  # The dimension along the main axis
  main <- switch(direction,
    row = "width",
    "row-reverse" = "width",
    column = "height",
    "column-reverse" = "height",
    stop("Unexpected direction")
  )
  # The dimension along the cross axis
  cross <- if (main == "width") "height" else "width"

  divArgs <- list(
    class = sprintf("flexfill-container flexfill-container-%s", direction),
    style = css(
      display = "-webkit-flex",
      display = "-ms-flexbox",
      display = "flex",
      .webkit.flex.direction = direction,
      .ms.flex.direction = direction,
      flex.direction = direction,
      width = validateCssUnit(width),
      height = validateCssUnit(height)
    ),
    mapply(children, flex, FUN = function(el, flexValue) {
      if (is.na(flexValue)) {
        # If the flex value is NA, then put the element in a simple flex item
        # that sizes itself (along the main axis) to its contents
        tags$div(
          class = "flexfill-item",
          style = css(
            position = "relative",
            "-webkit-flex" = "none",
            "-ms-flex" = "none",
            flex = "none"
          ),
          style = paste0(main, ":auto;", cross, ":100%;"),
          el
        )
      } else if (is.numeric(flexValue)) {
        # If the flex value is numeric, we need *two* wrapper divs. The outer is
        # the flex item, and the inner is an absolute-fill div that is needed to
        # make percentage-based sizing for el work correctly. I don't understand
        # why this is needed but the truth is probably in this SO page:
        # http://stackoverflow.com/questions/15381172/css-flexbox-child-height-100
        tags$div(
          class = "flexfill-item",
          style = css(
            position = "relative",
            "-webkit-flex" = flexValue,
            "-ms-flex" = flexValue,
            flex = flexValue,
            width = "100%", height = "100%"
          ),
          tags$div(
            class = "flexfill-item-inner",
            style = css(
              position = "absolute",
              top = 0, left = 0, right = 0, bottom = 0
            ),
            el
          )
        )
      } else {
        stop("Unexpected flex argument: ", flexValue)
      }
    }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  )
  do.call(tags$div, c(attrs, divArgs))
}

css <- function(..., collapse_ = "") {
  props <- list(...)
  if (length(props) == 0) {
    return("")
  }

  if (is.null(names(props)) || any(names(props) == "")) {
    stop("cssList expects all arguments to be named")
  }

  # Necessary to make factors show up as level names, not numbers
  props[] <- lapply(props, paste, collapse = " ")

  # Drop null args
  props <- props[!sapply(props, empty)]
  if (length(props) == 0) {
    return("")
  }

  # Replace all '.' and '_' in property names to '-'
  names(props) <- gsub("[._]", "-", tolower(gsub("([A-Z])", "-\\1", names(props))))

  # Create "!important" suffix for each property whose name ends with !, then
  # remove the ! from the property name
  important <- ifelse(grepl("!$", names(props), perl = TRUE), " !important", "")
  names(props) <- sub("!$", "", names(props), perl = TRUE)

  paste0(names(props), ":", props, important, ";", collapse = collapse_)
}

empty <- function(x) {
  length(x) == 0 || (is.character(x) && !any(nzchar(x)))
}
