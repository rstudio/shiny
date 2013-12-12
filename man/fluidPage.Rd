\name{fluidPage}
\alias{fluidPage}
\alias{fluidRow}
\title{Create a page with fluid layout}
\usage{
  fluidPage(..., responsive = TRUE)

  fluidRow(...)
}
\arguments{
  \item{...}{Elements to include within the page}

  \item{responsive}{\code{TRUE} to use responsive layout
  (automatically adapt and resize page elements based on
  the size of the viewing device)}
}
\value{
  A UI defintion that can be passed to the \link{shinyUI}
  function.
}
\description{
  Functions for creating fluid page layouts. A fluid page
  layout consists of rows which in turn include columns.
  Rows exist for the purpose of making sure their elements
  appear on the same line (if the browser has adequate
  width). Columns exist for the purpose of defining how
  much horizontal space within a 12-unit wide grid it's
  elements should occupy. Fluid pages scale their
  components in realtime to fill all available browser
  width.
}
\details{
  To create a fluid page use the \code{fluidPage} function
  and include instances of \code{fluidRow} and
  \code{\link{column}} within it. As an alternative to
  low-level row and column functions you can also use
  higher-level layout functions like
  \code{\link{sidebarLayout}} and
  \code{\link{columnLayout}}.
}
\note{
  See the documentation on the bootstrap
  \href{http://getbootstrap.com/2.3.2/scaffolding.html#fluidGridSystem}{
  fluid grid system} for additional details.
}
\examples{
shinyUI(fluidPage(

  # Application title
  titlePanel("Hello Shiny!"),

  sidebarLayout(

    # Sidebar with a slider input
    sidebarPanel(
      sliderInput("obs",
                  "Number of observations:",
                  min = 0,
                  max = 1000,
                  value = 500)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

shinyUI(fluidPage(
  fluidRow(
    column(width = 4,
      "4"
    ),
    column(width = 3, offset = 2,
      "3 offset 2"
    )
  )
))
}
\seealso{
  \code{\link{column}}, \code{\link{sidebarLayout}},
  \code{\link{columnLayout}}
}

