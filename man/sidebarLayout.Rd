\name{sidebarLayout}
\alias{sidebarLayout}
\title{Layout a sidebar and main area}
\usage{
  sidebarLayout(sidebarPanel, mainPanel,
    position = c("left", "right"), fluid = TRUE,
    widths = c(4, 8))
}
\arguments{
  \item{sidebarPanel}{The \link{sidebarPanel} containing
  input controls}

  \item{mainPanel}{The \link{mainPanel} containing outputs}

  \item{position}{The position of the sidebar relative to
  the main area ("left" or "right")}

  \item{fixed}{\code{TRUE} to use fluid layout;
  \code{FALSE} to use fixed layout.}

  \item{widths}{The columns widths of the sidebar and main
  panel (the first value is for the sidebar; the second for
  the main panel). Note that if you are using fixed layout
  you may need to adjust these to accomodate the size of
  the container if it's less than 12 units wide.}
}
\description{
  Create a layout with a sidebar and main area. The sidebar
  is displayed with a distinct background color and
  typically contains input controls. The main area occupies
  2/3 of the horizontal width and typically contains
  outputs.
}
\examples{
# Define UI
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
}

