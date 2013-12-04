\name{panelWithSidebar}
\alias{panelWithSidebar}
\title{Create a panel with a sidebar}
\usage{
  panelWithSidebar(sidebarPanel, mainPanel)
}
\arguments{
  \item{sidebarPanel}{The \link{sidebarPanel} containing
  input controls}

  \item{mainPanel}{The \link{mainPanel} containing outputs}
}
\value{
  The newly created panel.
}
\description{
  Create a panel that includes a sidebar for input controls
  and a main area for output
}
\examples{
panelWithSidebar(

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
}

