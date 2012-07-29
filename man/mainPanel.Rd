\name{mainPanel}
\alias{mainPanel}
\title{Create a main panel}
\usage{
  mainPanel(...)
}
\arguments{
  \item{...}{Ouput elements to include in the main panel}
}
\value{
  A main panel that can be passed to \link{pageWithSidebar}
}
\description{
  Create a main panel containing output elements that can
  in turn be passed to \link{pageWithSidebar}.
}
\examples{
# Show the caption and plot of the requested variable against mpg
mainPanel(
   h3(textOutput("caption")),
   plotOutput("mpgPlot")
)
}

