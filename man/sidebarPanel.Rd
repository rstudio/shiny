\name{sidebarPanel}
\alias{sidebarPanel}
\title{Create a sidebar panel}
\usage{
  sidebarPanel(...)
}
\arguments{
  \item{...}{UI elements to include on the sidebar}
}
\value{
  A sidebar that can be passed to \link{pageWithSidebar}
}
\description{
  Create a sidebar panel containing input controls that can
  in turn be passed to \link{pageWithSidebar}.
}
\examples{
# Sidebar with controls to select a dataset and specify
# the number of observations to view
sidebarPanel(
  selectInput("dataset", "Choose a dataset:",
              choices = c("rock", "pressure", "cars")),

  numericInput("obs", "Observations:", 10)
)
}

