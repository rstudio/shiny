\name{tabsetPanel}
\alias{tabsetPanel}
\title{Create a tabset panel}
\usage{
  tabsetPanel(...)
}
\arguments{
  \item{...}{\link{tabPanel} elements to include in the
  tabset}
}
\value{
  A tabset that can be passed to \link{mainPanel}
}
\description{
  Create a tabset that contains \link{tabPanel} elements.
  Tabsets are useful for dividing output into multiple
  independently viewable sections.
}
\examples{
Show a tabset that includes a plot, summary, and
table view of the generated distribution
mainPanel(
  tabsetPanel(
    tabPanel("Plot", plotOutput("plot")),
    tabPanel("Summary", verbatimTextOutput("summary")),
    tabPanel("Table", tableOutput("table"))
  )
)
}

