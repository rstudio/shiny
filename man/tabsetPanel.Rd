\name{tabsetPanel}
\alias{tabsetPanel}
\title{Create a tabset panel}
\usage{
  tabsetPanel(..., id = NULL)
}
\arguments{
  \item{...}{\code{\link{tabPanel}} elements to include in
  the tabset}

  \item{id}{If provided, you can use
  \code{input$}\emph{\code{id}} in your server logic to
  determine which of the current tabs is active. The value
  will correspond to the \code{value} argument that is
  passed to \code{\link{tabPanel}}.}
}
\value{
  A tabset that can be passed to \code{\link{mainPanel}}
}
\description{
  Create a tabset that contains \code{\link{tabPanel}}
  elements. Tabsets are useful for dividing output into
  multiple independently viewable sections.
}
\examples{
# Show a tabset that includes a plot, summary, and
# table view of the generated distribution
mainPanel(
  tabsetPanel(
    tabPanel("Plot", plotOutput("plot")),
    tabPanel("Summary", verbatimTextOutput("summary")),
    tabPanel("Table", tableOutput("table"))
  )
)
}

