\name{tabPanel}
\alias{tabPanel}
\title{Create a tab panel}
\usage{
  tabPanel(title, ...)
}
\arguments{
  \item{title}{Display title for tab}

  \item{...}{UI elements to include within the tab}
}
\value{
  A tab that can be passed to \link{tabsetPanel}
}
\description{
  Create a tab panel that can be inluded within a
  \link{tabsetPanel}.
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

