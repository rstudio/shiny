\name{tabsetPage}
\alias{tabsetPage}
\title{Create a tabset page}
\usage{
  tabsetPage(headerPanel, ...)
}
\arguments{
  \item{headerPanel}{The \link{headerPanel} with the
  application title}

  \item{...}{\code{\link{tabPanel}} elements to include in
  the page}
}
\value{
  A UI defintion that can be passed to the \link{shinyUI}
  function.
}
\description{
  Create a page that contains \code{\link{tabPanel}}
  elements. Tabsets are useful for dividing applications
  into multiple independently viewable modes.
}
\examples{
shinyUI(tabsetPage(

  headerPanel("Hello Shiny!"),

  tabPanel("Plot", plotOutput("plot")),
  tabPanel("Summary", verbatimTextOutput("summary")),
  tabPanel("Table", tableOutput("table"))
))
}
\seealso{
  \code{\link{tabPanel}}, \code{\link{tabsetPanel}}
}

