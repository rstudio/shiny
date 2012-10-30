\name{conditionalPanel}
\alias{conditionalPanel}
\title{Conditional Panel}
\usage{
  conditionalPanel(condition, ...)
}
\arguments{
  \item{condition}{A JavaScript expression that will be
  evaluated repeatedly to determine whether the panel
  should be displayed.}

  \item{...}{Elements to include in the panel.}
}
\description{
  Creates a panel that is visible or not, depending on the
  value of a JavaScript expression. The JS expression is
  evaluated once at startup and whenever Shiny detects a
  relevant change in input/output.
}
\details{
  In the JS expression, you can refer to \code{input} and
  \code{output} JavaScript objects that contain the current
  values of input and output. For example, if you have an
  input with an id of \code{foo}, then you can use
  \code{input.foo} to read its value. (Be sure not to
  modify the input/output objects, as this may cause
  unpredictable behavior.)
}
\examples{
sidebarPanel(
   selectInput(
      "plotType", "Plot Type",
      c(Scatter = "scatter",
        Histogram = "hist")),

   # Only show this panel if the plot type is a histogram
   conditionalPanel(
      condition = "input.plotType == 'hist'",
      selectInput(
         "breaks", "Breaks",
         c("Sturges",
           "Scott",
           "Freedman-Diaconis",
           "[Custom]" = "custom")),

      # Only show this panel if Custom is selected
      conditionalPanel(
         condition = "input.breaks == 'custom'",
         sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
      )
   )
)
}

