\name{actionButton}
\alias{actionButton}
\title{Action button}
\usage{
  actionButton(inputId, label, icon = NULL)
}
\arguments{
  \item{inputId}{Specifies the input slot that will be used
  to access the value.}

  \item{label}{The contents of the button--usually a text
  label, but you could also use any other HTML, like an
  image.}

  \item{icon}{Optional \code{\link{icon}} to appear on the
  button}
}
\description{
  Creates an action button whose value is initially zero,
  and increments by one each time it is pressed.
}
\examples{
\dontrun{
# In server.R
output$distPlot <- renderPlot({
  # Take a dependency on input$goButton
  input$goButton

  # Use isolate() to avoid dependency on input$obs
  dist <- isolate(rnorm(input$obs))
  hist(dist)
})

# In ui.R
actionButton("goButton", "Go!")
}
}
\seealso{
  Other input.elements: \code{\link{checkboxGroupInput}},
  \code{\link{checkboxInput}}, \code{\link{dateInput}},
  \code{\link{dateRangeInput}}, \code{\link{fileInput}},
  \code{\link{numericInput}}, \code{\link{radioButtons}},
  \code{\link{selectInput}}, \code{\link{sliderInput}},
  \code{\link{submitButton}}, \code{\link{textInput}}
}

