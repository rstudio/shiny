% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input-text.R
\name{textInput}
\alias{textInput}
\title{Create a text input control}
\usage{
textInput(
  inputId,
  label,
  value = "",
  width = NULL,
  placeholder = NULL,
  ...,
  updateOn = c("change", "blur")
)
}
\arguments{
\item{inputId}{The \code{input} slot that will be used to access the value.}

\item{label}{Display label for the control, or \code{NULL} for no label.}

\item{value}{Initial value.}

\item{width}{The width of the input, e.g. \code{'400px'}, or \code{'100\%'};
see \code{\link[=validateCssUnit]{validateCssUnit()}}.}

\item{placeholder}{A character string giving the user a hint as to what can
be entered into the control. Internet Explorer 8 and 9 do not support this
option.}

\item{...}{Ignored, included to require named arguments and for future
feature expansion.}

\item{updateOn}{A character vector specifying when the input should be
updated. Options are \code{"change"} (default) and \code{"blur"}. Use \code{"change"} to
update the input immediately whenever the value changes. Use \code{"blur"}to
delay the input update until the input loses focus (the user moves away
from the input), or when Enter is pressed (or Cmd/Ctrl + Enter for
\code{\link[=textAreaInput]{textAreaInput()}}).}
}
\value{
A text input control that can be added to a UI definition.
}
\description{
Create an input control for entry of unstructured text values
}
\section{Server value}{

A character string of the text input. The default value is \code{""}
unless \code{value} is provided.
}

\examples{
## Only run examples in interactive R sessions
if (interactive()) {

ui <- fluidPage(
  textInput("caption", "Caption", "Data Summary"),
  verbatimTextOutput("value")
)
server <- function(input, output) {
  output$value <- renderText({ input$caption })
}
shinyApp(ui, server)
}

}
\seealso{
\code{\link[=updateTextInput]{updateTextInput()}}

Other input elements: 
\code{\link{actionButton}()},
\code{\link{checkboxGroupInput}()},
\code{\link{checkboxInput}()},
\code{\link{dateInput}()},
\code{\link{dateRangeInput}()},
\code{\link{fileInput}()},
\code{\link{numericInput}()},
\code{\link{passwordInput}()},
\code{\link{radioButtons}()},
\code{\link{selectInput}()},
\code{\link{sliderInput}()},
\code{\link{submitButton}()},
\code{\link{textAreaInput}()},
\code{\link{varSelectInput}()}
}
\concept{input elements}
