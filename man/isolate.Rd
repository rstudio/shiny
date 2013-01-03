\name{isolate}
\alias{isolate}
\title{Create a non-reactive scope for an expression}
\usage{
  isolate(expr)
}
\arguments{
  \item{expr}{An expression that can access reactive values
  or functions.}
}
\description{
  Executes the given expression in a scope where reactive
  values or functions can be read, but they cannot cause
  the reactive scope of the caller to be re-evaluated when
  they change.
}
\details{
  Ordinarily, the simple act of reading a reactive value
  causes a relationship to be established between the
  caller and the reactive value, where a change to the
  reactive value will cause the caller to re-execute. (The
  same applies for the act of getting a reactive function's
  value.) The \code{isolate} function lets you read a
  reactive value or function without establishing this
  relationship.
}
\examples{
\dontrun{
observer(function() {
  input$saveButton  # Do take a dependency on input$saveButton

  # isolate a simple expression
  data <- get(isolate(input$dataset))  # No dependency on input$dataset
  writeToDatabase(data)
})

observer(function() {
  input$saveButton  # Do take a dependency on input$saveButton

  # isolate a whole block
  data <- isolate({
    a <- input$valueA   # No dependency on input$valueA or input$valueB
    b <- input$valueB
    c(a=a, b=b)
  })
  writeToDatabase(data)
})
}
}

