% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{req}
\alias{req}
\title{Check for required values}
\usage{
req(..., cancelOutput = FALSE)
}
\arguments{
\item{...}{Values to check for truthiness.}

\item{cancelOutput}{If \code{TRUE} and an output is being evaluated, stop
processing as usual but instead of clearing the output, leave it in
whatever state it happens to be in. If \code{"progress"}, do the same as \code{TRUE},
but also keep the output in recalculating state; this is intended for cases
when an in-progress calculation will not be completed in this reactive
flush cycle, but is still expected to provide a result in the future.}
}
\value{
The first value that was passed in.
}
\description{
Ensure that values are available (\link[=isTruthy]{"truthy"}) before proceeding
with a calculation or action. If any of the given values is not truthy, the
operation is stopped by raising a "silent" exception (not logged by Shiny,
nor displayed in the Shiny app's UI).
}
\details{
The \code{req} function was designed to be used in one of two ways. The first
is to call it like a statement (ignoring its return value) before attempting
operations using the required values:

\if{html}{\out{<div class="sourceCode">}}\preformatted{rv <- reactiveValues(state = FALSE)
r <- reactive(\{
  req(input$a, input$b, rv$state)
  # Code that uses input$a, input$b, and/or rv$state...
\})
}\if{html}{\out{</div>}}

In this example, if \code{r()} is called and any of \code{input$a},
\code{input$b}, and \code{rv$state} are \code{NULL}, \code{FALSE}, \code{""},
etc., then the \code{req} call will trigger an error that propagates all the
way up to whatever render block or observer is executing.

The second is to use it to wrap an expression that must be truthy:

\if{html}{\out{<div class="sourceCode">}}\preformatted{output$plot <- renderPlot(\{
  if (req(input$plotType) == "histogram") \{
    hist(dataset())
  \} else if (input$plotType == "scatter") \{
    qplot(dataset(), aes(x = x, y = y))
  \}
\})
}\if{html}{\out{</div>}}

In this example, \code{req(input$plotType)} first checks that
\code{input$plotType} is truthy, and if so, returns it. This is a convenient
way to check for a value "inline" with its first use.
}
\section{Using \code{req(FALSE)}}{


You can use \code{req(FALSE)} (i.e. no condition) if you've already performed
all the checks you needed to by that point and just want to stop the reactive
chain now. There is no advantage to this, except perhaps ease of readability
if you have a complicated condition to check for (or perhaps if you'd like to
divide your condition into nested \code{if} statements).
}

\section{Using \code{cancelOutput = TRUE}}{


When \code{req(..., cancelOutput = TRUE)} is used, the "silent" exception is
also raised, but it is treated slightly differently if one or more outputs are
currently being evaluated. In those cases, the reactive chain does not proceed
or update, but the output(s) are left is whatever state they happen to be in
(whatever was their last valid state).

Note that this is always going to be the case if
this is used inside an output context (e.g. \code{output$txt <- ...}). It may
or may not be the case if it is used inside a non-output context (e.g.
\code{\link[=reactive]{reactive()}}, \code{\link[=observe]{observe()}} or \code{\link[=observeEvent]{observeEvent()}})
--- depending on whether or not there is an \code{output$...} that is triggered
as a result of those calls. See the examples below for concrete scenarios.
}

\examples{
## Only run examples in interactive R sessions
if (interactive()) {
  ui <- fluidPage(
    textInput('data', 'Enter a dataset from the "datasets" package', 'cars'),
    p('(E.g. "cars", "mtcars", "pressure", "faithful")'), hr(),
    tableOutput('tbl')
  )

  server <- function(input, output) {
    output$tbl <- renderTable({

      ## to require that the user types something, use: `req(input$data)`
      ## but better: require that input$data is valid and leave the last
      ## valid table up
      req(exists(input$data, "package:datasets", inherits = FALSE),
          cancelOutput = TRUE)

      head(get(input$data, "package:datasets", inherits = FALSE))
    })
  }

  shinyApp(ui, server)
}
}
