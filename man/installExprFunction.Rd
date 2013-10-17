\name{installExprFunction}
\alias{installExprFunction}
\title{Convert an expression to a function and place it in an environment}
\description{This is to be called from another function, because it will attempt
to get an unquoted expression from two calls back.}
\usage{
  installExprFunction(expr, name, eval.env = parent.frame(2), 
    quoted = FALSE, assign.env = parent.frame(1), 
    label = as.character(sys.call(-1)[[1]]))
}
\arguments{
  \item{expr}{A quoted or unquoted expression.}

  \item{name}{The name of the function object to create.}

  \item{eval.env}{The desired environment for the function. Defaults to the 
  calling environment two steps back.}

  \item{quoted}{Is the expression quoted?}
   
  \item{assign.env}{The environment in which to place the function object.
  Defaults to the calling environment.}
  
  \item{label}{A descriptive label for the function to be shown in the 
  debugger, if active. Defaults to the name of the calling function.}
}
\details{
  Converts expr to a function, using the semantics described in 
  \code{\link{exprToFunction}}. Installs the newly created function into an
  environment (the environment of the caller unless otherwise specified), and 
  registers debug hooks on the function object if a debugger is active so that 
  breakpoints may be set in it.
}
\seealso{
\code{link{exprToFunction}}
}
\examples{
# Example of a new renderer, similar to renderText
# This is something that toolkit authors will do
renderTriple <- function(expr, env=parent.frame(), quoted=FALSE) {
  # Create a function named "func" from the expression
  shiny::installExprFunction(expr, "func", env, quoted)

  function() {
    # Call the function just created
    value <- func()
    paste(rep(value, 3), collapse=", ")
  }
}

# Example of using the renderer.
# This is something that app authors will do.
values <- reactiveValues(A="text")

\dontrun{
# Create an output object
output$tripleA <- renderTriple({
  values$A
})
}

# At the R console, you can experiment with the renderer using isolate()
tripleA <- renderTriple({
  values$A
})

isolate(tripleA())
# "text, text, text"
}

