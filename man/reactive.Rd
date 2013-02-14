\name{reactive}
\alias{reactive}
\title{Create a reactive expression}
\usage{
  reactive(x, env = parent.frame(), quoted = FALSE,
    label = NULL)
}
\arguments{
  \item{x}{An expression (quoted or unquoted).}

  \item{env}{The parent environment for the reactive
  expression. By default, this is the calling environment,
  the same as when defining an ordinary non-reactive
  expression.}

  \item{quoted}{Is the expression quoted? By default, this
  is \code{FALSE}.  This is useful when you want to use an
  expression that is stored in a variable; to do so, it
  must be quoted with `quote()`.}

  \item{label}{A label for the reactive expression, useful
  for debugging.}
}
\description{
  Wraps a normal expression to create a reactive
  expression. Conceptually, a reactive expression is a
  expression whose result will change over time.
}
\details{
  Reactive expressions are expressions that can read
  reactive values and call other reactive expressions.
  Whenever a reactive value changes, any reactive
  expressions that depended on it are marked as
  "invalidated" and will automatically re-execute if
  necessary. If a reactive expression is marked as
  invalidated, any other reactive expressions that recently
  called it are also marked as invalidated. In this way,
  invalidations ripple through the expressions that depend
  on each other.

  See the
  \href{http://rstudio.github.com/shiny/tutorial/}{Shiny
  tutorial} for more information about reactive
  expressions.
}
\examples{
values <- reactiveValues(A=1)

reactiveB <- reactive({
  values$A + 1
})

# Can use quoted expressions
reactiveC <- reactive(quote({ values$A + 2 }), quoted = TRUE)

# To store expressions for later conversion to reactive, use quote()
expr_q <- quote({ values$A + 3 })
reactiveD <- reactive(expr_q, quoted = TRUE)

# View the values from the R console with isolate()
isolate(reactiveB())
isolate(reactiveC())
isolate(reactiveD())
}

