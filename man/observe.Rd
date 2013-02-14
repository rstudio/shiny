\name{observe}
\alias{observe}
\title{Create a reactive observer}
\usage{
  observe(x, env = parent.frame(), quoted = FALSE,
    label = NULL, suspended = FALSE)
}
\arguments{
  \item{x}{An expression (quoted or unquoted). Any return
  value will be ignored.}

  \item{env}{The parent environment for the reactive
  expression. By default, this is the calling environment,
  the same as when defining an ordinary non-reactive
  expression.}

  \item{quoted}{Is the expression quoted? By default, this
  is \code{FALSE}.  This is useful when you want to use an
  expression that is stored in a variable; to do so, it
  must be quoted with `quote()`.}

  \item{label}{A label for the observer, useful for
  debugging.}

  \item{suspended}{If \code{TRUE}, start the observer in a
  suspended state.  If \code{FALSE} (the default), start in
  a non-suspended state.}
}
\description{
  Creates an observer from the given expression An observer
  is like a reactive expression in that it can read
  reactive values and call reactive expressions, and will
  automatically re-execute when those dependencies change.
  But unlike reactive expression, it doesn't yield a result
  and can't be used as an input to other reactive
  expressions. Thus, observers are only useful for their
  side effects (for example, performing I/O).
}
\details{
  Another contrast between reactive expressions and
  observers is their execution strategy. Reactive
  expressions use lazy evaluation; that is, when their
  dependencies change, they don't re-execute right away but
  rather wait until they are called by someone else.
  Indeed, if they are not called then they will never
  re-execute. In contrast, observers use eager evaluation;
  as soon as their dependencies change, they schedule
  themselves to re-execute.
}
\examples{
values <- reactiveValues(A=1)

obsB <- observe({
  print(values$A + 1)
})

# Can use quoted expressions
obsC <- observe(quote({ print(values$A + 2) }), quoted = TRUE)

# To store expressions for later conversion to observe, use quote()
expr_q <- quote({ print(values$A + 3) })
obsD <- observe(expr_q, quoted = TRUE)

# In a normal Shiny app, the web client will trigger flush events. If you
# are at the console, you can force a flush with flushReact()
shiny:::flushReact()
}

