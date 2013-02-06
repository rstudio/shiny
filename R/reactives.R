Dependents <- setRefClass(
  'Dependents',
  fields = list(
    .dependents = 'Map'
  ),
  methods = list(
    register = function() {
      ctx <- .getReactiveEnvironment()$currentContext()
      if (!.dependents$containsKey(ctx$id)) {
        .dependents$set(ctx$id, ctx)
        ctx$onInvalidate(function() {
          .dependents$remove(ctx$id)
        })
      }
    },
    invalidate = function() {
      lapply(
        .dependents$values(),
        function(ctx) {
          ctx$invalidate()
          NULL
        }
      )
    }
  )
)


# ReactiveValues ------------------------------------------------------------

ReactiveValues <- setRefClass(
  'ReactiveValues',
  fields = list(
    .values = 'environment',
    .dependents = 'environment',
    # Dependents for the list of all names, including hidden
    .namesDeps = 'Dependents',
    # Dependents for all values, including hidden
    .allValuesDeps = 'Dependents',
    # Dependents for all values
    .valuesDeps = 'Dependents'
  ),
  methods = list(
    initialize = function() {
      .values <<- new.env(parent=emptyenv())
      .dependents <<- new.env(parent=emptyenv())
    },
    get = function(key) {
      ctx <- .getReactiveEnvironment()$currentContext()
      dep.key <- paste(key, ':', ctx$id, sep='')
      if (!exists(dep.key, where=.dependents, inherits=FALSE)) {
        assign(dep.key, ctx, pos=.dependents, inherits=FALSE)
        ctx$onInvalidate(function() {
          rm(list=dep.key, pos=.dependents, inherits=FALSE)
        })
      }
      
      if (!exists(key, where=.values, inherits=FALSE))
        NULL
      else
        base::get(key, pos=.values, inherits=FALSE)
    },
    set = function(key, value) {
      hidden <- substr(key, 1, 1) == "."

      if (exists(key, where=.values, inherits=FALSE)) {
        if (identical(base::get(key, pos=.values, inherits=FALSE), value)) {
          return(invisible())
        }
      }
      else {
        .namesDeps$invalidate()
      }

      if (hidden)
        .allValuesDeps$invalidate()
      else
        .valuesDeps$invalidate()
      
      assign(key, value, pos=.values, inherits=FALSE)
      dep.keys <- objects(
        pos=.dependents,
        pattern=paste('^\\Q', key, ':', '\\E', '\\d+$', sep=''),
        all.names=TRUE
      )
      lapply(
        mget(dep.keys, envir=.dependents),
        function(ctx) {
          ctx$invalidate()
          NULL
        }
      )
      invisible()
    },
    mset = function(lst) {
      lapply(base::names(lst),
             function(name) {
               .self$set(name, lst[[name]])
             })
    },
    names = function() {
      .namesDeps$register()
      return(ls(.values, all.names=TRUE))
    },
    toList = function(all.names=FALSE) {
      if (all.names)
        .allValuesDeps$register()

      .valuesDeps$register()

      return(as.list(.values, all.names=all.names))
    }
  )
)


# reactivevalues ------------------------------------------------------------
# S3 wrapper class for ReactiveValues reference class

#' Create an object for storing reactive values
#'
#' This function returns an object for storing reactive values. It is similar
#' to a list, but with special capabilities for reactive programming. When you
#' read a value from it, the calling reactive function takes a reactive
#' dependency on that value, and when you write to it, it notifies any reactive
#' functions that depend on that value.
#'
#' @examples
#' # Create the object with no values
#' values <- reactiveValues()
#'
#' # Assign values to 'a' and 'b'
#' values$a <- 3
#' values[['b']] <- 4
#'
#' \dontrun{
#' # From within a reactive context, you can access values with:
#' values$a
#' values[['a']]
#' }
#'
#' # If not in a reactive context (e.g., at the console), you can use isolate()
#' # to retrieve the value:
#' isolate(values$a)
#' isolate(values[['a']])
#'
#' # Set values upon creation
#' values <- reactiveValues(a = 1, b = 2)
#' isolate(values$a)
#'
#' @param ... Objects that will be added to the reactivevalues object. All of
#'   these objects must be named.
#'
#' @seealso \code{\link{isolate}}.
#'
#' @export
reactiveValues <- function(...) {
  args <- list(...)
  if ((length(args) > 0) && (is.null(names(args)) || any(names(args) == "")))
    stop("All arguments passed to reactiveValues() must be named.")

  values <- .createReactiveValues(ReactiveValues$new())

  # Use .subset2() instead of [[, to avoid method dispatch
  .subset2(values, 'impl')$mset(args)
  values
}

# Create a reactivevalues object
#
# @param values A ReactiveValues object
# @param readonly Should this object be read-only?
.createReactiveValues <- function(values = NULL, readonly = FALSE) {
  structure(list(impl=values), class='reactivevalues', readonly=readonly)
}

#' @S3method $ reactivevalues
`$.reactivevalues` <- function(x, name) {
  .subset2(x, 'impl')$get(name)
}

#' @S3method [[ reactivevalues
`[[.reactivevalues` <- `$.reactivevalues`

#' @S3method $<- reactivevalues
`$<-.reactivevalues` <- function(x, name, value) {
  if (attr(x, 'readonly')) {
    stop("Attempted to assign value to a read-only reactivevalues object")
  } else if (length(name) != 1 || !is.character(name)) {
    stop("Must use single string to index into reactivevalues")
  } else {
    .subset2(x, 'impl')$set(name, value)
    x
  }
}

#' @S3method [[<- reactivevalues
`[[<-.reactivevalues` <- `$<-.reactivevalues`

#' @S3method [ reactivevalues
`[.reactivevalues` <- function(values, name) {
  stop("Single-bracket indexing of reactivevalues object is not allowed.")
}

#' @S3method [<- reactivevalues
`[<-.reactivevalues` <- function(values, name, value) {
  stop("Single-bracket indexing of reactivevalues object is not allowed.")
}

#' @S3method names reactivevalues
names.reactivevalues <- function(x) {
  .subset2(x, 'impl')$names()
}

#' @S3method names<- reactivevalues
`names<-.reactivevalues` <- function(x, value) {
  stop("Can't assign names to reactivevalues object")
}

#' @S3method as.list reactivevalues
as.list.reactivevalues <- function(x, all.names=FALSE, ...) {
  .Deprecated("reactiveValuesToList",
    msg = paste("'as.list.reactivevalues' is deprecated. ",
      "Use reactiveValuesToList instead.",
      "\nPlease see ?reactiveValuesToList for more information.",
      sep = ""))

  reactiveValuesToList(x, all.names)
}

#' Convert a reactivevalues object to a list
#'
#' This function does something similar to what you might \code{\link{as.list}}
#' to do. The difference is that the calling context will take dependencies on
#' every object in the reactivevalues object. To avoid taking dependencies on
#' all the objects, you can wrap the call with \code{\link{isolate}()}.
#'
#' @param x A reactivevalues object.
#' @param all.names If \code{TRUE}, include objects with a leading dot. If
#'   \code{FALSE} (the default) don't include those objects.
#' @examples
#' values <- reactiveValues(a = 1)
#' \dontrun{
#' reactiveValuesToList(values)
#' }
#'
#' # To get the objects without taking dependencies on them, use isolate().
#' # isolate() can also be used when calling from outside a reactive context (e.g.
#' # at the console)
#' isolate(reactiveValuesToList(values))
#'
#' @export
reactiveValuesToList <- function(x, all.names=FALSE) {
  .subset2(x, 'impl')$toList(all.names)
}

# Observable ----------------------------------------------------------------

Observable <- setRefClass(
  'Observable',
  fields = list(
    .func = 'function',
    .label = 'character',
    .dependents = 'Dependents',
    .invalidated = 'logical',
    .running = 'logical',
    .value = 'ANY',
    .visible = 'logical',
    .execCount = 'integer'
  ),
  methods = list(
    initialize = function(func, label=deparse(substitute(func))) {
      if (length(formals(func)) > 0)
        stop("Can't make a reactive function from a function that takes one ",
             "or more parameters; only functions without parameters can be ",
             "reactive.")
      .func <<- func
      .invalidated <<- TRUE
      .running <<- FALSE
      .label <<- label
      .execCount <<- 0L
    },
    getValue = function() {
      .dependents$register()

      if (.invalidated || .running) {
        .self$.updateValue()
      }
      
      if (identical(class(.value), 'try-error'))
        stop(attr(.value, 'condition'))

      if (.visible)
        .value
      else
        invisible(.value)
    },
    .updateValue = function() {
      ctx <- Context$new(.label)
      ctx$onInvalidate(function() {
        .invalidated <<- TRUE
        .dependents$invalidate()
      })
      .execCount <<- .execCount + 1L

      .invalidated <<- FALSE

      wasRunning <- .running
      .running <<- TRUE
      on.exit(.running <<- wasRunning)

      ctx$run(function() {
        result <- withVisible(try(.func(), silent=FALSE))
        .visible <<- result$visible
        .value <<- result$value
      })
    }
  )
)

#' Create a Reactive Function
#' 
#' Wraps a normal function to create a reactive function. Conceptually, a 
#' reactive function is a function whose result will change over time.
#' 
#' Reactive functions are functions that can read reactive values and call other
#' reactive functions. Whenever a reactive value changes, any reactive functions
#' that depended on it are marked as "invalidated" and will automatically 
#' re-execute if necessary. If a reactive function is marked as invalidated, any
#' other reactive functions that recently called it are also marked as 
#' invalidated. In this way, invalidations ripple through the functions that 
#' depend on each other.
#' 
#' See the \href{http://rstudio.github.com/shiny/tutorial/}{Shiny tutorial} for 
#' more information about reactive functions.
#' 
#' @param x The value or function to make reactive. The function must not have 
#'   any parameters.
#' @return A reactive function. (Note that reactive functions can only be called
#'   from within other reactive functions.)
#'   
#' @export
reactive <- function(x) {
  UseMethod("reactive")
}
#' @S3method reactive function
reactive.function <- function(x) {
  return(Observable$new(x, deparse(substitute(x)))$getValue)
}
#' @S3method reactive default
reactive.default <- function(x) {
  stop("Don't know how to make this object reactive!")
}

# Return the number of times that a reactive function or observer has been run
execCount <- function(x) {
  if (is.function(x))
    return(environment(x)$.execCount)
  else if (is(x, 'Observer'))
    return(x$.execCount)
  else
    stop('Unexpected argument to execCount')
}

# Observer ------------------------------------------------------------------

Observer <- setRefClass(
  'Observer',
  fields = list(
    .func = 'function',
    .label = 'character',
    .flushCallbacks = 'list',
    .execCount = 'integer',
    .invalidated = 'logical',
    .suspended = 'logical'
  ),
  methods = list(
    initialize = function(func, label) {
      if (length(formals(func)) > 0)
        stop("Can't make an observer from a function that takes parameters; ",
             "only functions without parameters can be reactive.")

      .func <<- func
      .label <<- label
      .execCount <<- 0L
      .invalidated <<- TRUE
      .suspended <<- FALSE

      # Defer the first running of this until flushReact is called
      ctx <- Context$new(.label)
      ctx$onFlush(function() {
        run()
      })
      ctx$addPendingFlush()
    },
    .createContext = function() {
      ctx <- Context$new(.label)

      ctx$onInvalidate(function() {
        .invalidated <<- TRUE

        lapply(.flushCallbacks, function(func) {
          func()
          NULL
        })
        
        if (.suspended == FALSE)
          ctx$addPendingFlush()
      })
      
      ctx$onFlush(function() {
        run()
      })
      
      return(ctx)
    },
    run = function() {
      ctx <- .createContext()
      .execCount <<- .execCount + 1L
      .invalidated <<- FALSE
      ctx$run(.func)
    },
    onInvalidate = function(func) {
      .flushCallbacks <<- c(.flushCallbacks, func)
    },
    suspend = function() {
      "Causes this observer to stop re-executing in response to invalidations.
      If the observer was invalidated prior to this call but it has not
      re-executed yet (because it waits until onFlush is called) then that
      re-execution will still occur."
      .suspended <<- TRUE
    },
    resume = function() {
      "Causes this observer to start re-executing in response to invalidations.
      If the observer was invalidated while suspended, then it will schedule
      itself for re-execution (pending flush)."
      .suspended <<- FALSE
      
      if (.invalidated) {
        .createContext()$invalidate()
      }
    }
  )
)

#' Create a reactive observer
#' 
#' Creates an observer from the given function. An observer is like a reactive 
#' function in that it can read reactive values and call reactive functions, and
#' will automatically re-execute when those dependencies change. But unlike 
#' reactive functions, it doesn't yield a result and can't be used as an input 
#' to other reactive functions. Thus, observers are only useful for their side 
#' effects (for example, performing I/O).
#' 
#' Another contrast between reactive functions and observers is their execution
#' strategy. Reactive functions use lazy evaluation; that is, when their
#' dependencies change, they don't re-execute right away but rather wait until
#' they are called by someone else. Indeed, if they are not called then they
#' will never re-execute. In contrast, observers use eager evaluation; as soon
#' as their dependencies change, they schedule themselves to re-execute.
#' 
#' @param func The function to observe. It must not have any parameters. Any 
#'   return value from this function will be ignored.
#'   
#' @export
observe <- function(func) {
  invisible(Observer$new(func, deparse(substitute(func))))
}

# ---------------------------------------------------------------------------

#' Timer
#' 
#' Creates a reactive timer with the given interval. A reactive timer is like a 
#' reactive value, except reactive values are triggered when they are set, while
#' reactive timers are triggered simply by the passage of time.
#' 
#' \link[=reactive]{Reactive functions} and observers that want to be 
#' invalidated by the timer need to call the timer function that 
#' \code{reactiveTimer} returns, even if the current time value is not actually 
#' needed.
#' 
#' See \code{\link{invalidateLater}} as a safer and simpler alternative.
#' 
#' @param intervalMs How often to fire, in milliseconds
#' @return A no-parameter function that can be called from a reactive context, 
#'   in order to cause that context to be invalidated the next time the timer 
#'   interval elapses. Calling the returned function also happens to yield the 
#'   current time (as in \code{\link{Sys.time}}).
#' @seealso invalidateLater
#' @export
reactiveTimer <- function(intervalMs=1000) {
  dependents <- Map$new()
  timerCallbacks$schedule(intervalMs, function() {
    timerCallbacks$schedule(intervalMs, sys.function())
    lapply(
      dependents$values(),
      function(dep.ctx) {
        dep.ctx$invalidate()
        NULL
      })
  })
  return(function() {
    ctx <- .getReactiveEnvironment()$currentContext()
    if (!dependents$containsKey(ctx$id)) {
      dependents$set(ctx$id, ctx)
      ctx$onInvalidate(function() {
        dependents$remove(ctx$id)
      })
    }
    return(Sys.time())
  })
}

#' Scheduled Invalidation
#' 
#' Schedules the current reactive context to be invalidated in the given number 
#' of milliseconds.
#' @param millis Approximate milliseconds to wait before invalidating the
#'   current reactive context.
#' @export
invalidateLater <- function(millis) {
  ctx <- .getReactiveEnvironment()$currentContext()
  timerCallbacks$schedule(millis, function() {
    ctx$invalidate()
  })
  invisible()
}

#' Create a non-reactive scope for an expression
#' 
#' Executes the given expression in a scope where reactive values or functions 
#' can be read, but they cannot cause the reactive scope of the caller to be 
#' re-evaluated when they change.
#' 
#' Ordinarily, the simple act of reading a reactive value causes a relationship 
#' to be established between the caller and the reactive value, where a change 
#' to the reactive value will cause the caller to re-execute. (The same applies 
#' for the act of getting a reactive function's value.) The \code{isolate} 
#' function lets you read a reactive value or function without establishing this
#' relationship.
#' 
#' The expression given to \code{isolate()} is evaluated in the calling
#' environment. This means that if you assign a variable inside the
#' \code{isolate()}, its value will be visible outside of the \code{isolate()}.
#' If you want to avoid this, you can use \code{\link{local}()} inside the
#' \code{isolate()}.
#'
#' This function can also be useful for calling reactive functions at the
#' console, which can be useful for debugging. To do so, simply wrap the
#' functino calls with \code{isolate()}.
#'
#' @param expr An expression that can access reactive values or functions.
#' 
#' @examples
#' \dontrun{
#' observe(function() {
#'   input$saveButton  # Do take a dependency on input$saveButton
#'   
#'   # isolate a simple expression
#'   data <- get(isolate(input$dataset))  # No dependency on input$dataset
#'   writeToDatabase(data)
#' })
#' 
#' observe(function() {
#'   input$saveButton  # Do take a dependency on input$saveButton
#'   
#'   # isolate a whole block
#'   data <- isolate({
#'     a <- input$valueA   # No dependency on input$valueA or input$valueB
#'     b <- input$valueB
#'     c(a=a, b=b)
#'   })
#'   writeToDatabase(data)
#' })
#'
#' observe(function() {
#'   x <- 1
#'   # x outside of isolate() is affected
#'   isolate(x <- 2)
#'   print(x) # 2
#'
#'   y <- 1
#'   # Use local() to avoid affecting calling environment
#'   isolate(local(y <- 2))
#'   print(y) # 1
#' })
#'
#' }
#'
#' # Can also use isolate to call reactive functions from the R console
#' values <- reactiveValues(A=1)
#' fun <- reactive(function() as.character(values$A))
#' isolate(fun())
#' # "1"
#'
#' # isolate also works if the reactive function accesses values from the
#' # input object, like input$x
#'
#' @export
isolate <- function(expr) {
  ctx <- Context$new('[isolate]')
  ctx$run(function() {
    expr
  })
}
