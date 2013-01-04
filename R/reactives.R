Dependencies <- setRefClass(
  'Dependencies',
  fields = list(
    .dependencies = 'Map'
  ),
  methods = list(
    register = function() {
      ctx <- .getReactiveEnvironment()$currentContext()
      if (!.dependencies$containsKey(ctx$id)) {
        .dependencies$set(ctx$id, ctx)
        ctx$onInvalidate(function() {
          .dependencies$remove(ctx$id)
        })
      }
    },
    invalidate = function() {
      lapply(
        .dependencies$values(),
        function(ctx) {
          ctx$invalidateHint()
          ctx$invalidate()
          NULL
        }
      )
    },
    invalidateHint = function() {
      lapply(
        .dependencies$values(),
        function(dep.ctx) {
          dep.ctx$invalidateHint()
          NULL
        })
    }
  )
)

ReactiveValue <- setRefClass(
  'ReactiveValue',
  fields = list(
    .value = 'ANY',
    .dependencies = 'Dependencies'
  ),
  methods = list(
    initialize = function(value) {
      .value <<- value
    },
    get = function() {
      .dependencies$register()
      return(.value)
    },
    set = function(value) {
      if (identical(.value, value))
        return()
      .value <<- value
      .dependencies$invalidate()
      return()
    }
  )
)

#' @export
reactiveValue <- function(initialValue) {
  obj <- list(impl=ReactiveValue$new(initialValue))
  class(obj) <- 'reactvalue'
  return(obj)
}

#' @export
`value<-` <- function(x, value) {
  UseMethod('value<-')
}
#' @S3method value<- reactvalue
`value<-.reactvalue` <- function(x, value) {
  x[['impl']]$set(value)
  return(x)
}
#' @export
`value` <- function(x) {
  UseMethod('value')
}
#' @S3method value reactvalue
`value.reactvalue` <- function(x) {
  x[['impl']]$get()
}

Values <- setRefClass(
  'Values',
  fields = list(
    .values = 'environment',
    .dependencies = 'environment',
    # Dependencies for the list of names
    .namesDeps = 'Dependencies',
    # Dependencies for all values
    .allDeps = 'Dependencies'
  ),
  methods = list(
    initialize = function() {
      .values <<- new.env(parent=emptyenv())
      .dependencies <<- new.env(parent=emptyenv())
    },
    get = function(key) {
      ctx <- .getReactiveEnvironment()$currentContext()
      dep.key <- paste(key, ':', ctx$id, sep='')
      if (!exists(dep.key, where=.dependencies, inherits=FALSE)) {
        assign(dep.key, ctx, pos=.dependencies, inherits=FALSE)
        ctx$onInvalidate(function() {
          rm(list=dep.key, pos=.dependencies, inherits=FALSE)
        })
      }
      
      if (!exists(key, where=.values, inherits=FALSE))
        NULL
      else
        base::get(key, pos=.values, inherits=FALSE)
    },
    set = function(key, value) {
      if (exists(key, where=.values, inherits=FALSE)) {
        if (identical(base::get(key, pos=.values, inherits=FALSE), value)) {
          return(invisible())
        }
      }
      else {
        .namesDeps$invalidate()
      }
      .allDeps$invalidate()
      
      assign(key, value, pos=.values, inherits=FALSE)
      dep.keys <- objects(
        pos=.dependencies,
        pattern=paste('^\\Q', key, ':', '\\E', '\\d+$', sep=''),
        all.names=TRUE
      )
      lapply(
        mget(dep.keys, envir=.dependencies),
        function(ctx) {
          ctx$invalidateHint()
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
    toList = function() {
      .allDeps$register()
      return(as.list(.values))
    }
  )
)

`[.Values` <- function(values, name) {
  values$get(name)
}

`[<-.Values` <- function(values, name, value) {
  values$set(name, value)
  return(values)
}

.createValuesReader <- function(values) {
  acc <- list(impl=values)
  class(acc) <- 'reactvaluesreader'
  return(acc)
}

#' @S3method $ reactvaluesreader
`$.reactvaluesreader` <- function(x, name) {
  x[['impl']]$get(name)
}

#' @S3method names reactvaluesreader
names.reactvaluesreader <- function(x) {
  x[['impl']]$names()
}

#' @S3method as.list reactvaluesreader
as.list.reactvaluesreader <- function(x, ...) {
  x[['impl']]$toList()
}

Observable <- setRefClass(
  'Observable',
  fields = list(
    .func = 'function',
    .dependencies = 'Dependencies',
    .dirty = 'logical',
    .value = 'ANY'
  ),
  methods = list(
    initialize = function(func) {
      if (length(formals(func)) > 0)
        stop("Can't make a reactive function from a function that takes one ",
             "or more parameters; only functions without parameters can be ",
             "reactive.")
      .func <<- func
      .dirty <<- TRUE
    },
    getValue = function() {
      if (.dirty) {
        .self$.updateValue()
      }
      
      .dependencies$register()
      
      if (identical(class(.value), 'try-error'))
        stop(attr(.value, 'condition'))
      return(.value)
    },
    .updateValue = function() {
      ctx <- Context$new()
      ctx$onInvalidate(function() {
        .dirty <<- TRUE
        .dependencies$invalidate()
      })
      ctx$onInvalidateHint(function() {
        .dependencies$invalidateHint()
      })
      ctx$run(function() {
        .value <<- try(.func(), silent=FALSE)
      })
      .dirty <<- FALSE
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
  return(Observable$new(x)$getValue)
}
#' @S3method reactive default
reactive.default <- function(x) {
  stop("Don't know how to make this object reactive!")
}

Observer <- setRefClass(
  'Observer',
  fields = list(
    .func = 'function',
    .hintCallbacks = 'list'
  ),
  methods = list(
    initialize = function(func) {
      if (length(formals(func)) > 0)
        stop("Can't make an observer from a function that takes parameters; ",
             "only functions without parameters can be reactive.")

      .func <<- func

      # Defer the first running of this until flushReact is called
      ctx <- Context$new()
      ctx$onInvalidate(function() {
        run()
      })
      ctx$invalidate()
    },
    run = function() {
      ctx <- Context$new()
      ctx$onInvalidate(function() {
        run()
      })
      ctx$onInvalidateHint(function() {
        lapply(.hintCallbacks, function(func) {
          func()
          NULL
        })
      })
      ctx$run(.func)
    },
    onInvalidateHint = function(func) {
      .hintCallbacks <<- c(.hintCallbacks, func)
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
  Observer$new(func)
  invisible()
}

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
  dependencies <- Map$new()
  timerCallbacks$schedule(intervalMs, function() {
    timerCallbacks$schedule(intervalMs, sys.function())
    lapply(
      dependencies$values(),
      function(dep.ctx) {
        dep.ctx$invalidate()
        NULL
      })
  })
  return(function() {
    ctx <- .getReactiveEnvironment()$currentContext()
    if (!dependencies$containsKey(ctx$id)) {
      dependencies$set(ctx$id, ctx)
      ctx$onInvalidate(function() {
        dependencies$remove(ctx$id)
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
#' @param expr An expression that can access reactive values or functions.
#' 
#' @examples
#' \dontrun{
#' observer(function() {
#'   input$saveButton  # Do take a dependency on input$saveButton
#'   
#'   # isolate a simple expression
#'   data <- get(isolate(input$dataset))  # No dependency on input$dataset
#'   writeToDatabase(data)
#' })
#' 
#' observer(function() {
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
#' }
#' @export
isolate <- function(expr) {
  ctx <- Context$new()
  ctx$run(function() {
    eval.parent(expr)
  })
}
