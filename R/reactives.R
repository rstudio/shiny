Values <- setRefClass(
  'Values',
  fields = list(
    .values = 'environment',
    .dependencies = 'environment'
  ),
  methods = list(
    initialize = function() {
      .values <<- new.env(parent=emptyenv())
      .dependencies <<- new.env(parent=emptyenv())
    },
    get = function(key) {
      ctx <- .getReactiveEnvironment()$currentContext()
      dep.key <- paste(key, ':', ctx$id, sep='')
      if (!exists(dep.key, where=.dependencies, inherits=F)) {
        assign(dep.key, ctx, pos=.dependencies, inherits=F)
        ctx$onInvalidate(function() {
          rm(list=dep.key, pos=.dependencies, inherits=F)
        })
      }
      
      if (!exists(key, where=.values, inherits=F))
        NULL
      else
        base::get(key, pos=.values, inherits=F)
    },
    set = function(key, value) {
      if (exists(key, where=.values, inherits=F)) {
        if (identical(base::get(key, pos=.values, inherits=F), value)) {
          return(invisible())
        }
      }
      
      assign(key, value, pos=.values, inherits=F)
      dep.keys <- objects(
        pos=.dependencies,
        pattern=paste('^\\Q', key, ':', '\\E', '\\d+$', sep=''),
        all.names=T
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
      lapply(names(lst),
             function(name) {
               .self$set(name, lst[[name]])
             })
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

Observable <- setRefClass(
  'Observable',
  fields = list(
    .func = 'function',
    .dependencies = 'Map',
    .initialized = 'logical',
    .value = 'ANY'
  ),
  methods = list(
    initialize = function(func) {
      if (length(formals(func)) > 0)
        stop("Can't make a reactive function from a function that takes one ",
             "or more parameters; only functions without parameters can be ",
             "reactive.")
      .func <<- func
      .dependencies <<- Map$new()
      .initialized <<- F
    },
    getValue = function() {
      if (!.initialized) {
        .initialized <<- T
        .self$.updateValue()
      }
      
      ctx <- .getReactiveEnvironment()$currentContext()
      if (!.dependencies$containsKey(ctx$id)) {
        .dependencies$set(ctx$id, ctx)
        ctx$onInvalidate(function() {
          .dependencies$remove(ctx$id)
        })
      }
      
      if (identical(class(.value), 'try-error'))
        stop(attr(.value, 'condition'))
      return(.value)
    },
    .updateValue = function() {
      old.value <- .value
      
      ctx <- Context$new()
      ctx$onInvalidate(function() {
        .self$.updateValue()
      })
      ctx$onInvalidateHint(function() {
        lapply(
          .dependencies$values(),
          function(dep.ctx) {
            dep.ctx$invalidateHint()
            NULL
          })
      })
      ctx$run(function() {
        .value <<- try(.func(), silent=F)
      })
      if (!identical(old.value, .value)) {
        lapply(
          .dependencies$values(),
          function(dep.ctx) {
            dep.ctx$invalidate()
            NULL
          }
        )
      }
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

# NOTE: we de-roxygenized this comment because the function isn't exported
# Observe
# 
# Creates an observer from the given function. An observer is like a reactive 
# function in that it can read reactive values and call reactive functions, 
# and will automatically re-execute when those dependencies change. But unlike
# reactive functions, it doesn't yield a result and can't be used as an input 
# to other reactive functions. Thus, observers are only useful for their side 
# effects (for example, performing I/O).
#
# @param func The function to observe. It must not have any parameters. Any
# return value from this function will be ignored.
#
observe <- function(func) {
  Observer$new(func)
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
