Dependents <- setRefClass(
  'Dependents',
  fields = list(
    .dependents = 'Map'
  ),
  methods = list(
    register = function(depId=NULL, depLabel=NULL) {
      ctx <- .getReactiveEnvironment()$currentContext()
      if (!.dependents$containsKey(ctx$id)) {
        .dependents$set(ctx$id, ctx)
        ctx$onInvalidate(function() {
          .dependents$remove(ctx$id)
        })
        
        if (!is.null(depId) && nchar(depId) > 0)
          .graphDependsOnId(ctx$id, depId)
        if (!is.null(depLabel))
          .graphDependsOn(ctx$id, depLabel)
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
    # For debug purposes
    .label = 'character',
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
      .label <<- paste('reactiveValues', runif(1, min=1000, max=9999),
                       sep="")
      .values <<- new.env(parent=emptyenv())
      .dependents <<- new.env(parent=emptyenv())
    },
    get = function(key) {
      ctx <- .getReactiveEnvironment()$currentContext()
      dep.key <- paste(key, ':', ctx$id, sep='')
      if (!exists(dep.key, where=.dependents, inherits=FALSE)) {
        .graphDependsOn(ctx$id, sprintf('%s$%s', .label, key))
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

      .graphValueChange(sprintf('names(%s)', .label), ls(.values, all.names=TRUE))
      .graphValueChange(sprintf('%s (all)', .label), as.list(.values))
      .graphValueChange(sprintf('%s$%s', .label, key), value)

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
      .graphDependsOn(.getReactiveEnvironment()$currentContext()$id,
                      sprintf('names(%s)', .label))
      .namesDeps$register()
      return(ls(.values, all.names=TRUE))
    },
    toList = function(all.names=FALSE) {
      .graphDependsOn(.getReactiveEnvironment()$currentContext()$id,
                      sprintf('%s (all)', .label))
      if (all.names)
        .allValuesDeps$register()

      .valuesDeps$register()

      return(as.list(.values, all.names=all.names))
    },
    .setLabel = function(label) {
      .label <<- label
    }
  )
)


# reactivevalues ------------------------------------------------------------
# S3 wrapper class for ReactiveValues reference class

#' Create an object for storing reactive values
#'
#' This function returns an object for storing reactive values. It is similar
#' to a list, but with special capabilities for reactive programming. When you
#' read a value from it, the calling reactive expression takes a reactive
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
#' @seealso \code{\link{isolate}} and \code{\link{is.reactivevalues}}.
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

# Register the S3 class so that it can be used for a field in a Reference Class
setOldClass("reactivevalues")

# Create a reactivevalues object
#
# @param values A ReactiveValues object
# @param readonly Should this object be read-only?
.createReactiveValues <- function(values = NULL, readonly = FALSE) {
  structure(list(impl=values), class='reactivevalues', readonly=readonly)
}

#' Checks whether an object is a reactivevalues object
#'
#' Checks whether its argument is a reactivevalues object.
#'
#' @param x The object to test.
#' @seealso \code{\link{reactiveValues}}.
#' @export
is.reactivevalues <- function(x) inherits(x, 'reactivevalues')

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
  shinyDeprecated("reactiveValuesToList",
    msg = paste("'as.list.reactivevalues' is deprecated. ",
      "Use reactiveValuesToList instead.",
      "\nPlease see ?reactiveValuesToList for more information.",
      sep = ""))

  reactiveValuesToList(x, all.names)
}

# For debug purposes
.setLabel <- function(x, label) {
  .subset2(x, 'impl')$.setLabel(label)
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
    .execCount = 'integer',
    .mostRecentCtxId = 'character'
  ),
  methods = list(
    initialize = function(func, label=deparse(substitute(func))) {
      if (length(formals(func)) > 0)
        stop("Can't make a reactive expression from a function that takes one ",
             "or more parameters; only functions without parameters can be ",
             "reactive.")
      .func <<- func
      .invalidated <<- TRUE
      .running <<- FALSE
      .label <<- label
      .execCount <<- 0L
      .mostRecentCtxId <<- ""
    },
    getValue = function() {
      .dependents$register()

      if (.invalidated || .running) {
        .self$.updateValue()
      }

      .graphDependsOnId(getCurrentContext()$id, .mostRecentCtxId)
      
      if (identical(class(.value), 'try-error'))
        stop(attr(.value, 'condition'))

      if (.visible)
        .value
      else
        invisible(.value)
    },
    .updateValue = function() {
      ctx <- Context$new(.label, type='observable', prevId=.mostRecentCtxId)
      .mostRecentCtxId <<- ctx$id
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
        result <- withVisible(try(shinyCallingHandlers(.func()), silent=FALSE))
        .visible <<- result$visible
        .value <<- result$value
      })
    }
  )
)

#' Create a reactive expression
#'
#' Wraps a normal expression to create a reactive expression. Conceptually, a
#' reactive expression is a expression whose result will change over time.
#'
#' Reactive expressions are expressions that can read reactive values and call other
#' reactive expressions. Whenever a reactive value changes, any reactive expressions
#' that depended on it are marked as "invalidated" and will automatically
#' re-execute if necessary. If a reactive expression is marked as invalidated, any
#' other reactive expressions that recently called it are also marked as
#' invalidated. In this way, invalidations ripple through the expressions that
#' depend on each other.
#'
#' See the \href{http://rstudio.github.com/shiny/tutorial/}{Shiny tutorial} for
#' more information about reactive expressions.
#'
#' @param x For \code{reactive}, an expression (quoted or unquoted). For 
#'   \code{is.reactive}, an object to test.
#' @param env The parent environment for the reactive expression. By default, this
#'   is the calling environment, the same as when defining an ordinary
#'   non-reactive expression.
#' @param quoted Is the expression quoted? By default, this is \code{FALSE}.
#'   This is useful when you want to use an expression that is stored in a
#'   variable; to do so, it must be quoted with `quote()`.
#' @param label A label for the reactive expression, useful for debugging.
#' @return a function, wrapped in a S3 class "reactive"
#'
#' @examples
#' values <- reactiveValues(A=1)
#'
#' reactiveB <- reactive({
#'   values$A + 1
#' })
#'
#' # Can use quoted expressions
#' reactiveC <- reactive(quote({ values$A + 2 }), quoted = TRUE)
#'
#' # To store expressions for later conversion to reactive, use quote()
#' expr_q <- quote({ values$A + 3 })
#' reactiveD <- reactive(expr_q, quoted = TRUE)
#'
#' # View the values from the R console with isolate()
#' isolate(reactiveB())
#' isolate(reactiveC())
#' isolate(reactiveD())
#'
#' @export
reactive <- function(x, env = parent.frame(), quoted = FALSE, label = NULL) {
  fun <- exprToFunction(x, env, quoted)
  if (is.null(label))
    label <- sprintf('reactive(%s)', paste(deparse(body(fun)), collapse='\n'))

  o <- Observable$new(fun, label)
  registerDebugHook(".func", o, "Reactive")
  structure(o$getValue@.Data, observable = o, class = "reactive")
}

#' @S3method print reactive
print.reactive <- function(x, ...) {
  label <- attr(x, "observable")$.label
  cat(label, "\n")
}

#' @export
#' @rdname reactive
is.reactive <- function(x) inherits(x, "reactive")

# Return the number of times that a reactive expression or observer has been run
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
    .priority = 'numeric',
    .invalidateCallbacks = 'list',
    .execCount = 'integer',
    .onResume = 'function',
    .suspended = 'logical',
    .prevId = 'character'
  ),
  methods = list(
    initialize = function(func, label, suspended = FALSE, priority = 0) {
      if (length(formals(func)) > 0)
        stop("Can't make an observer from a function that takes parameters; ",
             "only functions without parameters can be reactive.")

      .func <<- func
      .label <<- label
      .priority <<- normalizePriority(priority)
      .execCount <<- 0L
      .suspended <<- suspended
      .onResume <<- function() NULL
      .prevId <<- ''

      # Defer the first running of this until flushReact is called
      .createContext()$invalidate()
    },
    .createContext = function() {
      ctx <- Context$new(.label, type='observer', prevId=.prevId)
      .prevId <<- ctx$id

      ctx$onInvalidate(function() {
        lapply(.invalidateCallbacks, function(func) {
          func()
          NULL
        })

        continue <- function() {
          ctx$addPendingFlush(.priority)
        }
        
        if (.suspended == FALSE)
          continue()
        else
          .onResume <<- continue
      })
      
      ctx$onFlush(function() {
        run()
      })
      
      return(ctx)
    },
    run = function() {
      ctx <- .createContext()
      .execCount <<- .execCount + 1L
      ctx$run(.func)
    },
    onInvalidate = function(callback) {
      "Register a callback function to run when this observer is invalidated.
      No arguments will be provided to the callback function when it is
      invoked."
      .invalidateCallbacks <<- c(.invalidateCallbacks, callback)
    },
    setPriority = function(priority = 0) {
      "Change this observer's priority. Note that if the observer is currently
      invalidated, then the change in priority will not take effect until the
      next invalidation--unless the observer is also currently suspended, in
      which case the priority change will be effective upon resume."
      .priority <<- normalizePriority(priority)
    },
    suspend = function() {
      "Causes this observer to stop scheduling flushes (re-executions) in
      response to invalidations. If the observer was invalidated prior to this
      call but it has not re-executed yet (because it waits until onFlush is
      called) then that re-execution will still occur, because the flush is
      already scheduled."
      .suspended <<- TRUE
    },
    resume = function() {
      "Causes this observer to start re-executing in response to invalidations.
      If the observer was invalidated while suspended, then it will schedule
      itself for re-execution (pending flush)."
      if (.suspended) {
        .suspended <<- FALSE
        .onResume()
        .onResume <<- function() NULL
      }
      invisible()
    }
  )
)

#' Create a reactive observer
#' 
#' Creates an observer from the given expression.
#' 
#' An observer is like a reactive
#' expression in that it can read reactive values and call reactive expressions, and
#' will automatically re-execute when those dependencies change. But unlike 
#' reactive expressions, it doesn't yield a result and can't be used as an input 
#' to other reactive expressions. Thus, observers are only useful for their side 
#' effects (for example, performing I/O).
#' 
#' Another contrast between reactive expressions and observers is their execution
#' strategy. Reactive expressions use lazy evaluation; that is, when their
#' dependencies change, they don't re-execute right away but rather wait until
#' they are called by someone else. Indeed, if they are not called then they
#' will never re-execute. In contrast, observers use eager evaluation; as soon
#' as their dependencies change, they schedule themselves to re-execute.
#' 
#' @param x An expression (quoted or unquoted). Any return value will be ignored.
#' @param env The parent environment for the reactive expression. By default, this
#'   is the calling environment, the same as when defining an ordinary
#'   non-reactive expression.
#' @param quoted Is the expression quoted? By default, this is \code{FALSE}.
#'   This is useful when you want to use an expression that is stored in a
#'   variable; to do so, it must be quoted with `quote()`.
#' @param label A label for the observer, useful for debugging.
#' @param suspended If \code{TRUE}, start the observer in a suspended state.
#'   If \code{FALSE} (the default), start in a non-suspended state.
#' @param priority An integer or numeric that controls the priority with which
#'   this observer should be executed. An observer with a given priority level
#'   will always execute sooner than all observers with a lower priority level. 
#'   Positive, negative, and zero values are allowed.
#' @return An observer reference class object. This object has the following 
#'   methods:
#'   \describe{
#'     \item{\code{suspend()}}{
#'       Causes this observer to stop scheduling flushes (re-executions) in
#'       response to invalidations. If the observer was invalidated prior to 
#'       this call but it has not re-executed yet then that re-execution will
#'       still occur, because the flush is already scheduled.
#'     }
#'     \item{\code{resume()}}{
#'       Causes this observer to start re-executing in response to
#'       invalidations. If the observer was invalidated while suspended, then it
#'       will schedule itself for re-execution.
#'     }
#'     \item{\code{setPriority(priority = 0)}}{
#'       Change this observer's priority. Note that if the observer is currently 
#'       invalidated, then the change in priority will not take effect until the
#'       next invalidation--unless the observer is also currently suspended, in 
#'       which case the priority change will be effective upon resume.
#'     }
#'     \item{\code{onInvalidate(callback)}}{
#'       Register a callback function to run when this observer is invalidated.
#'       No arguments will be provided to the callback function when it is
#'       invoked.
#'     }
#'   }
#'
#' @examples
#' values <- reactiveValues(A=1)
#'
#' obsB <- observe({
#'   print(values$A + 1)
#' })
#'
#' # Can use quoted expressions
#' obsC <- observe(quote({ print(values$A + 2) }), quoted = TRUE)
#'
#' # To store expressions for later conversion to observe, use quote()
#' expr_q <- quote({ print(values$A + 3) })
#' obsD <- observe(expr_q, quoted = TRUE)
#'
#' # In a normal Shiny app, the web client will trigger flush events. If you
#' # are at the console, you can force a flush with flushReact()
#' shiny:::flushReact()
#'
#' @export
observe <- function(x, env=parent.frame(), quoted=FALSE, label=NULL,
                    suspended=FALSE, priority=0) {

  fun <- exprToFunction(x, env, quoted)
  if (is.null(label))
    label <- sprintf('observe(%s)', paste(deparse(body(fun)), collapse='\n'))

  o <- Observer$new(fun, label=label, suspended=suspended, priority=priority)
  registerDebugHook(".func", o, "Observer")
  invisible(o)
}

#' Make a reactive variable
#' 
#' Turns a normal variable into a reactive variable, that is, one that has 
#' reactive semantics when assigned or read in the usual ways. The variable may 
#' already exist; if so, its value will be used as the initial value of the 
#' reactive variable (or \code{NULL} if the variable did not exist).
#' 
#' @param symbol A character string indicating the name of the variable that 
#'   should be made reactive
#' @param env The environment that will contain the reactive variable
#' 
#' @return None.
#' 
#' @examples
#' \dontrun{
#' a <- 10
#' makeReactiveBinding("a")
#' b <- reactive(a * -1)
#' observe(print(b()))
#' a <- 20
#' }  
#' @export
makeReactiveBinding <- function(symbol, env = parent.frame()) {
  if (exists(symbol, where = env, inherits = FALSE)) {
    initialValue <- get(symbol, pos = env, inherits = FALSE)
    rm(list = symbol, pos = env, inherits = FALSE)
  }
  else
    initialValue <- NULL
  values <- reactiveValues(value = initialValue)
  makeActiveBinding(symbol, env=env, fun=function(v) {
    if (missing(v))
      values$value
    else
      values$value <- v
  })
  
  invisible()
}

# `%<-reactive%` <- function(name, value) {
#   sym <- deparse(substitute(name))
#   assign(sym, value, pos = parent.frame())
#   makeReactiveBinding(sym, env=parent.frame())
#   invisible(NULL)
# }

# Causes flushReact to be called every time an expression is
# entered into the top-level prompt
setAutoflush <- local({
  callbackId <- NULL
  
  function(enable) {
    if (xor(is.null(callbackId), isTRUE(enable))) {
      return(invisible())
    }
    
    if (isTRUE(enable)) {
      callbackId <<- addTaskCallback(function(expr, value, ok, visible) {
        timerCallbacks$executeElapsed()
        flushReact()
        return(TRUE)
      })
    } else {
      removeTaskCallback(callbackId)
      callbackId <<- NULL
    }
    invisible()
  }
})

# ---------------------------------------------------------------------------

#' Timer
#' 
#' Creates a reactive timer with the given interval. A reactive timer is like a 
#' reactive value, except reactive values are triggered when they are set, while
#' reactive timers are triggered simply by the passage of time.
#' 
#' \link[=reactive]{Reactive expressions} and observers that want to be 
#' invalidated by the timer need to call the timer function that 
#' \code{reactiveTimer} returns, even if the current time value is not actually 
#' needed.
#' 
#' See \code{\link{invalidateLater}} as a safer and simpler alternative.
#' 
#' @param intervalMs How often to fire, in milliseconds
#' @param session A session object. This is needed to cancel any scheduled
#'   invalidations after a user has ended the session. If \code{NULL}, then
#'   this invalidation will not be tied to any session, and so it will still
#'   occur.
#' @return A no-parameter function that can be called from a reactive context, 
#'   in order to cause that context to be invalidated the next time the timer 
#'   interval elapses. Calling the returned function also happens to yield the 
#'   current time (as in \code{\link{Sys.time}}).
#' @seealso \code{\link{invalidateLater}}
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   # Anything that calls autoInvalidate will automatically invalidate
#'   # every 2 seconds.
#'   autoInvalidate <- reactiveTimer(2000, session)
#'
#'   observe({
#'     # Invalidate and re-execute this reactive expression every time the
#'     # timer fires.
#'     autoInvalidate()
#'
#'     # Do something each time this is invalidated.
#'     # The isolate() makes this observer _not_ get invalidated and re-executed
#'     # when input$n changes.
#'     print(paste("The value of input$n is", isolate(input$n)))
#'   })
#'
#'   # Generate a new histogram each time the timer fires, but not when
#'   # input$n changes.
#'   output$plot <- renderPlot({
#'     autoInvalidate()
#'     hist(isolate(input$n))
#'   })
#' })
#' }
#'
#' @export
reactiveTimer <- function(intervalMs=1000, session) {
  if (missing(session)) {
    warning("reactiveTimer should be passed a session object or NULL")
    session <- NULL
  }

  dependents <- Map$new()
  timerCallbacks$schedule(intervalMs, function() {
    # Quit if the session is closed
    if (!is.null(session) && session$isClosed()) {
      return(invisible())
    }

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
#'
#' If this is placed within an observer or reactive expression, that object will
#' be invalidated (and re-execute) after the interval has passed. The
#' re-execution will reset the invalidation flag, so in a typical use case, the
#' object will keep re-executing and waiting for the specified interval. It's
#' possible to stop this cycle by adding conditional logic that prevents the
#' \code{invalidateLater} from being run.
#'
#' @param millis Approximate milliseconds to wait before invalidating the
#'   current reactive context.
#' @param session A session object. This is needed to cancel any scheduled
#'   invalidations after a user has ended the session. If \code{NULL}, then
#'   this invalidation will not be tied to any session, and so it will still
#'   occur.
#'
#' @seealso \code{\link{reactiveTimer}} is a slightly less safe alternative.
#'
#' @examples
#' \dontrun{
#' shinyServer(function(input, output, session) {
#'
#'   observe({
#'     # Re-execute this reactive expression after 1000 milliseconds
#'     invalidateLater(1000, session)
#'
#'     # Do something each time this is invalidated.
#'     # The isolate() makes this observer _not_ get invalidated and re-executed
#'     # when input$n changes.
#'     print(paste("The value of input$n is", isolate(input$n)))
#'   })
#'
#'   # Generate a new histogram at timed intervals, but not when
#'   # input$n changes.
#'   output$plot <- renderPlot({
#'     # Re-execute this reactive expression after 2000 milliseconds
#'     invalidateLater(2000, session)
#'     hist(isolate(input$n))
#'   })
#' })
#' }
#'
#' @export
invalidateLater <- function(millis, session) {
  if (missing(session)) {
    warning("invalidateLater should be passed a session object or NULL")
    session <- NULL
  }

  ctx <- .getReactiveEnvironment()$currentContext()
  timerCallbacks$schedule(millis, function() {
    # Quit if the session is closed
    if (!is.null(session) && session$isClosed()) {
      return(invisible())
    }
    ctx$invalidate()
  })
  invisible()
}

coerceToFunc <- function(x) {
  force(x);
  if (is.function(x))
    return(x)
  else
    return(function() x)
}

#' Reactive polling
#' 
#' Used to create a reactive data source, which works by periodically polling a 
#' non-reactive data source.
#' 
#' \code{reactivePoll} works by pairing a relatively cheap "check" function with
#' a more expensive value retrieval function. The check function will be 
#' executed periodically and should always return a consistent value until the 
#' data changes. When the check function returns a different value, then the 
#' value retrieval function will be used to re-populate the data.
#' 
#' Note that the check function doesn't return \code{TRUE} or \code{FALSE} to 
#' indicate whether the underlying data has changed. Rather, the check function 
#' indicates change by returning a different value from the previous time it was
#' called.
#' 
#' For example, \code{reactivePoll} is used to implement 
#' \code{reactiveFileReader} by pairing a check function that simply returns the
#' last modified timestamp of a file, and a value retrieval function that 
#' actually reads the contents of the file.
#' 
#' As another example, one might read a relational database table reactively by 
#' using a check function that does \code{SELECT MAX(timestamp) FROM table} and 
#' a value retrieval function that does \code{SELECT * FROM table}.
#' 
#' The \code{intervalMillis}, \code{checkFunc}, and \code{valueFunc} functions
#' will be executed in a reactive context; therefore, they may read reactive
#' values and reactive expressions.
#' 
#' @param intervalMillis Approximate number of milliseconds to wait between 
#'   calls to \code{checkFunc}. This can be either a numeric value, or a 
#'   function that returns a numeric value.
#' @param session The user session to associate this file reader with, or 
#'   \code{NULL} if none. If non-null, the reader will automatically stop when 
#'   the session ends.
#' @param checkFunc A relatively cheap function whose values over time will be 
#'   tested for equality; inequality indicates that the underlying value has 
#'   changed and needs to be invalidated and re-read using \code{valueFunc}. See
#'   Details.
#' @param valueFunc A function that calculates the underlying value. See 
#'   Details.
#'   
#' @return A reactive expression that returns the result of \code{valueFunc}, 
#'   and invalidates when \code{checkFunc} changes.
#'   
#' @seealso \code{\link{reactiveFileReader}}
#'   
#' @examples
#' \dontrun{
#' # Assume the existence of readTimestamp and readValue functions
#' shinyServer(function(input, output, session) {
#'   data <- reactivePoll(1000, session, readTimestamp, readValue)
#'   output$dataTable <- renderTable({
#'     data()
#'   })
#' })
#' }
#'   
#' @export
reactivePoll <- function(intervalMillis, session, checkFunc, valueFunc) {
  intervalMillis <- coerceToFunc(intervalMillis)
  
  rv <- reactiveValues(cookie = isolate(checkFunc()))
  
  observe({
    rv$cookie <- checkFunc()
    invalidateLater(intervalMillis(), session)
  })
  
  # TODO: what to use for a label?
  re <- reactive({
    rv$cookie
    
    valueFunc()
    
  }, label = NULL)
  
  return(re)
}

#' Reactive file reader
#' 
#' Given a file path and read function, returns a reactive data source for the 
#' contents of the file.
#' 
#' \code{reactiveFileReader} works by periodically checking the file's last 
#' modified time; if it has changed, then the file is re-read and any reactive 
#' dependents are invalidated.
#' 
#' The \code{intervalMillis}, \code{filePath}, and \code{readFunc} functions 
#' will each be executed in a reactive context; therefore, they may read
#' reactive values and reactive expressions.
#' 
#' @param intervalMillis Approximate number of milliseconds to wait between 
#'   checks of the file's last modified time. This can be a numeric value, or a 
#'   function that returns a numeric value.
#' @param session The user session to associate this file reader with, or 
#'   \code{NULL} if none. If non-null, the reader will automatically stop when 
#'   the session ends.
#' @param filePath The file path to poll against and to pass to \code{readFunc}.
#'   This can either be a single-element character vector, or a function that 
#'   returns one.
#' @param readFunc The function to use to read the file; must expect the first 
#'   argument to be the file path to read. The return value of this function is 
#'   used as the value of the reactive file reader.
#' @param ... Any additional arguments to pass to \code{readFunc} whenever it is
#'   invoked.
#'   
#' @return A reactive expression that returns the contents of the file, and 
#'   automatically invalidates when the file changes on disk (as determined by 
#'   last modified time).
#'   
#' @seealso \code{\link{reactivePoll}}
#'   
#' @examples
#' \dontrun{
#' # Per-session reactive file reader
#' shinyServer(function(input, output, session)) {
#'   fileData <- reactiveFileReader(1000, session, 'data.csv', read.csv)
#'   
#'   output$data <- renderTable({
#'     fileData()
#'   })
#' }
#' 
#' # Cross-session reactive file reader. In this example, all sessions share
#' # the same reader, so read.csv only gets executed once no matter how many
#' # user sessions are connected.
#' fileData <- reactiveFileReader(1000, session, 'data.csv', read.csv)
#' shinyServer(function(input, output, session)) {
#'   output$data <- renderTable({
#'     fileData()
#'   })
#' }
#' }
#' 
#' @export
reactiveFileReader <- function(intervalMillis, session, filePath, readFunc, ...) {
  filePath <- coerceToFunc(filePath)
  extraArgs <- list(...)
  
  reactivePoll(
    intervalMillis, session,
    function() {
      path <- filePath()
      info <- file.info(path)
      return(paste(path, info$mtime, info$size))
    },
    function() {
      do.call(readFunc, c(filePath(), extraArgs))
    }
  )
}

#' Create a non-reactive scope for an expression
#' 
#' Executes the given expression in a scope where reactive values or expression 
#' can be read, but they cannot cause the reactive scope of the caller to be 
#' re-evaluated when they change.
#' 
#' Ordinarily, the simple act of reading a reactive value causes a relationship 
#' to be established between the caller and the reactive value, where a change 
#' to the reactive value will cause the caller to re-execute. (The same applies 
#' for the act of getting a reactive expression's value.) The \code{isolate} 
#' function lets you read a reactive value or expression without establishing this
#' relationship.
#' 
#' The expression given to \code{isolate()} is evaluated in the calling
#' environment. This means that if you assign a variable inside the
#' \code{isolate()}, its value will be visible outside of the \code{isolate()}.
#' If you want to avoid this, you can use \code{\link{local}()} inside the
#' \code{isolate()}.
#'
#' This function can also be useful for calling reactive expression at the
#' console, which can be useful for debugging. To do so, simply wrap the
#' calls to the reactive expression with \code{isolate()}.
#'
#' @param expr An expression that can access reactive values or expressions.
#' 
#' @examples
#' \dontrun{
#' observe({
#'   input$saveButton  # Do take a dependency on input$saveButton
#'   
#'   # isolate a simple expression
#'   data <- get(isolate(input$dataset))  # No dependency on input$dataset
#'   writeToDatabase(data)
#' })
#' 
#' observe({
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
#' observe({
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
#' # Can also use isolate to call reactive expressions from the R console
#' values <- reactiveValues(A=1)
#' fun <- reactive({ as.character(values$A) })
#' isolate(fun())
#' # "1"
#'
#' # isolate also works if the reactive expression accesses values from the
#' # input object, like input$x
#'
#' @export
isolate <- function(expr) {
  ctx <- Context$new('[isolate]', type='isolate')
  on.exit(ctx$invalidate())
  ctx$run(function() {
    expr
  })
}
