#' @include utils.R
NULL

Dependents <- R6Class(
  'Dependents',
  portable = FALSE,
  class = FALSE,
  public = list(
    .dependents = 'Map',

    initialize = function() {
      .dependents <<- Map$new()
    },
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

ReactiveValues <- R6Class(
  'ReactiveValues',
  portable = FALSE,
  public = list(
    # For debug purposes
    .label = character(0),
    .values = 'environment',
    .dependents = 'environment',
    # Dependents for the list of all names, including hidden
    .namesDeps = 'Dependents',
    # Dependents for all values, including hidden
    .allValuesDeps = 'Dependents',
    # Dependents for all values
    .valuesDeps = 'Dependents',

    initialize = function() {
      .label <<- paste('reactiveValues',
                       p_randomInt(1000, 10000),
                       sep="")
      .values <<- new.env(parent=emptyenv())
      .dependents <<- new.env(parent=emptyenv())
      .namesDeps <<- Dependents$new()
      .allValuesDeps <<- Dependents$new()
      .valuesDeps <<- Dependents$new()
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
               self$set(name, lst[[name]])
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
#' This function returns an object for storing reactive values. It is similar to
#' a list, but with special capabilities for reactive programming. When you read
#' a value from it, the calling reactive expression takes a reactive dependency
#' on that value, and when you write to it, it notifies any reactive functions
#' that depend on that value. Note that values taken from the reactiveValues
#' object are reactive, but the reactiveValues object itself is not.
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

checkName <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    stop("Must use single string to index into reactivevalues")
  }
}

# Create a reactivevalues object
#
# @param values A ReactiveValues object
# @param readonly Should this object be read-only?
# @param ns A namespace function (either `identity` or `NS(namespace)`)
.createReactiveValues <- function(values = NULL, readonly = FALSE,
  ns = identity) {

  structure(
    list(
      impl = values,
      readonly = readonly,
      ns = ns
    ),
    class='reactivevalues'
  )
}

#' Checks whether an object is a reactivevalues object
#'
#' Checks whether its argument is a reactivevalues object.
#'
#' @param x The object to test.
#' @seealso \code{\link{reactiveValues}}.
#' @export
is.reactivevalues <- function(x) inherits(x, 'reactivevalues')

#' @export
`$.reactivevalues` <- function(x, name) {
  checkName(name)
  .subset2(x, 'impl')$get(.subset2(x, 'ns')(name))
}

#' @export
`[[.reactivevalues` <- `$.reactivevalues`

#' @export
`$<-.reactivevalues` <- function(x, name, value) {
  if (.subset2(x, 'readonly')) {
    stop("Attempted to assign value to a read-only reactivevalues object")
  }
  checkName(name)
  .subset2(x, 'impl')$set(.subset2(x, 'ns')(name), value)
  x
}

#' @export
`[[<-.reactivevalues` <- `$<-.reactivevalues`

#' @export
`[.reactivevalues` <- function(values, name) {
  stop("Single-bracket indexing of reactivevalues object is not allowed.")
}

#' @export
`[<-.reactivevalues` <- function(values, name, value) {
  stop("Single-bracket indexing of reactivevalues object is not allowed.")
}

#' @export
names.reactivevalues <- function(x) {
  prefix <- .subset2(x, 'ns')("")
  results <- .subset2(x, 'impl')$names()
  if (nzchar(prefix)) {
    results <- results[substring(results, 1, nchar(prefix)) == prefix]
    results <- substring(results, nchar(prefix) + 1)
  }
  results
}

#' @export
`names<-.reactivevalues` <- function(x, value) {
  stop("Can't assign names to reactivevalues object")
}

#' @export
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

# This function is needed because str() on a reactivevalues object will call
# [[.reactivevalues(), which will give an error when it tries to access
# x[['impl']].
#' @export
str.reactivevalues <- function(object, indent.str = " ", ...) {
  utils::str(unclass(object), indent.str = indent.str, ...)
  # Need to manually print out the class field,
  cat(indent.str, '- attr(*, "class")=', sep = "")
  utils::str(class(object))
}

# Observable ----------------------------------------------------------------

Observable <- R6Class(
  'Observable',
  portable = FALSE,
  public = list(
    .func = 'function',
    .label = character(0),
    .domain = NULL,
    .dependents = 'Dependents',
    .invalidated = logical(0),
    .running = logical(0),
    .value = NULL,
    .error = FALSE,
    .visible = logical(0),
    .execCount = integer(0),
    .mostRecentCtxId = character(0),

    initialize = function(func, label = deparse(substitute(func)),
                          domain = getDefaultReactiveDomain(),
                          ..stacktraceon = TRUE) {
      if (length(formals(func)) > 0)
        stop("Can't make a reactive expression from a function that takes one ",
             "or more parameters; only functions without parameters can be ",
             "reactive.")

      .func <<- wrapFunctionLabel(func, paste("reactive", label),
        ..stacktraceon = ..stacktraceon)
      .label <<- label
      .domain <<- domain
      .dependents <<- Dependents$new()
      .invalidated <<- TRUE
      .running <<- FALSE
      .execCount <<- 0L
      .mostRecentCtxId <<- ""
    },
    getValue = function() {
      .dependents$register()

      if (.invalidated || .running) {
        ..stacktraceoff..(
          self$.updateValue()
        )
      }

      .graphDependsOnId(getCurrentContext()$id, .mostRecentCtxId)

      if (.error) {
        stop(.value)
      }

      if (.visible)
        .value
      else
        invisible(.value)
    },
    .updateValue = function() {
      ctx <- Context$new(.domain, .label, type = 'observable',
                         prevId = .mostRecentCtxId)
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
        result <- withCallingHandlers(

          {
            .error <<- FALSE
            withVisible(.func())
          },

          error = function(cond) {
            # If an error occurs, we want to propagate the error, but we also
            # want to save a copy of it, so future callers of this reactive will
            # get the same error (i.e. the error is cached).

            # We stripStackTrace in the next line, just in case someone
            # downstream of us (i.e. deeper into the call stack) used
            # captureStackTraces; otherwise the entire stack would always be the
            # same (i.e. you'd always see the whole stack trace of the *first*
            # time the code was run and the condition raised; there'd be no way
            # to see the stack trace of the call site that caused the cached
            # exception to be re-raised, and you need that information to figure
            # out what's triggering the re-raise).
            #
            # We use try(stop()) as an easy way to generate a try-error object
            # out of this condition.
            .value <<- cond
            .error <<- TRUE
            .visible <<- FALSE
          }
        )
        .value <<- result$value
        .visible <<- result$visible
      })
    }
  )
)

#' Create a reactive expression
#'
#' Wraps a normal expression to create a reactive expression. Conceptually, a
#' reactive expression is a expression whose result will change over time.
#'
#' Reactive expressions are expressions that can read reactive values and call
#' other reactive expressions. Whenever a reactive value changes, any reactive
#' expressions that depended on it are marked as "invalidated" and will
#' automatically re-execute if necessary. If a reactive expression is marked as
#' invalidated, any other reactive expressions that recently called it are also
#' marked as invalidated. In this way, invalidations ripple through the
#' expressions that depend on each other.
#'
#' See the \href{http://rstudio.github.com/shiny/tutorial/}{Shiny tutorial} for
#' more information about reactive expressions.
#'
#' @param x For \code{reactive}, an expression (quoted or unquoted). For
#'   \code{is.reactive}, an object to test.
#' @param env The parent environment for the reactive expression. By default,
#'   this is the calling environment, the same as when defining an ordinary
#'   non-reactive expression.
#' @param quoted Is the expression quoted? By default, this is \code{FALSE}.
#'   This is useful when you want to use an expression that is stored in a
#'   variable; to do so, it must be quoted with \code{quote()}.
#' @param label A label for the reactive expression, useful for debugging.
#' @param domain See \link{domains}.
#' @param ..stacktraceon Advanced use only. For stack manipulation purposes; see
#'   \code{\link{stacktrace}}.
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
reactive <- function(x, env = parent.frame(), quoted = FALSE, label = NULL,
                     domain = getDefaultReactiveDomain(),
                     ..stacktraceon = TRUE) {
  fun <- exprToFunction(x, env, quoted)
  # Attach a label and a reference to the original user source for debugging
  srcref <- attr(substitute(x), "srcref", exact = TRUE)
  if (is.null(label)) {
    label <- srcrefToLabel(srcref[[1]],
      sprintf('reactive(%s)', paste(deparse(body(fun)), collapse='\n')))
  }
  if (length(srcref) >= 2) attr(label, "srcref") <- srcref[[2]]
  attr(label, "srcfile") <- srcFileOfRef(srcref[[1]])
  o <- Observable$new(fun, label, domain, ..stacktraceon = ..stacktraceon)
  structure(o$getValue, observable = o, class = "reactive")
}

# Given the srcref to a reactive expression, attempts to figure out what the
# name of the reactive expression is. This isn't foolproof, as it literally
# scans the line of code that started the reactive block and looks for something
# that looks like assignment. If we fail, fall back to a default value (likely
# the block of code in the body of the reactive).
srcrefToLabel <- function(srcref, defaultLabel) {
  if (is.null(srcref))
    return(defaultLabel)

  srcfile <- attr(srcref, "srcfile", exact = TRUE)
  if (is.null(srcfile))
    return(defaultLabel)

  if (is.null(srcfile$lines))
    return(defaultLabel)

  lines <- srcfile$lines
  # When pasting at the Console, srcfile$lines is not split
  if (length(lines) == 1) {
    lines <- strsplit(lines, "\n")[[1]]
  }

  if (length(lines) < srcref[1]) {
    return(defaultLabel)
  }

  firstLine <- substring(lines[srcref[1]], 1, srcref[2] - 1)

  m <- regexec("(.*)(<-|=)\\s*reactive\\s*\\($", firstLine)
  if (m[[1]][1] == -1) {
    return(defaultLabel)
  }
  sym <- regmatches(firstLine, m)[[1]][2]
  res <- try(parse(text = sym), silent = TRUE)
  if (inherits(res, "try-error"))
    return(defaultLabel)

  if (length(res) != 1)
    return(defaultLabel)

  return(as.character(res))
}

#' @export
print.reactive <- function(x, ...) {
  label <- attr(x, "observable")$.label
  cat(label, "\n")
}

#' @export
#' @rdname reactive
is.reactive <- function(x) inherits(x, "reactive")

# Return the number of times that a reactive expression or observer has been run
execCount <- function(x) {
  if (is.reactive(x))
    return(attr(x, "observable")$.execCount)
  else if (inherits(x, 'Observer'))
    return(x$.execCount)
  else
    stop('Unexpected argument to execCount')
}

# Observer ------------------------------------------------------------------

Observer <- R6Class(
  'Observer',
  portable = FALSE,
  public = list(
    .func = 'function',
    .label = character(0),
    .domain = 'ANY',
    .priority = numeric(0),
    .autoDestroy = logical(0),
    .invalidateCallbacks = list(),
    .execCount = integer(0),
    .onResume = 'function',
    .suspended = logical(0),
    .destroyed = logical(0),
    .prevId = character(0),

    initialize = function(observerFunc, label, suspended = FALSE, priority = 0,
                          domain = getDefaultReactiveDomain(),
                          autoDestroy = TRUE, ..stacktraceon = TRUE) {
      if (length(formals(observerFunc)) > 0)
        stop("Can't make an observer from a function that takes parameters; ",
             "only functions without parameters can be reactive.")
registerDebugHook("observerFunc", environment(), label)
      .func <<- function() {
        tryCatch(
          if (..stacktraceon)
            ..stacktraceon..(observerFunc())
          else
            observerFunc(),
          validation = function(e) {
            # It's OK for a validation error to cause an observer to stop
            # running
          }
        )
      }
      .label <<- label
      .domain <<- domain
      .autoDestroy <<- autoDestroy
      .priority <<- normalizePriority(priority)
      .execCount <<- 0L
      .suspended <<- suspended
      .onResume <<- function() NULL
      .destroyed <<- FALSE
      .prevId <<- ''

      onReactiveDomainEnded(.domain, self$.onDomainEnded)

      # Defer the first running of this until flushReact is called
      .createContext()$invalidate()
    },
    .createContext = function() {
      ctx <- Context$new(.domain, .label, type='observer', prevId=.prevId)
      .prevId <<- ctx$id

      ctx$onInvalidate(function() {
        lapply(.invalidateCallbacks, function(invalidateCallback) {
          invalidateCallback()
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
        tryCatch({
          if (!.destroyed)
            shinyCallingHandlers(run())

        }, error = function(e) {
          printError(e)
          if (!is.null(.domain)) {
            .domain$unhandledError(e)
          }
        })
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
    setAutoDestroy = function(autoDestroy) {
      "Sets whether this observer should be automatically destroyed when its
      domain (if any) ends. If autoDestroy is TRUE and the domain already
      ended, then destroy() is called immediately."
      oldValue <- .autoDestroy
      .autoDestroy <<- autoDestroy
      if (!is.null(.domain) && .domain$isEnded()) {
        destroy()
      }
      invisible(oldValue)
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
    },
    destroy = function() {
      "Prevents this observer from ever executing again (even if a flush has
      already been scheduled)."

      suspend()
      .destroyed <<- TRUE
    },
    .onDomainEnded = function() {
      if (isTRUE(.autoDestroy)) {
        destroy()
      }
    }
  )
)

#' Create a reactive observer
#'
#' Creates an observer from the given expression.
#'
#' An observer is like a reactive expression in that it can read reactive values
#' and call reactive expressions, and will automatically re-execute when those
#' dependencies change. But unlike reactive expressions, it doesn't yield a
#' result and can't be used as an input to other reactive expressions. Thus,
#' observers are only useful for their side effects (for example, performing
#' I/O).
#'
#' Another contrast between reactive expressions and observers is their
#' execution strategy. Reactive expressions use lazy evaluation; that is, when
#' their dependencies change, they don't re-execute right away but rather wait
#' until they are called by someone else. Indeed, if they are not called then
#' they will never re-execute. In contrast, observers use eager evaluation; as
#' soon as their dependencies change, they schedule themselves to re-execute.
#'
#' Starting with Shiny 0.10.0, observers are automatically destroyed by default
#' when the \link[=domains]{domain} that owns them ends (e.g. when a Shiny session
#' ends).
#'
#' @param x An expression (quoted or unquoted). Any return value will be
#'   ignored.
#' @param env The parent environment for the reactive expression. By default,
#'   this is the calling environment, the same as when defining an ordinary
#'   non-reactive expression.
#' @param quoted Is the expression quoted? By default, this is \code{FALSE}.
#'   This is useful when you want to use an expression that is stored in a
#'   variable; to do so, it must be quoted with \code{quote()}.
#' @param label A label for the observer, useful for debugging.
#' @param suspended If \code{TRUE}, start the observer in a suspended state.
#'   If \code{FALSE} (the default), start in a non-suspended state.
#' @param priority An integer or numeric that controls the priority with which
#'   this observer should be executed. An observer with a given priority level
#'   will always execute sooner than all observers with a lower priority level.
#'   Positive, negative, and zero values are allowed.
#' @param domain See \link{domains}.
#' @param autoDestroy If \code{TRUE} (the default), the observer will be
#'   automatically destroyed when its domain (if any) ends.
#' @param ..stacktraceon Advanced use only. For stack manipulation purposes; see
#'   \code{\link{stacktrace}}.
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
#'     \item{\code{destroy()}}{
#'       Stops the observer from executing ever again, even if it is currently
#'       scheduled for re-execution.
#'     }
#'     \item{\code{setPriority(priority = 0)}}{
#'       Change this observer's priority. Note that if the observer is currently
#'       invalidated, then the change in priority will not take effect until the
#'       next invalidation--unless the observer is also currently suspended, in
#'       which case the priority change will be effective upon resume.
#'     }
#'     \item{\code{setAutoDestroy(autoDestroy)}}{
#'       Sets whether this observer should be automatically destroyed when its
#'       domain (if any) ends. If autoDestroy is TRUE and the domain already
#'       ended, then destroy() is called immediately."
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
                    suspended=FALSE, priority=0,
                    domain=getDefaultReactiveDomain(), autoDestroy = TRUE,
                    ..stacktraceon = TRUE) {

  fun <- exprToFunction(x, env, quoted)
  if (is.null(label))
    label <- sprintf('observe(%s)', paste(deparse(body(fun)), collapse='\n'))

  o <- Observer$new(fun, label=label, suspended=suspended, priority=priority,
                    domain=domain, autoDestroy=autoDestroy,
                    ..stacktraceon=..stacktraceon)
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
#'   autoInvalidate <- reactiveTimer(2000)
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
reactiveTimer <- function(intervalMs=1000, session = getDefaultReactiveDomain()) {
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
#'     invalidateLater(2000)
#'     hist(isolate(input$n))
#'   })
#' })
#' }
#'
#' @export
invalidateLater <- function(millis, session = getDefaultReactiveDomain()) {
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
  ctx <- Context$new(getDefaultReactiveDomain(), '[isolate]', type='isolate')
  on.exit(ctx$invalidate())
  # Matching ..stacktraceon../..stacktraceoff.. pair
  ..stacktraceoff..(ctx$run(function() {
    ..stacktraceon..(expr)
  }))
}

#' Evaluate an expression without a reactive context
#'
#' Temporarily blocks the current reactive context and evaluates the given
#' expression. Any attempt to directly access reactive values or expressions in
#' \code{expr} will give the same results as doing it at the top-level (by
#' default, an error).
#'
#' @param expr An expression to evaluate.
#' @return The value of \code{expr}.
#'
#' @seealso \code{\link{isolate}}
#'
#' @export
maskReactiveContext <- function(expr) {
  .getReactiveEnvironment()$runWith(NULL, function() {
    expr
  })
}

#' Event handler
#'
#' Respond to "event-like" reactive inputs, values, and expressions.
#'
#' Shiny's reactive programming framework is primarily designed for calculated
#' values (reactive expressions) and side-effect-causing actions (observers)
#' that respond to \emph{any} of their inputs changing. That's often what is
#' desired in Shiny apps, but not always: sometimes you want to wait for a
#' specific action to be taken from the user, like clicking an
#' \code{\link{actionButton}}, before calculating an expression or taking an
#' action. A reactive value or expression that is used to trigger other
#' calculations in this way is called an \emph{event}.
#'
#' These situations demand a more imperative, "event handling" style of
#' programming that is possible--but not particularly intuitive--using the
#' reactive programming primitives \code{\link{observe}} and
#' \code{\link{isolate}}. \code{observeEvent} and \code{eventReactive} provide
#' straightforward APIs for event handling that wrap \code{observe} and
#' \code{isolate}.
#'
#' Use \code{observeEvent} whenever you want to \emph{perform an action} in
#' response to an event. (Note that "recalculate a value" does not generally
#' count as performing an action--see \code{eventReactive} for that.) The first
#' argument is the event you want to respond to, and the second argument is a
#' function that should be called whenever the event occurs.
#'
#' Use \code{eventReactive} to create a \emph{calculated value} that only
#' updates in response to an event. This is just like a normal
#' \link[=reactive]{reactive expression} except it ignores all the usual
#' invalidations that come from its reactive dependencies; it only invalidates
#' in response to the given event.
#'
#' Both \code{observeEvent} and \code{eventReactive} take an \code{ignoreNULL}
#' parameter that affects behavior when the \code{eventExpr} evaluates to
#' \code{NULL} (or in the special case of an \code{\link{actionButton}},
#' \code{0}). In these cases, if \code{ignoreNULL} is \code{TRUE}, then an
#' \code{observeEvent} will not execute and an \code{eventReactive} will raise a
#' silent \link[=validate]{validation} error. This is useful behavior if you
#' don't want to do the action or calculation when your app first starts, but
#' wait for the user to initiate the action first (like a "Submit" button);
#' whereas \code{ignoreNULL=FALSE} is desirable if you want to initially perform
#' the action/calculation and just let the user re-initiate it (like a
#' "Recalculate" button).
#'
#' @param eventExpr A (quoted or unquoted) expression that represents the event;
#'   this can be a simple reactive value like \code{input$click}, a call to a
#'   reactive expression like \code{dataset()}, or even a complex expression
#'   inside curly braces
#' @param handlerExpr The expression to call whenever \code{eventExpr} is
#'   invalidated. This should be a side-effect-producing action (the return
#'   value will be ignored). It will be executed within an \code{\link{isolate}}
#'   scope.
#' @param valueExpr The expression that produces the return value of the
#'   \code{eventReactive}. It will be executed within an \code{\link{isolate}}
#'   scope.
#' @param event.env The parent environment for \code{eventExpr}. By default,
#'   this is the calling environment.
#' @param event.quoted Is the \code{eventExpr} expression quoted? By default,
#'   this is \code{FALSE}. This is useful when you want to use an expression
#'   that is stored in a variable; to do so, it must be quoted with
#'   \code{quote()}.
#' @param handler.env The parent environment for \code{handlerExpr}. By default,
#'   this is the calling environment.
#' @param handler.quoted Is the \code{handlerExpr} expression quoted? By
#'   default, this is \code{FALSE}. This is useful when you want to use an
#'   expression that is stored in a variable; to do so, it must be quoted with
#'   \code{quote()}.
#' @param value.env The parent environment for \code{valueExpr}. By default,
#'   this is the calling environment.
#' @param value.quoted Is the \code{valueExpr} expression quoted? By default,
#'   this is \code{FALSE}. This is useful when you want to use an expression
#'   that is stored in a variable; to do so, it must be quoted with \code{quote()}.
#' @param label A label for the observer or reactive, useful for debugging.
#' @param suspended If \code{TRUE}, start the observer in a suspended state. If
#'   \code{FALSE} (the default), start in a non-suspended state.
#' @param priority An integer or numeric that controls the priority with which
#'   this observer should be executed. An observer with a given priority level
#'   will always execute sooner than all observers with a lower priority level.
#'   Positive, negative, and zero values are allowed.
#' @param domain See \link{domains}.
#' @param autoDestroy If \code{TRUE} (the default), the observer will be
#'   automatically destroyed when its domain (if any) ends.
#' @param ignoreNULL Whether the action should be triggered (or value
#'   calculated, in the case of \code{eventReactive}) when the input is
#'   \code{NULL}. See Details.
#' @return \code{observeEvent} returns an observer reference class object (see
#'   \code{\link{observe}}). \code{eventReactive} returns a reactive expression
#'   object (see \code{\link{reactive}}).
#'
#' @seealso \code{\link{actionButton}}
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   ui <- fluidPage(
#'     column(4,
#'       numericInput("x", "Value", 5),
#'       br(),
#'       actionButton("button", "Show")
#'     ),
#'     column(8, tableOutput("table"))
#'   )
#'   server <- function(input, output) {
#'     # Take an action every time button is pressed;
#'     # here, we just print a message to the console
#'     observeEvent(input$button, {
#'       cat("Showing", input$x, "rows\n")
#'     })
#'     # Take a reactive dependency on input$button, but
#'     # not on any of the stuff inside the function
#'     df <- eventReactive(input$button, {
#'       head(cars, input$x)
#'     })
#'     output$table <- renderTable({
#'       df()
#'     })
#'   }
#'   shinyApp(ui=ui, server=server)
#' }
#'
#' @export
observeEvent <- function(eventExpr, handlerExpr,
  event.env = parent.frame(), event.quoted = FALSE,
  handler.env = parent.frame(), handler.quoted = FALSE,
  label=NULL, suspended=FALSE, priority=0, domain=getDefaultReactiveDomain(),
  autoDestroy = TRUE, ignoreNULL = TRUE) {

  eventFunc <- exprToFunction(eventExpr, event.env, event.quoted)
  if (is.null(label))
    label <- sprintf('observeEvent(%s)', paste(deparse(body(eventFunc)), collapse='\n'))
  eventFunc <- wrapFunctionLabel(eventFunc, "observeEventExpr", ..stacktraceon = TRUE)

  handlerFunc <- exprToFunction(handlerExpr, handler.env, handler.quoted)
  handlerFunc <- wrapFunctionLabel(handlerFunc, "observeEventHandler", ..stacktraceon = TRUE)

  invisible(observe({
    e <- eventFunc()

    if (ignoreNULL && isNullEvent(e)) {
      return()
    }

    isolate(handlerFunc())
  }, label = label, suspended = suspended, priority = priority, domain = domain,
    autoDestroy = TRUE, ..stacktraceon = FALSE))
}

#' @rdname observeEvent
#' @export
eventReactive <- function(eventExpr, valueExpr,
  event.env = parent.frame(), event.quoted = FALSE,
  value.env = parent.frame(), value.quoted = FALSE,
  label=NULL, domain=getDefaultReactiveDomain(),
  ignoreNULL = TRUE) {

  eventFunc <- exprToFunction(eventExpr, event.env, event.quoted)
  if (is.null(label))
    label <- sprintf('eventReactive(%s)', paste(deparse(body(eventFunc)), collapse='\n'))
  eventFunc <- wrapFunctionLabel(eventFunc, "eventReactiveExpr", ..stacktraceon = TRUE)

  handlerFunc <- exprToFunction(valueExpr, value.env, value.quoted)
  handlerFunc <- wrapFunctionLabel(handlerFunc, "eventReactiveHandler", ..stacktraceon = TRUE)

  invisible(reactive({
    e <- eventFunc()

    validate(need(
      !ignoreNULL || !isNullEvent(e),
      message = FALSE
    ))

    isolate(handlerFunc())
  }, label = label, domain = domain, ..stacktraceon = FALSE))
}

isNullEvent <- function(value) {
  is.null(value) || (inherits(value, 'shinyActionButtonValue') && value == 0)
}
