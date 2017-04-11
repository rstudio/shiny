Context <- R6Class(
  'Context',
  portable = FALSE,
  class = FALSE,
  public = list(
    id = character(0),
    .label = character(0),      # For debug purposes
    .invalidated = FALSE,
    .invalidateCallbacks = list(),
    .flushCallbacks = list(),
    .domain = NULL,

    initialize = function(domain, label='', type='other', prevId='') {
      id <<- .getReactiveEnvironment()$nextId()
      .label <<- label
      .domain <<- domain
      .graphCreateContext(id, label, type, prevId, domain)
    },
    run = function(func) {
      "Run the provided function under this context."

      promise::with_promise_domain(reactivePromiseDomain(), {
        withReactiveDomain(.domain, {
          env <- .getReactiveEnvironment()
          .graphEnterContext(id)
          on.exit(.graphExitContext(id), add = TRUE)
          env$runWith(self, func)
        })
      })
    },
    invalidate = function() {
      "Invalidate this context. It will immediately call the callbacks
        that have been registered with onInvalidate()."
      if (.invalidated)
        return()
      .invalidated <<- TRUE

      .graphInvalidate(id, .domain)
      lapply(.invalidateCallbacks, function(func) {
        func()
      })
      .invalidateCallbacks <<- list()
      NULL
    },
    onInvalidate = function(func) {
      "Register a function to be called when this context is invalidated.
        If this context is already invalidated, the function is called
        immediately."
      if (.invalidated)
        func()
      else
        .invalidateCallbacks <<- c(.invalidateCallbacks, func)
      NULL
    },
    addPendingFlush = function(priority) {
      "Tell the reactive environment that this context should be flushed the
        next time flushReact() called."
      if (!is.null(.domain)) {
        .domain$incrementBusyCount()
      }
      .getReactiveEnvironment()$addPendingFlush(self, priority)
    },
    onFlush = function(func) {
      "Register a function to be called when this context is flushed."
      .flushCallbacks <<- c(.flushCallbacks, func)
    },
    executeFlushCallbacks = function() {
      "For internal use only."

      on.exit({
        if (!is.null(.domain)) {
          .domain$decrementBusyCount()
        }
      }, add = TRUE)

      lapply(.flushCallbacks, function(flushCallback) {
        flushCallback()
      })
    }
  )
)

ReactiveEnvironment <- R6Class(
  'ReactiveEnvironment',
  portable = FALSE,
  class = FALSE,
  public = list(
    .currentContext = NULL,
    .nextId = 0L,
    .pendingFlush = 'PriorityQueue',
    .inFlush = FALSE,

    initialize = function() {
      .pendingFlush <<- PriorityQueue$new()
    },
    nextId = function() {
      .nextId <<- .nextId + 1L
      return(as.character(.nextId))
    },
    currentContext = function() {
      if (is.null(.currentContext)) {
        if (isTRUE(getOption('shiny.suppressMissingContextError'))) {
          return(getDummyContext())
        } else {
          stop('Operation not allowed without an active reactive context. ',
               '(You tried to do something that can only be done from inside a ',
               'reactive expression or observer.)')
        }
      }
      return(.currentContext)
    },
    runWith = function(ctx, contextFunc) {
      old.ctx <- .currentContext
      .currentContext <<- ctx
      on.exit(.currentContext <<- old.ctx)
      contextFunc()
    },
    addPendingFlush = function(ctx, priority) {
      .pendingFlush$enqueue(ctx, priority)
    },
    hasPendingFlush = function() {
      return(!.pendingFlush$isEmpty())
    },
    flush = function() {
      # If already in a flush, don't start another one
      if (.inFlush) return()
      .inFlush <<- TRUE
      on.exit(.inFlush <<- FALSE)

      while (hasPendingFlush()) {
        ctx <- .pendingFlush$dequeue()
        ctx$executeFlushCallbacks()
      }
    }
  )
)

.getReactiveEnvironment <- local({
  reactiveEnvironment <- NULL
  function() {
    if (is.null(reactiveEnvironment))
      reactiveEnvironment <<- ReactiveEnvironment$new()
    return(reactiveEnvironment)
  }
})

# Causes any pending invalidations to run.
flushReact <- function() {
  .getReactiveEnvironment()$flush()
}

# Retrieves the current reactive context, or errors if there is no reactive
# context active at the moment.
getCurrentContext <- function() {
  .getReactiveEnvironment()$currentContext()
}

getDummyContext <- function() {}
local({
  dummyContext <- NULL
  getDummyContext <<- function() {
    if (is.null(dummyContext)) {
      dummyContext <<- Context$new(getDefaultReactiveDomain(), '[none]',
        type='isolate')
    }
    return(dummyContext)
  }
})

wrapForContext <- function(func, ctx) {
  force(func)
  force(ctx)

  function(...) {
    ctx$run(function() {
      captureStackTraces(
        func(...)
      )
    })
  }
}

reactivePromiseDomain <- function() {
  promise::new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      force(onFulfilled)
      ctx <- getCurrentContext()
      wrapForContext(onFulfilled, ctx)
    },
    wrapOnRejected = function(onRejected) {
      force(onRejected)
      ctx <- getCurrentContext()
      wrapForContext(onRejected, ctx)
    }
  )
}
