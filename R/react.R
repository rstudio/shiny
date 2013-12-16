Context <- setRefClass(
  'Context',
  fields = list(
    id = 'character',
    .label = 'character',      # For debug purposes
    .invalidated = 'logical',
    .invalidateCallbacks = 'list',
    .flushCallbacks = 'list'
  ),
  methods = list(
    initialize = function(label='', type='other', prevId='') {
      id <<- .getReactiveEnvironment()$nextId()
      .invalidated <<- FALSE
      .invalidateCallbacks <<- list()
      .flushCallbacks <<- list()
      .label <<- label
      .graphCreateContext(id, label, type, prevId)
    },
    run = function(func) {
      "Run the provided function under this context."
      env <- .getReactiveEnvironment()
      .graphEnterContext(id)
      on.exit(.graphExitContext(id))
      env$runWith(.self, func)
    },
    invalidate = function() {
      "Invalidate this context. It will immediately call the callbacks
        that have been registered with onInvalidate()."
      if (.invalidated)
        return()
      .invalidated <<- TRUE

      .graphInvalidate(id)
      lapply(.invalidateCallbacks, function(func) {
        func()
      })
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
      .getReactiveEnvironment()$addPendingFlush(.self, priority)
    },
    onFlush = function(func) {
      "Register a function to be called when this context is flushed."
      .flushCallbacks <<- c(.flushCallbacks, func)
    },
    executeFlushCallbacks = function() {
      "For internal use only."
      lapply(.flushCallbacks, function(func) {
        withCallingHandlers({
          func()
        }, warning = function(e) {
          # TODO: Callbacks in app
        }, error = function(e) {
          # TODO: Callbacks in app
        })
      })
    }
  )
)

ReactiveEnvironment <- setRefClass(
  'ReactiveEnvironment',
  fields = list(
    .currentContext = 'ANY',
    .nextId = 'integer',
    .pendingFlush = 'PriorityQueue',
    .inFlush = 'logical'
  ),
  methods = list(
    initialize = function() {
      .currentContext <<- NULL
      .nextId <<- 0L
      .pendingFlush <<- PriorityQueue$new()
      .inFlush <<- FALSE
    },
    nextId = function() {
      .nextId <<- .nextId + 1L
      return(as.character(.nextId))
    },
    currentContext = function() {
      if (is.null(.currentContext)) {
        if (isTRUE(getOption('shiny.suppressMissingContextError', FALSE))) {
          return(dummyContext)
        } else {
          stop('Operation not allowed without an active reactive context. ',
               '(You tried to do something that can only be done from inside a ',
               'reactive expression or observer.)')
        }
      }
      return(.currentContext)
    },
    runWith = function(ctx, func) {
      old.ctx <- .currentContext
      .currentContext <<- ctx
      on.exit(.currentContext <<- old.ctx)
      shinyCallingHandlers(func())
    },
    addPendingFlush = function(ctx, priority) {
      .pendingFlush$enqueue(ctx, priority)
    },
    flush = function() {
      # If already in a flush, don't start another one
      if (.inFlush) return()
      .inFlush <<- TRUE
      on.exit(.inFlush <<- FALSE)

      while (!.pendingFlush$isEmpty()) {
        ctx <- .pendingFlush$dequeue()
        ctx$executeFlushCallbacks()
      }
    }
  )
)

.reactiveEnvironment <- ReactiveEnvironment$new()
.getReactiveEnvironment <- function() {
  .reactiveEnvironment
}

# Causes any pending invalidations to run.
flushReact <- function() {
  .getReactiveEnvironment()$flush()
}

# Retrieves the current reactive context, or errors if there is no reactive
# context active at the moment.
getCurrentContext <- function() {
  .getReactiveEnvironment()$currentContext()
}

delayedAssign("dummyContext", Context$new('[none]', type='isolate'))
