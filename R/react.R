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
    initialize = function(label='') {
      id <<- .getReactiveEnvironment()$nextId()
      .invalidated <<- FALSE
      .invalidateCallbacks <<- list()
      .flushCallbacks <<- list()
      .label <<- label
    },
    run = function(func) {
      "Run the provided function under this context."
      env <- .getReactiveEnvironment()
      env$runWith(.self, func)
    },
    invalidate = function() {
      "Invalidate this context. It will immediately call the callbacks
        that have been registered with onInvalidate()."
      if (.invalidated)
        return()
      .invalidated <<- TRUE

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
    addPendingFlush = function() {
      "Tell the reactive environment that this context should be flushed the
        next time flushReact() called."
      .getReactiveEnvironment()$addPendingFlush(.self)
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
    .pendingFlush = 'list',
    .inFlush = 'logical'
  ),
  methods = list(
    initialize = function() {
      .currentContext <<- NULL
      .nextId <<- 0L
      .pendingFlush <<- list()
      .inFlush <<- FALSE
    },
    nextId = function() {
      .nextId <<- .nextId + 1L
      return(as.character(.nextId))
    },
    currentContext = function() {
      if (is.null(.currentContext))
        stop('Operation not allowed without an active reactive context. ',
             '(You tried to do something that can only be done from inside a ',
             'reactive function.)')
      return(.currentContext)
    },
    runWith = function(ctx, func) {
      old.ctx <- .currentContext
      .currentContext <<- ctx
      on.exit(.currentContext <<- old.ctx)
      func()
    },
    addPendingFlush = function(ctx) {
      .pendingFlush <<- c(ctx, .pendingFlush)
    },
    flush = function() {
      # If already in a flush, don't start another one
      if (.inFlush) return()
      .inFlush <<- TRUE
      on.exit(.inFlush <<- FALSE)

      while (length(.pendingFlush) > 0) {
        ctx <- .pendingFlush[[1]]
        .pendingFlush <<- .pendingFlush[-1]
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
