Context <- setRefClass(
  'Context',
  fields = list(
    id = 'character',
    .invalidated = 'logical',
    .callbacks = 'list',
    .hintCallbacks = 'list'
  ),
  methods = list(
    initialize = function() {
      id <<- .getReactiveEnvironment()$nextId()
      .invalidated <<- F
      .callbacks <<- list()
      .hintCallbacks <<- list()
    },
    run = function(func) {
      "Run the provided function under this context."
      env <- .getReactiveEnvironment()
      env$runWith(.self, func)
    },
    invalidateHint = function() {
      "Let this context know it may or may not be invalidated very soon; that
      is, something in its dependency graph has been invalidated but there's no
      guarantee that the cascade of invalidations will reach all the way here.
      This is used to show progress in the UI."
      lapply(.hintCallbacks, function(func) {
        func()
      })
    },
    invalidate = function() {
      "Schedule this context for invalidation. It will not actually be
        invalidated until the next call to \\code{\\link{flushReact}}."
      if (.invalidated)
        return()
      .invalidated <<- T
      .getReactiveEnvironment()$addPendingInvalidate(.self)
      NULL
    },
    onInvalidate = function(func) {
      "Register a function to be called when this context is invalidated.
        If this context is already invalidated, the function is called
        immediately."
      if (.invalidated)
        func()
      else
        .callbacks <<- c(.callbacks, func)
      NULL
    },
    onInvalidateHint = function(func) {
      .hintCallbacks <<- c(.hintCallbacks, func)
    },
    executeCallbacks = function() {
      "For internal use only."
      lapply(.callbacks, function(func) {
        tryCatch({
          func()
        }, warning = function(e) {
          # TODO: Callbacks in app
          print(e)
        }, error = function(e) {
          # TODO: Callbacks in app
          print(e)
        })
      })
    }
  )
)

ReactiveEnvironment <- setRefClass(
  'ReactiveEnvironment',
  fields = c('.currentContext', '.nextId', '.pendingInvalidate'),
  methods = list(
    initialize = function() {
      .currentContext <<- NULL
      .nextId <<- 0L
      .pendingInvalidate <<- list()
    },
    nextId = function() {
      .nextId <<- .nextId + 1L
      return(as.character(.nextId))
    },
    currentContext = function() {
      if (is.null(.currentContext))
        stop('No reactive context is active')
      return(.currentContext)
    },
    runWith = function(ctx, func) {
      old.ctx <- .currentContext
      .currentContext <<- ctx
      on.exit(.currentContext <<- old.ctx)
      func()
    },
    addPendingInvalidate = function(ctx) {
      .pendingInvalidate <<- c(.pendingInvalidate, ctx)
    },
    flush = function() {
      while (length(.pendingInvalidate) > 0) {
        contexts <- .pendingInvalidate
        .pendingInvalidate <<- list()
        lapply(contexts, function(ctx) {
          ctx$executeCallbacks()
          NULL
        })
      }
    }
  )
)

.getReactiveEnvironment <- function() {
  if (!exists('.ReactiveEnvironment', envir=.GlobalEnv, inherits=F)) {
    assign('.ReactiveEnvironment', ReactiveEnvironment$new(), envir=.GlobalEnv)
  }
  get('.ReactiveEnvironment', envir=.GlobalEnv, inherits=F)
}

#' Causes any pending invalidations to run.
flushReact <- function() {
  .getReactiveEnvironment()$flush()
}

#' Retrieves the current reactive context, or errors if there is no reactive
#' context active at the moment.
getCurrentContext <- function() {
  .getReactiveEnvironment()$currentContext()
}
