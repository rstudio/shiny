processId <- local({
  # pid is not sufficient to uniquely identify a process, because
  # distributed futures span machines which could introduce pid
  # collisions.
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- rlang::hash(list(
        Sys.info(),
        Sys.time()
      ))
    }
    # Sys.getpid() cannot be cached because forked children will
    # then have the same processId as their parents.
    paste(cached, Sys.getpid())
  }
})

#' @include graph.R
Context <- R6Class(
  'Context',
  portable = FALSE,
  class = FALSE,
  public = list(
    id = character(0),
    .reactId = character(0),
    .reactType = "other",
    .label = character(0),      # For debug purposes
    .invalidated = FALSE,
    .invalidateCallbacks = list(),
    .flushCallbacks = list(),
    .domain = NULL,
    .pid = NULL,
    .weak = NULL,

    .isRecordingOtel = FALSE,
    .otelLabel = NULL,
    .otelAttrs = NULL,

    initialize = function(
      domain, label='', type='other', prevId='',
      reactId = rLog$noReactId,
      id = .getReactiveEnvironment()$nextId(), # For dummy context
      weak = FALSE
    ) {
      id <<- id
      .label <<- label
      .domain <<- domain
      .pid <<- processId()
      .reactId <<- reactId
      .reactType <<- type
      .weak <<- weak
      rLog$createContext(id, label, type, prevId, domain)
    },
    run = function(func) {
      "Run the provided function under this context."

      promises::with_promise_domain(reactivePromiseDomain(), {
        withReactiveDomain(.domain, {
          withCtxOspans(domain = .domain, {
            captureStackTraces({
              env <- .getReactiveEnvironment()
              rLog$enter(.reactId, id, .reactType, .domain)
              on.exit(rLog$exit(.reactId, id, .reactType, .domain), add = TRUE)
              env$runWith(self, func)
            })
          })
        })
      })
    },
    setOspanInfo = function(isRecordingOtel, otelLabel, otelAttrs) {
      self$.isRecordingOtel <- isRecordingOtel
      if (isRecordingOtel) {
        self$.otelLabel <- otelLabel
        self$.otelAttrs <- otelAttrs
      }
      invisible(self)
    },
    withCtxOspans = function(expr, domain) {
      if (!otel_is_tracing_enabled()) {
        return(force(expr))
      }

      # Always set the reactive update span as active
      # This ensures that any spans created within the reactive context
      # are at least children of the reactive update span
      with_reactive_update_active_ospan(domain = domain, {
        if (.isRecordingOtel) {
          with_shiny_ospan_async(.otelLabel, expr, attributes = .otelAttrs)
        } else {
          force(expr)
        }
      })
    },
    invalidate = function() {
      "Invalidate this context. It will immediately call the callbacks
        that have been registered with onInvalidate()."

      if (!identical(.pid, processId())) {
        rlang::abort("Reactive context was created in one process and invalidated from another.")
      }

      if (.invalidated)
        return()
      .invalidated <<- TRUE

      rLog$invalidateStart(.reactId, id, .reactType, .domain)
      on.exit(rLog$invalidateEnd(.reactId, id, .reactType, .domain), add = TRUE)

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

      if (!identical(.pid, processId())) {
        rlang::abort("Reactive context was created in one process and accessed from another.")
      }

      if (.invalidated)
        func()
      else
        .invalidateCallbacks <<- c(.invalidateCallbacks, func)
      NULL
    },
    addPendingFlush = function(priority) {
      "Tell the reactive environment that this context should be flushed the
        next time flushReact() called."
      .getReactiveEnvironment()$addPendingFlush(self, priority)
    },
    onFlush = function(func) {
      "Register a function to be called when this context is flushed."
      .flushCallbacks <<- c(.flushCallbacks, func)
    },
    executeFlushCallbacks = function() {
      "For internal use only."

      lapply(.flushCallbacks, function(flushCallback) {
        flushCallback()
      })
    },
    isWeak = function() {
      .weak
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
          rlang::abort(c(
            'Operation not allowed without an active reactive context.',
            paste0(
              'You tried to do something that can only be done from inside a ',
              'reactive consumer.'
            )
          ))
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
    # Returns TRUE if anything was actually called
    flush = function() {
      # If nothing to flush, exit early
      if (!hasPendingFlush()) return(invisible(FALSE))
      # If already in a flush, don't start another one
      if (.inFlush) return(invisible(FALSE))
      .inFlush <<- TRUE
      on.exit({
        .inFlush <<- FALSE
        rLog$idle(domain = NULL)
      })

      while (hasPendingFlush()) {
        ctx <- .pendingFlush$dequeue()
        ctx$executeFlushCallbacks()
      }

      invisible(TRUE)
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

# Causes any pending invalidations to run. Returns TRUE if any invalidations
# were pending (i.e. if work was actually done).
flushReact <- function() {
  return(.getReactiveEnvironment()$flush())
}

# Retrieves the current reactive context, or errors if there is no reactive
# context active at the moment.
getCurrentContext <- function() {
  .getReactiveEnvironment()$currentContext()
}
hasCurrentContext <- function() {
  !is.null(.getReactiveEnvironment()$.currentContext) ||
    isTRUE(getOption("shiny.suppressMissingContextError"))
}

getDummyContext <- function() {
  Context$new(
    getDefaultReactiveDomain(), '[none]', type = 'isolate',
    id = "Dummy", reactId = rLog$dummyReactId
  )
}

wrapForContext <- function(func, ctx) {
  force(func)
  force(ctx) # may be NULL (in the case of maskReactiveContext())

  function(...) {
    .getReactiveEnvironment()$runWith(ctx, function() {
      func(...)
    })
  }
}

reactivePromiseDomain <- function() {
  promises::new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      force(onFulfilled)

      # ctx will be NULL if we're in a maskReactiveContext()
      ctx <- if (hasCurrentContext()) getCurrentContext() else NULL

      wrapForContext(onFulfilled, ctx)
    },
    wrapOnRejected = function(onRejected) {
      force(onRejected)

      # ctx will be NULL if we're in a maskReactiveContext()
      ctx <- if (hasCurrentContext()) getCurrentContext() else NULL

      wrapForContext(onRejected, ctx)
    }
  )
}
