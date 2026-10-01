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

ctx_otel_info_obj <- function(
  isRecordingOtel = FALSE,
  otelLabel = "<unknown>",
  otelAttrs = list()
) {
  structure(
    list(
      isRecordingOtel = isRecordingOtel,
      otelLabel = otelLabel,
      otelAttrs = otelAttrs
    ),
    class = "ctx_otel_info"
  )
}

with_otel_span_context <- function(otel_info, expr, domain) {
  if (!otel_is_tracing_enabled()) {
    return(force(expr))
  }

  isRecordingOtel <- .subset2(otel_info, "isRecordingOtel")
  otelLabel <- .subset2(otel_info, "otelLabel")
  otelAttrs <- .subset2(otel_info, "otelAttrs")

  # Always set the reactive update span as active
  # This ensures that any spans created within the reactive context
  # are at least children of the reactive update span
  maybe_with_otel_span_reactive_update(domain = domain, {
    if (isRecordingOtel) {
      with_otel_span(
        otelLabel,
        {
          # Works with both sync and async expressions
          # Needed for both observer and reactive contexts
          hybrid_then(
            expr,
            on_failure = set_otel_exception_status_and_throw,
            # Must upgrade the error object
            tee = FALSE
          )
        },
        # expr,
        attributes = otelAttrs
      )
    } else {
      force(expr)
    }
  })

}




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
    # Edge-recording state for resume snapshots (see
    # snapshot_context_state()), or NULL: only the contexts of tracked
    # observers and reactive() expressions record.
    .snapshot = NULL,

    .otel_info = NULL,

    initialize = function(
      domain, label='', type='other', prevId='',
      reactId = rLog$noReactId,
      id = .getReactiveEnvironment()$nextId(), # For dummy context
      weak = FALSE,
      otel_info = ctx_otel_info_obj()
    ) {
      id <<- id
      .label <<- label
      .domain <<- domain
      .pid <<- processId()
      .reactId <<- reactId
      .reactType <<- type
      .weak <<- weak
      rLog$createContext(id, label, type, prevId, domain)
      if (!is.null(otel_info)) {
        if (IS_SHINY_LOCAL_PKG) {
          stopifnot(inherits(otel_info, "ctx_otel_info"))
        }
        .otel_info <<- otel_info
      }
    },
    run = function(func) {
      "Run the provided function under this context."

      # Use `promises::` as it shows up in the stack trace
      promises::with_promise_domain(reactivePromiseDomain(), {
        withReactiveDomain(.domain, {
          with_otel_span_context(.otel_info, domain = .domain, {
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

# What a recording context collects for the snapshot registry. An
# environment, so edges append in place.
# - upstream: the Dependents this context registered with, in registration
#   order, from which the registry computes an observer's transitive source
#   set. Holds them strongly; each holds its owner weakly.
# - adoptable, reason: cleared by context_not_adoptable() when the context
#   takes a dependency the snapshot cannot reproduce (invalidateLater(),
#   reactiveTimer(), an untracked reactive, a session URL). Observers
#   downstream of such a context are never adopted.
# - adopted: set on the context of an adopted observer.
# - settled: FALSE while the context runs and, if the run returns a promise,
#   until that promise settles. Continuations run under this context through
#   the promise domain and keep registering edges, so until then `upstream`
#   is incomplete. Continuations that are not returned from the body are
#   outside the reactive lifecycle and stay outside here. The context's owner
#   (Observer$run(), Observable$.updateValue()) maintains it, not
#   Context$run(), so that untracked runs pay nothing for it.
snapshot_context_state <- function() {
  state <- new.env(parent = emptyenv())
  state$upstream <- list()
  state$adoptable <- TRUE
  state$reason <- NA_character_
  state$adopted <- FALSE
  state$settled <- TRUE
  state
}

# Settles `state` once a run that produced `result` is over: now, or when a
# returned promise settles.
snapshot_settle <- function(state, result) {
  # is.object() first: only classed objects can be promising.
  if (is.object(result) && promises::is.promising(result)) {
    settle <- function(...) state$settled <- TRUE
    promises::then(result, onFulfilled = settle, onRejected = settle)
  } else {
    state$settled <- TRUE
  }
  invisible()
}

# A function, not a Context method: a context is created for every run, and
# each R6 method adds to that cost. The first reason wins; a context that does
# not record ignores it.
context_not_adoptable <- function(ctx, reason) {
  state <- ctx$.snapshot
  if (!is.null(state) && state$adoptable) {
    state$adoptable <- FALSE
    state$reason <- reason
  }
  invisible()
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
  new_promise_domain(
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
