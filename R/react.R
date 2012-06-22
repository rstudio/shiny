Context <- setRefClass(
  'Context',
  fields = list(
    id = 'integer',
    .invalidated = 'logical',
    .callbacks = 'list'
  ),
  methods = list(
    initialize = function() {
      id <<- get.reactive.environment()$next.id()
      .invalidated <<- F
      .callbacks <<- list()
    },
    run = function(func) {
      env <- get.reactive.environment()
      old.ctx <- env$current.context(warn=F)
      env$set.current.context(.self)
      on.exit(env$set.current.context(old.ctx))
      func()
    },
    invalidate = function() {
      if (.invalidated)
        return()
      .invalidated <<- T
      get.reactive.environment()$add.pending.invalidate(.self)
      NULL
    },
    on.invalidate = function(func) {
      if (.invalidated)
        func()
      else
        .callbacks <<- c(.callbacks, func)
      NULL
    },
    execute.callbacks = function() {
      lapply(.callbacks, function(func) {
        func()
      })
    }
  )
)

ReactiveEnvironment <- setRefClass(
  'ReactiveEnvironment',
  fields = c('.current.context', '.next.id', '.pending.invalidate'),
  methods = list(
    initialize = function() {
      .current.context <<- NULL
      .next.id <<- 0L
      .pending.invalidate <<- list()
    },
    next.id = function() {
      .next.id <<- .next.id + 1L
      return(.next.id)
    },
    current.context = function(warn=T) {
      if (warn && is.null(.current.context))
        warning('No reactive context is active')
      return(.current.context)
    },
    set.current.context = function(ctx) {
      .current.context <<- ctx
    },
    add.pending.invalidate = function(ctx) {
      .pending.invalidate <<- c(.pending.invalidate, ctx)
    },
    flush = function() {
      while (length(.pending.invalidate) > 0) {
        contexts <- .pending.invalidate
        .pending.invalidate <<- list()
        lapply(contexts, function(ctx) {
          ctx$execute.callbacks()
          NULL
        })
      }
    }
  )
)

Values <- setRefClass(
  'Values',
  fields = list(
    .values = 'environment',
    .dependencies = 'environment'
  ),
  methods = list(
    initialize = function() {
      .values <<- new.env()
      .dependencies <<- new.env()
    },
    get = function(key) {
      ctx <- get.reactive.environment()$current.context()
      dep.key <- paste(key, ':', ctx$id, sep='')
      if (!exists(dep.key, where=.dependencies, inherits=F)) {
        assign(dep.key, ctx, pos=.dependencies, inherits=F)
        ctx$on.invalidate(function() {
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
        pattern=paste('^\\Q', key, ':', '\\E', '\\d+$', sep='')
      )
      lapply(
        mget(dep.keys, envir=.dependencies),
        function(ctx) {
          ctx$invalidate()
          NULL
        }
      )
      invisible()
    }
  )
)

Observer <- setRefClass(
  'Observer',
  fields = list(
    .func = 'function'
  ),
  methods = list(
    initialize = function(func) {
      .func <<- func
      .self$run()
    },
    run = function() {
      ctx <- Context$new()
      ctx$on.invalidate(function() {
        run()
      })
      ctx$run(.func)
    }
  )
)

get.reactive.environment <- function() {
  if (!exists('.ReactiveEnvironment', envir=.GlobalEnv, inherits=F)) {
    assign('.ReactiveEnvironment', ReactiveEnvironment$new(), envir=.GlobalEnv)
  }
  get('.ReactiveEnvironment', envir=.GlobalEnv, inherits=F)
}

test <- function () {
  values <- Values$new()
  obs <- Observer$new(function() {print(values$get('foo'))})
  values$set('foo', 'bar')
}
