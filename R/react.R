# TESTS
# Simple set/get
# Simple remove
# Simple contains.key
# Simple keys
# Simple values
# Simple clear
# Get of unknown key returns NULL
# Remove of unknown key does nothing
# Setting a key twice always results in last-one-wins
# /TESTS
Map <- setRefClass(
  'Map',
  fields = list(
    .env = 'environment'
  ),
  methods = list(
    initialize = function() {
      .env <<- new.env(parent=emptyenv())
    },
    get = function(key) {
      if (.self$contains.key(key))
        return(base::get(key, pos=.env, inherits=F))
      else
        return(NULL)
    },
    set = function(key, value) {
      assign(key, value, pos=.env, inherits=F)
      return(value)
    },
    remove = function(key) {
      if (.self$contains.key(key)) {
        result <- .self$get(key)
        rm(list = key, pos=.env, inherits=F)
        return(result)
      }
      return(NULL)
    },
    contains.key = function(key) {
      exists(key, where=.env, inherits=F)
    },
    keys = function() {
      ls(envir=.env, all.names=T)
    },
    values = function() {
      mget(.self$keys(), envir=.env, inherits=F)
    },
    clear = function() {
      .env <<- new.env(parent=emptyenv())
      invisible(NULL)
    },
    size = function() {
      length(.env)
    }
  )
)

`[.Map` <- function(map, name) {
  map$get(name)
}

`[<-.Map` <- function(map, name, value) {
  map$set(name, value)
  return(map)
}

as.list.Map <- function(map) {
  sapply(map$keys(),
         map$get,
         simplify=F)
}
length.Map <- function(map) {
  map$size()
}

Context <- setRefClass(
  'Context',
  fields = list(
    id = 'character',
    .invalidated = 'logical',
    .callbacks = 'list'
  ),
  methods = list(
    initialize = function() {
      id <<- .get.reactive.environment()$next.id()
      .invalidated <<- F
      .callbacks <<- list()
    },
    run = function(func) {
      env <- .get.reactive.environment()
      old.ctx <- env$current.context(warn=F)
      env$set.current.context(.self)
      on.exit(env$set.current.context(old.ctx))
      func()
    },
    invalidate = function() {
      if (.invalidated)
        return()
      .invalidated <<- T
      .get.reactive.environment()$add.pending.invalidate(.self)
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
  fields = c('.current.context', '.next.id', '.pending.invalidate'),
  methods = list(
    initialize = function() {
      .current.context <<- NULL
      .next.id <<- 0L
      .pending.invalidate <<- list()
    },
    next.id = function() {
      .next.id <<- .next.id + 1L
      return(as.character(.next.id))
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
      .values <<- new.env(parent=emptyenv())
      .dependencies <<- new.env(parent=emptyenv())
    },
    get = function(key) {
      ctx <- .get.reactive.environment()$current.context()
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

make.values.accessor <- function(values) {
  acc <- list(impl=values)
  class(acc) <- 'ValuesAccessor'
  acc
}
`$.ValuesAccessor` <- function(acc, name) {
  acc[['impl']]$get(name)
}

Observable <- setRefClass(
  'Observable',
  fields = c(
    '.func',         # function
    '.dependencies', # Map
    '.initialized',  # logical
    '.value'         # any
  ),
  methods = list(
    initialize = function(func) {
      .func <<- func
      .dependencies <<- Map$new()
      .initialized <<- F
    },
    get.value = function() {
      if (!.initialized) {
        .initialized <<- T
        .self$.update.value()
      }
      
      ctx <- .get.reactive.environment()$current.context()
      if (!.dependencies$contains.key(ctx$id)) {
        .dependencies$set(ctx$id, ctx)
        ctx$on.invalidate(function() {
          .dependencies$remove(ctx$id)
        })
      }
      return(.value)
    },
    .update.value = function() {
      old.value <- .value
      
      ctx <- Context$new()
      ctx$on.invalidate(function() {
        .self$.update.value()
      })
      ctx$run(function() {
        .value <<- .func()
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

observable <- function(func) {
  Observable$new(func)
}

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

.get.reactive.environment <- function() {
  if (!exists('.ReactiveEnvironment', envir=.GlobalEnv, inherits=F)) {
    assign('.ReactiveEnvironment', ReactiveEnvironment$new(), envir=.GlobalEnv)
  }
  get('.ReactiveEnvironment', envir=.GlobalEnv, inherits=F)
}

flush.react <- function() {
  .get.reactive.environment()$flush()
}

test <- function () {
  values <- Values$new()
  obs <- Observer$new(function() {print(values$get('foo'))})
  flush.react()
  values$set('foo', 'bar')
  flush.react()
  
  values$set('a', 100)
  values$set('b', 250)
  observable <- Observable$new(function() {
    values$get('a') + values$get('b')
  })
  obs2 <- Observer$new(function() {print(paste0('a+b: ', observable$get.value()))})
  flush.react()
  values$set('b', 300)
  flush.react()
  values$mset(list(a = 10, b = 20))
  flush.react()
}
