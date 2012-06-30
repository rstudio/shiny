# TESTS
# Simple set/get
# Simple remove
# Simple containsKey
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
      if (.self$containsKey(key))
        return(base::get(key, pos=.env, inherits=F))
      else
        return(NULL)
    },
    set = function(key, value) {
      assign(key, value, pos=.env, inherits=F)
      return(value)
    },
    remove = function(key) {
      if (.self$containsKey(key)) {
        result <- .self$get(key)
        rm(list = key, pos=.env, inherits=F)
        return(result)
      }
      return(NULL)
    },
    containsKey = function(key) {
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
      id <<- .getReactiveEnvironment()$nextId()
      .invalidated <<- F
      .callbacks <<- list()
    },
    run = function(func) {
      env <- .getReactiveEnvironment()
      old.ctx <- env$currentContext(warn=F)
      env$set.currentContext(.self)
      on.exit(env$set.currentContext(old.ctx))
      func()
    },
    invalidate = function() {
      if (.invalidated)
        return()
      .invalidated <<- T
      .getReactiveEnvironment()$addPendingInvalidate(.self)
      NULL
    },
    onInvalidate = function(func) {
      if (.invalidated)
        func()
      else
        .callbacks <<- c(.callbacks, func)
      NULL
    },
    executeCallbacks = function() {
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
    currentContext = function(warn=T) {
      if (warn && is.null(.currentContext))
        warning('No reactive context is active')
      return(.currentContext)
    },
    set.currentContext = function(ctx) {
      .currentContext <<- ctx
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
      ctx <- .getReactiveEnvironment()$currentContext()
      dep.key <- paste(key, ':', ctx$id, sep='')
      if (!exists(dep.key, where=.dependencies, inherits=F)) {
        assign(dep.key, ctx, pos=.dependencies, inherits=F)
        ctx$onInvalidate(function() {
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

.createValuesReader <- function(values) {
  acc <- list(impl=values)
  class(acc) <- 'reactvaluesreader'
  return(acc)
}
`$.reactvaluesreader` <- function(x, name) {
  x[['impl']]$get(name)
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
    getValue = function() {
      if (!.initialized) {
        .initialized <<- T
        .self$.updateValue()
      }
      
      ctx <- .getReactiveEnvironment()$currentContext()
      if (!.dependencies$containsKey(ctx$id)) {
        .dependencies$set(ctx$id, ctx)
        ctx$onInvalidate(function() {
          .dependencies$remove(ctx$id)
        })
      }
      return(.value)
    },
    .updateValue = function() {
      old.value <- .value
      
      ctx <- Context$new()
      ctx$onInvalidate(function() {
        .self$.updateValue()
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

reactive <- function(x) {
  UseMethod("reactive")
}
reactive.function <- function(func) {
  return(Observable$new(func)$getValue)
}
reactive.default <- function(x) {
  stop("Don't know how to make this value reactive!")
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
      ctx$onInvalidate(function() {
        run()
      })
      ctx$run(.func)
    }
  )
)

.getReactiveEnvironment <- function() {
  if (!exists('.ReactiveEnvironment', envir=.GlobalEnv, inherits=F)) {
    assign('.ReactiveEnvironment', ReactiveEnvironment$new(), envir=.GlobalEnv)
  }
  get('.ReactiveEnvironment', envir=.GlobalEnv, inherits=F)
}

flush.react <- function() {
  .getReactiveEnvironment()$flush()
}

.test <- function () {
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
  obs2 <- Observer$new(function() {print(paste0('a+b: ', observable$getValue()))})
  flush.react()
  values$set('b', 300)
  flush.react()
  values$mset(list(a = 10, b = 20))
  flush.react()
}
