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

#' @S3method $ reactvaluesreader
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

#' Wraps a normal function to create a reactive function.
#' 
#' A reactive function is a function that knows its result will change over
#' time.
#' 
#' 
#' 
#' Reactive values are values that can change over time.
#' 
#' Reactive functions are functions that can read reactive values and call other
#' reactive functions. Whenever a reactive value changes, any reactive functions
#' that depended on it will re-execute.
#' 
#' @param x The value or function to make reactive.
#' 
#' @export
reactive <- function(x) {
  UseMethod("reactive")
}
#' @S3method reactive function
reactive.function <- function(x) {
  return(Observable$new(x)$getValue)
}
#' @S3method reactive default
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

#' Creates a reactive timer with the given interval.
#' @param intervalMs Interval to fire, in milliseconds
#' @export
reactiveTimer <- function(intervalMs=1000) {
  dependencies <- Map$new()
  timerCallbacks$schedule(intervalMs, function() {
    timerCallbacks$schedule(intervalMs, sys.function())
    lapply(
      dependencies$values(),
      function(dep.ctx) {
        dep.ctx$invalidate()
        NULL
      })
  })
  return(function() {
    ctx <- .getReactiveEnvironment()$currentContext()
    if (!dependencies$containsKey(ctx$id)) {
      dependencies$set(ctx$id, ctx)
      ctx$onInvalidate(function() {
        dependencies$remove(ctx$id)
      })
    }
  })
}

.test <- function () {
  values <- Values$new()
  obs <- Observer$new(function() {print(values$get('foo'))})
  flushReact()
  values$set('foo', 'bar')
  flushReact()
  
  values$set('a', 100)
  values$set('b', 250)
  observable <- Observable$new(function() {
    values$get('a') + values$get('b')
  })
  obs2 <- Observer$new(function() {print(paste0('a+b: ', observable$getValue()))})
  flushReact()
  values$set('b', 300)
  flushReact()
  values$mset(list(a = 10, b = 20))
  flushReact()
}
