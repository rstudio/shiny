# Return the current time, in milliseconds from epoch, with
# unspecified time zone.
getNow <- function() {
  as.numeric(Sys.time()) * 1000
}

BaseTimerCallbacks <- R6Class(
  'TimerCallbacks',
  portable = FALSE,
  class = FALSE,
  public = list(
    .nextId = 0L,
    .funcs = 'Map',
    .times = data.frame(),
    .now = 'Function',

    initialize = function(nowFn=getNow) {
      .funcs <<- Map$new()
      .now <<- nowFn
    },
    clear = function() {
      .nextId <<- 0L
      .funcs$clear()
      .times <<- data.frame()
    },
    schedule = function(millis, func) {
      # If args could fail to evaluate, let's make them do that before
      # we change any state
      force(millis)
      force(func)

      id <- .nextId
      .nextId <<- .nextId + 1L

      t <- .now()

      # TODO: Horribly inefficient, use a heap instead
      .times <<- rbind(.times, data.frame(time=t+millis,
                                          scheduled=t,
                                          id=id))
      .times <<- .times[order(.times$time),]

      .funcs$set(as.character(id), func)

      return(id)
    },
    unschedule = function(id) {
      toRemoveIndices <- .times$id %in% id
      toRemoveIds <- .times[toRemoveIndices, "id", drop = TRUE]
      if (length(toRemoveIds) > 0) {
        .times <<- .times[!toRemoveIndices,]
        for (toRemoveId in as.character(toRemoveIds)) {
          .funcs$remove(toRemoveId)
        }
      }
      return(id %in% toRemoveIds)
    },
    timeToNextEvent = function() {
      if (dim(.times)[1] == 0)
        return(Inf)
      return(.times[1, 'time'] - .now())
    },
    takeElapsed = function() {
      t <- .now()
      elapsed <- .times$time < .now()
      result <- .times[elapsed,]
      .times <<- .times[!elapsed,]

      # TODO: Examine scheduled column to check if any funny business
      #       has occurred with the system clock (e.g. if scheduled
      #       is later than .now())

      return(result)
    },
    executeElapsed = function() {
      elapsed <- takeElapsed()
      if (nrow(elapsed) == 0)
        return(FALSE)

      for (id in elapsed$id) {
        thisFunc <- .funcs$remove(as.character(id))
        # TODO: Catch exception, and...?
        # TODO: Detect NULL, and...?
        thisFunc()
      }
      return(TRUE)
    }
  )
)

TimerCallbacks <- R6Class(
  'TimerCallbacks',
  inherit=BaseTimerCallbacks,
  portable = FALSE,
  class = FALSE,
  public = list(
    # Empty constructor defaults to the getNow implementation
    initialize = function() {
      super$initialize(getNow)
    }
  )
)

MockableTimerCallbacks <- R6Class(
  'MockableTimerCallbacks',
  inherit=BaseTimerCallbacks,
  portable = FALSE,
  class = FALSE,
  public = list(
    # Empty constructor defaults to the getNow implementation
    initialize = function() {
      super$initialize(self$now)
    },
    now = function(){
      return(private$time)
    },
    elapse = function(millis){
      private$time <<- private$time + millis
    },
    getElapsed = function(){
      private$time
    }
  ), private = list(
    time = 0L
  )
)

timerCallbacks <- TimerCallbacks$new()

scheduleTask <- function(millis, callback) {
  cancelled <- FALSE
  id <- timerCallbacks$schedule(millis, callback)

  function() {
    invisible(timerCallbacks$unschedule(id))
  }
}

#' Get a scheduler function for scheduling tasks. Give priority to the
#' session scheduler, but if it doesn't exist, use the global one.
#' @noRd
defineScheduler <- function(session){
  if (!is.null(session)){
    if (!is.null(session$scheduleTask)){
      return(session$scheduleTask)
    }
  }
  scheduleTask
}


#' Get the current time a la `Sys.time()`. Prefer to get it via the
#' `session$now()` function, but if that's not available, just return the
#' current system time.
#' @noRd
getTime <- function(session){
  if (!is.null(session)){
    if (!is.null(session$now)){
      return(session$now())
    }
  }
  Sys.time()
}
