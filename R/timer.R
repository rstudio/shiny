# Return the current time, in milliseconds from epoch, with
# unspecified time zone.
now <- function() {
  as.numeric(Sys.time()) * 1000
}

TimerCallbacks <- R6Class(
  'TimerCallbacks',
  portable = FALSE,
  class = FALSE,
  public = list(
    .nextId = 0L,
    .funcs = 'Map',
    .times = data.frame(),

    initialize = function() {
      .funcs <<- Map$new()
    },
    clear = function() {
      .nextId <<- 0L
      .funcs$clear()
      .times <<- data.frame()
    },
    schedule = function(millis, func) {
      id <- .nextId
      .nextId <<- .nextId + 1L

      t <- now()

      # TODO: Horribly inefficient, use a heap instead
      .times <<- rbind(.times, data.frame(time=t+millis,
                                          scheduled=t,
                                          id=id))
      .times <<- .times[order(.times$time),]

      .funcs$set(as.character(id), func)

      return(id)
    },
    timeToNextEvent = function() {
      if (dim(.times)[1] == 0)
        return(Inf)
      return(.times[1, 'time'] - now())
    },
    takeElapsed = function() {
      t <- now()
      elapsed <- .times$time < now()
      result <- .times[elapsed,]
      .times <<- .times[!elapsed,]

      # TODO: Examine scheduled column to check if any funny business
      #       has occurred with the system clock (e.g. if scheduled
      #       is later than now())

      return(result)
    },
    executeElapsed = function() {
      elapsed <- takeElapsed()
      if (length(elapsed) == 0)
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

timerCallbacks <- TimerCallbacks$new()
