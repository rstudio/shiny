Stack <- R6Class(
  'Stack',
  portable = FALSE,
  public = list(
    .stack = list(),

    push = function(obj) {
      .stack <<- c(.stack, list(obj))
      invisible(self)
    },
    pop = function() {
      len <- length(.stack) 
      if (len == 0)
        return(NULL)
      obj <- .stack[[len]]
      .stack <<- .stack[-len]
      obj
    },
    peek = function() {
      len <- length(.stack) 
      if (len == 0) return(NULL)
      .stack[[len]]
    },
    size = function() {
      length(.stack)
    }
  )
)
