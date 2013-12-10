#' Make a random number generator repeatable
#' 
#' Given a function that generates random data, returns a wrapped version of 
#' that function that always uses the same seed when called. The seed to use can
#' be passed in explicitly if desired; otherwise, a random number is used.
#' 
#' @param rngfunc The function that is affected by the R session's seed.
#' @param seed The seed to set every time the resulting function is called.
#' @return A repeatable version of the function that was passed in.
#'   
#' @note When called, the returned function attempts to preserve the R session's
#'   current seed by snapshotting and restoring
#'   \code{\link[base]{.Random.seed}}.
#'
#' @examples
#' rnormA <- repeatable(rnorm)
#' rnormB <- repeatable(rnorm)
#' rnormA(3)  # [1]  1.8285879 -0.7468041 -0.4639111
#' rnormA(3)  # [1]  1.8285879 -0.7468041 -0.4639111
#' rnormA(5)  # [1]  1.8285879 -0.7468041 -0.4639111 -1.6510126 -1.4686924
#' rnormB(5)  # [1] -0.7946034  0.2568374 -0.6567597  1.2451387 -0.8375699
#' 
#' @export
repeatable <- function(rngfunc, seed = runif(1, 0, .Machine$integer.max)) {
  force(seed)
  
  function(...) {
    # When we exit, restore the seed to its original state
    if (exists('.Random.seed', where=globalenv())) {
      currentSeed <- get('.Random.seed', pos=globalenv())
      on.exit(assign('.Random.seed', currentSeed, pos=globalenv()))
    }
    else {
      on.exit(rm('.Random.seed', pos=globalenv()))
    }
    
    set.seed(seed)
    
    rngfunc(...)
  }
}

`%OR%` <- function(x, y) {
  ifelse(is.null(x) || is.na(x), y, x)
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

`%.%` <- function(x, y) {
  paste(x, y, sep='')
}

# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

knownContentTypes <- Map$new()
knownContentTypes$mset(
  html='text/html; charset=UTF-8',
  htm='text/html; charset=UTF-8',
  js='text/javascript',
  css='text/css',
  png='image/png',
  jpg='image/jpeg',
  jpeg='image/jpeg',
  gif='image/gif',
  svg='image/svg+xml',
  txt='text/plain',
  pdf='application/pdf',
  ps='application/postscript',
  xml='application/xml',
  m3u='audio/x-mpegurl',
  m4a='audio/mp4a-latm',
  m4b='audio/mp4a-latm',
  m4p='audio/mp4a-latm',
  mp3='audio/mpeg',
  wav='audio/x-wav',
  m4u='video/vnd.mpegurl',
  m4v='video/x-m4v',
  mp4='video/mp4',
  mpeg='video/mpeg',
  mpg='video/mpeg',
  avi='video/x-msvideo',
  mov='video/quicktime',
  ogg='application/ogg',
  swf='application/x-shockwave-flash',
  doc='application/msword',
  xls='application/vnd.ms-excel',
  ppt='application/vnd.ms-powerpoint',
  xlsx='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
  xltx='application/vnd.openxmlformats-officedocument.spreadsheetml.template',
  potx='application/vnd.openxmlformats-officedocument.presentationml.template',
  ppsx='application/vnd.openxmlformats-officedocument.presentationml.slideshow',
  pptx='application/vnd.openxmlformats-officedocument.presentationml.presentation',
  sldx='application/vnd.openxmlformats-officedocument.presentationml.slide',
  docx='application/vnd.openxmlformats-officedocument.wordprocessingml.document',
  dotx='application/vnd.openxmlformats-officedocument.wordprocessingml.template',
  xlam='application/vnd.ms-excel.addin.macroEnabled.12',
  xlsb='application/vnd.ms-excel.sheet.binary.macroEnabled.12')

getContentType <- function(ext, defaultType='application/octet-stream') {
  knownContentTypes$get(tolower(ext)) %OR% defaultType
}

# Create a zero-arg function from a quoted expression and environment
# @examples
# makeFunction(body=quote(print(3)))
makeFunction <- function(args = pairlist(), body, env = parent.frame()) {
  eval(call("function", args, body), env)
}

#' Convert an expression or quoted expression to a function
#'
#' This is to be called from another function, because it will attempt to get
#' an unquoted expression from two calls back.
#'
#' If expr is a quoted expression, then this just converts it to a function.
#' If expr is a function, then this simply returns expr (and prints a
#'   deprecation message).
#' If expr was a non-quoted expression from two calls back, then this will
#'   quote the original expression and convert it to a function.
#
#' @param expr A quoted or unquoted expression, or a function.
#' @param env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
#' @param caller_offset If specified, the offset in the callstack of the 
#'   functiont to be treated as the caller. 
#'
#' @examples
#' # Example of a new renderer, similar to renderText
#' # This is something that toolkit authors will do
#' renderTriple <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a function
#'   func <- shiny::exprToFunction(expr, env, quoted)
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#'
#' # Example of using the renderer.
#' # This is something that app authors will do.
#' values <- reactiveValues(A="text")
#'
#' \dontrun{
#' # Create an output object
#' output$tripleA <- renderTriple({
#'   values$A
#' })
#' }
#'
#' # At the R console, you can experiment with the renderer using isolate()
#' tripleA <- renderTriple({
#'   values$A
#' })
#'
#' isolate(tripleA())
#' # "text, text, text"
#'
#' @export
exprToFunction <- function(expr, env=parent.frame(2), quoted=FALSE, 
                           caller_offset=1) {
  # Get the quoted expr from two calls back
  expr_sub <- eval(substitute(substitute(expr)), parent.frame(caller_offset))

  # Check if expr is a function, making sure not to evaluate expr, in case it
  # is actually an unquoted expression.
  # If expr is a single token, then indexing with [[ will error; if it has multiple
  # tokens, then [[ works. In the former case it will be a name object; in the
  # latter, it will be a language object.
  if (!is.name(expr_sub) && expr_sub[[1]] == as.name('function')) {
    # Get name of function that called this function
    called_fun <- sys.call(-1 * caller_offset)[[1]]
    
    shinyDeprecated(msg = paste("Passing functions to '", called_fun,
      "' is deprecated. Please use expressions instead. See ?", called_fun,
      " for more information.", sep=""))
    return(expr)
  }

  if (quoted) {
    # expr is a quoted expression
    makeFunction(body=expr, env=env)
  } else {
    # expr is an unquoted expression
    makeFunction(body=expr_sub, env=env)
  }
}

#' Installs an expression in the given environment as a function, and registers
#' debug hooks so that breakpoints may be set in the function.
#' 
#' This function can replace \code{exprToFunction} as follows: we may use
#' \code{func <- exprToFunction(expr)} if we do not want the debug hooks, or
#' \code{installExprFunction(expr, "func")} if we do. Both approaches create a
#' function named \code{func} in the current environment.
#' 
#' @seealso Wraps \code{exprToFunction}; see that method's documentation for 
#'   more documentation and examples.
#'   
#' @param expr A quoted or unquoted expression
#' @param name The name the function should be given 
#' @param eval.env The desired environment for the function. Defaults to the
#'   calling environment two steps back. 
#' @param quoted Is the expression quoted?
#' @param assign.env The environment in which the function should be assigned.
#' @param label A label for the object to be shown in the debugger. Defaults
#'   to the name of the calling function. 
#' 
#' @export
installExprFunction <- function(expr, name, eval.env = parent.frame(2), 
                                quoted = FALSE, 
                                assign.env = parent.frame(1),
                                label = as.character(sys.call(-1)[[1]])) {
  func <- exprToFunction(expr, eval.env, quoted, 2)
  assign(name, func, envir = assign.env)
  registerDebugHook(name, assign.env, label)
}

#' Parse a GET query string from a URL
#'
#' Returns a named character vector of key-value pairs.
#'
#' @param str The query string. It can have a leading \code{"?"} or not.
#' @export
#' @examples
#' parseQueryString("?foo=1&bar=b%20a%20r")
#'
#' \dontrun{
#' # Example of usage within a Shiny app
#' shinyServer(function(input, output, clientData) {
#'
#'   output$queryText <- renderText({
#'     query <- parseQueryString(clientData$url_search)
#'
#'     # Ways of accessing the values
#'     if (as.numeric(query$foo) == 1) {
#'       # Do something
#'     }
#'     if (query[["bar"]] == "targetstring") {
#'       # Do something else
#'     }
#'
#'     # Return a string with key-value pairs
#'     paste(names(query), query, sep = "=", collapse=", ")
#'   })
#' })
#' }
#'
parseQueryString <- function(str) {
  if (is.null(str) || nchar(str) == 0)
    return(list())

  # Remove leading ?
  if (substr(str, 1, 1) == '?')
    str <- substr(str, 2, nchar(str))

  pairs <- strsplit(str, '&', fixed = TRUE)[[1]]
  pairs <- strsplit(pairs, '=', fixed = TRUE)

  keys   <- vapply(pairs, function(x) x[1], FUN.VALUE = character(1))
  values <- vapply(pairs, function(x) x[2], FUN.VALUE = character(1))
  # Replace NA with '', so they don't get converted to 'NA' by URLdecode
  values[is.na(values)] <- ''

  # Convert "+" to " ", since URLdecode doesn't do it
  keys   <- gsub('+', ' ', keys,   fixed = TRUE)
  values <- gsub('+', ' ', values, fixed = TRUE)

  keys   <- vapply(keys,   function(x) URLdecode(x), FUN.VALUE = character(1))
  values <- vapply(values, function(x) URLdecode(x), FUN.VALUE = character(1))

  setNames(as.list(values), keys)
}

# decide what to do in case of errors; it is customizable using the shiny.error
# option (e.g. we can set options(shiny.error = recover))
shinyCallingHandlers <- function(expr) {
  withCallingHandlers(expr, error = function(e) {
    handle <- getOption('shiny.error')
    if (is.function(handle)) handle()
  })
}

#' Print message for deprecated functions in Shiny
#'
#' To disable these messages, use \code{options(shiny.deprecation.messages=FALSE)}.
#'
#' @param new Name of replacement function.
#' @param msg Message to print. If used, this will override the default message.
#' @param old Name of deprecated function.
shinyDeprecated <- function(new=NULL, msg=NULL,
                            old=as.character(sys.call(sys.parent()))[1L]) {

  if (getOption("shiny.deprecation.messages", default=TRUE) == FALSE)
    return(invisible())

  if (is.null(msg)) {
    msg <- paste(old, "is deprecated.")
    if (!is.null(new))
      msg <- paste(msg, "Please use", new, "instead.",
        "To disable this message, run options(shiny.deprecation.messages=FALSE)")
  }
  # Similar to .Deprecated(), but print a message instead of warning
  message(msg)
}

#' Register a function with the debugger (if one is active). 
#' 
#' Call this function after exprToFunction to give any active debugger a hook
#' to set and clear breakpoints in the function. A debugger may implement
#' registerShinyDebugHook to receive callbacks when Shiny functions are 
#' instantiated at runtime. 
#'
#' @param name Name of the field or object containing the function. 
#' @param where The reference object or environment containing the function.
#' @param label A label to display on the function in the debugger.
#' @noRd
registerDebugHook <- function(name, where, label) {
  if (exists("registerShinyDebugHook", mode = "function")) {
    registerShinyDebugHook <- get("registerShinyDebugHook", mode = "function")
    params <- new.env(parent = emptyenv())
    params$name <- name
    params$where <- where
    params$label <- label
    registerShinyDebugHook(params)
  }
}

Callbacks <- setRefClass(
  'Callbacks',
  fields = list(
    .nextId = 'integer',
    .callbacks = 'Map'
  ),
  methods = list(
    initialize = function() {
      .nextId <<- as.integer(.Machine$integer.max)
    },
    register = function(callback) {
      id <- as.character(.nextId)
      .nextId <<- .nextId - 1L
      .callbacks$set(id, callback)
      return(function() {
        .callbacks$remove(id)
      })
    },
    invoke = function(..., onError=NULL) {
      for (callback in .callbacks$values()) {
        if (is.null(onError)) {
          callback(...)
        } else {
          tryCatch(callback(...), error = onError)
        }
      }
    },
    count = function() {
      .callbacks$size()
    }
  )
)

# convert a data frame to JSON as required by DataTables request
dataTablesJSON <- function(data, query) {
  n <- nrow(data)
  with(parseQueryString(query), {
    # global searching
    i <- seq_len(n)
    if (nzchar(sSearch)) {
      i0 <- apply(data, 2, function(x) grep(sSearch, as.character(x)))
      i <- intersect(i, unique(unlist(i0)))
    }
    # search by columns
    if (length(i)) for (j in seq_len(as.integer(iColumns)) - 1) {
      if (is.null(k <- get_exists(sprintf('sSearch_%d', j), 'character'))) next
      if (nzchar(k)) i <- intersect(grep(k, as.character(data[, j + 1])), i)
      if (length(i) == 0) break
    }
    if (length(i) != n) data <- data[i, , drop = FALSE]
    # sorting
    oList <- list()
    for (j in seq_len(as.integer(iSortingCols)) - 1) {
      if (is.null(k <- get_exists(sprintf('iSortCol_%d', j), 'character'))) break
      desc = get_exists(sprintf('sSortDir_%d', j), 'character')
      if (is.character(desc)) {
        col <- data[, as.integer(k) + 1]
        oList[[length(oList) + 1]] <- (if (desc == 'asc') identity else `-`)(
          if (is.numeric(col)) col else xtfrm(col)
        )
      }
    }
    if (length(oList)) {
      i <- do.call(order, oList)
      data <- data[i, , drop = FALSE]
    }
    # paging
    i <- seq(as.integer(iDisplayStart) + 1L, length.out = as.integer(iDisplayLength))
    i <- i[i <= n]
    fdata <- data[i, , drop = FALSE]  # filtered data
    fdata <- unname(as.matrix(fdata))
    if (nrow(fdata) == 0) fdata = list()

    toJSON(list(
      sEcho = as.integer(sEcho),
      iTotalRecords = n,
      iTotalDisplayRecords = nrow(data),
      aaData = fdata
    ))
  })
}

get_exists = function(x, mode) {
  if (exists(x, envir = parent.frame(), mode = mode, inherits = FALSE))
    get(x, envir = parent.frame(), mode = mode, inherits = FALSE)
}
