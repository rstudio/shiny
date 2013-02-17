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
    
    do.call(rngfunc, list(...))
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
#'   deprecation message.
#' If expr was a non-quoted expression from two calls back, then this will
#'   quote the original expression and convert it to a function.
#
#' @param expr A quoted or unquoted expression, or a function.
#' @param env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
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
exprToFunction <- function(expr, env=parent.frame(2), quoted=FALSE) {
  # Get the quoted expr from two calls back
  expr_sub <- eval(substitute(substitute(expr)), parent.frame())

  # Check if expr is a function, making sure not to evaluate expr, in case it
  # is actually an unquoted expression.
  # If expr is a single token, then indexing with [[ will error; if it has multiple
  # tokens, then [[ works. In the former case it will be a name object; in the
  # latter, it will be a language object.
  if (!is.name(expr_sub) && expr_sub[[1]] == as.name('function')) {
    # Get name of function that called this function
    called_fun <- sys.call(-1)[[1]]

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
