writeReactLog <- function(file=stdout()) {
  cat(RJSONIO::toJSON(.graphEnv$log, pretty=TRUE), file=file)
}

#' Reactive Log Visualizer
#' 
#' Provides an interactive browser-based tool for visualizing reactive 
#' dependencies and execution in your application.
#' 
#' To use the reactive log visualizer, start with a fresh R session and 
#' run the command \code{options(shiny.reactlog=TRUE)}; then launch your
#' application in the usual way (e.g. using \code{\link{runApp}}). At 
#' any time you can hit Ctrl+F3 (or for Mac users, Command+F3) in your 
#' web browser to launch the reactive log visualization.
#' 
#' The reactive log visualization only includes reactive activity up 
#' until the time the report was loaded. If you want to see more recent 
#' activity, refresh the browser.
#' 
#' Note that Shiny does not distinguish between reactive dependencies 
#' that "belong" to one Shiny user session versus another, so the
#' visualization will include all reactive activity that has taken place
#' in the process, not just for a particular application or session.
#' 
#' As an alternative to pressing Ctrl/Command+F3--for example, if you 
#' are using reactives outside of the context of a Shiny 
#' application--you can run the \code{showReactLog} function, which will
#' generate the reactive log visualization as a static HTML file and 
#' launch it in your default browser. In this case, refreshing your 
#' browser will not load new activity into the report; you will need to 
#' call \code{showReactLog()} explicitly.
#' 
#' For security and performance reasons, do not enable 
#' \code{shiny.reactlog} in production environments. When the option is 
#' enabled, it's possible for any user of your app to see at least some 
#' of the source code of your reactive expressions and observers.
#' 
#' @export
showReactLog <- function() {
  browseURL(renderReactLog())
}

renderReactLog <- function() {
  templateFile <- system.file('www/reactive-graph.html', package='shiny')
  html <- paste(readLines(templateFile, warn=FALSE), collapse='\r\n')
  tc <- textConnection(NULL, 'w')
  on.exit(close(tc))
  writeReactLog(tc)
  cat('\n', file=tc)
  flush(tc)
  html <- sub('__DATA__', paste(textConnectionValue(tc), collapse='\r\n'), html, fixed=TRUE)
  file <- tempfile(fileext = '.html')
  writeLines(html, file)
  return(file)
}

.graphAppend <- function(logEntry) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphEnv$log <- c(.graphEnv$log, list(logEntry))
}

.graphDependsOn <- function(id, label) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='dep', id=id, dependsOn=label))
}

.graphDependsOnId <- function(id, dependee) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='depId', id=id, dependsOn=dependee))
}

.graphCreateContext <- function(id, label, type, prevId) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(
      action='ctx', id=id, label=paste(label, collapse='\n'), type=type, prevId=prevId
    ))
}

.graphEnterContext <- function(id) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='enter', id=id))
}

.graphExitContext <- function(id) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='exit', id=id))
}

.graphValueChange <- function(label, value) {
  if (isTRUE(getOption('shiny.reactlog', FALSE))) {
    .graphAppend(list(
      action = 'valueChange',
      id = label,
      value = paste(capture.output(str(value)), collapse='\n')
    ))
  }
}

.graphInvalidate <- function(id) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='invalidate', id=id))
}

.graphEnv <- new.env()
.graphEnv$log <- list()
